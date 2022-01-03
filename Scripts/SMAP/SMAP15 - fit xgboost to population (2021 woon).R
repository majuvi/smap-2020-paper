library(xgboost)
library(Matrix)
library(data.table)
library(tidytable)
options(na.action='na.pass')

ref_date <- "20180101"

# Load population and health monitor data
data.population <- readRDS(file=sprintf("Data/Populatiebestanden/popdata_%s_survey.rds", ref_date)) 
data.woon      <- readRDS(file=sprintf("Data/Populatiebestanden/woondata_%s.rds", ref_date))
merge.index     <- match(data.population$rinpersoon, data.woon$rinpersoon)
options(na.action='na.pass')

# Construct oblique geographic coordinates (ogc) by adding columns x1, ..., xn
n <- 24
theta <- seq(from=0, to=pi*(n-1)/n, by=pi/n)
x <- data.population[["x_coord"]]
y <- data.population[["y_coord"]]
for (i in 1:n) {
  xn <- paste("x", i, sep="")
  data.population[[xn]] <- sqrt(x^2 + y^2)*cos(theta[i]-atan(y/x))
}
ogc.string <- paste(paste("x", 1:n, sep=""), collapse=" + ")


# Train on observed y labels and we predict the missing labels
predict_y_bootstrap <- function (X, y, class.index, prediction.intervals="none", n.bootstrap = 50) {
  
  # Split the data into train and test set
  observation <- !is.na(y)
  X.train <- X[observation,]
  y.train <- y[observation]
  
  # Save all individual predictions temporarily here
  class.index$y_pred <- NA
  
  # Estimate the number of trees
  print("Estimating the number of trees (5-CV)...")
  start = Sys.time()
  set.seed(42)
  model <- xgb.cv(list(objective="reg:squarederror", eval_metric="rmse"), data=X.train, label=y.train, 
                  eta=0.1, nrounds=1000, verbose=0, nfold=5, stratified=T, early_stopping_rounds=10)
  optimal_nrounds = copy(model$best_iteration)
  rmse_validation = copy(model$evaluation_log$test_rmse_mean[model$best_iteration])
  print(sprintf("Optimal number of rounds: %s", optimal_nrounds))
  end = Sys.time()
  print(end-start)
  
  # Train a full model on original data to predict the probabilities
  print("Training the model...")
  start = Sys.time()
  model <- xgboost(data=X.train, label=y.train, objective="reg:squarederror", eta=0.1, 
                   nrounds=optimal_nrounds, verbose=0)
  # Predict all labels and fill the observed training labels 
  y.pred <- predict(model, X)
  y.pred[observation] <- y.train
  # Map predictions to the multiple classes and save the original model predictions
  class.index$y_pred <- y.pred[class.index$index]
  pred.class         <- class.index %>% summarise.(N = .N, y.model = mean(y_pred), .by = class, .sort=T)
  stats              <- pred.class %>% select.(class, N, y.model)
  end = Sys.time()
  print(end-start)
  
  if (prediction.intervals == "outcome") {
    # Prediction intervals of type "outcome" assume that the model is correct
    print("Calculating statistics...")
    start = Sys.time()
    # Calculate prediction intervals from implied outcome probabilities given residuals
    y.pred.se <- rep(rmse_validation, nrow(X))
    y.pred.se[observation] <- 0.0
    class.index$y_pred <- y.pred.se[class.index$index]
    stats.outcomes <- class.index %>% summarise.(N = .N, y.model.sd = sqrt(mean(y_pred**2)/.N), .by = class, .sort=T)
    # Merge together to get predicted counts in original model and implied outcomes
    stats = stats %>% 
      left_join.(stats.outcomes, by="class") %>% mutate.(
        y.outcomes.lower   = y.model + qnorm(0.025) * y.model.sd,
        y.outcomes.upper   = y.model + qnorm(0.975) * y.model.sd
      ) %>% select.(class, N, y.model, y.outcomes.lower, y.outcomes.upper) 
    end = Sys.time()
    print(end-start)
  } else if (prediction.intervals == "full") {
    # Otherwise we bootstrap resample the data set, fit model to each bootstrap and sample the predicted outcome
    print("Bootstrapping...")
    # This is where bootstrap and sampled outcome predictions are saved
    pred.bootstrap <- class.index %>% summarise.(N = .N, .by = class, .sort=T)

    set.seed(42)
    # Bootstrap resample the data, save average bootstrapped model prediction in training data
    y.train.bootstrap <- rep(0.0, length(y.train))
    for (n in 1:n.bootstrap) {
      print(sprintf("Bootstrap %d", n))
      
      # Bootstrap resample a new training set
      indices <- sample(1:nrow(X.train), replace=T)
      X.bootstrap <- X.train[indices,]
      y.bootstrap <- y.train[indices]
      
      # Train on boostrapped sample
      print("Training on bootstrapped sample...")
      start = Sys.time()
      model <- xgboost(objective="reg:squarederror", data=X.bootstrap, label=y.bootstrap, 
                       nrounds=optimal_nrounds, verbose=0)
      # Predict all labels and fill the observed training labels 
      y.pred <- predict(model, X)
      y.train.bootstrap <- y.train.bootstrap + y.pred[observation]
      y.pred[observation] <- y.train
      # Map predictions to the multiple classes and save the bootstrap sample results
      class.index$y_pred <- y.pred[class.index$index]
      pred.class         <- class.index %>% summarise.(y = mean(y_pred), .by = class)
      pred.bootstrap     <- pred.bootstrap %>% left_join.(pred.class, by = "class")
      end = Sys.time()
      print(end-start)
    }
    # Calculate residual from the bootstrapped mean
    y.train.bootstrap <- y.train.bootstrap / n.bootstrap
    rmse <- sd(y.train - y.train.bootstrap)
    y.pred.se <- rep(rmse, nrow(X))
    ## Option: train a model to predict the root mean squared error
    #y.train.se <- abs(y.train - y.train.bootstrap)
    #model <- xgboost(objective="reg:squarederror", data=X.train, label=y.train.se, nrounds=50, verbose=0)
    #y.pred.se <- predict(model, X)
    y.pred.se[observation] <- 0.0
    class.index$y_pred <- y.pred.se[class.index$index]
    stats.outcomes <- class.index %>% summarise.(N = .N, y.model.sd = sqrt(mean(y_pred**2)/.N), .by = class, .sort=T)
    
    print("Calculating statistics...")
    start = Sys.time()
    # Statistics from re-training the model in bootstrap samples
    stats.bootstrap <- pred.bootstrap %>% select.(-N) %>%
      mutate_rowwise.(y.bootstrap.mean = mean(c_across.(where(is.numeric))), 
                      y.bootstrap.sd   = sd(c_across.(where(is.numeric)))) %>%
      select.(class, y.bootstrap.mean, y.bootstrap.sd)
    # Merge together to get predicted expected value in original model, bootstrap
    # Shortcut: re-training the model in bootstrap samples and resampling the outcome from constant residual implies..
    stats = stats %>% 
      left_join.(stats.bootstrap, by="class") %>% 
      left_join.(stats.outcomes, by="class") %>% mutate.(
        y.outcomes.sd    = sqrt(y.bootstrap.sd ** 2 + y.model.sd ** 2)
      ) %>% mutate.(
        y.outcomes.lower = y.model + qnorm(0.025) * y.outcomes.sd,
        y.outcomes.upper = y.model + qnorm(0.975) * y.outcomes.sd
      ) %>% select.(class, N, y.model, y.bootstrap.mean, y.bootstrap.sd, 
                   y.outcomes.sd, y.outcomes.lower, y.outcomes.upper) 
    end = Sys.time()
    print(end-start)
  }
  
  return(stats)
}




# outcome: the file name (column: column from GEMON to use, subset: subset from population to use)
columns  = colnames(data.woon)
outcomes = columns[3:(length(columns))]

# Predict prevalence based on these variables
formula <- as.formula(sprintf("y ~ age + sex + ethnicity + marital_status + education + hhtype + hhsize + 
                              hhhomeownership + hhincomesource + hhincome + hhassets + oad + %s - 1",
                              ogc.string))
X = sparse.model.matrix(formula, data.population)

# Predict prevalence per age|gm/wk/bu_code classes
data.population$index = 1:nrow(data.population)
# Each age maps to the one or more age categories 18-65, 18+, 65+
map_age <- list("age" = c(seq(18, 64), seq(18, 120),  seq(65, 120)), 
                "age_class" = c(rep("18-65", 65-18), rep("18+", 121-18),  rep("65+", 121-65)))
# Create mapping rinpersoon (rowid) -> class (age, bu/wk/gm_code), where each person can belong to multiple classes
class.index <- merge(x=data.population[,c("index", "gm_code", "wk_code", "bu_code", "age")],
                     y=as.data.frame(map_age), by="age", all.x=T, allow.cartesian=TRUE)
class.index <- class.index %>% 
  pivot_longer.(c("bu_code", "gm_code", "wk_code"), values_to="geo_id") %>% 
  mutate.(class = paste(age_class %>% as.character, geo_id %>% as.character, sep="|") %>% as.factor) %>%
  select.(class, index) %>% arrange.(class)
class.index <- data.table(class = class.index$class, index = class.index$index)

# Loop over every health indicator and the corresponding subset of the population
apply_forloop <- function(outcomes, data.gemon, X, class.index, merge.index) {
  for (outcome in outcomes) {
    start = Sys.time()
    print(outcome)
    
    # Observed y
    y <- data.woon[[outcome]][merge.index]
    
    # Predictions for all of the classes
    y.predictions <- predict_y_bootstrap(X, y, class.index, prediction.intervals="full", n.bootstrap=50)
    write.csv(y.predictions, sprintf("results/smap/bootstrap_woon/%s.csv", outcome), row.names = F)
    
    end = Sys.time()
    print(end - start)
    gc()
  }
}

try(apply_forloop(outcomes, data.gemon, X, class.index, merge.index))


# Combine predictions to one file in the results folder
dfs <- list()
for (file in list.files("results/smap/bootstrap_woon")) {
  print(file)
  dfs[[file]] = read.table(file=file.path('results', 'smap', 'bootstrap_woon', file), sep=",", header=T)
}
dfs <- rbindlist(dfs, idcol="indicator")
# Add indicator of outcome based on file name and split age|geo_id to two columns
dfs <- dfs %>% 
  mutate.(indicator = substr(indicator, 1, nchar(indicator)-4)) %>% 
  separate.(class, c("age", "geo_id"), sep = "|")
# Round to 3 decimal places
pred.cols <- colnames(dfs)[startsWith(colnames(dfs), "y.")]
dfs[, (pred.cols) := lapply(.SD, function(x) round(x, 1)), .SDcols=pred.cols]
# Remove counts < 10 and save
dfs <- as.data.frame(dfs)
dfs[(dfs$N < 10), c("N", pred.cols)] <- NA
fwrite(format(dfs[, c("indicator", "age", "geo_id", "N", pred.cols)], 4), 
          "results/smap/paper/predictions_xgboost_woon_full.csv", row.names = F, sep = ';')

library(xgboost)
library(Matrix)
library(data.table)
library(tidytable)
options(na.action='na.pass')

ref_date <- "20200901"

# Load population and health monitor data
data.population <- readRDS(file = sprintf("Data/Populatiebestanden/popdata_%s.rds", ref_date))
data.gemon      <- readRDS(file = sprintf("Data/Populatiebestanden/gemondata_%s.rds", ref_date))
merge.index     <- match(data.population$rinpersoon, data.gemon$rinpersoon) # reference to outcome y for each row
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

# Function to calculate Poisson Binomial distribution quantiles
qpoisbinomzero <- function(p, q) {
  if (length(p) < 1) { # Fix crashing for empty vector
    N <- as.integer(0)
  } else if (length(p) > 1000) { # For large N this is faster
    N <- as.integer(qnorm(q, sum(p), sqrt(sum(p*(1-p)))))
  } else {
    N <- as.integer(qpoisbinom(q, p))
  }
  return(N)
}

# Train on observed y labels and we predict the missing labels
predict_y_bootstrap <- function (X, y, class.index, prediction.intervals="none", n.bootstrap = 25, n.bootstrap.inner=40) {

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
  model <- xgb.cv(list(objective="binary:logistic", eval_metric="auc"), data=X.train, label=y.train, 
                  eta=0.1, nrounds=1000, verbose=0, nfold=5, stratified=T, early_stopping_rounds=10)
  optimal_nrounds = copy(model$best_iteration)
  print(sprintf("Optimal number of rounds: %s", optimal_nrounds))
  end = Sys.time()
  print(end-start)

  # Train a full model on original data to predict the probabilities
  print("Training the model...")
  start = Sys.time()
  model <- xgboost(data=X.train, label=y.train, objective="binary:logistic", eta=0.1, 
                   nrounds=optimal_nrounds, verbose=0)
  # Predict all labels and fill the observed training labels 
  y.pred <- predict(model, X)
  y.pred[observation] <- y.train
  # Map predictions to the multiple classes and save the original model predictions
  class.index$y_pred <- y.pred[class.index$index]
  pred.class         <- class.index %>% summarise.(N = .N, n = sum(y_pred), .by = class, .sort=T)
  stats              <- pred.class %>% mutate.(y.model = n / N) %>% select.(class, N, y.model)
  end = Sys.time()
  print(end-start)

  if (prediction.intervals == "outcome") {
    # Prediction intervals of type "outcome" assume that the model is correct
    library(poisbinom) # If p is a vector of probabilities of i.d. Bernoulli trials, the sum is Poisson Binomial
    print("Calculating statistics...")
    start = Sys.time()
    # Calculate prediction intervals from implied outcome probabilities 
    stats.outcomes <- class.index %>% summarise.(N = .N, n = sum(y_pred),
                                                 n.outcomes.lower=qpoisbinomzero(y_pred, 0.025),
                                                 n.outcomes.upper=qpoisbinomzero(y_pred, 0.975), .by = class, .sort=T)
    # Merge together to get predicted counts in original model and implied outcomes
    stats = stats %>% 
      left_join.(stats.outcomes, by="class") %>% mutate.(
        y.outcomes.lower   = n.outcomes.lower / N,
        y.outcomes.upper   = n.outcomes.upper / N
      ) %>% select.(class, N, y.model, y.outcomes.lower, y.outcomes.upper) 
    end = Sys.time()
    print(end-start)
  } else if (prediction.intervals == "full") {
    # Otherwise we bootstrap resample the data set, fit model to each bootstrap and sample the predicted outcome
    print("Bootstrapping...")
    # This is where bootstrap and sampled outcome predictions are saved
    pred.bootstrap <- class.index %>% summarise.(N = .N, .by = class, .sort=T)
    pred.outcomes  <- class.index %>% summarise.(N = .N, .by = class, .sort=T)
    
    set.seed(42)
    # Bootstrap resample the data
    for (n in 1:n.bootstrap) {
      print(sprintf("Bootstrap %d", n))
      
      # Bootstrap resample a new training set
      indices <- sample(1:nrow(X.train), replace=T)
      X.bootstrap <- X.train[indices,]
      y.bootstrap <- y.train[indices]
      
      # Train on boostrapped sample
      print("Training on bootstrapped sample...")
      start = Sys.time()
      model <- xgboost(objective="binary:logistic", data=X.bootstrap, label=y.bootstrap, eta = 0.1,
                       nrounds=optimal_nrounds, verbose=0)
      # Predict all labels and fill the observed training labels 
      y.pred <- predict(model, X)
      y.pred[observation] <- y.train
      # Map predictions to the multiple classes and save the bootstrap sample results
      class.index$y_pred <- y.pred[class.index$index]
      pred.class         <- class.index %>% summarise.(n = sum(y_pred), .by = class)
      pred.bootstrap     <- pred.bootstrap %>% left_join.(pred.class, by = "class")
      end = Sys.time()
      print(end-start)
      
      # Inner bootstrap loop
      for (m in 1:n.bootstrap.inner) {
        print(sprintf("Sample outcome %d", m))
        #Sample from the predicted probabilities 
        y.sample <- rbinom(length(y.pred), 1, prob=y.pred)
        # Count the sum and map to classes
        class.index$y_pred <- y.sample[class.index$index]
        pred.class         <- class.index %>% summarise.(n = sum(y_pred), .by = class)
        pred.outcomes      <- pred.outcomes %>% left_join.(pred.class, by = "class")
      }
    }
    
    print("Calculating statistics...")
    start = Sys.time()
    # Statistics from re-training the model in bootstrap samples
    stats.bootstrap <- pred.bootstrap %>% select.(-N) %>%
      mutate_rowwise.(n.bootstrap.mean = mean(c_across.(where(is.numeric))), 
                      n.bootstrap.sd   = sd(c_across.(where(is.numeric)))) %>%
      select.(class, n.bootstrap.mean, n.bootstrap.sd)
    # Statistics from re-training the model in bootstrap samples and sampling the outcome
    stats.outcomes <- pred.outcomes %>% select.(-N) %>%
      mutate_rowwise.(n.outcomes.sd    = sd(c_across.(where(is.numeric))), 
                      n.outcomes.lower = quantile(c_across.(where(is.numeric)), 0.025, na.rm=T), 
                      n.outcomes.upper = quantile(c_across.(where(is.numeric)), 0.975, na.rm=T)) %>%
      select.(class, n.outcomes.sd, n.outcomes.lower, n.outcomes.upper)
    # Merge together to get predicted counts in original model, bootstrap and sampled outcomes
    stats = stats %>% 
      left_join.(stats.bootstrap, by="class") %>% 
      left_join.(stats.outcomes, by="class") %>% mutate.(
        y.bootstrap.mean = n.bootstrap.mean / N,
        y.bootstrap.sd   = n.bootstrap.sd / N,
        y.outcomes.sd      = n.outcomes.sd / N,
        y.outcomes.lower   = n.outcomes.lower / N,
        y.outcomes.upper   = n.outcomes.upper / N
      ) %>% select.(class, N, y.model, y.bootstrap.mean, y.bootstrap.sd, 
                    y.outcomes.sd, y.outcomes.lower, y.outcomes.upper) 
    end = Sys.time()
    print(end-start)
  }

  return(stats)
}


# outcome: the file name (column: column from GEMON to use, subset: subset from population to use)
columns  <- colnames(data.gemon)
outcomes <- columns[2:(length(columns))][c(-2, -4,-11, -13,-14)] # there are a few unnecessary ones
#outcomes <- rev(outcomes)

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
    y <- data.gemon[[outcome]][merge.index]
    
    # Predictions for all of the classes
    y.predictions <- predict_y_bootstrap(X, y, class.index, prediction.intervals="full", 
                                         n.bootstrap = 50, n.bootstrap.inner=50)
    write.csv(y.predictions, sprintf("results/smap/bootstrap/%s.csv", outcome), row.names = F)
    
    end = Sys.time()
    print(end - start)
    gc()
  }
}

try(apply_forloop(outcomes, data.gemon, X, class.index, merge.index))


# Combine predictions to one file in the results folder
dfs <- list()
for (file in list.files("results/smap/bootstrap")) {
  print(file)
  dfs[[file]] = read.table(file=file.path('results', 'smap', 'bootstrap', file), sep=",", header=T)
}
dfs <- rbindlist(dfs, idcol="indicator")
# Add indicator of outcome based on file name and split age|geo_id to two columns
dfs <- dfs %>% 
  mutate.(indicator = substr(indicator, 1, nchar(indicator)-4)) %>% 
  separate.(class, c("age", "geo_id"), sep = "|")
# Round to 3 decimal places
pred.cols <- colnames(dfs)[startsWith(colnames(dfs), "y.")]
dfs[, (pred.cols) := lapply(.SD, function(x) round(x*100, 1)), .SDcols=pred.cols]
# Remove counts < 10 and save
dfs <- as.data.frame(dfs)
dfs[(dfs$N < 10), c("N", pred.cols)] <- NA
fwrite(format(dfs[, c("indicator", "age", "geo_id", "N", pred.cols)], 4), 
       "results/smap/paper/predictions_xgboost_full.csv", row.names = F, sep = ';')


#source("Scripts/General/data_loading.R")
source("Scripts/General/geometry_loading.R")
source("Scripts/General/smap_modeling.R")
source("Scripts/General/evaluation.R")
library(xgboost)
library(Matrix)
library(data.table)
library(caret)
library(magrittr)

options(na.action='na.pass')
options(expressions = 5e5)

ref_date <- "20200901"

# Load population and health monitor data
data.population <- readRDS(file=sprintf("Data/Populatiebestanden/popdata_%s_filled.rds", ref_date))
data.gemon      <- readRDS(file=sprintf("Data/Populatiebestanden/gemondata_%s.rds", ref_date))
#data.population <- droplevels(data.population)
merge.index     <- match(data.population$rinpersoon, data.gemon$rinpersoon)

# =====================================================================
#   INFORMATION ABOUT DATA
# =====================================================================

# Number of missing values in population data
data.original   <- readRDS(file=sprintf("Data/Populatiebestanden/popdata_%s.rds", ref_date))
na.num          <- data.original %>% sapply(FUN = is.na) %>% colSums %>% sort
na.num[['n']]   <- nrow(data.original)
na.population   <- data.frame(data="population", variable=names(na.num), na=na.num)
data.original   <- NULL
#print(round(na.num/na.num[['n']]*100, 2))
# Number of missing values in population data
na.num        <- data.gemon %>% sapply(FUN = is.na) %>% colSums %>% sort
na.num[['n']] <- nrow(data.gemon)
na.gemon      <- data.frame(data="gemon", variable=names(na.num), na=na.num)
#print(round(na.num/na.num[['n']]*100, 2))
na <- rbind(na.population, na.gemon, make.row.names=F)
write.csv(na, "results/smap/paper/na_num.txt", row.names = F)

# Population feature information
info <- c()
cols <- colnames(data.population)
for (col in cols[2:length(cols)]) {
  vals <- data.population[[col]]
  # For categorical features: categories (# of categories)
  if (is.factor(vals)) {
    lvls   <- levels(vals)
    lvls.n <- length(lvls)
    if (lvls.n > 10) lvls <- c(lvls[1:10], "...")
    info[[col]] <- sprintf("%s (%d)", paste(lvls, collapse=", "), lvls.n)
  }
  # For numerical features: median (min - max)
  else if (is.numeric(vals)) {
    lvls <- round(c(median(vals, na.rm=T), min(vals, na.rm=T), max(vals, na.rm=T)),0)
    info[[col]] <- sprintf("%d (%d-%d)", lvls[1], lvls[2], lvls[3])
  }
}
write.table(format(info, 4), "results/smap/paper/info.txt", col.names = F, sep=",")
#read.table("results/smap/paper/info.txt",col.names=c("variable", "values"), sep=",")

# =====================================================================
#   EXPERIMENT: PREDICT MISSING "DRINKER" INDICATOR IN HEALTH MONITOR 
# =====================================================================

# Take drinker as the example outcome
outcome = "drinker"
data.population$y <- data.gemon[[outcome]][merge.index]

# Train is the observed values and test is the remaining population
index <- is.na(data.population$y)
train  <- data.population[!index,]
test   <- data.population[index,]

# Save observed y values and model predictions for missing values 
y.predict = data.frame(bu_code = data.population$bu_code,
                       y       = data.population$y)
predictions <- data.population$y

# nullmodel: predict the mean value for every person
# --------------------------------------------------

model <- nullmodel(train)
predictions[index] <- predict(model, test)
y.predict[[ "nullmodel"]] <- predictions

# glm: Simple linear model without spatial information
# ----------------------------------------------------

model <- glm(y ~ age + sex + ethnicity + marital_status + education + 
               hhtype + hhsize + hhhomeownership + hhincomesource + hhincome + hhassets + oad,
             data = train, family = "binomial")
predictions[index] <- predict(model, newdata = test, type = "response")
y.predict[[ "glm"]] <- predictions

# smapmodel: The original SMAP model
# ----------------------------------

# SMAP is fitted separately to each ggd region using spatial information
ggd.bu_codes <- ggd_bucodes(data.population) # bu_codes for each ggd region
bu_codes.sf  <- DataGeometry(ref_date)       # bu_codes geometry information
model <- smapmodel(ggd.bu_codes, bu_codes.sf)
formula = y ~
  s(age, by = sex,  bs = "ps", k = 10) +
  s(age, by = ethnicity,  bs = "ps", k = 10) +
  s(age, by = marital_status, bs = "ps", k = 10) +
  s(age, by = education, bs = "ps", k = 10) +
  s(sex, ethnicity,  bs = "re") +
  s(sex, marital_status, bs = "re") +
  s(sex, education, bs = "re") +
  s(hhtype, bs = "re") +
  s(hhsize, bs = "ps", k = 5) +
  s(hhincomesource, bs = "re") +
  s(hhhomeownership, bs = "re") +
  s(hhincome, bs = "ps", k = 10) +
  s(hhassets, bs = "ps", k = 10) +
  s(oad, bs = "ps", k = 10)
model <- fit.smapmodel(model, train, formula)
predictions[index] <- predict.smapmodel(model, test)
y.predict[[ "smapmodel"]] <- predictions

#  CALCULATE TERMS FOR SMAPMODEL ("DRINKER" HEALTH INDICATOR)

predi <- terms.smapmodel(model, train)

# Calculate the mean effect over different ggd models
shap.values <- list()
# age x sex
term                <- 's(age, by = sex,  bs = "ps", k = 10)'
predi$shap          <- rowSums(predi[,c('s(age):sexman', 's(age):sexwoman')])
shap.df             <- predi[,.(shap=mean(shap), N=.N), by=.(sex,age)] 
colnames(shap.df)   <- c("by", "value", "shap", "n") 
shap.values[[term]] <- shap.df
# age x ethnicity
term                <- 's(age, by = ethnicity,  bs = "ps", k = 10)'
predi$shap          <- rowSums(predi[,c("s(age):ethnicitynetherlands", "s(age):ethnicitymarokko", "s(age):ethnicityturkey",
                                        "s(age):ethnicitysuriname", "s(age):ethnicityantille", "s(age):ethnicityother_nonwestern", 
                                        "s(age):ethnicityother_western")])
shap.df             <- predi[,.(shap=mean(shap), N=.N), by=.(ethnicity,age)] 
colnames(shap.df)   <- c("by", "value", "shap", "n") 
shap.values[[term]] <- shap.df
# age x marital_status
term                <- 's(age, by = marital_status, bs = "ps", k = 10)'
predi$shap          <- rowSums(predi[,c("s(age):marital_statusmarried", "s(age):marital_statussingle", 
                                        "s(age):marital_statusdivorced", "s(age):marital_statuswidowed")])
shap.df             <- predi[,.(shap=mean(shap), N=.N), by=.(marital_status,age)] 
colnames(shap.df)   <- c("by", "value", "shap", "n") 
shap.values[[term]] <- shap.df
# age x education
term                <- 's(age, by = education, bs = "ps", k = 10)'
predi$shap          <- rowSums(predi[,c("s(age):educationbasis", "s(age):educationvmbo_bk", "s(age):educationvmbo_gt", 
                                        "s(age):educationmbo_23", "s(age):educationmbo_4", "s(age):educationhavo_vwo", 
                                        "s(age):educationhbo_wo_bc", "s(age):educationhbo_wo_ma")])
shap.df             <- predi[,.(shap=mean(shap), N=.N), by=.(education,age)] 
colnames(shap.df)   <- c("by", "value", "shap", "n") 
shap.values[[term]] <- shap.df
# sex x ethnicity
term                <- 's(sex, ethnicity,  bs = "re")'
predi$shap          <- predi[["s(sex,ethnicity)"]]
shap.df             <- predi[,.(shap=mean(shap), N=.N), by=.(sex,ethnicity)] 
colnames(shap.df)   <- c("by", "value", "shap", "n") 
shap.values[[term]] <- shap.df
# sex x marital_status
term                <- 's(sex, marital_status,  bs = "re")'
predi$shap          <- predi[["s(sex,marital_status)"]]
shap.df             <- predi[,.(shap=mean(shap), N=.N), by=.(sex,marital_status)] 
colnames(shap.df)   <- c("by", "value", "shap", "n") 
shap.values[[term]] <- shap.df
# sex x education
term                <- 's(sex, education,  bs = "re")'
predi$shap          <- predi[["s(sex,education)"]]
shap.df             <- predi[,.(shap=mean(shap), N=.N), by=.(sex,education)] 
colnames(shap.df)   <- c("by", "value", "shap", "n") 
shap.values[[term]] <- shap.df
# non-interactions
for (col in c("hhtype" , "hhsize", "hhincomesource", "hhhomeownership", "hhincome", "hhassets", "oad", "bu_code")) {
  term <- sprintf("s(%s)", col)
  predi$shap          <- predi[[term]]
  shap.df             <- predi[,.(shap=mean(shap), N=.N), by=.(get(col))]
  colnames(shap.df)   <- c("value", "shap", "n") 
  shap.values[[term]] <- shap.df
}
# Save
shap.values <- rbindlist(shap.values, idcol = "column", fill=T)
shap.values$shap <- round(shap.values$shap, 4)
shap.values$value <- as.character(shap.values$value)
shap.values[(shap.values$n < 10), c("shap", "n")] <- NA
write.csv(shap.values, sprintf("results/smap/paper/%s_interpretation_smap.txt", outcome), row.names=F)


# XGBoost model 
# --------------

# Load population and health monitor data
data.population <- readRDS(file=sprintf("Data/Populatiebestanden/popdata_%s.rds", ref_date))
data.gemon      <- readRDS(file=sprintf("Data/Populatiebestanden/gemondata_%s.rds", ref_date))
#data.population <- droplevels(data.population)
merge.index     <- match(data.population$rinpersoon, data.gemon$rinpersoon)

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

# Take drinker as the example outcome
outcome = "drinker"
data.population$y <- data.gemon[[outcome]][merge.index]

# Train is the observed values and test is the remaining population
index <- is.na(data.population$y)
train  <- data.population[!index,]
test   <- data.population[index,]

# Model matrix with x & y coordinates 
formula    <- y ~ age + sex + ethnicity + marital_status + education + 
  hhtype + hhsize + hhhomeownership + hhincomesource + hhincome + hhassets + oad + x_coord + y_coord - 1
X.train <- sparse.model.matrix(formula, train)
X.test  <- sparse.model.matrix(formula, test)

# xgboost_xy: XGBoost model with spatial information as x & y coordinates (default hyperparameters)
# -----------------------------------------------------------------------------------

model <- xgboost(data=X.train, label=train$y, objective="binary:logistic", nrounds=50, verbose=0)
predictions[index] <- predict(model, X.test)
y.predict[[ "xgboost0_xy"]] <- predictions

# xgboost_xy: XGBoost model with spatial information as x & y coordinates
# -----------------------------------------------------------------------------------

model <- xgb.cv(list(objective="binary:logistic", eval_metric="auc"), data=X.train, label=train$y, 
                eta=0.1, nrounds=1000, verbose=0, nfold=5, stratified=T, early_stopping_rounds=10)
model <- xgboost(data=X.train, label=train$y, objective="binary:logistic", 
                 eta=0.1, nrounds=model$best_iteration, verbose=0)
predictions[index] <- predict(model, X.test)
y.predict[[ "xgboost_xy"]] <- predictions

# Model matrix with oblique coordinates 
formula <- as.formula(sprintf("y ~ age + sex + ethnicity + marital_status + education + hhtype + hhsize + 
                              hhhomeownership + hhincomesource + hhincome + hhassets + oad + %s - 1",
                              ogc.string))
X.train <- sparse.model.matrix(formula, train)
X.test  <- sparse.model.matrix(formula, test)

# xgboost_ogc: XGBoost model with spatial information as oblique coordinates (default hyperparameters)
# -----------------------------------------------------------------------------------

model <- xgboost(data=X.train, label=train$y, objective="binary:logistic", nrounds=50, verbose=0)
predictions[index] <- predict(model, X.test)
y.predict[[ "xgboost0_ogc"]] <- predictions

# xgboost_ogc: XGBoost model with spatial information as oblique coordinates 
# -----------------------------------------------------------------------------------

model <- xgb.cv(list(objective="binary:logistic", eval_metric="auc"), data=X.train, label=train$y, 
                eta=0.1, nrounds=1000, verbose=0, nfold=5, stratified=T, early_stopping_rounds=10)
model <- xgboost(data=X.train, label=train$y, objective="binary:logistic", 
                 eta=0.1, nrounds=model$best_iteration, verbose=0)
predictions[index] <- predict(model, X.test)
y.predict[[ "xgboost_ogc"]] <- predictions

#  CALCULATE SHAP VALUES FOR XGBOOST ("DRINKER" HEALTH INDICATOR)

# XGBoost sparse matrix with one-hot encoding
contrasts.list <- list()
cols <- colnames(train)
for (col in cols[6:length(cols)]) {
  if (is.factor(train[[col]]))
    contrasts.list[[col]] <- contrasts(train[[col]], contrasts=F)
}
X.train <- sparse.model.matrix(y ~ age + sex + ethnicity + marital_status + education + hhtype + hhsize + 
                                 hhhomeownership + hhincomesource + hhincome + hhassets + oad + x_coord + y_coord, train, 
                               contrasts.arg=contrasts.list)
# Train model and calculate SHAP values
#model <- xgboost(data=X, label=y, objective="binary:logistic", nrounds=50, verbose=0)
model <- xgb.cv(list(objective="binary:logistic", eval_metric="auc"), data=X.train, label=train$y, 
                eta=0.1, nrounds=1000, verbose=0, nfold=5, stratified=T, early_stopping_rounds=10)
model <- xgboost(data=X.train, label=train$y, objective="binary:logistic", eta=0.1, 
                 nrounds=model$best_iteration, verbose=0)
# These are the SHAP values
contr <- data.frame(predict(model, X.train, predcontrib=T)) 
setDT(contr)
#contrI<- data.frame(predict(model, X, predinteraction=T))
# Transform SHAP values so that there is one visualization per feature
shap.values <- list()
for (column in c("age", "sex", "ethnicity", "marital_status", "education", "hhtype", "hhsize", 
                 "hhhomeownership", "hhincomesource", "hhincome", "hhassets", "oad")) {
  relevant.columns <- colnames(contr)[(substr(colnames(contr), 1, nchar(column)) == column)]
  if (length(relevant.columns) > 1)
    contr$shap <- rowSums(contr[, ..relevant.columns])
  else 
    contr$shap <- contr[[relevant.columns]]
  contr$value <- train[[column]]
  shap.df <- contr[,.(shap=mean(shap), n=.N), by=.(value)]#aggregate(shap ~ value, FUN=mean)
  shap.values[[column]] <- shap.df
}
# Calculate spatial effect
xy   <- with(train, data.frame(x=x_coord, y=y_coord))
temp <- st_set_crs(st_cast(st_sfc(st_multipoint(as.matrix(xy))), "POINT"),  28992)
sf <- DataGeometry(ref_date, which="bu")
bu_code.idx <- unlist(as.numeric(as.character(st_intersects(temp, sf))))
xy$value <- sf$bu_code[bu_code.idx]
xy$shap    <- rowSums(contr[, c("x_coord", "y_coord")])
setDT(xy)
shap.values[["bu_code"]] <- xy[,.(shap=mean(shap), n=.N), by=.(value)]
# Save
shap.values <- rbindlist(shap.values, idcol = "column")
shap.values$shap <- round(shap.values$shap, 4)
shap.values[(shap.values$n < 10), c("shap", "n")] <- NA
write.csv(format(shap.values, 4), sprintf("results/smap/paper/%s_interpretation_xgboost.txt", outcome), row.names=F)


# OUTPUT

# Calculate health monitor respondents and population in each bu_code
setDT(y.predict)
y.population <- y.predict[order(bu_code), .(N.gemon = sum(!is.na(y)), N.population =.N), by=bu_code]
#write.csv(y.population, sprintf("results/smap/paper/%s_population.txt", outcome), row.names = F)
## Save the value counts of different bu_code population sizes in health monitor and population
#N.lvl <- seq(0, max(y.population$N.population), 1)
#y.valuecount <- data.table(
#  n.bu_code        = N.lvl,
#  count.gemon      = as.vector(table(factor(y.population$N.gemon, levels=N.lvl))), 
#  count.population = as.vector(table(factor(y.population$N.population, levels=N.lvl)))
#)
#write.csv(y.valuecount, sprintf("results/smap/paper/%s_valuecount.txt", outcome), row.names = F)

# Calculate CIs for xgboost
library(poisbinom)
ppoisbinom_fixed <- function(x, y) {
  p <- y[!is.na(y)]
  if (length(p) < 1) {
    pval <- 1.0
  } else {
    pval <- ppoisbinom(x, p)
  }
  return(pval)
}
y.mean  <- mean(y.predict$y, na.rm=T) #
temp <- y.predict[,c("bu_code", "y", "xgboost_ogc")]
temp[!is.na(temp$y), "xgboost_ogc"] <- NA
setDT(temp)
y.xgboost_ci <- temp[order(bu_code),.(p = round(ppoisbinom_fixed(ceiling(y.mean*length(y) - sum(y, na.rm=T)), 
                                                                 xgboost_ogc), 4)), by=bu_code]

# Save model predictions for each bu_code where N < 10 has been censored
y.prevalence <- y.predict[order(bu_code), lapply(.SD, mean, na.rm=T), by=bu_code]
models <- colnames(y.prevalence)[2:ncol(y.prevalence)]
y.prevalence[,(models):=round(.SD, 4), .SDcols=models]

#Combine
y.prevalence <- cbind(y.prevalence, y.xgboost_ci[,c("p")], y.population[,c("N.gemon", "N.population")])
y.prevalence[(y.population$N.population < 10), c(models, "p", "N.population")] <- NA
y.prevalence[(y.population$N.gemon      < 10), c("y", "N.gemon")] <- NA
write.csv(format(y.prevalence, 4), sprintf("results/smap/paper/%s_prevalence.txt", outcome), row.names = F)



# =====================================================================
#  XGBOOST HYPERPARAMETER SELECTION (5CV) IN HEALTH MONITOR ("DRINKER")
# =====================================================================

# XGBoost input and output
X.train <- sparse.model.matrix(y ~ age + sex + ethnicity + marital_status + education + hhtype + hhsize + hhhomeownership + 
                           hhincomesource + hhincome + hhassets + oad + x_coord + y_coord - 1, train)
y <- train$y

# Validation AUC as a function of iterations with different learning rates
evaluation <- data.frame()
for (eta in c(1, 0.3, 0.1, 0.01, 0.001)) {
  print(eta)
  model <- xgb.cv(list(objective="binary:logistic", eval_metric="rmse"), data=X.train, label=y, eta=eta, 
                  nrounds=15000, verbose=1, nfold=5, stratified=T, early_stopping_rounds=100, print_every_n=100)
  evaluation.eta <- round(data.table(eta = eta, iter = model$evaluation_log$iter, 
                                     train_rmse = model$evaluation_log$train_rmse_mean, 
                                     test_rmse  = model$evaluation_log$test_rmse_mean), 4)
  evaluation <- rbind(evaluation, evaluation.eta)
}

write.csv(format(evaluation, 4), sprintf("results/smap/paper/%s_hyperparameters_xgboost.txt", outcome), row.names=F)


# =====================================================================
#  XGBOOST LEARNED TREES VISUALIZATION IN HEALTH MONITOR ("DRINKER")
# =====================================================================
evaluation <- data.frame()
for (max_depth in c(1,2,3,4,5,6)) {
  print(max_depth)
  model <- xgb.cv(list(objective="binary:logistic", eval_metric="rmse"), data=X.train, label=y, max_depth=max_depth, 
                  nrounds=500, verbose=1, nfold=5, stratified=T, early_stopping_rounds=50, eta=1)
  evaluation.eta <- round(data.table(max_depth = max_depth, iter = model$evaluation_log$iter, 
                                     train_rmse = model$evaluation_log$train_rmse_mean, 
                                     test_rmse  = model$evaluation_log$test_rmse_mean), 4)
  evaluation <- rbind(evaluation, evaluation.eta)
}
p1 <- ggplot(evaluation, aes(x=iter, y=test_rmse, group=max_depth, color=max_depth)) + geom_line() + scale_x_log10() +
  labs(title="Hyperparameters: max_depth, nrounds", x="Iteration (nrounds)", y="Validation RMSE")
max_depth.max <- aggregate(test_rmse ~ max_depth, evaluation, min)
p2 <-ggplot(max_depth.max, aes(x=max_depth, y=test_rmse)) + geom_line(color='blue') + scale_x_log10() +
  labs(title="Optimal eta with early stopping", x="Learning rate (eta)", y="Lowest validation RMSE")
ggarrange(p1, p2, ncol=2, nrow=1, legend="top")

# 10 trees with depth 2
library(DiagrammeR)
model <- xgboost(data=X.train, label=y, objective="reg:squarederror", eta=1, max_depth=2, nrounds=10, verbose=0)
model.plot <- xgb.plot.tree(model=model, trees=1:10, plot_width=1000, plot_height=5000, render=F)
export_graph(model.plot, "xgboost_tree_plot.png", width=1000, height=5000)
subset(evaluation, (max_depth == 2) & (iter == 10))

# 1 tree with depth 6
model <- xgboost(data=X.train, label=y, objective="reg:squarederror", eta=1, max_depth=6, nrounds=2, verbose=0)
model.plot <- xgb.plot.tree(model=model, trees=1, plot_width=2000, plot_height=5000, render=F)
export_graph(model.plot, "xgboost_tree_cart.png", width=2000, height=5000)
subset(evaluation, (max_depth == 6) & (iter == 1))

# =====================================================================
#   EXPERIMENT: PREDICTION ACCURACY (5CV) OF MODELS IN HEALTH MONITOR 
# =====================================================================

ref_date <- "20200901"

# Load population and health monitor data
data.population <- readRDS(file=sprintf("Data/Populatiebestanden/popdata_%s_filled.rds", ref_date))
data.gemon      <- readRDS(file=sprintf("Data/Populatiebestanden/gemondata_%s.rds", ref_date))
#data.population <- droplevels(data.population)
merge.index     <- match(data.population$rinpersoon, data.gemon$rinpersoon)

for (outcome in colnames(data.gemon)[2:ncol(data.gemon)]) { #
  #outcome = "drinker"
  print(outcome)
  
  # Take the data set where the outcome exists for every person
  data.population$y <- data.gemon[[outcome]][merge.index]
  data <- data.population[!is.na(data.population$y),]
  
  # Create cross validation folds
  set.seed(42)
  cv.index <- createFolds(as.factor(data$y), k=5)
  # Create vector that identifies to which test fold each sample belongs
  folds <- names(cv.index)
  cv.folds <- vector(mode="double", length=nrow(data))
  for (fold in folds) cv.folds[cv.index[[fold]]] <- fold
  
  # Files to save results into
  fn.predict <- sprintf("results/smap/paper/cache/%s_predictions.txt", outcome)
  fn.fittime <- sprintf("results/smap/paper/cache/%s_time.txt", outcome)
  
  # Save the predictions
  if (!file.exists(fn.predict))
  {
    n <- nrow(data)
    y.predict = data.frame(bu_code   = data$bu_code,
                           cv.fold   = cv.folds,
                           nullmodel = rep(NA,n),
                           y_true    = data$y)
  } else {
    y.predict <- read.csv(fn.predict)
  }
  # Save time taken to train
  if (!file.exists(fn.predict))
  {
    k <- length(folds)
    y.fittime = data.frame(row.names = folds,
                           nullmodel = rep(NA,k))
  } else {
    y.fittime <- read.csv(fn.fittime)
    rownames(y.fittime) <- y.fittime$X
    y.fittime$X         <- NULL
  }
  
  # For every train and test fold pair, fit each model to training data and predict values in test
  for (fold in folds) {
    print(fold)
    
    # Get train and test sets for the fold
    index = cv.index[[fold]]
    train <- data[-index,]
    test <- data[index,]
    
    # nullmodel: predict the mean value for every person
    # --------------------------------------------------

    time.start <- Sys.time()
    model <- nullmodel(train)
    predictions <- predict(model, test)
    time.end <- Sys.time()
    timediff <- difftime(time.end, time.start, units="secs")[[1]]

    y.predict[index, "nullmodel"] <- predictions
    y.fittime[fold, "nullmodel"]  <- timediff


    # glm: Simple linear model without spatial information
    # ----------------------------------------------------

    time.start <- Sys.time()
    model <- glm(y ~ age + sex + ethnicity + marital_status + education +
                 hhtype + hhsize + hhhomeownership + hhincomesource + hhincome + hhassets + oad,
                 data = train, family = "binomial")
    predictions <- predict(model, newdata = test, type = "response")
    time.end <- Sys.time()
    timediff <- difftime(time.end, time.start, units="secs")[[1]]

    y.predict[index, "glm"] <- predictions
    y.fittime[fold, "glm"]  <- timediff
    
    # smapmodel: The original SMAP model
    # ----------------------------------

    time.start <- Sys.time()
    # SMAP is fitted separately to each ggd region using spatial information
    ggd.bu_codes <- ggd_bucodes(data.population) # bu_codes for each ggd region
    bu_codes.sf  <- DataGeometry(ref_date)       # bu_codes geometry information
    model <- smapmodel(ggd.bu_codes, bu_codes.sf)
    formula = y ~
      s(age, by = sex,  bs = "ps", k = 10) +
      s(age, by = ethnicity,  bs = "ps", k = 10) +
      s(age, by = marital_status, bs = "ps", k = 10) +
      s(age, by = education, bs = "ps", k = 10) +
      s(sex, ethnicity,  bs = "re") +
      s(sex, marital_status, bs = "re") +
      s(sex, education, bs = "re") +
      s(hhtype, bs = "re") +
      s(hhsize, bs = "ps", k = 5) +
      s(hhincomesource, bs = "re") +
      s(hhhomeownership, bs = "re") +
      s(hhincome, bs = "ps", k = 10) +
      s(hhassets, bs = "ps", k = 10) +
      s(oad, bs = "ps", k = 10)
    model <- fit.smapmodel(model, train, formula)
    predictions <- predict.smapmodel(model, test)
    time.end <- Sys.time()
    timediff <- difftime(time.end, time.start, units="secs")[[1]]

    y.predict[index, "smapmodel"] <- predictions
    y.fittime[fold, "smapmodel"]  <- timediff

  }
  
  write.csv(format(y.predict, 4), fn.predict, row.names = F)
  write.csv(format(y.fittime, 4), fn.fittime, row.names = T)
}


ref_date <- "20200901"

# Load population and health monitor data
data.population <- readRDS(file=sprintf("Data/Populatiebestanden/popdata_%s.rds", ref_date))
data.gemon      <- readRDS(file=sprintf("Data/Populatiebestanden/gemondata_%s.rds", ref_date))
#data.population <- droplevels(data.population)
merge.index     <- match(data.population$rinpersoon, data.gemon$rinpersoon)

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

for (outcome in colnames(data.gemon)[2:ncol(data.gemon)]) { #
  #outcome = "drinker"
  print(outcome)
  
  # Take the data set where the outcome exists for every person
  data.population$y <- data.gemon[[outcome]][merge.index]
  data <- data.population[!is.na(data.population$y),]
  
  # Create cross validation folds
  set.seed(42)
  cv.index <- createFolds(as.factor(data$y), k=5)
  # Create vector that identifies to which test fold each sample belongs
  folds <- names(cv.index)
  cv.folds <- vector(mode="double", length=nrow(data))
  for (fold in folds) cv.folds[cv.index[[fold]]] <- fold
  
  # Files to save results into
  fn.predict <- sprintf("results/smap/paper/cache/%s_predictions.txt", outcome)
  fn.fittime <- sprintf("results/smap/paper/cache/%s_time.txt", outcome)
  
  # Save the predictions
  if (!file.exists(fn.predict))
  {
    n <- nrow(data)
    y.predict = data.frame(bu_code   = data$bu_code,
                           cv.fold   = cv.folds,
                           nullmodel = rep(NA,n),
                           y_true    = data$y)
  } else {
    y.predict <- read.csv(fn.predict)
  }
  # Save time taken to train
  if (!file.exists(fn.predict))
  {
    k <- length(folds)
    y.fittime = data.frame(row.names = folds,
                           nullmodel = rep(NA,k))
  } else {
    y.fittime <- read.csv(fn.fittime)
    rownames(y.fittime) <- y.fittime$X
    y.fittime$X         <- NULL
  }
  
  # For every train and test fold pair, fit each model to training data and predict values in test
  for (fold in folds) {
    print(fold)
    
    # Get train and test sets for the fold
    index = cv.index[[fold]]
    train <- data[-index,]
    test <- data[index,]

    # xgboost
    # -----------------------------------------------------------------------------------
    
    # Model matrix with x & y coordinates
    formula    <- y ~ age + sex + ethnicity + marital_status + education +
      hhtype + hhsize + hhhomeownership + hhincomesource + hhincome + hhassets + oad + x_coord + y_coord - 1
    X.train <- sparse.model.matrix(formula, train)
    X.test  <- sparse.model.matrix(formula, test)
    
    # xgboost0_xy: XGBoost model with spatial information as x & y coordinates (default hyperparameters)
    # -----------------------------------------------------------------------------------
    
    time.start <- Sys.time()
    model <- xgboost(data=X.train, label=train$y, objective="binary:logistic", nrounds=50, verbose=0)
    predictions <- predict(model, X.test)
    time.end <- Sys.time()
    timediff <- difftime(time.end, time.start, units="secs")[[1]]
    
    y.predict[index, "xgboost0_xy"] <- predictions
    y.fittime[fold, "xgboost0_xy"]  <- timediff
    
    # xgboost_xy: XGBoost model with spatial information as x & y coordinates
    # -----------------------------------------------------------------------------------
    
    time.start <- Sys.time()
    model <- xgb.cv(list(objective="binary:logistic", eval_metric="auc"), data=X.train, label=train$y,
                    eta=0.1, nrounds=1000, print_every_n=100, verbose=1, nfold=5, stratified=T, early_stopping_rounds=10)
    model <- xgboost(data=X.train, label=train$y, objective="binary:logistic",
                     eta=0.1, nrounds=model$best_iteration, verbose=0)
    #model <- xgboost(data=X.train, label=y.train, objective="binary:logistic", eta=0.1, nrounds=200, verbose=0)
    predictions <- predict(model, X.test)
    time.end <- Sys.time()
    timediff <- difftime(time.end, time.start, units="secs")[[1]]
    
    y.predict[index, "xgboost_xy"] <- predictions
    y.fittime[fold, "xgboost_xy"]  <- timediff
    
    X.train <- NULL
    X.test  <- NULL
    
    # Model matrix with oblique coordinates
    formula <- as.formula(sprintf("y ~ age + sex + ethnicity + marital_status + education + hhtype + hhsize + 
                                  hhhomeownership + hhincomesource + hhincome + hhassets + oad + %s - 1",
                                  ogc.string))
    X.train <- sparse.model.matrix(formula, train)
    X.test  <- sparse.model.matrix(formula, test)
    
    # xgboost0_ogc: XGBoost model with spatial information as oblique coordinates (default hyperparameters)
    # -----------------------------------------------------------------------------------
    
    time.start <- Sys.time()
    model <- xgboost(data=X.train, label=train$y, objective="binary:logistic", nrounds=50, verbose=0)
    predictions <- predict(model, X.test)
    time.end <- Sys.time()
    timediff <- difftime(time.end, time.start, units="secs")[[1]]
    
    y.predict[index, "xgboost0_ogc"] <- predictions
    y.fittime[fold, "xgboost0_ogc"]  <- timediff
    
    # xgboost_ogc: XGBoost model with spatial information as oblique coordinates
    # -----------------------------------------------------------------------------------
    time.start <- Sys.time()
    model <- xgb.cv(list(objective="binary:logistic", eval_metric="auc"), data=X.train, label=train$y, 
                    eta=0.1, nrounds=1000, print_every_n=100, verbose=1, nfold=5, stratified=T, early_stopping_rounds=10)
    model <- xgboost(data=X.train, label=train$y, objective="binary:logistic", 
                     eta=0.1, nrounds=model$best_iteration, verbose=0)
    #model <- xgboost(data=X.train, label=y.train, objective="binary:logistic", eta=0.1, nrounds=200, verbose=0)
    predictions <- predict(model, X.test)
    time.end <- Sys.time()
    timediff <- difftime(time.end, time.start, units="secs")[[1]]
    
    y.predict[index, "xgboost_ogc"] <- predictions
    y.fittime[fold, "xgboost_ogc"]  <- timediff
    
    X.train <- NULL
    X.test  <- NULL
    
  }
  
  write.csv(format(y.predict, 4), fn.predict, row.names = F)
  write.csv(format(y.fittime, 4), fn.fittime, row.names = T)
}


# Read all predictions and save statistics of each model
models <- c("nullmodel", "glm", "xgboost0_xy", "xgboost_xy", "xgboost0_ogc", "xgboost_ogc", "smapmodel")
indicators <- list()
for (outcome in colnames(data.gemon)[2:ncol(data.gemon)]) { 
  #outcome = "drinker"
  print(outcome)
  
  fn.predict <- sprintf("results/smap/paper/cache/%s_predictions.txt", outcome)
  if (file.exists(fn.predict)) {
    # rmse of each model
    y.predict <- read.csv(fn.predict)
    y.predmse <- unlist(lapply(y.predict[,models], 
                               function(y_pred) round(calculate_mse(y.predict$y_true, y_pred), 4)))
    # fitting times
    y.fittime <- read.csv(sprintf("results/smap/paper/cache/%s_time.txt", outcome))
    rownames(y.fittime) <- y.fittime$X
    y.fittime$X         <- NULL
    y.fittime           <- round(colMeans(y.fittime),1)
    # extra information
    setDT(y.predict)
    y.prevalence <- y.predict[order(bu_code), .(xgboost_ogc = round(mean(xgboost_ogc), 4),
                                                smapmodel   = round(mean(smapmodel), 4)), by=bu_code]
    extra <- c(correlation = round(with(y.prevalence, cor(xgboost_ogc, smapmodel, method="pearson")), 2),
               pred.improv = round((1 - y.predmse[["xgboost_ogc"]] / y.predmse[["smapmodel"]]) * 100, 2),
               time.improv = round((1 - y.fittime[["xgboost_ogc"]] / y.fittime[["smapmodel"]]) * 100, 2))
    N <- c(n=nrow(y.predict))
    # row in the table
    indicators[[outcome]] <- c(y.predmse, extra, N)
  } else {
    message(sprintf("File doesn't exist %s", fn.predict))
  }
}

metrics.table <- t(data.frame(indicators))
write.csv(metrics.table, sprintf("results/smap/paper/all_metrics.txt"), row.names = T)


# =====================================================================
#  STATISTICS OF MODEL PREDICTIONS (5CV) IN HEALTH MONITOR ("DRINKER")
#         - Table of metrics for different model
#         - ROC curves of different models
#         - Calibration curves of different models
#         - MSE as a function of area size
# =====================================================================

# Read one example indicator
outcome <- "drinker"
y.predict <- read.csv(sprintf("results/smap/paper/cache/%s_predictions.txt", outcome))
y.fittime <- read.csv(sprintf("results/smap/paper/cache/%s_time.txt", outcome), row.names=1)

# Calculate the Accuracy, AUC, Brier Score, Logloss of each model
models <- c("nullmodel", "glm", "xgboost0_xy", "xgboost_xy", "xgboost0_ogc", "xgboost_ogc", "smapmodel")
get_metric <- function(y_pred) calculate_metrics(y.predict$y_true, y_pred)
metrics <- t(data.frame(lapply(y.predict[,models], get_metric)))
metrics <- data.frame(metrics, time=round(colMeans(y.fittime[,models]),1), n=nrow(y.predict))
write.csv(metrics, sprintf("results/smap/paper/%s_metrics.txt", outcome), row.names = T)

# Calculate the ROC curve of each model
get_roc <- function(y_pred) calculate_roc_curve(y.predict$y_true, y_pred)
df.rocs <- rbindlist(lapply(y.predict[,models], get_roc), idcol = "model")
df.rocs$n <- nrow(y.predict)
write.csv(df.rocs, sprintf("results/smap/paper/%s_roc.txt", outcome), row.names = F)

# Calculate the calibration curve of each model
get_calibration <- function(y_pred) calculate_calibration_curve(y.predict$y_true, y_pred)
df.calibration <- rbindlist(lapply(y.predict[,models], get_calibration), idcol = "model")
write.csv(df.calibration, sprintf("results/smap/paper/%s_calibration.txt", outcome), row.names=F)

setDT(y.predict)

## XGBoost vs. smapmodel predicted prevalence in each neighbourhood for the subset with labels
#y.prevalence <- y.predict[order(bu_code), .(N = .N, xgboost_ogc = round(mean(xgboost_ogc), 4),
#                                            smapmodel   = round(mean(smapmodel), 4)), by=bu_code]
#print(with(y.prevalence, cor(xgboost_ogc, smapmodel, method="pearson")))
#y.prevalence[N < 10, c("N", "xgboost_ogc", "smapmodel")] <- NA
#write.csv(format(y.prevalence, 4), sprintf("results/smap/paper/%s_prevalence_subset.txt", outcome), row.names = F)

# XGBoost vs. smapmodel mse as a function of area size
# Calculate the number of health monitor respondents and corresponding quantile 
qcut <- function(x) cut(x, breaks=quantile(x, probs=seq(0,1,0.1), na.rm=T), include.lowest=T)
y.quantile           <- y.predict[, .(N = .N), by=bu_code]
y.quantile$area_size <- qcut(y.quantile$N)
y.predict            <- merge(y.predict, y.quantile, sort=F)
# Calculate rmse over different quantile binned area sizes
mse.quantile         <- y.predict[order(area_size),
                                  .(xgboost_ogc = round(calculate_mse(y_true, xgboost_ogc), 4),
                                    smapmodel   = round(calculate_mse(y_true, smapmodel), 4),
                                    n=.N), 
                                  by=.(area_size)]
write.csv(mse.quantile, sprintf("results/smap/paper/%s_areamse.txt", outcome), row.names=F)






# =====================================================================
#   EXPERIMENT: PREDICTION ACCURACY (5CV) OF MODELS IN NOISE DISTURBANCE
# =====================================================================

ref_date <- "20160901"
data.population <- readRDS(file=sprintf("Data/Populatiebestanden/popdata_%s_filled.rds", ref_date))
data.population <- data.population[data.population$age %in% 18:64,]
data.noise      <- readRDS(file=sprintf("Data/Populatiebestanden/noisedata_%s.rds", ref_date))
data.gemon      <- readRDS(file=sprintf("Data/Populatiebestanden/gemonnoisedata_%s.rds", ref_date))

# Number of missing values
data.original   <- readRDS(file=sprintf("Data/Populatiebestanden/popdata_%s.rds", ref_date))
data.original   <- data.original[data.original$age %in% 18:64,]
na.num          <- data.original %>% sapply(FUN = is.na) %>% colSums %>% sort
na.num[['n']]   <- nrow(data.original)
na.population   <- data.frame(data="population", variable=names(na.num), na=na.num)
data.original   <- NULL
na.num        <- data.gemon %>% sapply(FUN = is.na) %>% colSums %>% sort
na.num[['n']] <- nrow(data.gemon)
na.gemon      <- data.frame(data="gemon", variable=names(na.num), na=na.num)
na.num        <- data.noise %>% sapply(FUN = is.na) %>% colSums %>% sort
na.num[['n']] <- nrow(data.noise)
na.noise      <- data.frame(data="noise", variable=names(na.num), na=na.num)
na <- rbind(na.population, na.gemon, na.noise, make.row.names=F)
write.csv(na, "results/smap/paper/na_num_noise.txt", row.names = F)

# Population feature information
info <- c()
cols <- colnames(data.noise)
for (col in cols[2:length(cols)]) {
  vals <- data.noise[[col]]
  # For categorical features: categories (# of categories)
  if (is.factor(vals)) {
    lvls   <- levels(vals)
    lvls.n <- length(lvls)
    if (lvls.n > 10) lvls <- c(lvls[1:10], "...")
    info[[col]] <- sprintf("%s (%d)", paste(lvls, collapse=", "), lvls.n)
  }
  # For numerical features: median (min - max)
  else if (is.numeric(vals)) {
    lvls <- round(c(median(vals, na.rm=T), min(vals, na.rm=T), max(vals, na.rm=T)),0)
    info[[col]] <- sprintf("%d (%d-%d)", lvls[1], lvls[2], lvls[3])
  }
}
write.table(format(info, 4), "results/smap/paper/info_noise.txt", col.names = F, sep=",")

# Data frame of population data for 18-64 year olds and their noise disturbance indicators
data.population <- merge(data.population, data.noise, by="rinpersoon")
merge.index     <- match(data.population$rinpersoon, data.gemon$rinpersoon)


# Noise disturbance indicator and the corresponding noise measurement used for prediction
measurements <- list(c("road_high","lden_wegv"), 
                     c("rail_high","lden_rail"), c("air_high","lden_air"), 
                     c("road_medhigh","lden_wegv"), c("rail_medhigh","lden_rail"), c("air_medhigh","lden_air"),
                      # Road traffic < 50km/h noise disturbance on municipal roads and > 50km/h noise disturbance of provincial and national roads
                      c("road_high_gt50" , "lden_weg_pwrw"), c("road_medhigh_gt50" , "lden_weg_pwrw"), 
                      c("road_high_sm50" , "lden_wegv_gw"), c("road_medhigh_sm50" , "lden_wegv_gw"),
                      # It is possible that the sum of road traffic (< 50km/h and > 50km/h) is a better predictor of  noise disturbance
                      c("road_high_gt50" , "lden_wegv"), c("road_medhigh_gt50" , "lden_wegv"), c("road_high_sm50" , "lden_wegv"), c("road_medhigh_sm50" , "lden_wegv"))

for (measurement in measurements){
  
  # Data with the given noise disturbance indicator and the corresponding noise measurement
  noise.disturbance = measurement[[1]] #noise.disturbance <- "road_high"
  noise.measurement = measurement[[2]] #noise.measurement <- "lden_wegv"
  experiment <- paste(noise.disturbance, "_", noise.measurement)
  print(experiment)
  
  # Take the data set where the outcome exists for every person
  data.population$y <- data.gemon[[noise.disturbance]][merge.index]
  data <- data.population[!is.na(data.population$y),]
  
  # Create cross validation folds
  set.seed(42)
  cv.index <- createFolds(as.factor(data$y), k=5)
  # Create vector that identifies to which test fold each sample belongs
  folds <- names(cv.index)
  cv.folds <- vector(mode="double", length=nrow(data))
  for (fold in folds) cv.folds[cv.index[[fold]]] <- fold
  
  # Files to save results into
  fn.predict <- sprintf("results/smap/paper/cache2/%s_predictions.txt", experiment)
  fn.fittime <- sprintf("results/smap/paper/cache2/%s_time.txt", experiment)
  
  # Save the predictions
  if (!file.exists(fn.predict))
  {
    n <- nrow(data)
    y.predict = data.frame(bu_code   = data$bu_code,
                           cv.fold   = cv.folds,
                           nullmodel = rep(NA,n),
                           y_true    = data$y)
  } else {
    y.predict <- read.csv(fn.predict)
  }
  # Save time taken to train
  if (!file.exists(fn.predict))
  {
    k <- length(folds)
    y.fittime = data.frame(row.names = folds,
                           nullmodel = rep(NA,k))
  } else {
    y.fittime <- read.csv(fn.fittime)
    rownames(y.fittime) <- y.fittime$X
    y.fittime$X         <- NULL
  }
  
  # For every train and test fold pair, fit each model to training data and predict values in test
  for (fold in folds) {
    print(fold)
    
    # Get train and test sets for the fold
    index = cv.index[[fold]]
    train <- data[-index,]
    test <- data[index,]
    
    # nullmodel: predict the mean value for every person
    # --------------------------------------------------
    
    time.start <- Sys.time()
    model <- nullmodel(train)
    predictions <- predict(model, test)
    time.end <- Sys.time()
    timediff <- difftime(time.end, time.start, units="secs")[[1]]
    
    y.predict[index, "nullmodel"] <- predictions
    y.fittime[fold, "nullmodel"]  <- timediff
    
    
    # glm: Simple linear model without spatial information
    # ----------------------------------------------------
    
    time.start <- Sys.time()
    formula <- as.formula(sprintf("y ~ age + sex + ethnicity + marital_status + education + hhtype + hhsize +
                                  hhhomeownership + hhincomesource + hhincome + hhassets + oad + %s",
                                  noise.measurement))
    model <- glm(formula, data = train, family = "binomial")
    predictions <- predict(model, newdata = test, type = "response")
    time.end <- Sys.time()
    timediff <- difftime(time.end, time.start, units="secs")[[1]]
    
    y.predict[index, "glm"] <- predictions
    y.fittime[fold, "glm"]  <- timediff

    # smapmodel: The original SMAP model
    # ----------------------------------

    time.start <- Sys.time()
    # SMAP is fitted separately to each ggd region using spatial information
    ggd.bu_codes <- ggd_bucodes(data.population) # bu_codes for each ggd region
    bu_codes.sf  <- DataGeometry(ref_date)       # bu_codes geometry information
    model <- smapmodel(ggd.bu_codes, bu_codes.sf)
    formula <- as.formula(sprintf('y ~
                                  s(age, by = sex,  bs = "ps", k = 10) +
                                  s(age, by = marital_status, bs = "ps", k = 10) +
                                  s(age, by = education, bs = "ps", k = 10) +
                                  s(sex, ethnicity,  bs = "re") +
                                  s(sex, marital_status, bs = "re") +
                                  s(sex, education, bs = "re") +
                                  s(hhtype, bs = "re") +
                                  s(hhsize, bs = "ps", k = 5) +
                                  s(hhincomesource, bs = "re") +
                                  s(hhhomeownership, bs = "re") +
                                  s(hhincome, bs = "ps", k = 10) +
                                  s(hhassets, bs = "ps", k = 10) +
                                  s(%s, bs = "ps", k = 10) +
                                  s(oad, bs = "ps", k = 10)', noise.measurement))
    model <- fit.smapmodel(model, train, formula)
    predictions <- predict.smapmodel(model, test)
    time.end <- Sys.time()
    timediff <- difftime(time.end, time.start, units="secs")[[1]]

    y.predict[index, "smapmodel"] <- predictions
    y.fittime[fold, "smapmodel"]  <- timediff

    
  }
  
  write.csv(format(y.predict, 4), fn.predict, row.names = F)
  write.csv(format(y.fittime, 4), fn.fittime, row.names = T)
}


ref_date <- "20160901"
data.population <- readRDS(file=sprintf("Data/Populatiebestanden/popdata_%s.rds", ref_date))
data.population <- data.population[data.population$age %in% 18:64,]
data.noise      <- readRDS(file=sprintf("Data/Populatiebestanden/noisedata_%s.rds", ref_date))
data.gemon      <- readRDS(file=sprintf("Data/Populatiebestanden/gemonnoisedata_%s.rds", ref_date))

# Data frame of population data for 18-64 year olds and their noise disturbance indicators
data.population <- merge(data.population, data.noise, by="rinpersoon")
merge.index     <- match(data.population$rinpersoon, data.gemon$rinpersoon)

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

# Noise disturbance indicator and the corresponding noise measurement used for prediction
measurements <- list(c("road_high","lden_wegv"), 
                     c("rail_high","lden_rail"), c("air_high","lden_air"), 
                     c("road_medhigh","lden_wegv"), c("rail_medhigh","lden_rail"), c("air_medhigh","lden_air"),
                     # Road traffic < 50km/h noise disturbance on municipal roads and > 50km/h noise disturbance of provincial and national roads
                     c("road_high_gt50" , "lden_weg_pwrw"), c("road_medhigh_gt50" , "lden_weg_pwrw"), 
                     c("road_high_sm50" , "lden_wegv_gw"), c("road_medhigh_sm50" , "lden_wegv_gw"),
                     # It is possible that the sum of road traffic (< 50km/h and > 50km/h) is a better predictor of  noise disturbance
                     c("road_high_gt50" , "lden_wegv"), c("road_medhigh_gt50" , "lden_wegv"), c("road_high_sm50" , "lden_wegv"), c("road_medhigh_sm50" , "lden_wegv"))

for (measurement in measurements){
  
  # Data with the given noise disturbance indicator and the corresponding noise measurement
  noise.disturbance = measurement[[1]] #noise.disturbance <- "road_high"
  noise.measurement = measurement[[2]] #noise.measurement <- "lden_wegv"
  experiment <- paste(noise.disturbance, "_", noise.measurement)
  print(experiment)
  
  # Take the data set where the outcome exists for every person
  data.population$y <- data.gemon[[noise.disturbance]][merge.index]
  data <- data.population[!is.na(data.population$y),]
  
  # Create cross validation folds
  set.seed(42)
  cv.index <- createFolds(as.factor(data$y), k=5)
  # Create vector that identifies to which test fold each sample belongs
  folds <- names(cv.index)
  cv.folds <- vector(mode="double", length=nrow(data))
  for (fold in folds) cv.folds[cv.index[[fold]]] <- fold
  
  # Files to save results into
  fn.predict <- sprintf("results/smap/paper/cache2/%s_predictions.txt", experiment)
  fn.fittime <- sprintf("results/smap/paper/cache2/%s_time.txt", experiment)
  
  # Save the predictions
  if (!file.exists(fn.predict))
  {
    n <- nrow(data)
    y.predict = data.frame(bu_code   = data$bu_code,
                           cv.fold   = cv.folds,
                           nullmodel = rep(NA,n),
                           y_true    = data$y)
  } else {
    y.predict <- read.csv(fn.predict)
  }
  # Save time taken to train
  if (!file.exists(fn.predict))
  {
    k <- length(folds)
    y.fittime = data.frame(row.names = folds,
                           nullmodel = rep(NA,k))
  } else {
    y.fittime <- read.csv(fn.fittime)
    rownames(y.fittime) <- y.fittime$X
    y.fittime$X         <- NULL
  }
  
  # For every train and test fold pair, fit each model to training data and predict values in test
  for (fold in folds) {
    print(fold)
    
    # Get train and test sets for the fold
    index = cv.index[[fold]]
    train <- data[-index,]
    test <- data[index,]
    
    # xgboost
    # -----------------------------------------------------------------------------------
    
    # Model matrix with x & y coordinates
    formula <- as.formula(sprintf("y ~ age + sex + ethnicity + marital_status + education + hhtype + hhsize +
                                  hhhomeownership + hhincomesource + hhincome + hhassets + oad + 
                                  x_coord + y_coord + %s - 1",
                                  noise.measurement))
    X.train <- sparse.model.matrix(formula, train)
    X.test  <- sparse.model.matrix(formula, test)
    
    # xgboost0_xy: XGBoost model with spatial information as x & y coordinates (default hyperparameters)
    # -----------------------------------------------------------------------------------
    
    time.start <- Sys.time()
    model <- xgboost(data=X.train, label=train$y, objective="binary:logistic", nrounds=50, verbose=0)
    predictions <- predict(model, X.test)
    time.end <- Sys.time()
    timediff <- difftime(time.end, time.start, units="secs")[[1]]
    
    y.predict[index, "xgboost0_xy"] <- predictions
    y.fittime[fold, "xgboost0_xy"]  <- timediff
    
    # xgboost_xy: XGBoost model with spatial information as x & y coordinates
    # -----------------------------------------------------------------------------------
    
    time.start <- Sys.time()
    model <- xgb.cv(list(objective="binary:logistic", eval_metric="auc"), data=X.train, label=train$y,
                    eta=0.1, nrounds=1000, print_every_n=100, verbose=1, nfold=5, stratified=T, early_stopping_rounds=10)
    model <- xgboost(data=X.train, label=train$y, objective="binary:logistic",
                     eta=0.1, nrounds=model$best_iteration, verbose=0)
    #model <- xgboost(data=X.train, label=y.train, objective="binary:logistic", eta=0.1, nrounds=200, verbose=0)
    predictions <- predict(model, X.test)
    time.end <- Sys.time()
    timediff <- difftime(time.end, time.start, units="secs")[[1]]
    
    y.predict[index, "xgboost_xy"] <- predictions
    y.fittime[fold, "xgboost_xy"]  <- timediff
    
    X.train <- NULL
    X.test  <- NULL
    
    # Model matrix with oblique coordinates
    formula <- as.formula(sprintf("y ~ age + sex + ethnicity + marital_status + education + hhtype + hhsize + 
                                  hhhomeownership + hhincomesource + hhincome + hhassets + oad + %s + %s - 1",
                                  ogc.string, noise.measurement))
    X.train <- sparse.model.matrix(formula, train)
    X.test  <- sparse.model.matrix(formula, test)
    
    # xgboost0_ogc: XGBoost model with spatial information as oblique coordinates (default hyperparameters)
    # -----------------------------------------------------------------------------------
    
    time.start <- Sys.time()
    model <- xgboost(data=X.train, label=train$y, objective="binary:logistic", nrounds=50, verbose=0)
    predictions <- predict(model, X.test)
    time.end <- Sys.time()
    timediff <- difftime(time.end, time.start, units="secs")[[1]]
    
    y.predict[index, "xgboost0_ogc"] <- predictions
    y.fittime[fold, "xgboost0_ogc"]  <- timediff
    
    # xgboost_ogc: XGBoost model with spatial information as oblique coordinates
    # -----------------------------------------------------------------------------------
    time.start <- Sys.time()
    model <- xgb.cv(list(objective="binary:logistic", eval_metric="auc"), data=X.train, label=train$y, 
                    eta=0.1, nrounds=1000, print_every_n=100, verbose=1, nfold=5, stratified=T, early_stopping_rounds=10)
    model <- xgboost(data=X.train, label=train$y, objective="binary:logistic", 
                     eta=0.1, nrounds=model$best_iteration, verbose=0)
    #model <- xgboost(data=X.train, label=y.train, objective="binary:logistic", eta=0.1, nrounds=200, verbose=0)
    predictions <- predict(model, X.test)
    time.end <- Sys.time()
    timediff <- difftime(time.end, time.start, units="secs")[[1]]
    
    y.predict[index, "xgboost_ogc"] <- predictions
    y.fittime[fold, "xgboost_ogc"]  <- timediff
    
    X.train <- NULL
    X.test  <- NULL
    
  }
  
  write.csv(format(y.predict, 4), fn.predict, row.names = F)
  write.csv(format(y.fittime, 4), fn.fittime, row.names = T)
}




# Read all predictions and save statistics of each model
models <- c("nullmodel", "glm", "xgboost0_xy", "xgboost_xy", "xgboost0_ogc", "xgboost_ogc", "smapmodel")
indicators <- list()
for (measurement in measurements){
  
  # Data with the given noise disturbance indicator and the corresponding noise measurement
  noise.disturbance = measurement[[1]] #noise.disturbance <- "road_high"
  noise.measurement = measurement[[2]] #noise.measurement <- "lden_wegv"
  experiment <- paste(noise.disturbance, "_", noise.measurement)
  print(experiment)
  
  # rmse of each model
  y.predict <- read.csv(sprintf("results/smap/paper/cache2/%s_predictions.txt", experiment))
  y.predmse <- unlist(lapply(y.predict[,models], 
                             function(y_pred) round(calculate_mse(y.predict$y_true, y_pred), 4)))
  # fitting times
  y.fittime <- read.csv(sprintf("results/smap/paper/cache2/%s_time.txt", experiment))
  rownames(y.fittime) <- y.fittime$X
  y.fittime$X         <- NULL
  y.fittime           <- round(colMeans(y.fittime),1)
  # extra information
  setDT(y.predict)
  y.prevalence <- y.predict[order(bu_code), .(xgboost_ogc = round(mean(xgboost_ogc), 4),
                                              smapmodel   = round(mean(smapmodel), 4)), by=bu_code]
  extra <- c(correlation = round(with(y.prevalence, cor(xgboost_ogc, smapmodel, method="pearson")), 2),
             pred.improv = round((1 - y.predmse[["xgboost_ogc"]] / y.predmse[["smapmodel"]]) * 100, 2),
             time.improv = round((1 - y.fittime[["xgboost_ogc"]] / y.fittime[["smapmodel"]]) * 100, 2))
  N <- c(n=nrow(y.predict))
  # row in the table
  indicators[[experiment]] <- c(y.predmse, extra, N)
}

metrics.table <- t(data.frame(indicators))
write.csv(metrics.table, sprintf("results/smap/paper/geluid_metrics.txt"), row.names = T)



# =====================================================================
#   EXPERIMENT: PREDICTION ACCURACY (5CV) OF MODELS IN WOON
# =====================================================================
ref_date <- "20180101"

# Load population and health monitor data
data.population <- readRDS(file=sprintf("Data/Populatiebestanden/popdata_%s_survey_filled.rds", ref_date)) 
data.woon      <- readRDS(file=sprintf("Data/Populatiebestanden/woondata_%s.rds", ref_date))
#data.population <- droplevels(data.population)
merge.index     <- match(data.population$rinpersoon, data.woon$rinpersoon)

# Number of missing values
data.original   <- readRDS(file=sprintf("Data/Populatiebestanden/popdata_%s_survey.rds", ref_date))
na.num          <- data.original %>% sapply(FUN = is.na) %>% colSums %>% sort
na.num[['n']]   <- nrow(data.original)
na.population   <- data.frame(data="population", variable=names(na.num), na=na.num)
data.original   <- NULL
na.num        <- data.woon %>% sapply(FUN = is.na) %>% colSums %>% sort
na.num[['n']] <- nrow(data.woon)
na.woon      <- data.frame(data="woon", variable=names(na.num), na=na.num)
na <- rbind(na.population, na.woon, make.row.names=F)
write.csv(na, "results/smap/paper/na_num_woon.txt", row.names = F)

# Population feature information
info <- c()

cols <- colnames(data.population)
for (col in cols[3:length(cols)]) {
  vals <- data.population[[col]]
  # For categorical features: categories (# of categories)
  if (is.factor(vals)) {
    lvls   <- levels(vals)
    lvls.n <- length(lvls)
    if (lvls.n > 10) lvls <- c(lvls[1:10], "...")
    info[[col]] <- sprintf("%s (%d)", paste(lvls, collapse=", "), lvls.n)
  }
  # For numerical features: median (min - max)
  else if (is.numeric(vals)) {
    lvls <- round(c(median(vals, na.rm=T), min(vals, na.rm=T), max(vals, na.rm=T)),0)
    info[[col]] <- sprintf("%d (%d-%d)", lvls[1], lvls[2], lvls[3])
  }
}
write.table(format(info, 4), "results/smap/paper/info_woon.txt", col.names = F, sep=",")

# Run experiment for these outcomes
outcomes_to_use <- list( "afraid_ngbh", "social_cohesion", "satisfied_region", "annoyed_w_ngbh",
                         "attached_to_ngbh", "satisfied_house", "satisfied_surroundings", "at_home_in_ngbh")

for (outcome in outcomes_to_use) { 
  print(outcome)
  
  # Take the data set where the outcome exists for every person
  data.population$y <- data.woon[[outcome]][merge.index]
  data <- data.population[!is.na(data.population$y),]
  
  # Create cross validation folds
  set.seed(42)
  cv.index <- createFolds(as.factor(data$y), k=5)
  # Create vector that identifies to which test fold each sample belongs
  folds <- names(cv.index)
  cv.folds <- vector(mode="double", length=nrow(data))
  for (fold in folds) cv.folds[cv.index[[fold]]] <- fold
  
  # Files to save results into
  fn.predict <- sprintf("results/smap/paper/cache/%s_predictions.txt", outcome)
  fn.fittime <- sprintf("results/smap/paper/cache/%s_time.txt", outcome)
  
  # Save the predictions
  if (!file.exists(fn.predict))
  {
    n <- nrow(data)
    y.predict = data.frame(bu_code   = data$bu_code,
                           cv.fold   = cv.folds,
                           nullmodel = rep(NA,n),
                           y_true    = data$y)
  } else {
    y.predict <- read.csv(fn.predict)
  }
  # Save time taken to train
  if (!file.exists(fn.predict))
  {
    k <- length(folds)
    y.fittime = data.frame(row.names = folds,
                           nullmodel = rep(NA,k))
  } else {
    y.fittime <- read.csv(fn.fittime)
    rownames(y.fittime) <- y.fittime$X
    y.fittime$X         <- NULL
  }
  
  # For every train and test fold pair, fit each model to training data and predict values in test
  for (fold in folds) {
    print(fold)
    
    # Get train and test sets for the fold
    index = cv.index[[fold]]
    train <- data[-index,]
    test <- data[index,]
    
    # nullmodel: predict the mean value for every person
    # --------------------------------------------------
    
    time.start <- Sys.time()
    model <- nullmodel(train)
    predictions <- predict(model, test)
    time.end <- Sys.time()
    timediff <- difftime(time.end, time.start, units="secs")[[1]]
    
    y.predict[index, "nullmodel"] <- predictions
    y.fittime[fold, "nullmodel"]  <- timediff
    
    
    # glm: Simple linear model without spatial information
    # ----------------------------------------------------
    
    time.start <- Sys.time()
    model <- glm(y~ age + sex + ethnicity + marital_status + education +
                   hhtype + hhsize + hhincomesource + hhincome + hhassets + oad + hhhomeownership +
                   avg_household_size_nbh + perc_uninhabited_nbh + perc_singlefamhome_nbh +
                   perc_nonrentals_nbh + perc_housingcorporation_nbh + perc_builtbefore2000_nbh +
                   schools_within_3km_nbh + dist_to_public_green_total + dist_to_forest + dist_to_backwater - 1,
                 data = train, family = "gaussian")
    predictions <- predict(model, newdata = test, type = "response")
    time.end <- Sys.time()
    timediff <- difftime(time.end, time.start, units="secs")[[1]]
    
    y.predict[index, "glm"] <- predictions
    y.fittime[fold, "glm"]  <- timediff
    
    # smapmodel: The original SMAP model
    # ----------------------------------
    
    time.start <- Sys.time()
    # SMAP is fitted separately to each ggd region using spatial information
    ggd.bu_codes <- ggd_bucodes(data.population) # bu_codes for each ggd region
    bu_codes.sf  <- DataGeometry(ref_date)       # bu_codes geometry information
    model <- smapmodel(ggd.bu_codes, bu_codes.sf)
    formula = y ~
      s(age, by = sex,  bs = "ps", k = 10) +
      s(age, by = ethnicity,  bs = "ps", k = 10) +
      s(age, by = marital_status, bs = "ps", k = 10) +
      s(age, by = education, bs = "ps", k = 10) +
      s(sex, ethnicity,  bs = "re") +
      s(sex, marital_status, bs = "re") +
      s(sex, education, bs = "re") +
      s(hhtype, bs = "re") +
      s(hhsize, bs = "ps", k = 5) +
      s(hhincomesource, bs = "re") +
      s(hhhomeownership, bs = "re") +
      s(hhincome, bs = "ps", k = 10) +
      s(hhassets, bs = "ps", k = 10) +
      s(oad, bs = "ps", k = 10)+
      s(avg_household_size_nbh, bs="ps", k=5) +
      s(perc_uninhabited_nbh, bs="ps", k=5) +
      s(perc_singlefamhome_nbh, bs="ps", k=5) +
      s(perc_nonrentals_nbh, bs="ps", k=5) +
      s(perc_housingcorporation_nbh, bs="ps", k=5) +
      s(perc_builtbefore2000_nbh, bs="ps", k=5) +
      s(schools_within_3km_nbh, bs="ps", k=5) +
      s(dist_to_public_green_total, bs="ps", k=5) +
      s(dist_to_forest, bs="ps", k=5) +
      s(dist_to_backwater, bs="ps", k=5)
    model <- fit.smapmodel(model, train, formula, gaussian())
    predictions <- predict.smapmodel(model, test)
    time.end <- Sys.time()
    timediff <- difftime(time.end, time.start, units="secs")[[1]]
    
    y.predict[index, "smapmodel"] <- predictions
    y.fittime[fold, "smapmodel"]  <- timediff
    
  }
  
  write.csv(format(y.predict, 4), fn.predict, row.names = F)
  write.csv(format(y.fittime, 4), fn.fittime, row.names = T)
}



# Load population data (not filled)
data.population <- readRDS(file=sprintf("Data/Populatiebestanden/popdata_%s_survey.rds", ref_date))
#data.population <- droplevels(data.population)
merge.index     <- match(data.population$rinpersoon, data.woon$rinpersoon)

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

for (outcome in outcomes_to_use) { 
  print(outcome)
  
  # Take the data set where the outcome exists for every person
  data.population$y <- data.woon[[outcome]][merge.index]
  data <- data.population[!is.na(data.population$y),]
  
  # Create cross validation folds
  set.seed(42)
  cv.index <- createFolds(as.factor(data$y), k=5)
  # Create vector that identifies to which test fold each sample belongs
  folds <- names(cv.index)
  cv.folds <- vector(mode="double", length=nrow(data))
  for (fold in folds) cv.folds[cv.index[[fold]]] <- fold
  
  # Files to save results into
  fn.predict <- sprintf("results/smap/paper/cache/%s_predictions.txt", outcome)
  fn.fittime <- sprintf("results/smap/paper/cache/%s_time.txt", outcome)
  
  # Save the predictions
  if (!file.exists(fn.predict))
  {
    n <- nrow(data)
    y.predict = data.frame(bu_code   = data$bu_code,
                           cv.fold   = cv.folds,
                           nullmodel = rep(NA,n),
                           y_true    = data$y)
  } else {
    y.predict <- read.csv(fn.predict)
  }
  # Save time taken to train
  if (!file.exists(fn.predict))
  {
    k <- length(folds)
    y.fittime = data.frame(row.names = folds,
                           nullmodel = rep(NA,k))
  } else {
    y.fittime <- read.csv(fn.fittime)
    rownames(y.fittime) <- y.fittime$X
    y.fittime$X         <- NULL
  }
  
  # For every train and test fold pair, fit each model to training data and predict values in test
  for (fold in folds) {
    print(fold)
    
    # Get train and test sets for the fold
    index = cv.index[[fold]]
    train <- data[-index,]
    test <- data[index,]
    
    # xgboost
    # -----------------------------------------------------------------------------------
    
    # Model matrix with x & y coordinates
    formula    <- formula <- y ~ age + sex + ethnicity + marital_status + education +
      hhtype + hhsize + hhincomesource + hhincome + hhassets + oad +
      avg_household_size_nbh + perc_uninhabited_nbh + perc_singlefamhome_nbh +hhhomeownership +
      perc_nonrentals_nbh + perc_housingcorporation_nbh + perc_builtbefore2000_nbh +
      schools_within_3km_nbh + dist_to_forest + dist_to_public_green_total +  
      dist_to_backwater + x_coord + y_coord - 1
    X.train <- Matrix(model.matrix(formula, train), sparse=T)
    X.test  <- Matrix(model.matrix(formula, test), sparse=T)
    
    # xgboost0_xy: XGBoost model with spatial information as x & y coordinates (default hyperparameters)
    # -----------------------------------------------------------------------------------
    
    time.start <- Sys.time()
    model <- xgboost(data=X.train, label=train$y, objective="reg:squarederror", nrounds=50, verbose=0)
    predictions <- predict(model, X.test)
    time.end <- Sys.time()
    timediff <- difftime(time.end, time.start, units="secs")[[1]]
    
    y.predict[index, "xgboost0_xy"] <- predictions
    y.fittime[fold, "xgboost0_xy"]  <- timediff
    
    # xgboost_xy: XGBoost model with spatial information as x & y coordinates
    # -----------------------------------------------------------------------------------
    
    time.start <- Sys.time()
    model <- xgb.cv(list(objective="reg:squarederror"), data=X.train, label=train$y,
                    eta=0.1, nrounds=1000, print_every_n=100, verbose=1, nfold=5, stratified=T, 
                    early_stopping_rounds=10)
    model <- xgboost(data=X.train, label=train$y, objective="reg:squarederror",
                     eta=0.1, nrounds=model$best_iteration, verbose=0)
    #model <- xgboost(data=X.train, label=y.train, objective="binary:logistic", eta=0.1, nrounds=200, verbose=0)
    predictions <- predict(model, X.test)
    time.end <- Sys.time()
    timediff <- difftime(time.end, time.start, units="secs")[[1]]
    
    y.predict[index, "xgboost_xy"] <- predictions
    y.fittime[fold, "xgboost_xy"]  <- timediff
    
    X.train <- NULL
    X.test  <- NULL
    
    # Model matrix with oblique coordinates
    formula <- as.formula(sprintf("y ~  age + sex + ethnicity + marital_status + education +
                                  hhtype + hhsize + hhincomesource + hhincome + hhassets + oad +
                                  avg_household_size_nbh + perc_uninhabited_nbh + perc_singlefamhome_nbh +hhhomeownership +
                                  perc_nonrentals_nbh + perc_housingcorporation_nbh + perc_builtbefore2000_nbh +
                                  schools_within_3km_nbh + dist_to_forest + dist_to_public_green_total +  dist_to_backwater + %s - 1",
                                  ogc.string))
    X.train <- Matrix(model.matrix(formula, train), sparse=T)
    X.test  <- Matrix(model.matrix(formula, test), sparse=T)
    
    # xgboost0_ogc: XGBoost model with spatial information as oblique coordinates (default hyperparameters)
    # -----------------------------------------------------------------------------------
    
    time.start <- Sys.time()
    model <- xgboost(data=X.train, label=train$y, objective="reg:squarederror", nrounds=50, verbose=0)
    predictions <- predict(model, X.test)
    time.end <- Sys.time()
    timediff <- difftime(time.end, time.start, units="secs")[[1]]
    
    y.predict[index, "xgboost0_ogc"] <- predictions
    y.fittime[fold, "xgboost0_ogc"]  <- timediff
    
    # xgboost_ogc: XGBoost model with spatial information as oblique coordinates
    # -----------------------------------------------------------------------------------
    time.start <- Sys.time()
    model <- xgb.cv(list(objective="reg:squarederror"), data=X.train, label=train$y, 
                    eta=0.1, nrounds=1000, print_every_n=100, verbose=1, nfold=5, stratified=T, early_stopping_rounds=10)
    model <- xgboost(data=X.train, label=train$y, objective="reg:squarederror", 
                     eta=0.1, nrounds=model$best_iteration, verbose=0)
    #model <- xgboost(data=X.train, label=y.train, objective="binary:logistic", eta=0.1, nrounds=200, verbose=0)
    predictions <- predict(model, X.test)
    time.end <- Sys.time()
    timediff <- difftime(time.end, time.start, units="secs")[[1]]
    
    y.predict[index, "xgboost_ogc"] <- predictions
    y.fittime[fold, "xgboost_ogc"]  <- timediff
    
    X.train <- NULL
    X.test  <- NULL
    
  }
  
  write.csv(format(y.predict, 4), fn.predict, row.names = F)
  write.csv(format(y.fittime, 4), fn.fittime, row.names = T)
}



#SAVING THE METRICS
models <- c("nullmodel", "glm", "smapmodel", "xgboost0_xy", "xgboost_xy", "xgboost0_ogc", "xgboost_ogc")

indicators <- list()
for (outcome in outcomes_to_use){
  # rmse of each model
  y.predict <- read.csv(sprintf("results/smap/paper/cache/%s_predictions.txt", outcome))
  y.predmse <- unlist(lapply(y.predict[,models], 
                             function(y_pred) round(calculate_mse(y.predict$y_true, y_pred), 4)))
  # fitting times
  y.fittime <- read.csv(sprintf("results/smap/paper/cache/%s_time.txt", outcome))
  rownames(y.fittime) <- y.fittime$X
  y.fittime$X         <- NULL
  y.fittime           <- round(colMeans(y.fittime),1)
  # extra information
  setDT(y.predict)
  y.prevalence <- y.predict[order(bu_code), .(xgboost_ogc = round(mean(xgboost_ogc), 4),
                                              smapmodel   = round(mean(smapmodel), 4)), by=bu_code]
  extra <- c(correlation = round(with(y.prevalence, cor(xgboost_ogc, smapmodel, method="pearson")), 2),
             pred.improv = round((1 - y.predmse[["xgboost_ogc"]] / y.predmse[["smapmodel"]]) * 100, 2),
             time.improv = round((1 - y.fittime[["xgboost_ogc"]] / y.fittime[["smapmodel"]]) * 100, 2))
  N <- c(n=nrow(y.predict))
  # row in the table
  indicators[[outcome]] <- c(y.predmse, extra, N)
}

metrics.table <- t(data.frame(indicators))
write.csv(metrics.table, sprintf("results/smap/paper/woon_metrics.txt"), row.names = T)


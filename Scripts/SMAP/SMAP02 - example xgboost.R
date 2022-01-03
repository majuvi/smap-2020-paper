#source("Scripts/General/data_loading.R")
source("Scripts/General/smap_modeling.R")
source("Scripts/General/evaluation.R")
library(xgboost)
library(Matrix)
library(data.table)
options(na.action='na.pass')

ref_date <- "20160901"

# Load population and health monitor data
data.population <- readRDS(file=sprintf("Data/Populatiebestanden/popdata_%s.rds", ref_date))
data.gemon      <- readRDS(file=sprintf("Data/Populatiebestanden/gemondata_%s.rds", ref_date))
merge.index     <- match(data.population$rinpersoon, data.gemon$rinpersoon)

outcome = "drinker"
data.population$y <- data.gemon[[outcome]][merge.index]

# Evaluate the model by splitting the health monitor into train & test set
# ------------------------------------------------------------------------

# Take the data set where the example outcome "drinker" exists for every person and split into train & test
data <- data.population[!is.na(data.population$y),]
sample <- sample.int(n=nrow(data), size=round(0.66*nrow(data)), replace=F)
train  <- data[sample,]
test   <- data[-sample,]

# Xgboost requires the model matrix as input
formula <- y ~ age + sex + ethnicity + marital_status + education + 
  hhtype + hhsize + hhhomeownership + hhincomesource + hhincome + hhassets + oad + x_coord + y_coord - 1
X.train <- sparse.model.matrix(formula, train)
X.test  <- sparse.model.matrix(formula, test)
# Get optimal number of iterations with 10 fold inner cross validation, then train with those on train data
model <- xgb.cv(list(objective="binary:logistic", eval_metric="rmse"), data=X.train, label=train$y, 
                nrounds=300, verbose=1, nfold=10, stratified=T, early_stopping_rounds=10)
model <- xgboost(objective="binary:logistic", data=X.train, label=train$y, nrounds=model$best_iteration, verbose=0)
# Predict the missing outcomes
predictions <- predict(model, X.test)
print(calculate_auc(test$y, predictions))

# Predict missing population values from a model trained on the health monitor
# ----------------------------------------------------------------------------

# Train is the observed values and test is the remaining population
sample <- !is.na(data.population$y)
train  <- data.population[sample,]
test   <- data.population[!sample,]

# Xgboost requires the model matrix as input
formula <- y ~ age + sex + ethnicity + marital_status + education + 
  hhtype + hhsize + hhhomeownership + hhincomesource + hhincome + hhassets + oad + x_coord + y_coord - 1
X.train <- sparse.model.matrix(formula, train)
X.test  <- sparse.model.matrix(formula, test)
# Get optimal number of iterations with 10 fold inner cross validation, then train with those on train data
model <- xgb.cv(list(objective="binary:logistic", eval_metric="rmse"), data=X.train, label=train$y, 
                nrounds=300, verbose=1, nfold=10, stratified=T, early_stopping_rounds=10)
model <- xgboost(objective="binary:logistic", data=X.train, label=train$y, nrounds=model$best_iteration, verbose=0)
# Predict the missing outcomes
test$y <- predict(model, X.test)

# Calculate prevalence from observed and predicted values
prevalence <- rbind(train[, c("gm_code", "wk_code", "bu_code", "y")],
                    test[, c("gm_code", "wk_code", "bu_code", "y")])
prevalence.wk <- calculate_prevalence(prevalence, code="wk_code")
write.csv(format(prevalence.wk, 4), sprintf("results/smap/xgboost_%s_prevalence.txt", outcome), row.names=F)

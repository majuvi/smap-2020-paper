#source("Scripts/General/data_loading.R")
source("Scripts/General/geometry_loading.R")
source("Scripts/General/smap_modeling.R")
source("Scripts/General/evaluation.R")

ref_date <- "20160901"

# Load population and health monitor data
data.population <- readRDS(file=sprintf("Data/Populatiebestanden/popdata_%s_filled.rds", ref_date))
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

# SMAP is fitted separately to each ggd region using spatial information
ggd.bu_codes <- ggd_bucodes(data.population) # bu_codes for each ggd region
bu_codes.sf  <- DataGeometry(ref_date)       # bu_codes geometry information
model <- smapmodel(ggd.bu_codes, bu_codes.sf)

# Train the model
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

# Predict the missing outcomes
predictions <- predict.smapmodel(model, test)
print(calculate_auc(test$y, predictions))


# Predict missing population values from a model trained on the health monitor
# ----------------------------------------------------------------------------

# Train is the observed values and test is the remaining population
sample <- !is.na(data.population$y)
train  <- data.population[sample,]
test   <- data.population[!sample,]

# Train the model
model <- fit.smapmodel(model, train, formula)

# Predict the missing outcomes
test$y <- predict.smapmodel(model, test)

# Calculate prevalence from observed and predicted values
prevalence <- rbind(train[, c("gm_code", "wk_code", "bu_code", "y")],
                    test[, c("gm_code", "wk_code", "bu_code", "y")])
prevalence.wk <- calculate_prevalence(prevalence, code="wk_code")
write.csv(format(prevalence.wk, 4), sprintf("results/smap/smapmodel_%s_prevalence.txt", outcome), row.names=F)

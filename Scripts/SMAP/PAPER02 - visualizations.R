library(ggplot2)
library(grid)
library(gridExtra)
library(ggpubr)
library(sf)

DataGeometry <- function(ref_date, which="bu") {
  ref_year <- substr(ref_date, start = 1, stop = 4)
  if (which == "bu") {
    data.geometry <- st_read(sprintf("Data/Geodata/cbs_buurt/cbs_buurt_%s.geojson", ref_year))
    data.geometry <- data.geometry[data.geometry$statnaam != "",]
    colnames(data.geometry) <- c("bu_code", "bu_name", "geometry")
  } else if (which == "wk") {
    data.geometry <- st_read(sprintf("Data/Geodata/cbs_wijk/cbs_wijk_%s.geojson", ref_year))
    data.geometry <- data.geometry[data.geometry$statnaam != "",]
    colnames(data.geometry) <- c("wk_code", "wk_name", "geometry")
  } else {
    data.geometry <- st_read(sprintf("Data/Geodata/cbs_gemeente/cbs_gemeente_%s.geojson", ref_year))
    data.geometry <- data.geometry[data.geometry$statnaam != "",]
    colnames(data.geometry) <- c("gm_code", "gm_name", "geometry")
  }
  return(data.geometry)
}

ref_date <- "20200901"
outcome <- "drinker"

# TABLE : Number of missing values and sample size in GEMON / Population microdata
print(read.csv("results/smap/paper/na_num.txt"))


# TABLE : Population microdata features
print(read.csv("results/smap/paper/info.txt", col.names=c('variable', 'values')))


# IMAGE : map of example indicator "drinker" prevalence at the bu_code level, GEMON vs. model estimates
y.prevalence <- read.csv(sprintf("results/smap/paper/%s_prevalence.txt", outcome))
sf <- DataGeometry(ref_date, which="bu")
sf <- merge(x = sf, y = y.prevalence)
#to_quantile <- function(y, n=6) cut(y, round(quantile(y, na.rm=T, probs=seq(0,1,length.out=n)),2))
# Direct estimates
p.limits <- c(0.33, 0.78, 0.81, 0.84, 0.86, 0.94)
sf$direct  <- cut(sf$y, p.limits) #to_quantile(sf$y)

theme_to_use <- theme(legend.position="bottom", 
                      legend.title=element_blank(), legend.text=element_text(size=10), 
                      legend.key.size = unit(0.4, 'cm'),
                      panel.grid.major = element_blank(),
                      panel.background=element_rect(fill='lightgray'),
                      axis.ticks = element_blank(), 
                      axis.text = element_blank()
)
p1 <- ggplot(sf) + geom_sf(aes(fill=direct), color=NA) + scale_fill_brewer(palette="RdYlGn", direction=-1) +
  labs(title="Direct estimates") + theme_to_use + 
  guides(fill=guide_legend(nrow=2,byrow=T)) 
# XGBoost estimates
sf$xgboost <- cut(sf$xgboost0_ogc, p.limits)#to_quantile(sf$xgboost0_ogc)
p2 <- ggplot(sf) + geom_sf(aes(fill=xgboost), color=NA) + scale_fill_brewer(palette="RdYlGn", direction=-1) +
  labs(title="Model estimates") + theme_to_use +
  guides(fill=guide_legend(nrow=2,byrow=T)) 
# XGBoost probabilties of exceeding the mean
sf$risk_class <- cut(1.0-sf$p, c(0.0,0.05,0.10,0.90,0.95,1.0), include.lowest=T)
p3 <- ggplot(sf) + geom_sf(aes(fill=risk_class), color=NA) + scale_fill_brewer(palette="PiYG", direction=-1) +
  labs(title="Model risk class") + theme_to_use +
  guides(fill=guide_legend(nrow=2,byrow=T)) 
# Plot side-by-side
title.text <- sprintf("Percentage of drinkers and probability of exceeding the mean in each neighbourhood", outcome)
grid.arrange(p1, p2, p3, nrow=1, top=textGrob(title.text, gp=gpar(fontsize=16)))
#grid.arrange(p1, p2, nrow=1, top=textGrob(title.text, gp=gpar(fontsize=16)))

# IMAGE : map of example indicator "drinker" prevalence at the bu_code level, GEMON vs. model estimates
y.prevalence <- read.csv(sprintf("results/smap/paper/%s_prevalence.txt", outcome))
sf <- DataGeometry(ref_date, which="bu")
sf <- merge(x = sf, y = y.prevalence)
to_quantile <- function(y, n=10) {
  y = y *100
  return(cut(y, round(quantile(y, na.rm=T, probs=seq(0,1,length.out=n)),2)), include.lowest=T) }

# Direct estimates
n <- 10
quantiles.1 <- quantile(sf$smapmodel, probs=seq(0,1,length.out=n), na.rm=T)
quantiles.2 <- quantile(sf$xgboost_ogc, probs=seq(0,1,length.out=n), na.rm=T)
quantiles.min <- min(c(quantiles.1, quantiles.2), na.rm=T)
quantiles.max <- max(c(quantiles.1, quantiles.2), na.rm=T)
quantiles <- c(quantiles.min, quantiles.1[2:(n-1)], quantiles.max)
#sf$direct  <- to_quantile(sf$y)
#p1 <- ggplot(sf) + geom_sf(aes(fill=direct), color=NA) + scale_fill_brewer(palette="RdYlGn", direction=-1) +
#  labs(title="Direct estimates") + theme_to_use +
#  guides(fill=guide_legend(nrow=3,byrow=T))
# STAR estimates
sf$smapmodel <- cut(sf$smapmodel, quantiles, include.lowest = T, dig.lab=2)#to_quantile(sf$smapmodel)
p2 <- ggplot(sf) + geom_sf(aes(fill=smapmodel), color=NA) + scale_fill_brewer(palette="RdYlGn", direction=-1) +
  labs(title="STAR estimates") +theme_to_use #+ guides(fill=guide_legend(nrow=3,byrow=T))
# XGBoost estimates
sf$xgboost <- cut(sf$xgboost_ogc, quantiles, include.lowest = T, dig.lab=2)#to_quantile(sf$xgboost_ogc)
p3 <- ggplot(sf) + geom_sf(aes(fill=xgboost), color=NA) + scale_fill_brewer(palette="RdYlGn", direction=-1) +
  labs(title="XGBoost estimates") + theme_to_use #+ guides(fill=guide_legend(nrow=3,byrow=T))
# Plot side-by-side
#title.text <- sprintf("Percentage of drinkers in each neighbourhood", outcome)
#grid.arrange(p1, p2, p3, nrow=1, top=textGrob(title.text, gp=gpar(fontsize=16)))
ggarrange(p2, p3, nrow=1, common.legend = T, legend="bottom")

# # IMAGE : histogram number of people in each neighbourhood: GEMON / population
# y.valuecount <- read.csv(sprintf("results/smap/paper/%s_valuecount.txt", outcome))
# # These are the original counts per bu_code, though we don't know which bu_code
# n.gemon      <- with(y.valuecount, rep(n.bu_code, count.gemon))
# n.population <- with(y.valuecount, rep(n.bu_code, count.population))
# n.bu_code    <- rbind(data.frame(Data="Health Monitor", N=n.gemon),
#                       data.frame(Data="Population microdata", N=n.population))
# # Plot a side-by-side histogram of counts per bu_code
# ggplot(n.bu_code, aes(x=N, color=Data, fill=Data)) + 
#   geom_histogram(position = "dodge", alpha=0.5) + scale_x_log10(name=) +
#   labs(title="Motivation of Small Area Estimation: most neighbourhoods have few people", 
#        color="Data set", fill="Data set", x="Number of people", y="Number of neighbourhoods") +
#   theme(legend.position = "top") + facet_grid(.~Data)


# TABLE : accuracy metrics and cpu time of different models for an example indicator "drinker" 
metrics <- read.csv(sprintf("results/smap/paper/%s_metrics.txt", outcome))
print(metrics)

# TABLE : MSE of different models and xgboost vs. smap accuracy/time for all health indicators
metrics.all <- read.csv("results/smap/paper/all_metrics.txt")
print(metrics.all)

# TABLE : MSE of different models and xgboost vs. smap accuracy/time for all living quality indicators
metrics.all <- read.csv("results/smap/paper/woon_metrics.txt")
print(metrics.all)

# TABLE : MSE of different models and xgboost vs. smap accuracy/time for all noise indicators
metrics.geluid <- read.csv("results/smap/paper/geluid_metrics.txt")
print(metrics.geluid)

#setDT(metrics.all)
#rmse.long <- melt(metrics.all, id.vars="X", variable.name="model")
#rmse.long <- subset(rmse.long, model %in% colnames(metrics.all)[2:8])
#rmse.long$model <- with(rmse.long, relevel(model, "nullmodel"))
#ggplot(data=rmse.long, aes(x= X, y=value, fill=model)) + geom_bar(stat="identity", position=position_dodge()) + 
#  theme(legend.position="top", axis.text.x=element_text(angle=90, vjust=0.5, hjust=1)) + 
#  labs(title="Model performance for different health indicators", x="Health indicator", y="MSE") 

# IMAGE : ROC curve and calibration curve of each model for an example indicator "drinker"
models <- c("nullmodel", "starmodel", "xgboost0_xy", "xgboost_xy", "xgboost0_ogc", "xgboost_ogc")
df.roc_curve   <- read.csv(sprintf("results/smap/paper/%s_roc.txt", outcome))
df.calibration <- read.csv(sprintf("results/smap/paper/%s_calibration.txt", outcome))
levels(df.roc_curve$model)[levels(df.roc_curve$model) == "smapmodel"] <- "starmodel"
levels(df.calibration$model)[levels(df.calibration$model) == "smapmodel"] <- "starmodel"
df.roc_curve   <- subset(df.roc_curve, model %in% models)
df.calibration <- subset(df.calibration, model %in% models)
# Plot ROC curve
df.roc_curve$model <- with(df.roc_curve, relevel(model, "nullmodel"))
p1 <- ggplot(data=df.roc_curve, aes(x=1-specificity, y=sensitivity, group=model, color=model)) + 
  geom_line() + geom_point() + xlim(0,1) + ylim(0,1) + 
  labs(title="ROC curve", x="False positive rate", y="True positive rate") 
# Plot calibration curve
df.calibration$model <- with(df.calibration, relevel(model, "nullmodel"))
p2 <- ggplot(data=df.calibration, aes(x=y_pred, y=y_true, group=model, color=model)) + 
  geom_abline(intercept=0,slope=1,linetype="dashed") + 
  geom_line() + geom_point() + xlim(0,1) + ylim(0,1) + 
  labs(title="Calibration curve", x="Predicted probability", y="Observed probability")
# Plot side-by-side
#title.text <- sprintf("Discrimination and calibration of models for '%s' health indicator", outcome)
#grid.arrange(p1, p2, nrow=1, top=textGrob(title.text, gp=gpar(fontsize=16)))
ggarrange(p1, p2, ncol=2, nrow=1, legend="bottom", common.legend =T)


# IMAGE: XGBoost vs. SMAP predictions and effect of area size
y.prevalence <- read.csv(sprintf("results/smap/paper/%s_prevalence.txt", outcome))
title <- "Prevalence in each neighbourhood"
p1 <- ggplot(y.prevalence, aes(x=xgboost_ogc, y=smapmodel)) + 
  geom_point(color="darkblue") + geom_abline(intercept=0,slope=1) + xlim(0,1) + ylim(0,1) + 
  labs(title=title, x="XGBoost", y="STAR model") 
# Plot the effect of area size
mse.quantile <- read.csv(sprintf("results/smap/paper/%s_areamse.txt", outcome))
mse.quantile <- mse.quantile[,c("area_size","xgboost_ogc","smapmodel")]
mse.area_sizes <- mse.quantile$area_size
setDT(mse.quantile)
mse.quantile <- melt(mse.quantile, id.var="area_size", variable.name="model", value.name="mse")
levels(mse.quantile$model)[levels(mse.quantile$model) == "smapmodel"] <- "starmodel"
mse.quantile$area_size <- factor(mse.quantile$area_size, mse.area_sizes)
p2 <- ggplot(mse.quantile, aes(x=area_size, y=mse, group=model, color=model)) + geom_line() + 
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1)) +
  labs(title="Small Area Estimation accuracy", 
       x="Survey respondents in the neighbourhood", 
       y="Mean Squared Error (MSE)")
ggarrange(p1, p2, ncol=2, nrow=1, legend="bottom", common.legend =T)


# IMAGE: tuning the hyperparameters learning rate and number of iterations
evaluation <- read.csv(sprintf("results/smap/paper/%s_hyperparameters_xgboost.txt", outcome))
evaluation$eta.num <- evaluation$eta
evaluation$eta <- with(evaluation, as.factor(eta))
# Validation AUC as a function of iterations with different learning rates
p1 <- ggplot(evaluation, aes(x=iter, y=test_rmse, group=eta, color=eta)) + geom_line() + scale_x_log10() +
  labs(title="Hyperparameters: eta, nrounds", x="Iteration (nrounds)", y="Validation RMSE")
# Optimal validation AUC with early stopping using different learning rates
eta.max <- aggregate(test_rmse ~ eta.num, evaluation, min)
p2 <-ggplot(eta.max, aes(x=eta.num, y=test_rmse)) + geom_line(color='blue') + scale_x_log10() +
  labs(title="Optimal eta with early stopping", x="Learning rate (eta)", y="Lowest validation RMSE")
ggarrange(p1, p2, ncol=2, nrow=1, legend="top")
# IMAGE: Early stopping illustration, train and Validation AUC as a function of iterations
evaluation.subset <- data.table(subset(evaluation, eta == 0.3, select=c('iter', 'train_rmse', 'test_rmse')))
evaluation.subset <- melt(evaluation.subset, id.vars="iter")
ggplot(data=evaluation.subset, aes(x=iter, y=value, group=variable, color=variable)) + geom_line() +#+ xlim(0,300) + ylim(0,1) + 
  labs(title="5-fold CV: Train and Validation RMSE over iterations", x="Iteration", y="RMSE") 


# smapmodel: terms
shap.values <- read.csv(sprintf("results/smap/paper/%s_interpretation_smap.txt", outcome))
shap.values$column <- trimws(shap.values$column)
shap.values$value <- trimws(shap.values$value)
shap.values$by <- trimws(shap.values$by)
# Plot all interaction terms
p <- list()
for (col in c('s(age, by = sex,  bs = "ps", k = 10)', 's(age, by = ethnicity,  bs = "ps", k = 10)', 
              's(age, by = marital_status, bs = "ps", k = 10)', 's(age, by = education, bs = "ps", k = 10)',
              's(sex, ethnicity,  bs = "re")', 's(sex, marital_status,  bs = "re")', 's(sex, education,  bs = "re")')) {
  shap.subset <- subset(shap.values, column == col)
  if (col %in% c('s(age, by = sex,  bs = "ps", k = 10)', 's(age, by = ethnicity,  bs = "ps", k = 10)', 
                 's(age, by = marital_status, bs = "ps", k = 10)', 's(age, by = education, bs = "ps", k = 10)')) 
    shap.subset$value <- strtoi(shap.subset$value)
  p[[col]] <- ggplot(shap.subset, aes(x=value, y=shap, fill=by, color=by)) + geom_point() +
    labs(title=col, y="Term", x="Feature value") +  
    theme(plot.title=element_text(size=10), 
          axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x=element_text(angle=90), 
          legend.title=element_text(size=8), legend.text=element_text(size=8), legend.position="right")
}
# Plot all feature terms
for (col in c("hhtype" , "hhsize", "hhincomesource", "hhhomeownership", "hhincome", "hhassets", "oad")) {
  shap.subset <- subset(shap.values, column == sprintf("s(%s)", col))
  if (col %in% c("hhsize", "hhincome", "hhassets", "oad")) 
    shap.subset$value <- strtoi(shap.subset$value)
  p[[col]] <- ggplot(shap.subset, aes(x=value, y=shap)) + geom_point(color="blue") +
    labs(title=col, y="Term", x="Feature value") + theme(plot.title=element_text(size=10), 
                                                         axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x=element_text(angle=90), legend.position="right")
}
# smapmodel: bu_code markov random field
shap.subset <- subset(shap.values, column == "s(bu_code)")
sf <- DataGeometry(ref_date, which="bu")
sf <- merge(x = sf, y = shap.subset, by.x = "bu_code", by.y="value")
sf$quantile <- with(sf, cut(shap, quantile(shap, na.rm=T, probs=seq(0,1,length.out=11))))
p[["bu_code"]] <- ggplot(sf) + geom_sf(aes(fill=quantile), color=NA) + 
  theme(plot.title=element_text(size=10), legend.position="none") +
  scale_fill_brewer(palette="RdYlGn", direction=-1) + labs(title="bu_code") 
# plot all
grid.arrange(grobs=p, ncol=5, top=textGrob("STAR model: terms averaged over GGD region models", gp=gpar(fontsize=16)))


# XGBoost: feature SHAP values
shap.values <- read.csv(sprintf("results/smap/paper/%s_interpretation_xgboost.txt", outcome))
shap.values$column <- trimws(shap.values$column)
shap.values$value <- trimws(shap.values$value)
# Plot the average shapley value of each feature
p <- list()
for (col in unique(shap.values$column)) {
  shap.subset <- subset(shap.values, column == col)
  if (col %in% c("age", "hhsize", "hhincome", "hhassets", "oad")) 
    shap.subset$value <- strtoi(shap.subset$value)
  if (!(col %in% c("bu_code")))
    p[[col]] <- ggplot(shap.subset, aes(x=value, y=shap)) + geom_point(color="blue") +
      labs(title=col, y="Term", x="Feature value") + theme(plot.title=element_text(size=10), 
                                                           axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x=element_text(angle=90), legend.position="right")
}
# XGBoost: bu_code SHAP values
shap.subset <- subset(shap.values, column %in% c("bu_code"))
sf <- DataGeometry(ref_date, which="bu")
sf <- merge(x = sf, y = shap.subset, by.x = "bu_code", by.y="value")
sf$quantile <- with(sf, cut(shap, quantile(shap, na.rm=T, probs=seq(0,1,length.out=11))))
p[["bu_code"]] <- ggplot(sf) + geom_sf(aes(fill=quantile), color=NA) + 
  theme(plot.title=element_text(size=10), legend.position="none") +
  scale_fill_brewer(palette="RdYlGn", direction=-1) + labs(title="bu_code") 
#plot all
grid.arrange(grobs=p, ncol=5, top=textGrob("XGBoost: SHAP explanations averaged over feature values", gp=gpar(fontsize=16)))



library(data.table)
library(pROC)

calculate_auc <- function (y_true, y_pred) auc(roc(y_true, y_pred, levels=c(0, 1), direction = '<', quiet=T))
calculate_acc <- function (y_true, y_pred) 1 - mean(as.numeric(y_pred > 0.5) != y_true)
calculate_mse <- function (y_true, y_pred) mean((y_true - y_pred)**2)
calculate_rmse <- function (y_true, y_pred) sqrt(mean((y_true - y_pred)**2))
calculate_mae <-  function (y_true, y_pred) mean(abs(y_true - y_pred))
calculate_nll <- function (y_true, y_pred) -mean(y_true*log(y_pred) + (1-y_true)*log(1-y_pred))

calculate_metrics <- function (y_true, y_pred, round_to=4) round(c(accuracy = calculate_acc(y_true, y_pred),
                                                       auc      = calculate_auc(y_true, y_pred), 
                                                       brier    = calculate_mse(y_true, y_pred), 
                                                       logloss  = calculate_nll(y_true, y_pred)), round_to)

calculate_metrics_regr <- function (y_true, y_pred) round(c(mae = calculate_mae(y_true, y_pred),
                                                            mse      = calculate_mse(y_true, y_pred)
                                                            ), 3)

calculate_prevalence <- function(prevalence, code, round_to=3) {
  prevalence.code <- prevalence[,.(y=round(mean(y, na.rm=T),round_to), 
                                   N=sum(!is.na(y))), by=.(code=get(code))]
  prevalence.code[prevalence.code$N < 10, c("y")] <- NA
  return(prevalence.code)
}

calculate_roc_curve <- function(y_true, y_pred) {
  roc_curve <- roc(y_true, y_pred, quiet=T)
  roc_table <- round(coords(roc_curve, 1:100/100, input="threshold", quiet=T), 4)
  return(roc_table)
}

calculate_calibration_curve <- function(y_true, y_pred) {
  quantiles <- seq(from = 0, to = 1, length = 100 + 1)
  breaks <- quantile(y_pred, prob=quantiles, na.rm = TRUE)
  if (length(unique(breaks)) < 101) {
    obs <- unique(breaks)
    breaks <- c(obs, max(obs)+1e-4)
  }
  y.quantile <- cut(y_pred, breaks = breaks, right = FALSE, include.lowest = TRUE)
  calibration <- aggregate(cbind(y_true, y_pred), by=list(quantile=y.quantile), mean)
  calibration.n <- aggregate(y_true, by=list(quantile=y.quantile), length)
  calibration$y_true <- round(calibration$y_true, 4)
  calibration$y_pred <- round(calibration$y_pred, 4)
  calibration$n <- calibration.n$x
  return(calibration)
}

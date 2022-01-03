# Require packages
require(data.table)
require(tidytable)
require(dplyr)
require(stringr)
library(RColorBrewer)
source("Scripts/General/geometry_loading.R")

plot_prevalence <- function(data, ind, ref_date="20160901", which = "bu", nbreaks =10, title="") {
  pal <- colorRampPalette(colors = brewer.pal(name = "YlGnBu", n = 9))
  if (which == "bu") {
    bu.sf <- DataGeometry(ref_date, which="bu")
    bu.sf <- merge(x = bu.sf, y = data, by = "bu_code")
    plot(bu.sf[, "prevalence"], pal = pal, border = NA, breaks = "quantile", nbreaks =nbreaks, key.pos = 1,
         main = title)
    
  } else if (which == "wk") {
    wk.sf <- DataGeometry(ref_date, which="wk")
    wk.sf <- merge(x = wk.sf, y = data, by = "wk_code")
    plot(wk.sf[, "prevalence"], pal = pal, border = NA, breaks = "quantile", nbreaks = nbreaks, key.pos = 1,
         main = title)
    
  } else {
    gm.sf <- DataGeometry(ref_date, which="gm")
    gm.sf <- merge(x = gm.sf, y = data, by = "gm_code")
    plot(gm.sf[, "prevalence"], pal = pal, border = NA, breaks = "quantile", nbreaks = nbreaks, key.pos = 1,
         main = title)
  }
}

plot_calibration <- function(y_true, y_pred) {
	quantiles <- seq(from = 0, to = 1, length = 200 + 1)
	y_quantile <- cut(y_pred, breaks = quantile(y_pred, prob=quantiles, na.rm = TRUE), right = FALSE, include.lowest = TRUE)
	calibration <- aggregate(cbind(y_true, y_pred), by=list(quantile=y_quantile), mean)
	
	n.obs <- mean(table(y_quantile))
	p <- pretty(calibration$y_pred, n = 30)
	p.lwr <- qbeta(p = 0.025, shape1 = p*n.obs + 0.5, shape2 = (1 - p)*n.obs + 0.5)
	p.upr <- qbeta(p = 0.975, shape1 = p*n.obs + 0.5, shape2 = (1 - p)*n.obs + 0.5)
	with(calibration, plot(y_pred, y_true, asp = 1))
	matlines(p, cbind(p.lwr, p.upr), col = 1, lty = 2)
	abline(0, 1)
}

calibration_curve <- function(y_true, y_pred, col="black", add=F) {
  quantiles <- seq(from = 0, to = 1, length = 100 + 1)
  breaks <- quantile(y_pred, prob=quantiles, na.rm = TRUE)
  if (length(unique(breaks)) == 1) {
    y_quantile <- rep(breaks[[1]], length(y_pred))
  } else {
    y_quantile <- cut(y_pred, breaks = breaks, right = FALSE, include.lowest = TRUE)
  }
  calibration <- aggregate(cbind(y_true, y_pred), by=list(quantile=y_quantile), mean)
  if (!add) {
    plot(calibration$y_pred, calibration$y_true, asp = 1, col=col)
  } else {
    points(calibration$y_pred, calibration$y_true, asp = 1, col=col)
  }
}

plot_compare <- function(y_true, y_pred, area) {
  prevalence <- aggregate(cbind(y_true, y_pred), by=list(area=area), mean)
	with(prevalence, plot(y_pred, y_true, asp = 1, pch=3, cex=0.2))
	abline(0, 1)
}
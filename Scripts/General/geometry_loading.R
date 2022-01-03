
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

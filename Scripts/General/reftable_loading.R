require(data.table)
require(cbsodataR)
require(tidytable)
#require(dplyr)
#require(stringr)

# THIS SCRIPT CONTAINS FUNCTIONS TO LOAD REFERENCE TABLES FROM CBSODATA

# Creates one column per year, representing the corresponding column name for each variable as defined in kerncijfers_bu_features.csv
load_kerncijfers_bu_colnames <- function(path, kerncijfers_id_map) {
  kerncijfers_bu_features <- fread("Data/Mapping_tables/kerncijfers_bu_features.csv")
  for (ref_year in names(kerncijfers_id_map)) {
    id = kerncijfers_id_map[[ref_year]]
    df = cbs_get_data(id= id) 
    df = as.data.frame(list("orig_name" = colnames(df)))
    df = df %>% mutate(simple_name = gsub("_\\d+", "", orig_name) %>% tolower()) 
    kerncijfers_bu_features = kerncijfers_bu_features %>% 
      left_join(df) %>% 
      rename(!!ref_year := orig_name)
  }
  fwrite(kerncijfers_bu_features, path)
  return(kerncijfers_bu_features)
}

load_kerncijfers_bu <- function(ref_date) {
  ref_year <- substr(ref_date, start = 1, stop = 4)
  kerncijfers_id_map <- list("20092012" = "70904NED", 
                             "2013" = "82339NED",
                             "2014" = "82931NED",
                             "2015" = "83220NED",
                             "2016" = "83487NED", 
                             "2017" = "83765NED", 
                             "2018" = "84286NED", 
                             "2019" = "84583NED", 
                             "2020" = "84799NED")
  
  cache = "Data/Mapping_tables/kerncijfers_bu_colnames.csv"
  
  if (file.exists(cache)) {
    kerncijfers_bu_features <- fread(cache)
  }
  else {
    kerncijfers_bu_features <- load_kerncijfers_bu_colnames(cache, kerncijfers_id_map)
  }
  
  year_id = ifelse(ref_year <= "2012" & ref_year >= "2009", "20092012", ref_year)
  
  relevant_colnames = kerncijfers_bu_features %>% 
    select.(feature_name, divide_by_n, !!year_id) %>% 
    rename.(colname := !!year_id) %>% 
    filter.(!(colname == ""))
  
  unique_colnames = na.omit(unique(relevant_colnames$colname)) %>%
    unlist() %>% as.character()
  
  if (ref_year <= "2012" & ref_year >= "2009") {
    table = cbs_get_data(id=kerncijfers_id_map[[year_id]],
                         Perioden = has_substring(ref_year)) %>%
      filter.(SoortRegio_2 %like% "Buurt") %>% 
      rename.(bu_code = Codering_3) %>% 
      select.(c("bu_code", unique_colnames)) 
  }
  
  else if (ref_year == "2013") {
    table = cbs_get_data(id=kerncijfers_id_map[[ref_year]],
                         select = c("RegioS", unique_colnames)) %>%
      filter.(RegioS %like% "BU") %>% 
      rename.(bu_code = RegioS)
    
  }
  
  else if (ref_year %in% names(kerncijfers_id_map)) {
    table = cbs_get_data(id= kerncijfers_id_map[[ref_year]],   
                         WijkenEnBuurten = has_substring("BU"), 
                         select= c("WijkenEnBuurten", unique_colnames)) %>% 
      rename.(bu_code = WijkenEnBuurten)
  }
  
  
  else {
    print("ERROR! No Kerncijfers wijken en buurten for this year.")
    return()
  }
  table = table %>% setnames(old=relevant_colnames$colname %>% as.character(), 
                             new=relevant_colnames$feature_name %>% as.character(), 
                             skip_absent = T) %>% as.data.frame()
  table = table[, which(colMeans(is.na(table)) < 0.2)]
  table$bu_code <- as.factor(table$bu_code)
  setDT(table)
  return(table)
}

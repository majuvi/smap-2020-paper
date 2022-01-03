source("Scripts/General/reftable_loading.R")
#import("memisc", "cbsodataR", "data.table", "tidytable", "forcats", "lubridate", "randomForest")
#export("DataSetSMAP", "DataSetFilledSMAP", "DataSetNoise", "DataSetGemon", "DataSetGemonNoise")
library(memisc) # causes an error with library(Matrix)
library(cbsodataR)
library(data.table)
library(tidytable)
library(forcats)
library(lubridate)
library(randomForest)
library(stringr)

resolve <- function(folder, file) {
  match.all <- list.files(folder, pattern=file, ignore.case=T, full.names=T)
  match.latest <- sort(match.all, decreasing=T)[1]
  if (is.na(match.latest))
    stop(sprintf("%s not found.", file.path(folder, file)))
  return(match.latest)
}

survey_subset <- function(data, ref_date, survey_dates, start_column, end_column){
  rinpersoon <- data$RINPERSOON %>% as.factor
  start_date <- data[[start_column]] %>% as.character
  end_date   <- data[[end_column]] %>% as.character
  survey_date <- survey_dates[["date"]][match(rinpersoon, survey_dates$rinpersoon)]
  survey_date <- ifelse(is.na(survey_date), ref_date, survey_date)
  return(data[start_date <= survey_date & end_date >= survey_date,])
}


# INPUT:   ref_date       time of the survey for each person
# OUTPUT:  data.frame(gm_code, gg_code)
data_code_ggd <- function(ref_date) {
  ref_year <- substr(ref_date, start = 1, stop = 4)
  id_map = list("2012" = "81498ned",
                "2013" = "82000NED",
                "2015" = "82949NED",
                "2014" = "82496NED",
                "2016" = "83287NED", 
                "2017" = "83553NED", 
                "2018" = "83859NED", 
                "2019" = "84378NED" , 
                "2020" = "84721NED")
  id = id_map[[ref_year]] 
  
  if (ref_year == "2012") {
    table = cbs_get_data(id=id) %>%
      transmute.(gm_code = str_c("GM", str_pad(Code_1, 4, side="left", "0")), 
                 gg_code = str_c("GG", str_pad(Code_12, 4, side="left", "0")))
  }
  else if (ref_year %in% names(id_map)) {
    if (ref_year == "2013") {
      table = cbs_get_data(id= id) %>% transmute.(gm_code = Code_1, gg_code = Code_12)
    }
    else {
      table = cbs_get_data(id= id) %>% transmute.(gm_code = Code_1, gg_code = Code_14)
    }
  }
  else {
    stop("Error! No GGD mapping found for this ref year. Look in smap_utils and add.")
  }
  strip_whitespace <- function(x) gsub("[[:space:]]",  "", x)
  data <- with(table, data.table(
    gm_code      = table$gm_code %>% strip_whitespace %>% as.factor, 
    gg_code      = table$gg_code %>% strip_whitespace %>% as.factor
  )) 
  return(data)
}

# Get the oad column for each year in the API of CBS
get_oad_column <- function(cache, kerncijfers_id_map) {
  oad = list()
  for (ref_year in names(kerncijfers_id_map)) {
    id = kerncijfers_id_map[[ref_year]]
    columns = colnames(cbs_get_data(id = id)) 
    columns_simple = gsub("_\\d+", "", columns) %>% tolower()
    oad[[ref_year]] = columns[columns_simple == "omgevingsadressendichtheid"]
  }
  saveRDS(object = oad, file = cache)
  return(oad)
}

# INPUT:   ref_date       time of the survey for each person
# OUTPUT:  data.frame(bu_code, oad)
data_code_oad <- function(ref_date) {
  ref_year <- substr(ref_date, start = 1, stop = 4)
  id_year = ifelse(ref_year <= "2012" & ref_year >= "2009", "20092012", ref_year)
  kerncijfers_id_map <- list("20092012" = "70904NED", 
                             "2013" = "82339NED",
                             "2014" = "82931NED",
                             "2015" = "83220NED",
                             "2016" = "83487NED", 
                             "2017" = "83765NED", 
                             "2018" = "84286NED", 
                             "2019" = "84583NED", 
                             "2020" = "84799NED")
  
  cache = "Data/Populatiebestanden/TEMP/omgevingsadressendichtheid.rds"
  if (file.exists(cache)) {
    oad <- readRDS(cache)
  } else {
    oad <- get_oad_column(cache, kerncijfers_id_map)
  }
  oad_column = oad[[id_year]]
  
  if (ref_year <= "2012" & ref_year >= "2009") {
    table = cbs_get_data(id=kerncijfers_id_map[[id_year]], Perioden = has_substring(ref_year)) %>%
      filter.(SoortRegio_2 %like% "Buurt") %>% 
      rename.(bu_code = Codering_3) %>% 
      select.(c("bu_code", oad_column)) 
  } else if (ref_year == "2013") {
    table = cbs_get_data(id=kerncijfers_id_map[[id_year]], 
                         select = c("RegioS", oad_column)) %>%
      filter.(RegioS %like% "BU") %>% 
      rename.(bu_code = RegioS)
  } else if (ref_year %in% names(kerncijfers_id_map)) {
    table = cbs_get_data(id= kerncijfers_id_map[[id_year]], WijkenEnBuurten = has_substring("BU"), 
                         select= c("WijkenEnBuurten", oad_column)) %>% 
      rename.(bu_code = WijkenEnBuurten)
  } else {
    print("ERROR! No Kerncijfers wijken en buurten for this year.")
    return()
  }
  setnames(table, oad_column, "oad")
  table$oad <- as.integer(ifelse(is.na(table$oad), "0", table$oad)) # bu_code: km2 per area quantile
  table <- table %>% transmute.(
      bu_code = bu_code %>% as.factor,
      oad     = cut(oad, breaks = quantile(oad, prob = seq(from = 0, to = 1, by = 0.01)),
                    labels = FALSE, include.lowest = TRUE, right = FALSE)) 
  setDT(table)
  return(table)
}

# INPUT:   ref_date       time of the survey for each person
# OUTPUT:  data.frame(rinpersoon, rinobjectnummer)
data_address <- function(ref_date, survey_dates=NULL) {
  extension = ifelse(!is.null(survey_dates), "_survd", "")
  cache = sprintf("Data/Populatiebestanden/TEMP/GBAADRESOBJECTBUS_%s%s.rds", ref_date, extension)
  
  if (file.exists(cache)) {
    data <- readRDS(file = cache)
  }
  else {
    GBAADRESOBJECTBUS <- resolve("G:/Bevolking/GBAADRESOBJECTBUS/", "GBAADRESOBJECT.*.sav")

    if (!is.null(survey_dates)) {
      data <- subset(spss.system.file(GBAADRESOBJECTBUS), 
                       select = c(RINPERSOON, RINOBJECTNUMMER, GBADATUMAANVANGADRESHOUDING, GBADATUMEINDEADRESHOUDING))
      data <- survey_subset(data, ref_date, survey_dates, "GBADATUMAANVANGADRESHOUDING", "GBADATUMEINDEADRESHOUDING")
    }
    else {
      data <- subset(spss.system.file(GBAADRESOBJECTBUS), 
                       subset = GBADATUMAANVANGADRESHOUDING <= ref_date & GBADATUMEINDEADRESHOUDING >= ref_date,
                       select = c(RINPERSOON, RINOBJECTNUMMER))
    }
    
    data <- with(data, data.table(
      rinpersoon      = RINPERSOON %>% as.factor, 
      rinobjectnummer = RINOBJECTNUMMER %>% as.factor
    )) 
    
    saveRDS(object = data, file = cache)
  }
  return(data)
}

# INPUT:   ref_date       time of the survey for each person
# OUTPUT:  data.frame(rinobjectnummer, gm_code, wk_code, bu_code)
data_address_code <- function(ref_date) {
  cache = sprintf("Data/Populatiebestanden/TEMP/VSLGWBTAB_%s.rds", ref_date)
  
  if (file.exists(cache)) {
    data <- readRDS(file = cache)
  }
  else {
    VSLGWBTAB <- resolve("G:/BouwenWonen/VSLGWBTAB/", "VSLGWB.*.sav")

    ref_year <- substr(ref_date, start = 1, stop = 4)
    ref_gm <- paste("gem", ref_year, sep="") #gem2016
    ref_wk <- paste("wc", ref_year, sep="") #wc2016
    ref_bu <- paste("bc", ref_year, sep="") #bc2016
    # TODO: there must be a better way to do this
    data <- eval(str2expression(sprintf("subset(spss.system.file(VSLGWBTAB), 
                                         subset = !grepl(%s, pattern = \"--\"),
                                         select = c(RINOBJECTNUMMER, %s, %s, %s))", 
                                         ref_bu, ref_gm, ref_wk, ref_bu))) 
    data <- with(data, data.table(
      rinobjectnummer = RINOBJECTNUMMER %>% as.factor,
      gm_code         = paste("GM", get(ref_gm) %>% as.vector, sep=""),
      wk_code         = paste("WK", get(ref_wk) %>% as.vector, sep=""),
      bu_code         = paste("BU", get(ref_bu) %>% as.vector, sep="")
    ))
    
    data <- data %>% 
      mutate.(
        gm_code = gm_code %>% as.factor,
        wk_code = wk_code %>% as.factor,
        bu_code = bu_code %>% as.factor
      )
    
    setDT(data)
    saveRDS(object = data, file = cache)
  }
  return(data)
}

# INPUT:   ref_date     
# OUTPUT:  data.frame(rinobjectnummer, dist_to_public_green_total, dist_to_forest, dist_to_backwater)
data_address_green <- function(ref_date) {
  
  ref_year <- as.integer(substr(ref_date, start = 1, stop = 4))
  
  # We only have data from 2012 and 2015, pick closest
  if (ref_year > 2013) {
    year_to_use = 2015
  }  else {
    year_to_use = 2012
  }
  
  cache = sprintf("Data/Populatiebestanden/TEMP/NABIJHEIDGROENVOORZTAB_%s.rds", year_to_use)
  
  if (file.exists(cache)) {
    data <- readRDS(file = cache)
  }
  else {
    GROENVOORZTAB <- resolve(sprintf("G:/BouwenWonen/NABIJHEIDGROENVOORZTAB/%s/", year_to_use), "NABIJHEIDGROENVOORZ.*.sav")
    
    data <- subset(spss.system.file(GROENVOORZTAB), 
                   select = c(RINOBJECTNUMMER, VZAFSTANDOPENBAARGROENTOT, VZAFSTANDBOS, VZAFSTANDRECRBINNENWATERTOT ))
    data <- with(data, data.table(
      rinobjectnummer = RINOBJECTNUMMER %>% as.factor,
      dist_to_public_green_total = VZAFSTANDOPENBAARGROENTOT %>% as.integer,
      dist_to_forest         = VZAFSTANDBOS %>% as.integer,
      dist_to_backwater = VZAFSTANDRECRBINNENWATERTOT %>% as.integer
    ))
    
    saveRDS(object = data, file = cache)
  }
  return(data)
}

# INPUT:   ref_date       time of the survey for each person
# OUTPUT:  data.frame(rinobjectnummer, x_coord, y_coord)
data_address_coord <- function(ref_date) {
  cache = sprintf("Data/Populatiebestanden/TEMP/VSLCOORDTAB_%s.rds", ref_date)
  
  if (file.exists(cache)) {
    data <- readRDS(file = cache)
  }
  else {
    VSLCOORDTAB <- resolve("G:/BouwenWonen/VSLCOORDTAB/", "VSLCOORDTAB.*.sav")
    
    data <- subset(spss.system.file(VSLCOORDTAB), 
                     select = c(RINOBJECTNUMMER, XCOORDADRES, YCOORDADRES))
    data <- with(data, data.table(
      rinobjectnummer = RINOBJECTNUMMER %>% as.factor,
      x_coord         = XCOORDADRES %>% as.integer,
      y_coord         = YCOORDADRES %>% as.integer
    ))
    
    saveRDS(object = data, file = cache)
  }
  return(data)
}

# INPUT:   GELUID       SPSS file with the noise measurements
# OUTPUT:  data.frame(rinobjectnummer, lden_air, lden_rail, lden_wegv, lden_wegv_gw, lden_wegv_pw, lden_wegv_rw, lden_weg_pwrw)
data_address_noise <- function(GELUID) {
  data <- subset(spss.system.file(GELUID, to.lower = FALSE),
                 select = c(Rinobjectnr, Lden_air, Lden_rail, Lden_wegv, Lden_wegv_gw, Lden_wegv_pw, Lden_wegv_rw))
  
  to_dB <- function(x) ifelse(is.na(x) | (x == -9999), 200/10, x/10) # Replace NaNs and transform to dB
  
  data <- with(data, data.table(
    rinobjectnummer = Rinobjectnr %>% as.factor,
    lden_air = Lden_air %>% as.integer %>% to_dB,
    lden_rail = Lden_rail %>% as.integer %>% to_dB,
    lden_wegv = Lden_wegv %>% as.integer %>% to_dB,
    lden_wegv_gw = Lden_wegv_gw %>% as.integer %>% to_dB,
    lden_wegv_pw = Lden_wegv_pw %>% as.integer %>% to_dB,
    lden_wegv_rw = Lden_wegv_rw %>% as.integer %>% to_dB
  )) %>% 
    mutate.(
      lden_weg_pwrw = round(10*log10(10^(lden_wegv_pw/10)+10^(lden_wegv_rw/10)),1)
    ) ## Noise level caused by provincial and national roads together
  
  ## Use a lower limit specified in the original data transformation (35 dB)
  geluid.var <- c("lden_air", "lden_rail", "lden_wegv", "lden_wegv_gw", "lden_wegv_pw", "lden_wegv_rw", "lden_weg_pwrw")
  data[,(geluid.var) := lapply(.SD, function(x) ifelse(x < 35, 35, x)), .SDcols= geluid.var]
  
  return(data)
}

# INPUT:   ref_date       time of the survey for each person
# OUTPUT:  data.frame(rinpersoon, country, sex, age)
data_person <- function(ref_date) {
  cache = sprintf("Data/Populatiebestanden/TEMP/GBAPERSOONTAB_%s.rds", ref_date)
  
  if (file.exists(cache)) {
    data <- readRDS(file = cache)
  }
  else {
    ref_year <- substr(ref_date, start = 1, stop = 4)
    GBAPERSOONTAB <- resolve(sprintf("G:/Bevolking/GBAPERSOONTAB/%s/", ref_year), "GBAPERSOON.*.sav")

    data <- subset(spss.system.file(GBAPERSOONTAB), 
                     select=c(RINPERSOON, GBAGEBOORTEJAAR, GBAGEBOORTEMAAND, GBAGESLACHT, GBAHERKOMSTGROEPERING), 
                   )
    data <- with(data, data.table(
      rinpersoon  = RINPERSOON %>% as.factor, 
      country     = GBAHERKOMSTGROEPERING %>% as.factor,
      sex         = fct_recode(GBAGESLACHT %>% as.factor,  
                               man   = "Mannen", 
                               woman = "Vrouwen")  %>% droplevels,
      age = (ymd(paste(GBAGEBOORTEJAAR %>% as.vector, 
                       GBAGEBOORTEMAAND %>% as.vector, 
                       "15", sep="")) %--% ymd(ref_date)) 
      %/% dyears(1)
    ))
    
    saveRDS(object = data, file = cache)
  }
  return(data)
} 


# INPUT:   ref_date       time of the survey for each person
# OUTPUT:  data.frame(country, education)
data_ethnicity <- function(ref_date) {
  cache = sprintf("Data/Populatiebestanden/TEMP/LANDAKTUEELREF_%s.rds", ref_date)
  
  if (file.exists(cache)) {
    data <- readRDS(file = cache)
  }
  else {
    ref_year <- substr(ref_date, start = 1, stop = 4)
    LANDAKTUEELREF <- resolve("K:/Utilities/Code_Listings/SSBreferentiebestanden/", "LANDAKTUEELREF.*.sav")
    
    data <- subset(spss.system.file(LANDAKTUEELREF), 
                     select=c(LAND, ETNGRP))
    data <- with(data, data.table(
      country   = LAND %>% as.factor,
      ethnicity = fct_recode(ETNGRP %>% as.factor,
                             "netherlands"      = "Autochtoon",
                             "marokko"          = "Marokko",
                             "turkey"           = "Turkije",
                             "suriname"         = "Suriname",
                             "antille"          = "Voormalige Nederlandse Antillen en Aruba",
                             "other_nonwestern" = "Overige niet-westerse landen",
                             "other_western"    = "Overige westerse landen",
                             NULL               = "Onbekend") %>% droplevels
    ))
    
    saveRDS(object = data, file = cache)
  }
  return(data)
}

# INPUT:   ref_date       time of the survey for each person
# OUTPUT:  data.frame(rinpersoon, education)
data_education <- function(ref_date) {
  cache = sprintf("Data/Populatiebestanden/TEMP/HOOGSTEOPLTAB_%s.rds", ref_date)
  
  if (file.exists(cache)) {
    data <- readRDS(file = cache)
  }
  else {
    ref_year <- substr(ref_date, start = 1, stop = 4)
    
    if (as.integer(ref_year) >= 2013) {
      
      HOOGSTEOPLTAB <- resolve(sprintf("G:/Onderwijs/HOOGSTEOPLTAB/%s/", ref_year), "HOOGSTEOPL.*.sav")
      
      data <- subset(spss.system.file(HOOGSTEOPLTAB), 
                     select=c(RINPERSOON, OPLNIVSOI2016AGG4HBMETNIRWO))
      data <- with(data, data.table(
        rinpersoon = RINPERSOON %>% as.factor, 
        education = fct_recode(OPLNIVSOI2016AGG4HBMETNIRWO %>% as.factor,
                               "basis" = "Basisonderwijs gr1-2",
                               "basis" = "Basisonderwijs gr3-8",
                               "vmbo_bk" = "Praktijkonderwijs", 
                               "vmbo_bk" = "Vmbo-b/k",
                               "vmbo_bk" = "Mbo1",
                               "vmbo_gt" = "Vmbo-g/t",
                               "vmbo_gt" = "Havo-, vwo-onderbouw",
                               "mbo_23" = "Mbo2",
                               "mbo_23" = "Mbo3",
                               "mbo_4" = "Mbo4",
                               "havo_vwo" = "Havo-bovenbouw",
                               "havo_vwo" = "Vwo-bovenbouw",
                               "hbo_wo_bc" = "Hbo-associate degree",
                               "hbo_wo_bc" = "Hbo-bachelor",
                               "hbo_wo_bc" = "Wo-bachelor",
                               "hbo_wo_ma" = "Hbo-master",
                               "hbo_wo_ma" = "Wo-master",
                               "hbo_wo_ma" = "Doctor") %>% droplevels
      )) 
      
    } else {
      
      HOOGSTEOPLTAB <- resolve(sprintf("G:/Onderwijs/HOOGSTEOPLTAB/%s/", ref_year), "HOOGSTEOPL.*.sav")
      OPLEIDINGSNRREF <- resolve("K:/Utilities/Code_Listings/SSBreferentiebestanden", "OPLEIDINGSNRREF.*.sav")
      CTOREF <- resolve("K:/Utilities/Code_Listings/SSBreferentiebestanden", "CTOREF.*.sav")
      
      data.1 <- subset(spss.system.file(HOOGSTEOPLTAB), 
                       select=c(RINPERSOON,
                                OPLNRHB))
      data.1 <- with(data.1, data.table(
        rinpersoon = RINPERSOON %>% as.factor,
        education_ref = OPLNRHB %>% as.vector
      ))
      
      data.2 <- subset(spss.system.file(OPLEIDINGSNRREF), 
                       select=c(OPLNR,
                                CTO2016V))
      data.2 <- with(data.2, data.table(
        education_ref = OPLNR %>% as.vector,
        cto = CTO2016V %>% as.vector
      ))
      
      data.3 <- subset(spss.system.file(CTOREF), 
                       select=c(CTO,
                                OPLNIVSOI2016AGG4HB))
      data.3 <- with(data.3, data.table(
        cto = CTO %>% as.vector,
        education_code = OPLNIVSOI2016AGG4HB %>% as.factor
      ))
      
      data <- data.1 %>% left_join.(merge(data.2, data.3, all=T), by="education_ref") %>%
        mutate.(
          education = fct_recode(education_code %>% as.factor,
                                 "basis" = "Basisonderwijs gr1-2",
                                 "basis" = "Basisonderwijs gr3-8",
                                 "vmbo_bk" = "Praktijkonderwijs", 
                                 "vmbo_bk" = "Vmbo-b/k",
                                 "vmbo_bk" = "Mbo1",
                                 "vmbo_gt" = "Vmbo-g/t",
                                 "vmbo_gt" = "Havo-, vwo-onderbouw",
                                 "mbo_23" = "Mbo2",
                                 "mbo_23" = "Mbo3",
                                 "mbo_4" = "Mbo4",
                                 "havo_vwo" = "Havo-bovenbouw",
                                 "havo_vwo" = "Vwo-bovenbouw",
                                 "hbo_wo_bc" = "Hbo-associate degree",
                                 "hbo_wo_bc" = "Hbo-bachelor",
                                 "hbo_wo_bc" = "Wo-bachelor",
                                 "hbo_wo_ma" = "Hbo-master",
                                 "hbo_wo_ma" = "Wo-master",
                                 "hbo_wo_ma" = "Doctor") %>% droplevels) %>%
        select.(rinpersoon, education)
    }
    
    setDT(data)
    saveRDS(object = data, file = cache)
  }
  return(data)
}

# INPUT:   ref_date       time of the survey for each person
#          survey_dates   data.frame(rinpersoon, date) optional date when the survey was made for a specific person
# OUTPUT:  data.frame(rinpersoon, hhtype, hhsize)
data_household_info <- function(ref_date, survey_dates=NULL) {
  extension = ifelse(!is.null(survey_dates), "_survd", "")
  cache = sprintf("Data/Populatiebestanden/TEMP/GBAHUISHOUDENSBUS_%s%s.rds", ref_date, extension)
  
  if (file.exists(cache)) {
    data <- readRDS(file = cache)
  }
  else {
    ref_year <- substr(ref_date, start = 1, stop = 4)
    
    folder <- ifelse(as.integer(ref_year) >= 2015, "G:/Bevolking/GBAHUISHOUDENSBUS/", 
                     "G:/Bevolking/GBAHUISHOUDENSBUS/2015 Opgesplitste jaarbestanden/")
    GBAHUISHOUDENSBUS <- resolve(folder, sprintf("GBAHUISHOUDENS.*%s.*.sav", ref_year))
    
    if (!is.null(survey_dates)) {
      data <- subset(spss.system.file(GBAHUISHOUDENSBUS), 
                     select = c(RINPERSOON, TYPHH, AANTALPERSHH, DATUMAANVANGHH, DATUMEINDEHH))
      data <- survey_subset(data, ref_date, survey_dates, "DATUMAANVANGHH", "DATUMEINDEHH")
    }
    else {
      data <- subset(spss.system.file(GBAHUISHOUDENSBUS), 
                     subset = DATUMAANVANGHH <= ref_date & DATUMEINDEHH >= ref_date, 
                     select = c(RINPERSOON, TYPHH, AANTALPERSHH))
    }
    
    data <- with(data, data.table(
      rinpersoon = RINPERSOON %>% as.factor, 
      hhtype     = fct_recode(TYPHH %>% as.factor,
                              "single" = "Eenpersoonshuishouden",
                              "unmarried_nochildren" = "Niet-gehuwd paar zonder kinderen",
                              "married_nochildren" = "Gehuwd paar zonder kinderen",
                              "unmarried_children" = "Niet-gehuwd paar met kinderen",
                              "married_children" = "Gehuwd paar met kinderen", 
                              "single_parent" = "Eenouderhuishouden",
                              "other" = "Overig huishouden",
                              "institutional" = "Institutioneel huishouden") %>% droplevels,
      hhsize     = AANTALPERSHH %>% as.integer
    )) %>%
      mutate.(
        hhsize = ifelse(hhsize > 10, 10, hhsize)
      )
    
    saveRDS(object = data, file = cache)
  }
  return(data)
}


# INPUT:   ref_date       time of the survey for each person
# OUTPUT:  data.frame(rinpersoon, rinpersoonhkw)
data_household <- function(ref_date) {
  cache = sprintf("Data/Populatiebestanden/TEMP/KOPPELPERSOONHUISHOUDEN_%s.rds", ref_date)
  
  if (file.exists(cache)) {
    data <- readRDS(file = cache)
  }
  else {
    # 1st of January, so need to use year+1
    ref_year <- as.integer(substr(ref_date, start = 1, stop = 4))
    KOPPELPERSOONHUISHOUDEN <- resolve("G:/InkomenBestedingen/KOPPELPERSOONHUISHOUDEN/", 
                                       sprintf("KOPPELPERSOONHUISHOUDEN%d.*.sav", ref_year+1))

    data <- subset(spss.system.file(KOPPELPERSOONHUISHOUDEN), 
                     select=c(RINPERSOON, RINPERSOONHKW))
    data <- with(data, data.table(
      rinpersoon    = RINPERSOON %>% as.factor, 
      rinpersoonhkw = RINPERSOONHKW %>% as.factor
    )) 
    
    saveRDS(object = data, file = cache)
  }
  return(data)
}

# INPUT:   ref_date       time of the survey for each person
# OUTPUT:  data.frame(rinpersoonhkw, hhincomesource, hhhomeownership, hhincome)
data_household_income <- function(ref_date) {
  cache = sprintf("Data/Populatiebestanden/TEMP/INHATAB_%s.rds", ref_date)

  
  if (file.exists(cache)) {
    data <- readRDS(file = cache)
  }
  else {
    # 1st of January, so need to use year+1
    ref_year <- as.integer(substr(ref_date, start = 1, stop = 4))
    INHATAB <- resolve("G:/InkomenBestedingen/INHATAB/", sprintf("INHA%d.*.sav", ref_year+1))
    
    data <- subset(spss.system.file(INHATAB), 
                   select=c(RINPERSOONHKW, INHBBIHJ, INHEHALGR, INHP100HBEST))
    data <- with(data, data.table(
      rinpersoonhkw   = RINPERSOONHKW %>% as.factor, 
      hhincomesource = fct_recode(INHBBIHJ %>% as.factor,
                                  "wage" = "Loon",
                                  "wage_director_shareholder" = "Loon directeur-grootaandeelhouder",
                                  "self_employed" = "Winst zelfstandig ondernemer",
                                  "self_employed" = "Inkomen overige zelfstandige",
                                  "assistance_unemployed" = "Werkloosheidsuitkering",
                                  "assistance_social" = "Bijstandsuitkering",
                                  "assistance_other" = "Uitkering sociale voorziening overig",
                                  "assistance_sickness_disability" = "Uitkering ziekte/arbeidsongeschiktheid",
                                  "assistance_pension" = "Pensioenuitkering",
                                  "student" = "Studiefinanciering",
                                  "property" = "Inkomen uit vermogen",
                                  NULL= "Huishoudensinkomen onbekend") %>% droplevels,
      hhhomeownership = fct_recode(INHEHALGR %>% as.factor,
                                   "homeowner"          = "Eigen woning",
                                   "rental_noallowance" = "Huurwoning zonder huurtoeslag", 
                                   "rental_allowance"   = "Huurwoning met huurtoeslag",
                                   "institutional"      = "Institutioneel huishouden",
                                   NULL                 = "Onbekend huishouden") %>% droplevels,
      hhincome        = INHP100HBEST %>% as.integer
    )) %>% 
      mutate.(
        hhincome = ifelse(hhincome %in% c(-2, -1), NA, hhincome)
      )
    
    saveRDS(object = data, file = cache)
  }
  return(data)
}

# INPUT:   ref_date       time of the survey for each person
# OUTPUT:  data.frame(rinpersoonhkw, hhassets)
data_household_assets <- function(ref_date) {
  cache = sprintf("Data/Populatiebestanden/TEMP/VEHTAB_%s.rds", ref_date)
  
  if (file.exists(cache)) {
    data <- readRDS(file = cache)
  }
  else {
    # 1st of January, so need to use year+1
    ref_year <- as.integer(substr(ref_date, start = 1, stop = 4))
    VEHTAB <- resolve("G:/InkomenBestedingen/VEHTAB/", sprintf("VEH%d.*.sav", ref_year+1))
    
    data <- subset(spss.system.file(VEHTAB), 
                   select=c(RINPERSOONHKW, VEHP100HVERM))
    data <- with(data, data.table(
      rinpersoonhkw = RINPERSOONHKW %>% as.factor, 
      hhassets      = VEHP100HVERM %>% as.integer
    )) %>% 
      mutate.(
        hhassets      = ifelse(hhassets %in% c(-2, -1), NA, hhassets)
      )
    
    saveRDS(object = data, file = cache)
  }
  return(data)
}

# INPUT:   ref_date       time of the survey for each person
#          survey_dates   data.frame(rinpersoon, date) optional date when the survey was made for a specific person
# OUTPUT:  data.frame(rinpersoon, marital_status)
data_marital_status <- function(ref_date, survey_dates=NULL) {
  extension = ifelse(!is.null(survey_dates), "_survd", "")
  cache = sprintf("Data/Populatiebestanden/TEMP/GBABURGERLIJKESTAATBUS_%s%s.rds", ref_date, extension)
  
  if (file.exists(cache)) {
    data <- readRDS(file = cache)
  }
  else {
    ref_year <- substr(ref_date, start = 1, stop = 4)
    GBABURGERLIJKESTAATBUS <- resolve("G:/Bevolking/GBABURGERLIJKESTAATBUS/2019/", "GBABURGERLIJKESTAAT.*.sav")
    
    if (!is.null(survey_dates)) {
      data <- subset(spss.system.file(GBABURGERLIJKESTAATBUS), 
                     select = c(RINPERSOON,
                                GBABURGERLIJKESTAATNW, 
                                GBAAANVANGBURGERLIJKESTAAT,
                                GBAEINDEBURGERLIJKESTAAT
                     ))
      data <- survey_subset(data, ref_date, survey_dates, "GBAAANVANGBURGERLIJKESTAAT", "GBAEINDEBURGERLIJKESTAAT")
    }
    else {
      data <- subset(spss.system.file(GBABURGERLIJKESTAATBUS), 
                     subset = GBAAANVANGBURGERLIJKESTAAT <= ref_date & GBAEINDEBURGERLIJKESTAAT >= ref_date,
                     select = c(RINPERSOON, GBABURGERLIJKESTAATNW))
    }
    
    data <- with(data, data.table(
      rinpersoon     = RINPERSOON %>% as.factor, 
      marital_status = fct_recode(GBABURGERLIJKESTAATNW %>% as.factor,
                                  "single" = "Ongehuwd",
                                  "married" = "Gehuwd",
                                  "married" = "Partnerschap", 
                                  "divorced" = "Gescheiden na huwelijk",
                                  "divorced" = "Gescheiden na partnerschap",
                                  "widowed" = "Verweduwd na huwelijk",
                                  "widowed" = "Verweduwd na partnerschap") %>% droplevels))
    
    saveRDS(object = data, file = cache)
  }
  return(data)
}


impute <- function(formula, data, train.sample=10000) {
  
  # Fit random forest model to a subset with no missing values in features #attr(terms(formula), "term.labels")
  sub.data <- na.omit(data, cols=all.vars(formula))[sample(.N, train.sample)]
  rf.model <- randomForest(formula, data=sub.data)
  
  # Find the NaN entries in the response variable and fill them with predicted values
  resp <- all.vars(formula)[1]
  resp.na <- is.na(data[[resp]]) #paste0("data[, ", resp, "] %>% is.na") %>% parse(text = .) %>% eval
  pred <- predict(rf.model, newdata = data[resp.na, ])
  data[resp.na, resp] <- pred
  return(data)
}

ImputeSMAP <- function(pop.data, extra=FALSE) {
  
  ## The idea is to fill sequentially from least missing to most missing, assuming that sex, age, ethnicity have been observed
  #pop.data <- pop.data[!(is.na(x_coord) | is.na(y_coord) | is.na(sex) | is.na(age) | is.na(ethnicity))]
  
  # Faster with categorical variables
  pop.data[, hhincome.cat  := cut(hhincome, breaks = seq(from =  1, to = 101, by = 5), right = FALSE, include.lowest = TRUE)]
  pop.data[, hhassets.cat := cut(hhassets, breaks = seq(from =  1, to = 101, by = 5), right = FALSE, include.lowest = TRUE)]
  
  # Fill marital status
  pop.data <- impute(
    formula = marital_status ~ age + sex + ethnicity, 
    data = pop.data)
  
  # Fill household type
  pop.data <- impute(
    formula = hhtype ~ age + sex + ethnicity + marital_status,
    data = pop.data)
  
  # Fill household size
  pop.data <- impute(
    formula = hhsize ~ hhtype + age + sex + ethnicity + marital_status,
    data = pop.data)
  
  # Fill household homeownership
  pop.data <- impute(
    formula = hhhomeownership ~ age + sex + ethnicity + marital_status +
      hhtype + hhsize,
    data = pop.data)
  
  # Fill household income
  pop.data <- impute(
    formula = hhincomesource ~ age + sex + ethnicity + marital_status +
      hhtype + hhsize + hhhomeownership,
    data = pop.data)
  
  # Fill household income
  pop.data <- impute(
    formula = hhincome.cat ~ age + sex + ethnicity + marital_status +
      hhtype + hhsize + hhhomeownership + hhincomesource,
    data = pop.data)
  
  # Convert household income category into 1-100
  pop.data[,  hhincome := ifelse(is.na(hhincome),
                                 yes = (as.integer(hhincome.cat)-1)*5 + sample(1:5, size = nrow(pop.data), replace = TRUE),
                                 no  = hhincome)]
  
  # Fill household assets
  pop.data <- impute(
    formula =  hhassets.cat ~ age + sex + ethnicity + marital_status +
      hhtype + hhsize + hhhomeownership + hhincomesource + hhincome.cat,
    data = pop.data)
  
  # Convert household income category into 1-100
  pop.data[,  hhassets := ifelse(is.na(hhassets),
                                 yes = (as.integer(hhassets.cat)-1)*5 + sample(1:5, size = nrow(pop.data), replace = TRUE),
                                 no  = hhassets)]
  
  # Fill education
  pop.data <- impute(
    formula = education ~ age + sex + ethnicity + marital_status +
      hhtype + hhsize + hhhomeownership + hhincomesource + hhincome.cat + hhassets.cat,
    data = pop.data)
  
  # Fill extra WOON features
  if (extra) {
    pop.data <- impute(formula = avg_household_size_nbh ~ age + sex + ethnicity + marital_status + education +
                         hhtype + hhsize + hhhomeownership + hhincomesource + hhincome.cat + hhassets.cat, data = pop.data)
    pop.data <- impute(formula = schools_within_3km_nbh ~ age + sex + ethnicity + marital_status + education +
                         hhtype + hhsize + hhhomeownership + hhincomesource + hhincome.cat + hhassets.cat, data = pop.data)
    pop.data <- impute(formula = perc_uninhabited_nbh ~ age + sex + ethnicity + marital_status + education +
                         hhtype + hhsize + hhhomeownership + hhincomesource + hhincome.cat + hhassets.cat, data = pop.data)
    pop.data <- impute(formula = perc_singlefamhome_nbh ~ age + sex + ethnicity + marital_status + education +
                         hhtype + hhsize + hhhomeownership + hhincomesource + hhincome.cat + hhassets.cat, data = pop.data)
    pop.data <- impute(formula = perc_builtbefore2000_nbh ~ age + sex + ethnicity + marital_status + education +
                         hhtype + hhsize + hhhomeownership + hhincomesource + hhincome.cat + hhassets.cat, data = pop.data)
    pop.data <- impute(formula = perc_nonrentals_nbh ~ age + sex + ethnicity + marital_status + education +
                         hhtype + hhsize + hhhomeownership + hhincomesource + hhincome.cat + hhassets.cat, data = pop.data)
    pop.data <- impute(formula = perc_housingcorporation_nbh ~ age + sex + ethnicity + marital_status + education +
                         hhtype + hhsize + hhhomeownership + hhincomesource + hhincome.cat + hhassets.cat, data = pop.data)
    pop.data <- impute(formula = dist_to_public_green_total ~ age + sex + ethnicity + marital_status + education +
                         hhtype + hhsize + hhhomeownership + hhincomesource + hhincome.cat + hhassets.cat, data = pop.data)
    pop.data <- impute(formula = dist_to_forest ~ age + sex + ethnicity + marital_status + education +
                         hhtype + hhsize + hhhomeownership + hhincomesource + hhincome.cat + hhassets.cat, data = pop.data)
    pop.data <- impute(formula = dist_to_backwater ~ age + sex + ethnicity + marital_status + education +
                         hhtype + hhsize + hhhomeownership + hhincomesource + hhincome.cat + hhassets.cat, data = pop.data)

  }
  
  # Remove temporary categorical variables
  pop.data <- subset(pop.data, select = -c(hhincome.cat, hhassets.cat))
  
  return(pop.data)
}


DataSetSMAP2020 <- function(ref_date) {
  
  cache = sprintf("Data/Populatiebestanden/popdata_%s.rds", ref_date)
  
  if (file.exists(cache)) {
    data <- readRDS(file = cache)
  } else {
    data <- data_address("20200901") %>% # rinpersoon, rinobjectnummer
      inner_join.(data_address_code("20200901"), by="rinobjectnummer") %>% #rinobjectnummer, gm_code, wk_code, bu_code
      inner_join.(data_address_coord("20200901"), by="rinobjectnummer") %>% #rinobjectnummer, x_coord, y_coord
      left_join.(data_code_ggd("20200901"), by="gm_code") %>% # gm_code, gg_code
      left_join.(data_code_oad("20200901"), by="bu_code") %>% # bu_code, oad
      left_join.(data_person("20200901"), by="rinpersoon")  %>% # rinpersoon, country, sex, age
      left_join.(data_ethnicity("20200901"), by="country") %>% # country, ethnicity
      left_join.(data_marital_status("20200901"), by="rinpersoon") %>% # rinpersoon, marital_status
      left_join.(data_education("20190901"), by="rinpersoon") %>% # rinpersoon, education
      left_join.(data_household_info("20200901"), by="rinpersoon")  %>% # rinpersoon, hhtype, hhsize
      left_join.(data_household("20180901"), by="rinpersoon")  %>% # rinpersoon, rinpersoonhkw
      left_join.(data_household_income("20180901"), by="rinpersoonhkw") %>% # rinpersoonhkw, hhincomesource, hhhomeownership, hhincome
      left_join.(data_household_assets("20180901"), by="rinpersoonhkw") %>% # rinpersoonhkw, hhassets
      filter.(age >= 18 & (hhtype != "institutional" | is.na(hhtype)) &
                (hhhomeownership != "institutional"| is.na(hhhomeownership))) %>% # only 18+ persons and non institutional
      select.(rinpersoon, gg_code, gm_code, wk_code, bu_code, x_coord, y_coord, oad,
              sex, age, ethnicity, marital_status, education, 
              hhtype, hhsize, hhhomeownership, hhincomesource, hhincome, hhassets)
    data[,hhtype:=factor(hhtype)] # remove institutional level because data was filtered to exclude it
    data[,hhhomeownership:=factor(hhhomeownership)]
    data <- data[!(is.na(x_coord) | is.na(y_coord) | is.na(sex) | is.na(age) | is.na(ethnicity)),] # assuming that sex, age, ethnicity have been observed
    data <- data[!duplicated(data$rinpersoon),] # take first occurrence of every duplicate entry for rinpersoon
    
    setDT(data)
    saveRDS(object = data, file = cache)
  }
  return(data)
}

DataSetSMAP <- function(ref_date) {

  cache = sprintf("Data/Populatiebestanden/popdata_%s.rds", ref_date)
  
  if (file.exists(cache)) {
    data <- readRDS(file = cache)
  } else {
    data <- data_address(ref_date) %>% # rinpersoon, rinobjectnummer
      inner_join.(data_address_code(ref_date), by="rinobjectnummer") %>% #rinobjectnummer, gm_code, wk_code, bu_code
      inner_join.(data_address_coord(ref_date), by="rinobjectnummer") %>% #rinobjectnummer, x_coord, y_coord
      left_join.(data_code_ggd(ref_date), by="gm_code") %>% # gm_code, gg_code
      left_join.(data_code_oad(ref_date), by="bu_code") %>% # bu_code, oad
      left_join.(data_person(ref_date), by="rinpersoon")  %>% # rinpersoon, country, sex, age
      left_join.(data_ethnicity(ref_date), by="country") %>% # country, ethnicity
      left_join.(data_marital_status(ref_date), by="rinpersoon") %>% # rinpersoon, marital_status
      left_join.(data_education(ref_date), by="rinpersoon") %>% # rinpersoon, education
      left_join.(data_household_info(ref_date), by="rinpersoon")  %>% # rinpersoon, hhtype, hhsize
      left_join.(data_household(ref_date), by="rinpersoon")  %>% # rinpersoon, rinpersoonhkw
      left_join.(data_household_income(ref_date), by="rinpersoonhkw") %>% # rinpersoonhkw, hhincomesource, hhhomeownership, hhincome
      left_join.(data_household_assets(ref_date), by="rinpersoonhkw") %>% # rinpersoonhkw, hhassets
      filter.(age >= 18 & (hhtype != "institutional" | is.na(hhtype)) &
                (hhhomeownership != "institutional"| is.na(hhhomeownership))) %>% # only 18+ persons and non institutional
      select.(rinpersoon, rinobjectnummer, gg_code, gm_code, wk_code, bu_code, x_coord, y_coord, oad,
              sex, age, ethnicity, marital_status, education, 
              hhtype, hhsize, hhhomeownership, hhincomesource, hhincome, hhassets)
    data[,hhtype:=factor(hhtype)] # remove institutional level because data was filtered to exclude it
    data[,hhhomeownership:=factor(hhhomeownership)]
    data <- data[!(is.na(x_coord) | is.na(y_coord) | is.na(sex) | is.na(age) | is.na(ethnicity)),] # assuming that sex, age, ethnicity have been observed
    data <- data[!duplicated(data$rinpersoon),] # take first occurrence of every duplicate entry for rinpersoon
    
    setDT(data)
    saveRDS(object = data, file = cache)
  }
  return(data)
}


DataSetFilledSMAP <- function(ref_date) {
  cache = sprintf("Data/Populatiebestanden/popdata_%s_filled.rds", ref_date)
  
  if (file.exists(cache)) {
    data.filled <- readRDS(file = cache)
  }
  else {
    data <- DataSetSMAP(ref_date)

    # Fill the NaNs with random forest 
    print(data %>% sapply(FUN = is.na) %>% colSums %>% sort)
    data.filled <- ImputeSMAP(data)
    print(data.filled %>% sapply(FUN = is.na) %>% colSums %>% sort)
    
    setDT(data.filled)
    saveRDS(object = data.filled, file = cache)
  }
  
  return(data.filled)
}


DataSetNoise <- function(SPSS_noise, ref_date) {
  cache = sprintf("Data/Populatiebestanden/noisedata_%s.rds", ref_date)
  
  if (file.exists(cache)) {
    data <- readRDS(file = cache)
  }
  else {
    data <- data_address(ref_date) %>% # rinpersoon, rinobjectnummer
      inner_join.(data_address_code(ref_date), by="rinobjectnummer") %>% # rinobjectnummer, gm_code, wk_code, bu_code
      inner_join.(data_address_coord(ref_date), by="rinobjectnummer") %>% #rinobjectnummer, x_coord, y_coord
      left_join.(data_address_noise(SPSS_noise), by="rinobjectnummer") # rinobjectnummer, lden_air, lden_rail, lden_wegv, lden_wegv_gw, lden_wegv_pw, lden_wegv_rw, lden_weg_pwrw
    
    fill_mean <- function(x){
      fill.value <- mean(x, na.rm = TRUE)
      y <- ifelse(is.na(x), fill.value, x)
      return(y)
    }
    geluid.var <- c("lden_air", "lden_rail", "lden_wegv", "lden_wegv_gw", "lden_wegv_pw", "lden_wegv_rw", "lden_weg_pwrw")
    data[, (geluid.var) := lapply(.SD, fill_mean), by = bu_code, .SDcols=geluid.var]
    data[, (geluid.var) := lapply(.SD, fill_mean), by = wk_code, .SDcols=geluid.var] # Only wk in the original
    data[, (geluid.var) := lapply(.SD, fill_mean), by = gm_code, .SDcols=geluid.var]
    
    data <- data %>% select.(rinpersoon, lden_air, lden_rail, lden_wegv, lden_wegv_gw, lden_wegv_pw, lden_wegv_rw, lden_weg_pwrw)
    data <- data[!duplicated(data$rinpersoon),] # take first occurrence of every duplicate entry for rinpersoon
    
    setDT(data)
    saveRDS(object = data, file = cache)
  }
  return(data)
}


DataSetSurveySMAP <- function(ref_date, survey_dates=NULL) {
  
  cache = sprintf("Data/Populatiebestanden/popdata_%s_survey.rds", ref_date)
  
  if (file.exists(cache)) {
    data <- readRDS(file = cache)
  } else {
    data <- data_address(ref_date, survey_dates) %>% # rinpersoon, rinobjectnummer
      inner_join.(data_address_code(ref_date), by="rinobjectnummer") %>% #rinobjectnummer, gm_code, wk_code, bu_code
      left_join.(data_address_coord(ref_date), by="rinobjectnummer") %>% #rinobjectnummer, x_coord, y_coord
      left_join.(data_address_green(ref_date), by="rinobjectnummer") %>% # dist_to_public_green_total, dist_to_forest, dist_to_backwater
      left_join.(data_code_ggd(ref_date), by="gm_code") %>% # gm_code, gg_code
      left_join.(load_kerncijfers_bu(ref_date), by="bu_code") %>% # bu_code, oad
      left_join.(data_person(ref_date), by="rinpersoon")  %>% # rinpersoon, country, sex, age
      left_join.(data_ethnicity(ref_date), by="country") %>% # country, ethnicity
      left_join.(data_marital_status(ref_date, survey_dates), by="rinpersoon") %>% # rinpersoon, marital_status
      left_join.(data_education(ref_date), by="rinpersoon") %>% # rinpersoon, education
      left_join.(data_household_info(ref_date, survey_dates), by="rinpersoon")  %>% # rinpersoon, hhtype, hhsize
      left_join.(data_household(min(ref_date, "20180901")), by="rinpersoon")  %>% # rinpersoon, rinpersoonhkw
      left_join.(data_household_income(min(ref_date, "20180901")), by="rinpersoonhkw") %>% # rinpersoonhkw, hhincomesource, hhhomeownership, hhincome
      left_join.(data_household_assets(min(ref_date, "20180901")), by="rinpersoonhkw") %>% # rinpersoonhkw, hhassets
      filter.(age >= 18 & (hhtype != "institutional" | is.na(hhtype)) &
                (hhhomeownership != "institutional"| is.na(hhhomeownership))) %>% # only 18+ persons and non institutional
      select.(rinpersoon, rinobjectnummer, gg_code, gm_code, wk_code, bu_code, x_coord, y_coord, oad, 
              dist_to_public_green_total, dist_to_forest, dist_to_backwater,
              avg_household_size_nbh, perc_uninhabited_nbh, perc_singlefamhome_nbh, perc_nonrentals_nbh, perc_housingcorporation_nbh,
              perc_builtbefore2000_nbh, schools_within_3km_nbh, 
              sex, age, ethnicity, marital_status, education, 
              hhtype, hhsize, hhhomeownership, hhincomesource, hhincome, hhassets)
    data[,hhtype:=factor(hhtype)] # remove institutional level because data was filtered to exclude it
    data[,hhhomeownership:=factor(hhhomeownership)]
    data <- data[!(is.na(x_coord) | is.na(y_coord) | is.na(sex) | is.na(age) | is.na(ethnicity)),] # assuming that sex, age, ethnicity have been observed
    data <- data[!duplicated(data$rinpersoon),] # take first occurrence of every duplicate entry for rinpersoon
    
    setDT(data)
    saveRDS(object = data, file = cache)
  }
  return(data)
}

DataSetFilledSurveySMAP <- function(ref_date) {
  cache = sprintf("Data/Populatiebestanden/popdata_%s_survey_filled.rds", ref_date)
  
  if (file.exists(cache)) {
    data.filled <- readRDS(file = cache)
  }
  else {
    data <- DataSetSurveySMAP(ref_date)
    
    # Fill the NaNs with random forest 
    print(data %>% sapply(FUN = is.na) %>% colSums %>% sort)
    data.filled <- ImputeSMAP(data, extra=TRUE)
    print(data.filled %>% sapply(FUN = is.na) %>% colSums %>% sort)
    
    setDT(data.filled)
    saveRDS(object = data.filled, file = cache)
  }
  
  return(data.filled)
}


DataSetGemon <- function(ref_date) {
  cache = sprintf("Data/Populatiebestanden/gemondata_%s.rds", ref_date)
  
  # Transform: (x > limit) -> 1, (x <= limit) -> 0
  encode_limit <- function(x, limit=4, flip=F) ifelse(ifelse(rep(flip,length(x)), x <= limit, x > limit), 1, 0)
  
  # Transform: 1 -> 0, 2 -> 1, 3 -> 0, 4 -> NA
  encode_1234 <- function(x) ifelse(x == 3, yes =  0, no = ifelse(x == 4, yes = NA, no = x - 1))
  
  if (file.exists(cache)) {
    data <- readRDS(file = cache)
  }
  else {
    ref_year <- substr(ref_date, start = 1, stop = 4)
    if (ref_year == "2012") {
      
      GEMON <- resolve(sprintf("G:/GezondheidWelzijn/GEMON/%s/", ref_year), "GEMON.*.sav")
      
      data <- subset(spss.system.file(GEMON, to.lower = FALSE),
                     select = c(
                       RINPERSOON, 
                       AGGWS201, CALGB201, CALGB218, CALGB222, CALGB234, CALGB236, CALGB238, CALGB240, CALGS201, 
                       GGADA202, GGEES217, GGEES205, GGEES204, GGEES208, GGADA201,GGEES209,
                       KLGGA208, LFALA201, LFALA213, LFALS230, LFRKA205, LFRKA208, LGBPS203, 
                       LGBPS204, LGBPS205, LGBPS209, MCMZGS203, MMIKA201, fitnorm, rlbew, wwlmwk, wwfmwk
                     ))
      
      # Transform: 1 -> 0, 2 -> 1, 3 -> 0. SMAP 2012 used to process 3 -> 0 instead of 3 -> NA
      encode_123 <- function(x) ifelse(x == 3, 0, x - 1)
      # Transform: 2 -> 0, 1 -> 1
      encode_12_flip <- function(x) 2 - x
      
      data <- with(data, data.table(
        rinpersoon = RINPERSOON %>% as.factor,
        drinker_ = LFALA201 %>% as.factor %>% as.integer %>% encode_123,
        drinker_heavy = LFALA213 %>% as.factor %>% as.integer %>% encode_123, 
        drinker_excess_old = LFALS230 %>% as.factor %>% as.integer %>% encode_123,
        smoker = LFRKA205 %>% as.factor %>% as.integer %>% encode_123,
        #smoker_heavy = LFRKA208 %>% as.factor %>% as.integer %>% encode_123,
        weight_overweight_ = (AGGWS201 >= 25) %>% as.integer, 
        weight_obese_ = (AGGWS201 >= 30) %>% as.integer, 
        weight_underweight_ = (AGGWS201 < 18.5) %>% as.integer,
        weight_healthly_ = (AGGWS201 >= 18.5 & AGGWS201 < 25) %>% as.integer, 
        # weight_overweight_moderate = (AGGWS201 >=25 & AGGWS201 < 30) %>% as.integer,
        # disease_diabetes = CALGB201 %>% as.factor %>% as.integer %>% encode_12_flip,
        # disease_high_bloodpressure = CALGB218 %>% as.factor %>% as.integer %>% encode_12_flip,
        # disease_asthma_or_copd = CALGB222 %>% as.factor %>% as.integer %>% encode_12_flip,
        # disease_joint_wear = CALGB234 %>% as.factor %>% as.integer %>% encode_12_flip,
        # disease_joint_inflammation = CALGB236 %>% as.factor %>% as.integer %>% encode_12_flip,
        # disease_back = CALGB238 %>% as.factor %>% as.integer %>% encode_12_flip,
        # disease_neck_or_shoulder = CALGB240 %>% as.factor %>% as.integer %>% encode_12_flip,
        # disease_any = CALGS201 %>% as.factor %>% as.integer %>% encode_123,
        disability_hearing = LGBPS203 %>% as.factor %>% as.integer %>% encode_123,
        disability_vision = LGBPS204 %>% as.factor %>% as.integer %>% encode_123,
        disability_mobility = LGBPS205 %>% as.factor %>% as.integer %>% encode_123,
        disability_any = LGBPS209 %>% as.factor %>% as.integer %>% encode_123,
        health_reportgood = KLGGA208 %>% as.factor %>% as.integer %>% encode_123,
        # health_fit = fitnorm %>% as.factor %>% as.integer %>% encode_123,
        # health_exercise = rlbew %>% as.factor %>% as.integer %>% encode_123,
        anxitydepression_moderate = GGADA202 %>% as.factor %>% as.integer %>% encode_123,
        anxitydepression_high_ = GGADA201 %>% as.factor %>% as.integer %>% encode_limit(limit=2), 
        lonely = GGEES217 %>% as.factor %>% as.integer %>% encode_123,
        lonely_severe = GGEES209 %>% as.factor %>% as.integer %>% encode_1234,
        lonely_emotional_ = GGEES204 %>% as.factor %>% as.integer %>% encode_limit(limit=2), 
        caregiver_informal = MCMZGS203 %>% as.factor %>% as.integer %>% encode_123,
        difficultyfinancial_12m = MMIKA201 %>% as.factor %>% as.integer %>% encode_123, 
        walk_to_work = as.integer(wwlmwk %>% as.integer >= 1), 
        bike_to_work = as.integer(wwfmwk %>% as.integer >= 1),
        walk_or_bike_to_work = (
          (wwlmwk %>% as.integer >= 1) | 
            (wwfmwk %>% as.integer >= 1)
        ) 
        %>% as.integer
      ))
      
    }
    else if (ref_year == "2016") {
      
      GEMON <- resolve(sprintf("G:/GezondheidWelzijn/GEMON/%s/", ref_year), "GEZOMONITOR.*.sav")
      
      data <- subset(spss.system.file(GEMON, to.lower = FALSE),
                     select=c(RINPERSOON,
                              LFALA201, LFALA213, LFALS231, LFALS230, LFALS232, AGGWS204, AGGWS205, AGGWS206, 
                              AGGWS207, AGGWS208, LFRKA205, LFRKA206, LFRKA207, KLGGA208, CALGA260, CALGA261, 
                              CALGA262, CALGS260, LGBPS203, LGBPS204, LGBPS205, LGBPS209, GGRLS203, GGADA202,
                              GGADA203, KI_RLBEW2017, KIsporter, GGEES217, GGEES208, GGEES209, GGEES215, GGEES216,
                              WOGHB206, GGEES204, GGEES205,
                              MMVWA201, MMIKA201, MCMZGS203, MCMZOS304, MCMZOS305, 
                              wwlmwk, wwfmwk))
      
      # Transform: 1 -> 0, 2 -> 1, 3 -> NA
      encode_123 <- function(x) ifelse(x == 3, NA, x - 1)
      
      
      # Transform smoker_past and lonely_severe with encode_1234 and others with encode_123
      data <- with(data, data.table(
        rinpersoon = RINPERSOON %>% as.factor,
        drinker_ = LFALA201 %>% as.factor %>% as.integer %>% encode_123,
        drinker_heavy = LFALA213 %>% as.factor %>% as.integer %>% encode_123, 
        #drinker_excess = LFALS231 %>% as.factor %>% as.integer %>% encode_123,
        drinker_excess_old = LFALS230 %>% as.factor %>% as.integer %>% encode_123,
        drinker_under1gd = LFALS232 %>% as.factor %>% as.integer %>% encode_123,
        weight_overweight = AGGWS204 %>% as.factor %>% as.integer %>% encode_123,
        weight_obese = AGGWS205 %>% as.factor %>% as.integer %>% encode_123,
        weight_underweight = AGGWS206 %>% as.factor %>% as.integer %>% encode_123,
        weight_healthly = AGGWS207 %>% as.factor %>% as.integer %>% encode_123,
        weight_overweight_moderate = AGGWS208 %>% as.factor %>% as.integer %>% encode_123,
        smoker = LFRKA205 %>% as.factor %>% as.integer %>% encode_123,
        #smoker_past = LFRKA206 %>% as.factor %>% as.integer %>% encode_1234, 
        #smoker_never = LFRKA207 %>% as.factor %>% as.integer %>% encode_123,
        health_reportgood = KLGGA208 %>% as.factor %>% as.integer %>% encode_123,
        illness_longterm = CALGA260 %>% as.factor %>% as.integer %>% encode_123,
        # health_limited = CALGA261 %>% as.factor %>% as.integer %>% encode_123,
        # health_limited_severe = CALGA262 %>% as.factor %>% as.integer %>% encode_123,
        # illness_longterm_limited = CALGS260 %>% as.factor %>% as.integer %>% encode_123,
        disability_hearing = LGBPS203 %>% as.factor %>% as.integer %>% encode_123,
        disability_vision = LGBPS204 %>% as.factor %>% as.integer %>% encode_123,
        disability_mobility = LGBPS205 %>% as.factor %>% as.integer %>% encode_123,
        disability_any = LGBPS209 %>% as.factor %>% as.integer %>% encode_123,
        feels_lifecontrol = GGRLS203 %>% as.factor %>% as.integer %>% encode_123,
        anxitydepression_moderate = GGADA202 %>% as.factor %>% as.integer %>% encode_123,
        anxitydepression_high = GGADA203 %>% as.factor %>% as.integer %>% encode_123,
        exercise_guideline = KI_RLBEW2017 %>% as.factor %>% as.integer %>% encode_123,
        exercise_weekly = KIsporter %>% as.factor %>% as.integer %>% encode_123, 
        lonely = GGEES217 %>% as.factor %>% as.integer %>% encode_123,
        lonely_severe = GGEES209 %>% as.factor %>% as.integer %>% encode_1234, 
        lonely_emotional = GGEES204 %>% as.factor %>% as.integer %>% encode_limit(limit=2), 
        lonely_social = GGEES205 %>% as.factor %>% as.integer %>% encode_limit(limit=2), 
        volunteer = MMVWA201 %>% as.factor %>% as.integer %>% encode_123,
        difficultyfinancial_12m = MMIKA201 %>% as.factor %>% as.integer %>% encode_123,
        caregiver_informal = MCMZGS203 %>% as.factor %>% as.integer %>% encode_123,
        # caregiver_receiving_12m_65p = MCMZOS304 %>% as.factor %>% as.integer %>% encode_123,
        # caregiver_receiving_now_65p = MCMZOS305 %>% as.factor %>% as.integer %>% encode_123, 
        
        walk_to_work = as.integer(wwlmwk %>% as.integer >= 1), 
        bike_to_work = as.integer(wwfmwk %>% as.integer >= 1),
        
        # walk_or_bike_to_work: True if someones bikes at least one minute or walks at least one minute per week. 
        walk_or_bike_to_work = (
          (wwlmwk %>% as.integer >= 1) | 
            (wwfmwk %>% as.integer >= 1)
        ) 
        %>% as.integer
      ))
    }
    else if (ref_year == "2020") {
      GEMON <- resolve(sprintf("G:/GezondheidWelzijn/GEMON/%s/", ref_year), "GEMON.*.sav")
      
      data <- subset(spss.system.file(GEMON, to.lower = FALSE),
                     select=c(RINPERSOON, 
                              LFALA217, LFALAA213, LFALS231, LFALS230, LFALS232,
                              AGGWS204, AGGWS205, AGGWS206, AGGWS207, AGGWS208, 
                              LFRKA205, LFRKA206, LFRKA207, 
                              KLGGA208, CALGA260, CALGA264, CALGA265, CALGS267,
                              LGBPS203, LGBPS204, LGBPS205, LGBPS209, 
                              GGRLS203, GGADA202, GGADA203,
                              KI_RLBEW2017, KIsporter,
                              GGEES217, GGEES209, GGEES204, GGEES205, 
                              GGEES218, GGEES219,
                              MMVWA201, MMIKA201, MCMZGS203, 
                              LFALA213,
                              wwlmwk, wwfmwk,
                              GGSTS16, WOGHBA218
                              ))
      
      # Transform: 1 -> 0, 2 -> 1, 3 -> NA
      encode_123 <- function(x) ifelse(x == 3, NA, x - 1)
      # Transform: 1 -> 0, 2 -> 1, 3 -> 0, 4 -> NA
      encode_1234 <- function(x) ifelse(x == 3, yes =  0, no = ifelse(x == 4, yes = NA, no = x - 1))
      
      
      data <- with(data, data.table(
        rinpersoon = RINPERSOON %>% as.factor,
        drinker = LFALA217 %>% as.factor %>% as.integer %>% encode_123, 
        drinker_over6gd = LFALAA213 %>% as.factor %>% as.integer %>% encode_limit(limit=4, flip=T), #not used
        drinker_heavy = LFALA213 %>% as.factor %>% as.integer %>% encode_123, 
        drinker_excess = LFALS231 %>% as.factor %>% as.integer %>% encode_123, #not used on Statline, but it is the new definition
        drinker_excess_old = LFALS230 %>% as.factor %>% as.integer %>% encode_123, #used instead of the above on Statline
        drinker_under1gd = LFALS232 %>% as.factor %>% as.integer %>% encode_123, 
        weight_overweight = AGGWS204 %>% as.factor %>% as.integer %>% encode_123,
        weight_obese = AGGWS205 %>% as.factor %>% as.integer %>% encode_123,
        weight_underweight = AGGWS206 %>% as.factor %>% as.integer %>% encode_123,
        weight_healthly = AGGWS207 %>% as.factor %>% as.integer %>% encode_123,
        weight_overweight_moderate = AGGWS208 %>% as.factor %>% as.integer %>% encode_123,
        smoker = LFRKA205 %>% as.factor %>% as.integer %>% encode_123,
        smoker_past = LFRKA206 %>% as.factor %>% as.integer %>% encode_1234,#not used
        smoker_never = LFRKA207 %>% as.factor %>% as.integer %>% encode_123,#not used
        health_reportgood = KLGGA208 %>% as.factor %>% as.integer %>% encode_123,
        illness_longterm = CALGA260 %>% as.factor %>% as.integer %>% encode_123,
        health_limited = CALGA264 %>% as.factor %>% as.integer %>% encode_123, 
        health_limited_severe = CALGA265 %>% as.factor %>% as.integer %>% encode_123, 
        illness_longterm_limited = CALGS267 %>% as.factor %>% as.integer %>% encode_123, 
        disability_hearing = LGBPS203 %>% as.factor %>% as.integer %>% encode_123, 
        disability_vision = LGBPS204 %>% as.factor %>% as.integer %>% encode_123,
        disability_mobility = LGBPS205 %>% as.factor %>% as.integer %>% encode_123,
        disability_any = LGBPS209 %>% as.factor %>% as.integer %>% encode_123,
        feels_lifecontrol = GGRLS203 %>% as.factor %>% as.integer %>% encode_123,
        anxitydepression_moderate = GGADA202 %>% as.factor %>% as.integer %>% encode_123,
        anxitydepression_high = GGADA203 %>% as.factor %>% as.integer %>% encode_123,
        exercise_guideline = KI_RLBEW2017 %>% as.factor %>% as.integer %>% encode_123, 
        exercise_weekly = KIsporter %>% as.factor %>% as.integer %>% encode_123,
        lonely = GGEES217 %>% as.factor %>% as.integer %>% encode_123, 
        lonely_severe = GGEES209 %>% as.factor %>% as.integer %>% encode_1234, 
        lonely_emotional = GGEES218 %>% as.factor %>% as.integer %>% encode_123, 
        lonely_social = GGEES219 %>% as.factor %>% as.integer %>% encode_123,
        volunteer = MMVWA201 %>% as.factor %>% as.integer %>% encode_123,
        difficultyfinancial_12m = MMIKA201 %>% as.factor %>% as.integer %>% encode_123,
        caregiver_informal = MCMZGS203 %>% as.factor %>% as.integer %>% encode_123,
        much_stress = GGSTS16 %>% as.factor %>% as.integer %>% encode_123, 
        severe_noise_disturbance_neighbours = WOGHBA218 %>% as.factor %>% as.integer %>% encode_123,
        # walk/Bike to work: at least 1 minute per week. If "onbekend", it is set to NA by as.integer.
        # There are no cases where #minutes per week > 7*24*60, for neither biking nor walking
        walk_to_work = as.integer(wwlmwk %>% as.integer >= 1), 
        bike_to_work = as.integer(wwfmwk %>% as.integer >= 1),
        
        # walk_or_bike_to_work: True if someones bikes at least one minute or walks at least one minute per week. 
        # If one of them is unknown, this can still be True if the other is not zero. Example:
        # wwlmwk > 1 en wwfmwk = "onbekend", dan walk_or_bike_to_work = 1
        # wwlmwk = 0 en wwfmwk = "onbekend", dan walk_or_bike_to_work = "onbekend"
        walk_or_bike_to_work = (
          (wwlmwk %>% as.integer >= 1) | 
          (wwfmwk %>% as.integer >= 1)
          ) 
        %>% as.integer

      )
      
      
      )
    }
    else {
      stop("GEMON for that year not defined.")
    }
    
    # Remove rinpersoons with duplicate entries and riinpersoons with only missing values
    all_missing <- data %>% select.(-rinpersoon) %>% is.na %>% apply(MARGIN = 1, FUN = all)
    data <- data %>%  filter.(!duplicated(rinpersoon), !all_missing) 
    
    setDT(data)
    saveRDS(object = data, file = cache)
  }
  return(data)
}


DataSetGemonNoise <- function(ref_date) {
  
  cache = sprintf("Data/Populatiebestanden/gemonnoisedata_%s.rds", ref_date)
  
  if (file.exists(cache)) {
    data <- readRDS(file = cache)
  }
  else {
    ref_year <- substr(ref_date, start = 1, stop = 4)
    if (ref_year == "2016") {
      GEMON <- resolve(sprintf("G:/GezondheidWelzijn/GEMON/%s/", ref_year), "GEZOMONITOR.*.sav")
      
      data <- subset(spss.system.file(GEMON, to.lower = FALSE),
                     select = c(
                       RINPERSOON, 
                       WOGHBA202, WOGHBA203, WOGHBA205, WOGHBA206, WOGHBA208, WOGHBA209, WOGHBA211, WOGHBA212
                     ))
      
      # Transform: 1 -> 0, 2 -> 1, 3 -> NA.
      encode_123 <- function(x) ifelse(x == 3, NA, x - 1)
      
      # MEHweg50plus = WOGHBA202 = Experiences moderate or severe noise nuisance from road traffic > 50 km/h
      # EHweg50plus = WOGHBA203 = Experiences serious noise nuisance from road traffic > 50 km/h
      # MEHweg50min = WOGHBA205 = Experiences moderate or severe noise nuisance from road traffic < 50 km/h
      # EHweg50min = WOGHBA206 = Experiences serious noise nuisance fom road traffic < 50 km/h
      # MEHrail = WOGHBA208 = Experiences moderate or severe noise nuisance from train traffic
      # EHrail = WOGHBA209 = Experiences serious noise nuisance from train traffic
      # MEHvlieg = WOGHBA211 = Experiences moderate or severe noise pollution from air traffic
      # EHvlieg = WOGHBA212 = Experiences serious noise pollution from air traffic
      data <- with(data, data.table(
        rinpersoon = RINPERSOON %>% as.factor,
        road_medhigh_gt50 = WOGHBA202 %>% as.factor %>% as.integer %>% encode_123,
        road_high_gt50 = WOGHBA203 %>% as.factor %>% as.integer %>% encode_123,
        road_medhigh_sm50 = WOGHBA205 %>% as.factor %>% as.integer %>% encode_123,
        road_high_sm50 = WOGHBA206 %>% as.factor %>% as.integer %>% encode_123,
        rail_medhigh = WOGHBA208 %>% as.factor %>% as.integer %>% encode_123,
        rail_high = WOGHBA209 %>% as.factor %>% as.integer %>% encode_123,
        air_medhigh = WOGHBA211 %>% as.factor %>% as.integer %>% encode_123,
        air_high = WOGHBA212 %>% as.factor %>% as.integer %>% encode_123
      )) %>% 
        mutate.(road_medhigh = ifelse(is.na(road_medhigh_sm50) | (road_medhigh_gt50 == 1), yes = road_medhigh_gt50, no = road_medhigh_sm50),
                road_high = ifelse(is.na(road_high_sm50) | (road_high_gt50 == 1), yes= road_high_gt50, no = road_high_sm50)
        ) # Experiences moderate or severe noise nuisance from road traffic
    }
    else {
      stop("GEMON for that year not defined.")
    }
    
    # Remove rinpersoons with duplicate entries and riinpersoons with only missing values
    all_missing <- data %>% select.(-rinpersoon) %>% is.na %>% apply(MARGIN = 1, FUN = all)
    data <- data %>%  filter.(!duplicated(rinpersoon), !all_missing) 
    
    setDT(data)
    saveRDS(object = data, file = cache)
  }
  return(data)
}



DataSetWoon <- function(ref_date) {
  cache = sprintf("Data/Populatiebestanden/woondata_%s.rds", ref_date)
  
  encode_outcomes <- function(ys) {
    map = list("Helemaal mee eens"=5, 
               "Mee eens"=4, 
               "Niet mee eens, maar ook niet mee oneens" = 3, 
               "Mee oneens" = 2, 
               "Helemaal mee oneens" = 1,
               
               # For satisfied_region
               "Zeer tevreden" = 5,
               "Tevreden" = 4, 
               "Niet tevreden, maar ook niet ontevreden" = 3, 
               "Ontevreden" = 2, 
               "Zeer ontevreden" = 1)
    encode_outcome <- function(y) {
      if (!(y %in% names(map))) {
        return(NA)
      } else {
        return (map[[y]])
      }
    }
    return (ys %>% lapply(encode_outcome) %>% unlist)
  }
  
  if (file.exists(cache)) {
    data <- readRDS(file = cache)
  }
  else {
    ref_year <- substr(ref_date, start = 1, stop = 4)
    WOON <- resolve(sprintf("G:/BouwenWonen/WOON/%s/", ref_year), "*.sav")
    
    if (ref_year %in% c("2015", "2018")) {

      data <- subset(spss.system.file(WOON, to.lower = T),
                     select = c(rinpersoon, datwm, cohesie, tevrstr, twoning, twoonomg, tvervele, tgehecht, brtthuis, brtveilig))
      
      data <- with(data, data.table(
        rinpersoon = rinpersoon %>% as.factor,
        date = datwm %>% as.character,
        social_cohesion = cohesie %>% as.numeric,
        satisfied_region = tevrstr %>% as.character %>% encode_outcomes,
        satisfied_house = twoning %>% as.character %>% encode_outcomes,
        satisfied_surroundings = twoonomg %>% as.character %>% encode_outcomes,
        annoyed_w_ngbh = tvervele %>% as.character %>% encode_outcomes,
        attached_to_ngbh = tgehecht %>% as.character %>% encode_outcomes,
        at_home_in_ngbh = brtthuis %>% as.character %>% encode_outcomes,
        afraid_ngbh = brtveilig %>% as.character %>% encode_outcomes
      )) %>% 
        mutate.(
          date = gsub("\\D", "", date %>% as.character())
          )
    }
    #In 2006 there are less indicators and no survey dates
    else if (ref_year == "2006") {
      data <- subset(spss.system.file(WOON, to.lower = T),
                     select = c(rinpersoon, cohesie, twoning, twoonomg, tvervele, tgehecht, brtthuis, onvbuit))
      # for some reason, social cohesion does not get parsed by spss.system.file.
      
      data <- with(data, data.table(
        rinpersoon = rinpersoon %>% as.factor,
        social_cohesion = cohesie %>% as.numeric,
        satisfied_house = twoning %>% as.character %>% encode_outcomes,
        satisfied_surroundings = twoonomg %>% as.character %>% encode_outcomes,
        annoyed_w_ngbh = tvervele %>% as.character %>% encode_outcomes,
        attached_to_ngbh = tgehecht  %>% as.character %>% encode_outcomes,
        at_home_in_ngbh = brtthuis  %>% as.character %>% encode_outcomes
      ))
    }
    else {
      stop("Reference year not available. ")
    }
    
    setDT(data)
    saveRDS(object = data, file = cache)
  }
  
  # There are no rinpersoons with only missing values, and no duplicated rows
  
  return(data)
}


# Run only if the script is run by itself
if (sys.nframe() == 0) {
  print("Fetching data...")
  
  #SMAP 2020
  ref_date        <- "20200901"
  data.gemon      <- DataSetGemon(ref_date)       # health monitor
  data.population <- DataSetSMAP2020(ref_date)    # population data with missing values,
                                                  # some SPSS files for 2020 not yet in RA...
  data.population <- DataSetFilledSMAP(ref_date)  # population data with missing filled
  
  # SMAP 2016
  ref_date        <- "20160901"
  data.gemon      <- DataSetGemon(ref_date)       # health monitor
  data.population <- DataSetSMAP(ref_date)        # population data with missing values
  data.population <- DataSetFilledSMAP(ref_date)  # population data with missing filled
   
  # SMAP 2016 additional noise disturbance
  data.gemon <-   DataSetGemonNoise(ref_date)
  data.noise <-   DataSetNoise('Data/7975bag2017_geluid_upload_cbs_07052018CBKV1.sav', ref_date)
   
  # SMAP 2012
  ref_date        <- "20120901"
  data.gemon      <- DataSetGemon(ref_date)       # health monitor
  data.population <- DataSetSMAP(ref_date)        # population data with missing values
  data.population <- DataSetFilledSMAP(ref_date)  # population data with missing filled
  
  # WOON
  ref_date <- "20180101" #"20150101" "20060101"
  data.woon       <- DataSetWoon(ref_date)                      # housing survey
  survey_dates    <- data.woon %>% select.(rinpersoon, date)    # population survey dates for a subset
  data.population <- DataSetSurveySMAP(ref_date, survey_dates)  # population data with missing values
  data.population <- DataSetFilledSurveySMAP(ref_date)          # population data with missing filled

  #ref_date <- "20190101"
  #data.population <- DataSetSurveySMAP(ref_date,  filter_18yo = F)
  #data.population <- DataSetFilledSurveySMAP(ref_date) 
 }
# SMAP 2020 Paper

Reproducible experiments for the paper "A machine learning approach to small area estimation: predicting the health, housing and well-being of the population of Netherlands" and Netherlands 2020 GEMON small area estimates.

## Getting started

Access to the [CBS remote access environment](https://www.cbs.nl/en-gb/onze-diensten/customised-services-microdata/microdata-conducting-your-own-research) is required to fetch and use the data sets.

## Obtaining the data sets

- [Scripts/General/data_loading.R](Scripts/General/data_loading.R) fetches the SMAP 2012/2016/2020, WOON 2006/2015/2018, and SMAP 2016 noise measurement data sets.

Run the script to fetch the required files. Only the SMAP 2020, SMAP 2016 with noise measurements, and WOON 2018 are required for the paper. The large script contains the following functions that merge several smaller data sources into the requested data set: 

```
#SMAP 2020
ref_date        <- "20200901"
data.gemon      <- DataSetGemon(ref_date)       # health monitor
data.population <- DataSetSMAP2020(ref_date)    # population data with missing values
data.population <- DataSetFilledSMAP(ref_date)  # population data with missing filled

# SMAP 2016
ref_date        <- "20160901"
data.gemon      <- DataSetGemon(ref_date)       # health monitor
data.population <- DataSetSMAP(ref_date)        # population data with missing values
data.population <- DataSetFilledSMAP(ref_date)  # population data with missing filled

# SMAP 2016 additional noise disturbance
data.gemon <-   DataSetGemonNoise(ref_date)
data.noise <-   DataSetNoise('Data/7975bag2017_geluid_upload_cbs_07052018CBKV1.sav', ref_date)

# WOON
ref_date <- "20180101" #"20150101" "20060101"
data.woon       <- DataSetWoon(ref_date)                      # housing survey
survey_dates    <- data.woon %>% select.(rinpersoon, date)    # population survey dates for a subset
data.population <- DataSetSurveySMAP(ref_date, survey_dates)  # population data with missing values
data.population <- DataSetFilledSurveySMAP(ref_date)          # population data with missing filled
```

Once the data sets are fetched, cached versions are saved in Data/Populatiebestanden.

## Reproducible experiments 

- [Scripts/SMAP/PAPER01 - experiments.R](Scripts/SMAP/PAPER01 - experiments.R) runs all the experiments and saves the results to results/smap/paper.
- [Scripts/SMAP/PAPER02 - visualizations.R](Scripts/SMAP/PAPER02 - visualizations.R) has visualization codes to plot the aforementioned results.

The results in the paper are the files saved into results/smap/paper and the plots created from these using the visualization code.

## Small area estimates

- [Scripts/SMAP/SMAP14 - fit xgboost to gemon.R](Scripts/SMAP/SMAP14 - fit xgboost to gemon.R) produces the 2020 GEMON estimates reported in statline.
- [Scripts/SMAP/SMAP15 - fit xgboost to woon.R](Scripts/SMAP/SMAP15 - fit xgboost to woon.R) produces the latest 2018 WOON estimates.

The scripts create bootstrap based prediction intervals and take a while to run. See [RIVM statline](https://statline.rivm.nl/#/RIVM/nl/dataset/50090NED) for the 2020 GEMON estimates.

## Simple example

- [Scripts/SMAP/SMAP01 - example SMAP.R](Scripts/SMAP/SMAP01 - example SMAP.R) is a simple example of how to use the original STAR model.
- [Scripts/SMAP/SMAP02 - example xgboost.R](Scripts/SMAP/SMAP02 - example xgboost.R) is a simple example of how to use the new XGBoost model.

Both examples contain a very short procedual script that shows how to:
1. Evaluate the model by splitting the health monitor into train & test set. 
2. Predict missing population values from a model trained on the health monitor

## Authors 

The new XGBoost model and source codes for data sets, experiments, and examples was created by Markus Viljanen (markus.viljanen@rivm.nl) and Lotta Meijerink (lotta.meijerink@rivm.nl). 
The original STAR model and data processing was created by Jan van de Kassteele (jan.van.de.kassteele@rivm.nl), see also https://github.com/kassteele/SMAP, a refractored version appears here.

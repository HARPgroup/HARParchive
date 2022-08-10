# This script will add a RomTtimeSeries for om_model_run
basepath='/var/www/R';
source("/var/www/R/config.R")

suppressPackageStartupMessages(library(R.utils))

argst <- commandArgs(trailingOnly = T)
featureid <- argst[1] # pid of model being run (featureID = hydroID)
entity_type <- argst[2] #  dh_timeseries
varkey <- argst[3] # om_model_run
tstime <- argst[4] # timestamp that model was initiated
tsendtime <- argst[5] # timestamp that model was completed (should be NULL when model is initiated)
tsvalue <- argst[6] # run status (0=finished, 1=initializing, 2=running)
tscode <- argst[7] # scenario/runid (hsp2_2022)


# Exports to VAHydro 
# Make sure that a run record is created if one doesn't already exist

ds <- RomDataSource$new(site, rest_uname = rest_uname)
ds$get_token(rest_pw)

riverseg<- RomFeature$new(
  ds,
  list(
    hydroid= featureid
  ),
  TRUE
)

model <- RomProperty$new(
  ds,
  list(
    varkey= varkey, 
    featureid= riverseg$hydroid, 
    entity_type= entity_type, 
    propcode= tscode,
    propvalue= tsvalue,
    startdate = tstime,
    enddate= tsendtime
  ), 
  TRUE
)
model$save(TRUE)





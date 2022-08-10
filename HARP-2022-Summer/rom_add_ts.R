# This script will add a RomTtimeSeries for om_model_run
basepath='/var/www/R';
source("/var/www/R/config.R")

suppressPackageStartupMessages(library(R.utils))

argst <- commandArgs(trailingOnly = T)
featureid <- argst[1] # pid of model being run (featureID = hydroID)
entity_type <- argst[2] # ex. dh_feature
varkey <- argst[3] # om_model_run
tstime <- argst[4] # timestamp that model was initiated
tsendtime <- argst[5] # timestamp that model was completed (should be NULL when model is initiated)
tsvalue <- argst[6] # run status (0=finished, 1=initializing, 2=running)
tscode <- argst[7] # scenario/runid (hsp2_2022)


# Exports to VAHydro 
# Make sure that a run record is created if one doesn't already exist

ds <- RomDataSource$new(site, rest_uname = rest_uname)
ds$get_token(rest_pw)
rseg_ftype='vahydro'

riverseg<- RomFeature$new(
  ds,
  list(
    hydroid=featureid
  ),
  TRUE
)

model <- RomProperty$new(
  ds,
  list(
    varkey=varkey, 
    featureid=riverseg$hydroid, 
    entity_type=entity_type, 
    propcode="cbp-5.3.2" 
  ), 
  TRUE
)
model$save(TRUE)

model_scenario <- RomProperty$new( 
  ds,
  list(
    varkey="om_scenario", 
    featureid=model$pid, 
    entity_type="dh_properties", 
    propname=tscode, 
    propcode=tscode
  ), 
  TRUE
)
model_scenario$save(TRUE)

#Want to attatch and export tstime and tsendtime
model_ts <- RomTimeSeries$new( 
  ds,
  list(
    varkey="om_class_textField", 
    featureid=model$pid, 
    entity_type="dh_timeseries", 
    propname='model ts'
  ), 
  TRUE
)
model_ts$propcode <- as.character(paste(tstime, tsendtime, sep = '-'))
model_ts$save(TRUE)


# This script will add a RomTtimeSeries for om_model_run
basepath='/var/www/R';
source("/var/www/R/config.R")

suppressPackageStartupMessages(library(R.utils))

argst <- commandArgs(trailingOnly = T)
featureid <- argst[1] # pid of model run/element being modified 
entity_type <- argst[2] # describes what we're adding the timeseries to (dh_properties)
varkey <- argst[3] 
tstime <- argst[4] # timestamp that model was initiated
tsendtime <- argst[5] # timestamp that model was completed (should be NULL when model is initiated)
tsvalue <- argst[6] # run status (0=finished, 1=initializing, 2=running)
tscode <- argst[7] # scenario/runid (hsp2_2022)


# Exports to VAHydro 
# Make sure that a run record is created if one doesn't already exist

ds <- RomDataSource$new(site, rest_uname = rest_uname)
ds$get_token(rest_pw)


model_ts <- RomTS$new(
  ds,
  list(
    entity_type= entity_type,
    featureid= featureid,
    varkey= varkey, 
    tstime = tstime,
    tsendtime = tsendtime,
    tscode = tscode,
    tsvalue = tsvalue
  ),
  TRUE
)
model_ts$save(TRUE)



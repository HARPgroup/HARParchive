basepath='/var/www/R';
source("/var/www/R/config.R")

suppressPackageStartupMessages(library(R.utils))

argst <- commandArgs(trailingOnly = T)
featureid <- argst[1] 
entity_type <- argst[2] 
varkey <- argst[3] 
tstime <- argst[4] 
tsendtime <- argst[5] # timestamp that model was completed (should be NULL when model is initiated)
tsvalue <- argst[6] # run status (0=finished, 1=initializing, 2=running)
tscode <- argst[7] # scenario/runid (hsp2_2022)

tstime <- as.numeric(as.POSIXct(tstime))
tsendtime <- as.numeric(as.POSIXct(tsendtime))

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

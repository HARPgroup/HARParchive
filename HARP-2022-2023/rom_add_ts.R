basepath='/var/www/R';
source("/var/www/R/config.R")

suppressPackageStartupMessages(library(R.utils))

argst <- commandArgs(trailingOnly = T)
featureid <- argst[1] 
entity_type <- argst[2] 
varkey <- argst[3] 
tstime <- argst[4] 
tsendtime <- argst[5] 
tsvalue <- argst[6] 
tscode <- argst[7] 

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
    tscode = tscode
  ),
  TRUE
)
model_ts$tsvalue = tsvalue
model_ts$tstime = tstime
model_ts$tsendtime = tsendtime
model_ts$save(TRUE)

#batch script for updating Smin_CPL for impoundments & river elements 

# Load Libraries
library(stringr)
library(hydrotools)
basepath='/var/www/R';
source(paste(basepath,'config.R',sep='/'))
# authenticate
ds <- RomDataSource$new(site, rest_uname)
ds$get_token(rest_pw)

source(paste0(github_location,"/HARParchive/HARP-2023-Summer/fn_get_pd_min.R"),local = TRUE) #load Smin_CPL function

# Read Args
argst <- commandArgs(trailingOnly=T)
runid <- as.integer(argst[1]) #number-only part of a runid (ex. 11)
#runid = 11

#get all impoundment features 
df <- data.frame(
  'model_version' = c('vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0'),
  'runid' = c('runid_11', 'runid_400', 'runid_600', 'runid_13'),
  'metric' = c('usable_pct_p0','usable_pct_p0', 'usable_pct_p0', 'usable_pct_p0'),
  'runlabel' = c('Smin_pct_11', 'Smin_pct_perm', 'Smin_pct_prop', 'Smin_pct_800')
)
all_imp_data <- om_vahydro_metric_grid(
  metric = metric, runids = df, bundle = 'all', ftype = "all",
  base_url = paste(site,'entity-model-prop-level-export',sep="/"),
  ds = ds
)

## add loop-through of all elements in all_imp_data
#get model element
model <- RomProperty$new(ds,list(
  featureid = all_imp_data$featureid[1],
  propcode = "vahydro-1.0",
  varkey = "om_hydroimpoundment"),
  TRUE
)

#get om_element_connection value for elid to get runfile 
token = ds$get_token(rest_pw) #needed for om_get_model_elid
elid <- om_get_model_elementid(
  base_url = site,
  mid = model$pid
)
rm(token) #security!

#get runfile whcih has storage timeseries data 
dat <- fn_get_runfile(elid, runid, site= omsite,  cached = FALSE)

#is the impoundment active? (imp_off = 0?)
cols <- names(dat)
if ("imp_off" %in% cols) {
  imp_off <- as.integer(median(dat$imp_off))
} else {
  # imp_off is NOT in the cols, so impoundment must be active
  imp_off = 0
}

#should we retrieve L30_year and L90_year from the riverseg-model-scenario here?

#this would be classified as an exact method, while using sqldf to crop by L30/L90_year would be an approx. method 
Smin_L90 <- fn_get_pd_min(ts_data = dat, critical_pd_length = 90, date_filter = c('1995-01-01','2020-12-31'), colname = "Storage")


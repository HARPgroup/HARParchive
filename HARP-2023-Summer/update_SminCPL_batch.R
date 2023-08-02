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

#all_imp_data$Smin_L90_exact <- NA
#all_imp_data$Smin_L90_approx <- NA

## add loop-through of all elements in all_imp_data ?
#for (i in 1:nrow(all_imp_data)) { #fails on i=2

#set i = row for test case of interest
i = 28

#get model element
model <- RomProperty$new(ds,list( #only works for impoundment features not those within riverseg models 
  featureid = all_imp_data$featureid[i],
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

#apply Smin_cpl function
#this would be classified as an exact method, while using sqldf to crop by L30/L90_year would be an approx. method 
Smin_L90_exact <- fn_get_pd_min(ts_data = dat, critical_pd_length = 90, date_filter = c('1995-01-01','2020-12-31'), colname = "Storage")

#L30_year and L90_year from the riverseg-model-scenario -- should be in loop
riverseg<- RomFeature$new( #get riverseg feature from vahydro
  ds,
  list(
    hydrocode=paste('vahydrosw_wshed_',all_imp_data$riverseg[i], sep = ''),
    ftype='vahydro',
    bundle='watershed'
  ),
  TRUE)
  
model <- RomProperty$new( #get vahydro-1.0 model feature from vahydro
  ds,
  list(
    featureid=riverseg$hydroid, 
    propcode='vahydro-1.0'
  ), 
  TRUE)
  
model_scenario <- RomProperty$new( #get scenario/runid from vahydro
  ds,
  list(
    varkey="om_scenario", 
    featureid=model$pid, 
    propname= paste0('runid_',runid) 
  ), 
  TRUE)
  
L30prop <- RomProperty$new( #get metric from vahydro
  ds, list(
    featureid=model_scenario$pid,
    entity_type='dh_properties',
    propname = 'l30_year'
  ),
  TRUE)
L30_year <- L30prop$propvalue

L90prop <- RomProperty$new( #get metric from vahydro
  ds, list(
    featureid=model_scenario$pid,
    entity_type='dh_properties',
    propname = 'l90_year'
  ),
  TRUE)
L90_year <- L90prop$propvalue

dat_df <- as.data.frame(dat) #zoo to df
dat_df$L90_year <- L90_year

#Smin using approx method
Smin_L90 <- sqldf("SELECT min(Storage), year
                    FROM dat_df 
                    WHERE year = L90_year")

Smin_L90_approx <- as.numeric(Smin_L90$`min(Storage)`)

#}

paste0(all_imp_data$propname[i])
paste0('Approx Smin_L90: ', round(Smin_L90_approx, digits = 3))
paste0('Exact Smin_L90: ', round(Smin_L90_exact, digits = 3))

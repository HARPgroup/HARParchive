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
options(scipen = 999) #disable scientific notation

#read args
argst <- commandArgs(trailingOnly=T)
#runid <- as.integer(argst[1]) #number-only part of a runid (ex. 11)
runid = 11

#get all impoundment features 
#df_imp <- data.frame(
#  'model_version' = c('vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0'),
#  'runid' = c('runid_11', 'runid_400', 'runid_600', 'runid_13'),
#  'metric' = c('usable_pct_p0','usable_pct_p0', 'usable_pct_p0', 'usable_pct_p0'),
#  'runlabel' = c('Smin_pct_11', 'Smin_pct_perm', 'Smin_pct_prop', 'Smin_pct_800')
#)

df_imp <- data.frame(
  'model_version' = c('vahydro-1.0'),
  'runid' = c(paste0('runid_',runid)),
  'metric' = c('usable_pct_p0'),
  'runlabel' = c(paste0('Smin_pct_',runid))
)

all_imp_data <- om_vahydro_metric_grid(
  metric = metric, runids = df_imp, bundle = 'all', ftype = "all",
  base_url = paste(site,'entity-model-prop-level-export',sep="/"),
  ds = ds
)

#L90 and L30 years for all riversegs
#df_yr <- data.frame(
#  'model_version' = c('vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0'),
#  'runid' = c('runid_11', 'runid_13', 'runid_11', 'runid_13'),
#  'metric' = c('l30_year','l30_year', 'l90_year', 'l90_year'),
#  'runlabel' = c('L30_year_11', 'L30_year_13', 'L90_year_11', 'L90_year_13')
#)

df_yr <- data.frame(
  'model_version' = c('vahydro-1.0'),
  'runid' = c(paste0('runid_',runid), paste0('runid_',runid)),
  'metric' = c('l30_year','l90_year'),
  'runlabel' = c(paste0('L30_year_',runid), paste0('L90_year_',runid))
)

lyear_data <- om_vahydro_metric_grid(
  metric = metric, runids = df_yr, bundle = 'watershed', 
  base_url = paste(site,'entity-model-prop-level-export',sep="/"),
  ds = ds
)

#join L90 & L30 years with imp data
#all_imp_data <- sqldf("SELECT a.*, b.L30_year_11, b.L30_year_13, b.L90_year_11, b.L90_year_13
#                      FROM all_imp_data as a
#                      LEFT OUTER JOIN l90year_data as b
#                      ON (a.riverseg = b.riverseg)")

all_imp_data <- sqldf(paste0("SELECT a.*, b.L30_year_", runid, ", b.L90_year_", runid, 
                      " FROM all_imp_data as a
                      LEFT OUTER JOIN lyear_data as b
                      ON (a.riverseg = b.riverseg)"))

token = ds$get_token(rest_pw) #needed for elid function in loop
for (i in 1:nrow(all_imp_data)) { 

#get om_element_connection value for elid to get runfile 
 #needed for om_get_model_elid
elid <- om_get_model_elementid(
  base_url = site,
  mid = all_imp_data$pid[i]
)

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

#different names for same storage value
names(dat)[names(dat) == 'impoundment_Storage'] <- 'Storage'
names(dat)[names(dat) == 'local_impoundment_Storage'] <- 'Storage'

#apply Smin_cpl function, exact method
all_imp_data$Smin_L90_exact[i] <- round(fn_get_pd_min(ts_data = dat, critical_pd_length = 90,
                                                date_filter = c('1980-01-01','2022-12-31'), colname = "Storage"), digits = 3)
all_imp_data$Smin_L30_exact[i] <- round(fn_get_pd_min(ts_data = dat, critical_pd_length = 30,
                                                date_filter = c('1980-01-01','2022-12-31'), colname = "Storage"), digits = 3)

dat_df <- as.data.frame(dat)

#Smin using approx method

#get low flow years
L90_year <- all_imp_data[i,paste0('L90_year_',runid)]
L30_year <- all_imp_data[i,paste0('L30_year_',runid)]

#don't break if one or both low flow years weren't found 
if (is.null(L90_year)==FALSE & is.na(L90_year)==FALSE ) {
  Smin_L90 <- sqldf(paste0("SELECT min(Storage), year
                    FROM dat_df 
                    WHERE year = ", L90_year))
  all_imp_data$Smin_L90_approx[i] <- as.numeric(Smin_L90$`min(Storage)`)
} else {
  all_imp_data$Smin_L90_approx[i] <- NA
}

if (is.null(L30_year)==FALSE & is.na(L30_year)==FALSE) {
  Smin_L30 <- sqldf(paste0("SELECT min(Storage), year
                    FROM dat_df 
                    WHERE year = ", L30_year))
  
  all_imp_data$Smin_L30_approx[i] <- as.numeric(Smin_L30$`min(Storage)`)
} else {
  all_imp_data$Smin_L30_approx[i] <- NA
}

#get scenario prop from vahydro, where metric will be posted?
#scenprop <- RomProperty$new(ds, list(
#  varkey = 'om_scenario',
#  propname = paste0('runid_', runid),
#  featureid = all_imp_data$pid[1], #model pids in all_imp_data
#  entity_type = "dh_properties",
#  bundle = "dh_properties"), 
#TRUE)

#post metrics? units: mg
#vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'Smin_L90_mg', Smin_L90_mg, ds)
#vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'Smin_L30_mg', Smin_L30_mg, ds)

}
rm(token) #security!

data <- data_frame(Name = all_imp_data$propname,
                   Riverseg = all_imp_data$riverseg,
                   L90_year = all_imp_data[,paste0('L90_year_',runid)],
                   L30_year = all_imp_data[,paste0('L30_year_',runid)],
                   Smin_L30_exact = all_imp_data$Smin_L30_exact,
                   Smin_L30_approx = all_imp_data$Smin_L30_approx,
                   Smin_L90_exact = all_imp_data$Smin_L90_exact,
                   Smin_L90_approx = all_imp_data$Smin_L90_approx
                    )




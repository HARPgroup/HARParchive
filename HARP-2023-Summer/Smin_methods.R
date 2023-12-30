#Comparing different methods of calculating Smin for water availability calculations 

# Methods:
##  Approximate: the minimum storage that occurs during a low-flow year (L30_year)
##  Near-exact: the minimum storage that occurs during a low flow period (30 or 90-day period when L30/L90 occurs)
##  Exact: the near-exact method divided by the number of days into that low flow period which the minimum occurs 
# Note: approx. and near-exact methods result in a volume, while exact method gives a volume/day 

# Load Libraries
library(data.table)
library(stringr)
library(hydrotools)
library(zoo)
library(IHA)
library(dplyr)

basepath='/var/www/R';
source(paste(basepath,'config.R',sep='/'))
# authenticate
ds <- RomDataSource$new(site, rest_uname)
ds$get_token(rest_pw)

source('https://github.com/HARPgroup/om/raw/master/R/summarize/fn_get_pd_min.R') #load Smin_CPL function, approx method
source(paste0(github_location,"/HARParchive/HARP-2023-Summer/fn_pct_diff.R"),local = TRUE) #load % difference function
options(scipen = 999) #disable scientific notation

#get all impoundment features 
# df_imp <- data.frame(
#   'model_version' = c('vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0'),
#   'runid' = c('runid_400', 'runid_400', 'runid_400', 'runid_400'),
#   'metric' = c('usable_pct_p0','usable_pct_p0', 'usable_pct_p0', 'usable_pct_p0'),
#   'runlabel' = c('Smin_pct_11', 'Smin_pct_perm', 'Smin_pct_prop', 'Smin_pct_800')
# )
# all_imp_data <- om_vahydro_metric_grid(
#   metric = metric, runids = df_imp, bundle = 'all', ftype = "all",
#   base_url = paste(site,'entity-model-prop-level-export',sep="/"),
#   ds = ds
# )

runid <- 11
runlabel <- paste0('runid_', runid)

#Pulling in Smin metrics from vahydro (approx method):
df_storage <- data.frame(
  'model_version' = c('vahydro-1.0', 'vahydro-1.0'),
  'runid' = c('runid_11', 'runid_11'),
  'metric' = c('Smin_L30_mg', 'Smin_L90_mg'),
  'runlabel' = c(paste0('SminL30mg_', runid, '_vah'), paste0('SminL90mg_', runid, '_vah'))
)
storage_data <- om_vahydro_metric_grid(
  metric = metric, runids = df_storage, bundle = 'all', ftype = "all",
  base_url = paste(site,'entity-model-prop-level-export',sep="/"),
  ds = ds
)

#storage_data <- head(storage_data, -2) #remove 2 non-impoundments from the bottom from testing 

#Convert approx. values to mgd
# storage_data$Smin_L30_11_apx_mgd <- storage_data$SminL30mg_11 / 30
# storage_data$Smin_L30_13_apx_mgd <- storage_data$SminL30mg_13 / 30
# storage_data$Smin_L90_11_apx_mgd <- storage_data$SminL90mg_11 / 30
# storage_data$Smin_L90_13_apx_mgd <- storage_data$SminL90mg_13 / 30

#Columns that will hold # of days ouside the low-flow periods that Smin occurs for approx method 
storage_data$outside_pd30 <- NA
storage_data$outside_pd90 <- NA

#Comparing methods
for (i in 1:nrow(storage_data)) {

  ## Runfiles are saved locally now to save time 
  #Get runfile w/ timeseries data
  # pid <- storage_data$pid[i]
  # 
  # token = ds$get_token(rest_pw) #needed for elid function
  # elid <- om_get_model_elementid(
  #   base_url = site,
  #   mid = storage_data$pid[i]
  # )
  # rm(token)
  # 
  # dat <- fn_get_runfile(elid, runid, site= omsite,  cached = FALSE) #get timeseries data (read in as zoo)
  # mode(dat) <- 'numeric'

  
  #Reading in runfiles saved locally (runid11): 
  dat <- fread(paste0(github_location,"/HARParchive/HARP-2023-Summer/impoundment_runfiles/runfile_imp_",storage_data$featureid[i],"_",runid,".csv"))
  dat <- zoo(dat, order.by = dat$timestamp) #make zoo to mimic fn_get_runfile 
  
  #trim runfile
  syear = as.integer(min(dat$year))
  eyear = as.integer(max(dat$year))
  model_run_start <- min(dat$thisdate)
  model_run_end <- max(dat$thisdate)
  if (syear < (eyear - 2)) {
    sdate <- as.Date(paste0(syear,"-10-01"))
    edate <- as.Date(paste0(eyear,"-09-30"))
    flow_year_type <- 'water'
  } else {
    sdate <- as.Date(paste0(syear,"-02-01"))
    edate <- as.Date(paste0(eyear,"-12-31"))
    flow_year_type <- 'calendar'
  }
  dat <- window(dat, start = sdate, end = edate);
  mode(dat) <- 'numeric' 
  
  #is the impoundment active? (imp_off = 0?)
  cols <- names(dat)
  
  if ("imp_off" %in% cols) {
    imp_off <- as.integer(median(dat$imp_off))
  } else {
    # imp_off is NOT in the cols, so impoundment must be active
    imp_off = 0
  }
  
  #Different names for storage and Qin values:
  
  if (!('Storage' %in% cols)) { #if a column named Storage does not exist 
    
    if ('impoundment_Storage' %in% cols) { 
      names(dat)[names(dat) == 'impoundment_Storage'] <- 'Storage'
    } else if ('local_impoundment_Storage' %in% cols) {  
      names(dat)[names(dat) == 'local_impoundment_Storage'] <- 'Storage'
    } else {
      dat$Storage <- 0 #set storage to 0 if not an impoundment feature 
    }
  }
  
  
  if (!('Qin' %in% cols)) { #if a Qin column does not exist 
    
    if ('impoundment_Qin' %in% cols) {
      names(dat)[names(dat) == 'impoundment_Qin'] <- 'Qin'
    } else if ('Qreach' %in% cols) {
      names(dat)[names(dat) == 'Qreach'] <- 'Qin'
    }
    
  }
  
  #find l30 and l90 years based on Qin
  flows <- zoo(dat$Qin, order.by = index(dat));
  loflows <- group2(flows, year = 'calendar') #vahydro Smin metrics used calendar year method 
  
  l90 <- loflows["90 Day Min"];
  ndx = which.min(as.numeric(l90[,"90 Day Min"]));
  l90_Qout = round(loflows[ndx,]$"90 Day Min",6);
  l90_year = loflows[ndx,]$"year";
  l90_start = as.Date(paste0(l90_year,"-01-01"))
  l90_end = as.Date(paste0(l90_year,"-12-31"))
  datpd_90 <- window(
    dat,
    start = l90_start,
    end = l90_end
  )
  
  l30 <- loflows["30 Day Min"];
  ndx = which.min(as.numeric(l30[,"30 Day Min"]));
  l30_Qout = round(loflows[ndx,]$"30 Day Min",6);
  l30_year = loflows[ndx,]$"year";
  l30_start = as.Date(paste0(l30_year,"-01-01"))
  l30_end = as.Date(paste0(l30_year,"-12-31"))
  datpd_30 <- window(
    dat,
    start = l30_start,
    end = l30_end
  )

  ##Approximate method: Smin within low-flow years:
  Smin_L30_11_approx_acf <- fn_get_pd_min(ts_data = dat, start_date = l30_start, end_date = l30_end, colname = "Storage")
  Smin_L90_11_approx_acf <- fn_get_pd_min(ts_data = dat, start_date = l90_start, end_date = l90_end, colname = "Storage")
 
  storage_data$Smin_L30_11_approx_mg[i] <- Smin_L30_11_approx_acf / 3.069
  storage_data$Smin_L90_11_approx_mg[i] <- Smin_L90_11_approx_acf / 3.069
  
  
  ##Near-exact method: Smin within the L30 and L90 periods:
  
  #data for each l30 and l90 years
  l30yr_flows <- window(flows, start = l30_start, end = l30_end)
  l90yr_flows <- window(flows, start = l90_start, end = l90_end)
  
  l30yr_data <- window(dat, start = l30_start, end = l30_end)
  l90yr_data <- window(dat, start = l90_start, end = l90_end)
  
  #zoo to data frame
  l30yr_flows <- as.data.frame(l30yr_flows)
  l90yr_flows <- as.data.frame(l90yr_flows)
  
  l30yr_data <- as.data.frame(l30yr_data)
  l90yr_data <- as.data.frame(l90yr_data)
  
  l30yr_df <- l30yr_flows %>% mutate(rollmean_30 = rollmean(l30yr_flows, k=30, fill=NA, align='left' ))
  l90yr_df <- l90yr_flows %>% mutate(rollmean_90 = rollmean(l90yr_flows, k=90, fill=NA, align='left'))
  
  #start dates for low flow periods
  rownum_start90 <- which.min(l90yr_df$rollmean_90) 
  rownum_start30 <- which.min(l30yr_df$rollmean_30)
  
  l30pd_start <- as.Date(row.names(l30yr_df[which.min(l30yr_df$rollmean_30),]))
  l90pd_start <- as.Date(row.names(l90yr_df[which.min(l90yr_df$rollmean_90),]))
  
  #end dates for low flow periods
  rownum_end90 <- which.min(l90yr_df$rollmean_90) + 90
  rownum_end30 <- which.min(l30yr_df$rollmean_30) + 30
  
  #end rownum cant be greater than length of data 
  if (rownum_end90 > nrow(l90yr_df)) {
    rownum_end90 = nrow(l90yr_df)
  }
  if (rownum_end30 > nrow(l30yr_df)) {
    rownum_end30 = nrow(l30yr_df)
  }
  
  l30pd_end <- as.Date(row.names(l30yr_df[rownum_end30, ]))
  l90pd_end <- as.Date(row.names(l90yr_df[rownum_end90, ]))
  
  #flow data for the drought periods 
  l30pd_flows <- window(dat, start = l30pd_start, end = l30pd_end)
  l90pd_flows <- window(dat, start = l90pd_start, end = l90pd_end)
  
  l30pd_df <- as.data.frame(l30pd_flows)
  l90pd_df <- as.data.frame(l90pd_flows)
  
  #Smin within the low flow periods
    #Storage needs to be converted from acre-feet to million gallons
  storage_data$Smin_L30_nearexact[i] <- min(l30pd_df$Storage) / 3.069
  storage_data$Smin_L90_nearexact[i] <- min(l90pd_df$Storage) / 3.069
  
  
  # storage_data$Smin_L90_nearexact_perday[i] <- (storage_data$Smin_L90_nearexact[i] / 90) / 3.069 #convert afd to mgd
  # storage_data$Smin_L30_nearexact_perday[i] <- (storage_data$Smin_L30_nearexact[i] / 30) / 3.069
  
  # ##Exact method: dividing Smin within low-flow period by # of days into that period Smin occurs 
  # dayno_90 <- which.min(l90pd_df$Storage)
  # dayno_30 <- which.min(l30pd_df$Storage)
  # 
  # storage_data$Smin_L90_exact_perday[i] <- storage_data$Smin_L90_nearexact[i] / dayno_90
  # storage_data$Smin_L30_exact_perday[i] <- storage_data$Smin_L30_nearexact[i] / dayno_30
  
  #Method comparison: Does the Smin in the low flow year (approx method) occur within low-flow period? (near-exact)
  minstorage30yr <- min(l30yr_data$Storage)
  minstorage90yr <- min(l90yr_data$Storage)
  
  n_mins30 <- length(which(l30yr_data$Storage==minstorage30yr))
  n_mins90 <- length(which(l90yr_data$Storage==minstorage90yr))
  
  yearMinRow30 <- which.min(l30yr_data$Storage) 
  yearMinRow90 <- which.min(l90yr_data$Storage)

  if (n_mins30 > 1) {  # workaround for when a min value is repeated, ex. 100s across all storage vals 
    storage_data$min_in_pd30[i] <- TRUE
  } else {
    storage_data$min_in_pd30[i] <- between(yearMinRow30, rownum_start30, rownum_end30)
  }
  if (n_mins90 > 1) {
    storage_data$min_in_pd90[i] <- TRUE
  } else {
  storage_data$min_in_pd90[i] <- between(yearMinRow90, rownum_start90, rownum_end90)
  }
  
  #If not, how far outisde the low flow period?
  if (storage_data$min_in_pd30[i] == FALSE) {
    not_before <- rownum_start30 < yearMinRow30
    not_after <- rownum_end30 > yearMinRow30
    if (not_before == FALSE) {
      storage_data$outside_pd30[i] <- as.numeric(rownum_start30 - yearMinRow30)
    } else if (not_after == FALSE) {
      storage_data$outside_pd30[i] <- as.numeric(yearMinRow30 - rownum_end30)
    }
  }
  if (storage_data$min_in_pd90[i] == FALSE) {
    not_before <- rownum_start90 < yearMinRow90
    not_after <- rownum_end90 > yearMinRow90
    if (not_before == FALSE) {
      storage_data$outside_pd90[i] <- as.numeric(rownum_start90 - yearMinRow90)
    } else if (not_after == FALSE) {
      storage_data$outside_pd90[i] <- as.numeric(yearMinRow90 - rownum_end90) 
    }
  }
  
}



#Difference between approx and near-exact Smin in units of million gallons 
## approximate values will always be less than or equal to the near-exact values, so these differences SHOULD be >= 0
# storage_data$diff_L30_calc <- storage_data$Smin_L30_nearexact - storage_data$Smin_L30_approx_mg
# storage_data$diff_L90_calc <- storage_data$Smin_L90_nearexact - storage_data$Smin_L90_approx_mg
# 
# storage_data$diff_L30_vah <- storage_data$Smin_L30_nearexact - storage_data$SminL30mg_11_vah
# storage_data$diff_L90_vah <- storage_data$Smin_L90_nearexact - storage_data$SminL90mg_11_vah

#Percent difference
#storage_data <- fn_pct_diff(data = storage_data, column1 = "Smin_L30_nearexact", column2 = "SminL30mg_11", new_col = "pct_diff_L30", geom = FALSE)


#Neater dataframe:
# approx_vs_nearexact <- data.frame(propname = storage_data$propname,
#                        riverseg = storage_data$riverseg,
#                        Smin_L90_approx = storage_data$SminL90mg_11,
#                        Smin_L30_approx = storage_data$SminL30mg_11,
#                        Smin_L90_nearexact = storage_data$Smin_L90_nearexact,
#                        Smin_L30_nearexact = storage_data$Smin_L30_nearexact,
#                        min_in_pd90 = storage_data$min_in_pd90,
#                        min_in_pd30 = storage_data$min_in_pd30,
#                        days_outside_pd90 = storage_data$outside_pd90,
#                        days_outside_pd30 = storage_data$outside_pd30)




## Saving impoundment runfiles to save time 

# runid = 11
# 
# for (i in 1:nrow(storage_data)) {
# 
#    #Get runfile w/ timeseries data
#   pid <- storage_data$pid[i]
# 
#   token = ds$get_token(rest_pw) #needed for elid function
#   elid <- om_get_model_elementid(
#     base_url = site,
#     mid = storage_data$pid[i]
#   )
#   rm(token)
# 
#   dat <- fn_get_runfile(elid, runid , site= omsite,  cached = FALSE) #get timeseries data
#   dat <- zoo(dat, order.by = dat$timestamp) #make sure it's ordered correctly
# 
#   #save as a csv to local folder
#   write.zoo(dat, paste0(github_location,"/HARParchive/HARP-2023-Summer/impoundment_runfiles/runfile_imp_",storage_data$featureid[i],"_",runid,".csv"))
# 
# }





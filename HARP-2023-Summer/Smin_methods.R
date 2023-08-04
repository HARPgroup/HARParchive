#comparing different methods of calculating Smin for water availability calculations 

# Load Libraries
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

source(paste0(github_location,"/HARParchive/HARP-2023-Summer/fn_get_pd_min.R"),local = TRUE) #load Smin_CPL function, approx method

#get all impoundment features 
df_imp <- data.frame(
  'model_version' = c('vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0'),
  'runid' = c('runid_11', 'runid_400', 'runid_600', 'runid_13'),
  'metric' = c('usable_pct_p0','usable_pct_p0', 'usable_pct_p0', 'usable_pct_p0'),
  'runlabel' = c('Smin_pct_11', 'Smin_pct_perm', 'Smin_pct_prop', 'Smin_pct_800')
)
all_imp_data <- om_vahydro_metric_grid(
  metric = metric, runids = df_imp, bundle = 'all', ftype = "all",
  base_url = paste(site,'entity-model-prop-level-export',sep="/"),
  ds = ds
)

runid <- 11

for (i in 1:nrow(all_imp_data)) {

pid <- all_imp_data$pid[i]

token = ds$get_token(rest_pw) #needed for elid function
elid <- om_get_model_elementid(
  base_url = site,
  mid = all_imp_data$pid[i]
)
rm(token)

dat <- fn_get_runfile(elid, runid, site= omsite,  cached = FALSE) #get timeseries data
mode(dat) <- 'numeric'

#is the impoundment active? (imp_off = 0?)
cols <- names(dat)
  
if ("imp_off" %in% cols) {
  imp_off <- as.integer(median(dat$imp_off))
} else {
  # imp_off is NOT in the cols, so impoundment must be active
  imp_off = 0
}
  
#different names for storage and Qin values
names(dat)[names(dat) == 'impoundment_Storage'] <- 'Storage'
names(dat)[names(dat) == 'local_impoundment_Storage'] <- 'Storage'
if (!('Qin' %in% cols)) {
  names(dat)[names(dat) == 'impoundment_Qin'] <- 'Qin'
}

#find l30 and l90 years based on Qin
flows <- zoo(dat$Qin, order.by = index(dat));
loflows <- group2(flows)

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

#Approximate method: Smin within low-flow years:
all_imp_data$Smin_L90_approx[i] <- fn_get_pd_min(ts_data = dat, critical_pd_length = 90, 
                                                 start_date = l90_start, end_date = l90_end, colname = "Storage")
all_imp_data$Smin_L30_approx[i] <- fn_get_pd_min(ts_data = dat, critical_pd_length = 30, 
                                                 start_date = l30_start, end_date = l30_end, colname = "Storage")

#Exact method: Smin within the L30 and L90 periods:

#data for each l30 and l90 years
l30yr_flows <- window(flows, start = l30_start, end = l30_end)
l90yr_flows <- window(flows, start = l90_start, end = l90_end)

#zoo to data frame
l30yr_df <- as.data.frame(l30yr_flows)
l90yr_df <- as.data.frame(l90yr_flows)

l30yr_df <- l30yr_df %>% mutate(rollmean_30 = rollmean(l30yr_flows, k=30, fill=NA, align='left' ))
l90yr_df <- l90yr_df %>% mutate(rollmean_90 = rollmean(l90yr_flows, k=90, fill=NA, align='left'))

#start dates for low flow periods
l30pd_start <- as.Date(row.names(l30yr_df[which.min(l30yr_df$rollmean_30),]))
l90pd_start <- as.Date(row.names(l90yr_df[which.min(l90yr_df$rollmean_90),]))

#end dates for low flow periods
rownum_end90 <- which.min(l90yr_df$rollmean_90) + 90
rownum_end30 <- which.min(l30yr_df$rollmean_30) + 30

l30pd_end <- as.Date(row.names(l30yr_df[rownum_end30, ]))
l90pd_end <- as.Date(row.names(l90yr_df[rownum_end90, ]))

#flow data for the drought periods 
l30pd_flows <- window(dat, start = l30pd_start, end = l30pd_end)
l90pd_flows <- window(dat, start = l90pd_start, end = l90pd_end)

l30pd_df <- as.data.frame(l30pd_flows)
l90pd_df <- as.data.frame(l90pd_flows)

#Smin within the low flow periods
all_imp_data$Smin_L90_exact[i] <- min(l90pd_df$Storage)
all_imp_data$Smin_L30_exact[i] <- min(l30pd_df$Storage)

}

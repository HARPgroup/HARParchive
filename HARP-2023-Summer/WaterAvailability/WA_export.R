#----------------------------------------------
site <- "http://deq1.bse.vt.edu/d.dh"    #Specify the site of interest, either d.bet OR d.dh
#----------------------------------------------
# Load Libraries
basepath='/var/www/R';
source(paste(basepath,'config.R',sep='/'))
library(hydrotools)

# authenticate
ds <- RomDataSource$new(site, rest_uname)
ds$get_token(rest_pw)

## Abbreviated Form of WA Eqn:
## WA_cpl = Qdemand_cpl - MIF*Qbase_cpl + Smin_cpl/CPL
### Where CPL = Critical Period Length 

# Read Args
argst <- commandArgs(trailingOnly=T)
pid <- as.integer(argst[1])
elid <- as.integer(argst[2])
runid_dem <- as.integer(argst[3]) #demand scenario (ex. 11)
runid_base <- as.integer(argst[4]) #baseline scenario, default to 0 if none is provided (ex. 0 or NA)
CPL <- as.integer(argst[5]) #critical period length (days), default to 90
PoF <- as.integer(argst[6]) #minimum instream flow coefficient, default to 0.9

#For testing: Lake Pelham
# pid = 5714522 ; elid = 352006 ; runid_dem = 11 ; runid_base = 0 ; CPL <- 30 ; PoF <- 0.9

#Set defaults 
if (!length(argst) == 6) { #set defaults if not all arguments are provided 
  runid_base <- 0 
  CPL <- 90 #default to a 90-day critical period length 
  PoF <- 0.9 #default to 0.9 for percent of instream flow required 
  print("Not all required inputs provided, defaults will be used")
}

demand_scenario <- paste0('runid_', runid_dem)
baseline_scenario <- paste0('runid_', runid_base)

#Pull metrics for WA eqn: Qdemand, Qbase, Smin
df_metrics <- data.frame(
  'model_version' = c('vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0'),
  'runid' = c(demand_scenario, demand_scenario, baseline_scenario),
  'metric' = c(paste0('Smin_L', CPL, '_mg'), paste0('l', CPL, '_Qout'), paste0('l', CPL, '_Qout')),
  'runlabel' = c('Smin_mg','lCPL_Qout_dem', 'lCPL_Qout_base')
)
metrics_data <- om_vahydro_metric_grid(
  metric = metric, runids = df_metrics, bundle = 'all', ftype = "all",
  base_url = paste(site,'entity-model-prop-level-export',sep="/"),
  ds = ds
)

#Get object of interest using the given pid and elid 
obj <- metrics_data[metrics_data$pid == pid, ]

#Calculate Qavailable and WA
obj$Qout_mif <- PoF*obj$lCPL_Qout_base #min instream flow (cfs)
obj$Qavailable_cfs <- round((obj$lCPL_Qout_dem - obj$Qout_mif), digits = 3) #available flow (cfs): Qavailable = Qdemand - PoF*Qbaseline 
obj$WA_mgd = round((obj$Qavailable_cfs / 1.547) + (obj$Smin_mg / CPL), digits = 3)

#Get scenario 
sceninfo <- list(
  varkey = 'om_scenario',
  propname = demand_scenario,
  featureid = pid,
  entity_type = "dh_properties",
  bundle = "dh_properties"
)
scenprop <- RomProperty$new( ds, sceninfo, TRUE)

#Export metrics to VAhydro
vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, paste0('Qavailable_', CPL, '_mgd'), obj$Qavailable_cfs / 1.547, ds)
vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, paste0('WA_', CPL, '_mgd'), obj$WA_mgd, ds)

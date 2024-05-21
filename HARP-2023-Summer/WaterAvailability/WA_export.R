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
runid_dem <- as.integer(argst[3]) #demand scenario 
runid_base <- as.integer(argst[4]) #baseline scenario, default to 0 if none is provided 

#For testing: Lake Pelham
# pid = 5714522
# elid = 352006
# runid_dem = 11
# runid_base = 0

demand_scenario <- paste0("runid_", runid_dem)
baseline_scenario <- paste0("runid_", runid_base)

#Pull metrics for WA eqn: Qdemand, Qbase, Smin
df_metrics <- data.frame(
  'model_version' = c('vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0'),
  'runid' = c(demand_scenario, demand_scenario, demand_scenario, demand_scenario, baseline_scenario, baseline_scenario),
  'metric' = c('Smin_L30_mg', 'Smin_L90_mg', 'l30_Qout', 'l90_Qout', 'l30_Qout', 'l90_Qout'),
  'runlabel' = c('Smin_L30_mg','Smin_L90_mg', 'l30_Qout_dem', 'l90_Qout_dem', 'l30_Qout_base', 'l90_Qout_base')
)
metrics_data <- om_vahydro_metric_grid(
  metric = metric, runids = df_metrics, bundle = 'all', ftype = "all",
  base_url = paste(site,'entity-model-prop-level-export',sep="/"),
  ds = ds
)

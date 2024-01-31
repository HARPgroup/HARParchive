#Updating all impoundment models across a runid to include Smin 
# Load Libraries
library(hydrotools)
basepath='/var/www/R';
source(paste(basepath,'config.R',sep='/'))
# Authenticate
ds <- RomDataSource$new(site, rest_uname)
ds$get_token(rest_pw)

# Read args
argst <- commandArgs(trailingOnly=T)
runid <- as.integer(argst[1]) #numeric part of a runid (ex. 11)

runlabel <- paste0('runid_', runid)

df_imp <- data.frame(
  'model_version' = 'vahydro-1.0',
  'runid' = runlabel,
  'metric' = 'usable_pct_p0',
  'runlabel' = paste0('Smin_pct_', runid)
)
all_imp_data <- om_vahydro_metric_grid(
  metric = metric, runids = df_imp, bundle = 'all', ftype = "all",
  base_url = paste(site,'entity-model-prop-level-export',sep="/"),
  ds = ds
)

for (i in 1:length(all_imp_data)) {
  pid <- all_imp_data$pid[i]
  
  commandArgs <- function(...) c(pid,runid)
  source(paste0("~/HARParchive/HARP-2023-Summer/WaterAvailability/update_Smin.R")) #call summary script
  
}

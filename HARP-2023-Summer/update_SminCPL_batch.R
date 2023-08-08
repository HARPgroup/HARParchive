#batch script for updating Smin_CPL for impoundments & river elements 
# Load Libraries
library(stringr)
library(hydrotools)
basepath='/var/www/R';
source(paste(basepath,'config.R',sep='/'))
# Authenticate
ds <- RomDataSource$new(site, rest_uname)
ds$get_token(rest_pw)
# Read args
argst <- commandArgs(trailingOnly=T)
runid <- as.integer(argst[1]) #number-only part of a runid (ex. 11)
#runid <- 11 #for testing

# Get all impoundment features 
df_imp <- data.frame(
  'model_version' = 'vahydro-1.0',
  'runid' = paste0('runid_',runid),
  'metric' = 'usable_pct_p0',
  'runlabel' = paste0('Smin_pct_',runid)
)
all_imp_data <- om_vahydro_metric_grid(
  metric = metric, runids = df_imp, bundle = 'all', ftype = "all",
  base_url = paste(site,'entity-model-prop-level-export',sep="/"),
  ds = ds
)

# Get object_class for each impoundment to determine which script to run
token = ds$get_token(rest_pw) #needed for elid function in loop
for (i in 1:nrow(all_imp_data)) { 
  pid <- all_imp_data$pid[i]
  elid <- om_get_model_elementid(base_url = site, mid = pid) #get om_element_connection value for elid
  om_model <- RomProperty$new(ds,list(pid=pid),TRUE)
  object_class <- RomProperty$new(ds,list(featureid = om_model$pid, propname = 'object_class'),TRUE)$propcode

  #Run 1 of 3 om post-processing scripts based on type of impoundment
  if (object_class=="waterSupplyModelNode") {
    commandArgs <- function(...) c(pid,elid,runid)
    source(paste0(github_location,"/HARParchive/HARP-2023-Summer/waterSupplyModelNode.R")) #riverseg imp
  } else if (object_class=="waterSupplyElement") {
    commandArgs <- function(...) c(pid,elid,runid)
    source(paste0(github_location,"/HARParchive/HARP-2023-Summer/waterSupplyElement.R")) #facility imp
  } else if (object_class=="hydroImpoundment") {
    commandArgs <- function(...) c(pid,elid,runid)
    source(paste0(github_location,"/HARParchive/HARP-2023-Summer/hydroImpoundment.R")) #standalone imp
  }
}
rm(token) #security
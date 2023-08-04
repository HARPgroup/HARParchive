#batch script for updating Smin_CPL for impoundments & river elements 

# Load Libraries
library(stringr)
library(hydrotools)
basepath='/var/www/R';
source(paste(basepath,'config.R',sep='/'))
# authenticate
ds <- RomDataSource$new(site, rest_uname)
ds$get_token(rest_pw)

#read args
argst <- commandArgs(trailingOnly=T)
runid <- as.integer(argst[1]) #number-only part of a runid (ex. 11)
#runid = 11

#get all impoundment features 
runlab <- paste0('runid_',runid)
df_imp <- data.frame(
  'model_version' = c('vahydro-1.0'),
  'runid' = runlab,
  'metric' = c('usable_pct_p0'),
  'runlabel' = c(paste0('Smin_pct_',runid))
)

all_imp_data <- om_vahydro_metric_grid(
  metric = metric, runids = df_imp, bundle = 'all', ftype = "all",
  base_url = paste(site,'entity-model-prop-level-export',sep="/"),
  ds = ds
)

token = ds$get_token(rest_pw) #needed for elid function in loop
for (i in 1:nrow(all_imp_data)) { 
  
  #get om_element_connection value for elid 
  elid <- om_get_model_elementid(
    base_url = site,
    mid = all_imp_data$pid[i]
  )

  pid <- all_imp_data$pid[i]

  #Run 1 of 3 om post-processing scripts based on type of impoundment
  #hydrocode is distinct for riverseg and facil impoundments 
  if ("vahydrosw_wshed" %in% all_imp_data[i, "hydrocode"]) {
    commandArgs <- function(...) c(pid,elid,runid)
    source(paste0(github_location,"/HARParchive/HARP-2023-Summer/waterSupplyModelNode.R"))
  } else if ("vwuds" %in% all_imp_data[i, "hydrocode"]) {
    commandArgs <- function(...) c(pid,elid,runid)
    source(paste0(github_location,"/HARParchive/HARP-2023-Summer/waterSupplyElement.R"))
  } else {
    commandArgs <- function(...) c(pid,elid,runid)
    source(paste0(github_location,"/HARParchive/HARP-2023-Summer/hydroImpoundment.R"))
  }
  
}

rm(token) #security

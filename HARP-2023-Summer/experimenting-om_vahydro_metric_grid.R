library(hydrotools)
basepath='/var/www/R'
source('/var/www/R/config.R') 
ds <- RomDataSource$new(site, rest_uname)
ds$get_token(rest_pw)

runid_list <- c("runid_11","runid_11")
model_version <- c("vahydro-1.0","usgs-1.0")
#metric_mod <- c("l90_Qout","90 Day Min Low Flow")
metric_mod <- c("7q10")
runlabel <- c("Model_7q10","Gage_7q10")

df <- data.frame(runid=runid_list, model_version, metric=metric_mod, runlabel) 

model <- om_vahydro_metric_grid(
  metric=FALSE, runids=df, featureid='all', 
  entity_type='dh_feature', bundle='watershed',
  ftype='all', model_version=model_version,
  base_url=paste(site,"/entity-model-prop-level-export",sep=''), #http://deq1.bse.vt.edu/d.dh
  ds=ds
)
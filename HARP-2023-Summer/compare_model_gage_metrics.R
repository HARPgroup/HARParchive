library(hydrotools)
basepath='/var/www/R'
source('/var/www/R/config.R') 
ds <- RomDataSource$new(site, rest_uname)
ds$get_token(rest_pw)

options(scipen = 999) #disable scientific notation

#runid_list <- c("runid_11","runid_11")
#model_version <- c("vahydro-1.0","usgs-1.0")
#metric_mod <- c("l90_Qout","90 Day Min Low Flow")
#metric_mod <- c("7q10")
#runlabel <- c("Model_7q10","Gage_7q10")
#df <- data.frame(runid=runid_list, model_version, metric=metric_mod, runlabel) 

df <- data.frame(
  'model_version' = c('vahydro-1.0', 'usgs-1.0', 'vahydro-1.0', 'usgs-1.0', 'vahydro-1.0', 'usgs-1.0'),
  'runid' = c('runid_11', 'runid_11', 'runid_11', 'runid_11', 'runid_11', 'runid_11'),
  'metric' = c('7q10','7q10', 'l30_Qout', 'l30_Qout', 'l90_Qout', 'l90_Qout'),
  'runlabel' = c('Model_7q10','Gage_7q10', 'Model_L30','Gage_L30', 'Model_L90','Gage_L90')
)

#pull metrics 
metric_data <- om_vahydro_metric_grid(
  metric=FALSE, runids=df, featureid='all', 
  entity_type='dh_feature', bundle='watershed',
  ftype='all', model_version=model_version,
  base_url=paste(site,"/entity-model-prop-level-export",sep=''), #http://deq1.bse.vt.edu/d.dh
  ds=ds
)

#find % difference between gage and model metrics 
metric_data$pct_diff_7q10 <- (abs(metric_data$Model_7q10 -  metric_data$Gage_7q10) / ((metric_data$Model_7q10 + metric_data$Gage_7q10)/2))*100
metric_data$pct_diff_L30 <- (abs(metric_data$Model_L30 -  metric_data$Gage_L30) / ((metric_data$Model_L30 + metric_data$Gage_L30)/2))*100
metric_data$pct_diff_L90 <- (abs(metric_data$Model_L90 -  metric_data$Gage_L90) / ((metric_data$Model_L90 + metric_data$Gage_L90)/2))*100
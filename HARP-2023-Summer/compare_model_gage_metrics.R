library(hydrotools)
basepath='/var/www/R'
source('/var/www/R/config.R') 
ds <- RomDataSource$new(site, rest_uname)
ds$get_token(rest_pw)

options(scipen = 999) #disable scientific notation

#note: this should work for any future runids, model versions, and metrics AS LONG AS the gage model version name contains 'usgs'
runid_list <- c("runid_11")
model_versions <- c("vahydro-1.0","usgs-1.0")
metrics <- c("7q10","l30_Qout","l90_Qout")

mv_names <- gsub("[.]", "", model_versions) #om_vahydro_metric_grid didn't handle periods in 'runlabel' well
for(r in 1:length(runid_list)){
  for(i in 1:length(metrics)){ #generate runlabels for Model/Gage per each metric
    for(v in 1:length(model_versions)){
      if(v==1 & i==1 & r==1){
        if(length(grep('usgs', model_versions[v], invert=TRUE, ignore.case=TRUE)) == 1){ #model data
          runlabel <- paste0("Model_",metrics[i],"_",runid_list[r],"_", mv_names[v])
        }
        if(length(grep('usgs', model_versions[v], ignore.case=TRUE)) == 1){ #gage data
          runlabel <- paste0("Gage_",metrics[i],"_",runid_list[r],"_", mv_names[v])
        }
      }
      else{
        if(length(grep('usgs', model_versions[v], invert=TRUE, ignore.case=TRUE)) == 1){ #model data
          runlabel <- c(runlabel, paste0("Model_",metrics[i],"_",runid_list[r],"_", mv_names[v]))
        }
        if(length(grep('usgs', model_versions[v], ignore.case=TRUE)) == 1){ #gage data
          runlabel <- c(runlabel, paste0("Gage_",metrics[i],"_",runid_list[r],"_", mv_names[v]))
        }
      }
    }
    if(i==1){
      metric <- rep(metrics[i], length(model_versions))
    }else{
      metric <- c(metric, (rep(metrics[i], length(model_versions))) )
    }
  }
  if(r==1){
    rep_runid <- length(runlabel)
    runid <- rep(runid_list[r], rep_runid)
  }else{
    runid <- c(runid, rep(runid_list[r], rep_runid) )
  }
}

#create df to pass into om_vahydro_metric_grid() ; this is robust for the future if more than 1 runid
df <- data.frame(
  'model_version' = rep(model_versions, length(metrics)),
  'runid' = runid,
  'metric' = metric,
  'runlabel' = runlabel
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

for(r in 1:length(runid_list)){
  for(m in 1:length(metrics)){
    cols <- grep(paste0(metrics[m],"_",runid_list[r]), colnames(metric_data))
    model <- grep("model", colnames(metric_data[cols]))
  }
}




metric_data$pct_diff_7q10 <- (abs(metric_data$Model_7q10 -  metric_data$Gage_7q10) / ((metric_data$Model_7q10 + metric_data$Gage_7q10)/2))*100
metric_data$pct_diff_L30 <- (abs(metric_data$Model_L30 -  metric_data$Gage_L30) / ((metric_data$Model_L30 + metric_data$Gage_L30)/2))*100
metric_data$pct_diff_L90 <- (abs(metric_data$Model_L90 -  metric_data$Gage_L90) / ((metric_data$Model_L90 + metric_data$Gage_L90)/2))*100
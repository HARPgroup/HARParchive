#Water Availability methods & calculations 

# Example equation: For a L90 scenario:
# Qdemand = l90_Qout(cfs)
# Qbaseline (approximation when baseline scenario unavailable) = l90_Qout + wd_mgd - ps_mgd 
# WA = Qdem - 0.9*Qbase + Smin/CPL = l90_Qout - 0.9*(l90_Qout + wd_mgd - ps_mgd) + Smin_perday (final units should be mgd)

## Setup
library(hydrotools)
options(scipen = 999) #disable scientific notation

basepath='/var/www/R';
source(paste(basepath,'config.R',sep='/'))
# authenticate
ds <- RomDataSource$new(site, rest_uname)
ds$get_token(rest_pw)
##

## This will ulttimately be user-controlled:
runid <- 11
##
runlabel <- paste0('runid_', runid)

#Pulling in Smin metrics (approx method):
df_storage <- data.frame(
  'model_version' = c('vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0'),
  'runid' = c('runid_11', 'runid_11', 'runid_13', 'runid_13'),
  'metric' = c('Smin_L30_mg', 'Smin_L90_mg','Smin_L30_mg', 'Smin_L90_mg'),
  'runlabel' = c('SminL30mg_11', 'SminL90mg_11','SminL30mg_13', 'SminL90mg_13')
)
storage_data <- om_vahydro_metric_grid(
  metric = metric, runids = df_storage, bundle = 'all', ftype = "all",
  base_url = paste(site,'entity-model-prop-level-export',sep="/"),
  ds = ds
)

#Convert storage from mg to mgd 
##This should ultimately be user-controlled and not hard-coded
storage_data$SminL30mgd_11 <- storage_data$SminL30mg_11 / 30 #dividing by # of days in low flow period 
storage_data$SminL30mgd_13 <- storage_data$SminL30mg_13 / 30
storage_data$SminL90mgd_11 <- storage_data$SminL90mg_11 / 30
storage_data$SminL90mgd_13 <- storage_data$SminL90mg_13 / 30

#Getting other metrics used in WA equation
df_metrics <- data.frame(
  'model_version' = c('vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0','vahydro-1.0','vahydro-1.0'),
  'runid' = c(runlabel, runlabel, runlabel, runlabel, runlabel, runlabel),
  'metric' = c('l90_Qout', 'l30_Qout', 'l90_year', 'l30_year', 'wd_cumulative_mgd','ps_cumulative_mgd'),
  'runlabel' = c('l90_Qout', 'l30_Qout', 'l90_year', 'l30_year','wd_cumulative_mgd', 'ps_cumulative_mgd')
)
metric_data <- om_vahydro_metric_grid(
  metric = metric, runids = df_metrics, bundle = 'all', ftype = "all",
  base_url = paste(site,'entity-model-prop-level-export',sep="/"),
  ds = ds
)

#convert cfs to mgd 
metric_data$l30_Qout_mgd = metric_data$l30_Qout / 1.547
metric_data$l90_Qout_mgd = metric_data$l90_Qout / 1.547

#again, hard-coded runids (temporary)
metric_data <- sqldf('SELECT a.*, b.SminL30mgd_11, b.SminL90mgd_11
                   FROM metric_data as a
                   LEFT OUTER JOIN storage_data as b
                   ON (a.riverseg = b.riverseg)') #joining rseg data to storage data


#Linking riversegs with upstream impoundments 
metric_data$SminL30_total <- NA #empty rows to distinguish upstream storage
metric_data$SminL90_total <- NA 

for (i in 1:nrow(metric_data)) {
  ups_imp <- fn_extract_basin(storage_data, metric_data$riverseg[i]) #find if any upstream segments have an impoundment within
  if (nrow(ups_imp) > 0) {
    ups_impsegs <- data.frame(rivseg = ups_imp$riverseg)
    ups_df <- sqldf("select a.*
                    from storage_data as a
                    where riverseg in (select rivseg from ups_impsegs)") #get the upstream impoundments 
    metric_data$SminL30_total[i] <- sum(ups_df$SminL30mgd_11) #sum storage available locally and upstream (S in afd)
    metric_data$SminL90_total[i] <- sum(ups_df$SminL90mgd_11)
    
  }
}

colnames(metric_data)[which(names(metric_data) == "SminL90mgd_11")] <- "SminL30_local"
colnames(metric_data)[which(names(metric_data) == "SminL90mgd_11")] <- "SminL90_local"

metric_data[is.na(metric_data)] <- 0 #replace NA storage values with 0

#solve for WA
metric_data$WA_L30_mgd = metric_data$l30_Qout_mgd - 0.9*(metric_data$l30_Qout_mgd + metric_data$wd_cumulative_mgd - metric_data$ps_cumulative_mgd) + metric_data$SminL30_total 
metric_data$WA_L90_mgd = metric_data$l90_Qout_mgd - 0.9*(metric_data$l90_Qout_mgd + metric_data$wd_cumulative_mgd - metric_data$ps_cumulative_mgd) + metric_data$SminL90_total 

#WA as a % of flow 
metric_data$pct_WA30 = (metric_data$WA_L30_mgd / metric_data$l30_Qout_mgd)*100
metric_data$pct_WA90 = (metric_data$WA_L90_mgd / metric_data$l90_Qout_mgd)*100




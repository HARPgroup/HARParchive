## Water Availability methods & calculations 

# Example equation: For a L90 scenario:
# Qdemand = l90_Qout(cfs)
# Qbaseline: either l90_Qout for runid_0, or when baseline scenario (runid_0) unavailable = l90_Qout + wd_mgd - ps_mgd 
# WA = Qdem - (Min In stream Flow Coeff)*Qbase + Smin/CPL = l90_Qout - 0.9*(l90_Qout + wd_mgd - ps_mgd) + Smin_perday (final units should be mgd)
#note: 0.9 is assumed as the Minimum In stream Flow Coefficient for this script currently

## Next steps:
# Quantify the difference between actual baseline (runid_0) and approximated baseline (w/ cumulative vars)
# Add inputs for total user-control: runid, Instream Flow Coefficient
# Present negative WAs as a volume for storage needed 

## Setup
library(hydrotools)
library(sqldf)
options(scipen = 999) #disable scientific notation

basepath='/var/www/R';
source(paste(basepath,'config.R',sep='/'))
# authenticate
ds <- RomDataSource$new(site, rest_uname)
ds$get_token(rest_pw)

source(paste0(github_location,"/HARParchive/HARP-2023-Summer/fn_pct_diff.R"),local = TRUE) #load % difference function 

## Inputs
runid <- 11 #currently hard-coded in many places 
# mifc <- 0.9 #minimum instream flow coefficient #currently hard-coded throughout

runlabel <- paste0('runid_', runid)

###--- Pull Data From VAhydro ---###

#note: a single om_vahydro_metric_grid() would be sufficient and more concise for 
# getting all needed metrics in the future as long as the input df is formatted correctly 

#Pulling in Smin metrics (exported using fn_get_pd_min(), which uses the Smin approx. method):
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

#Pulling baseline scenario low-flow metrics (runid_0)
df_baseline <- data.frame(
  'model_version' = c('vahydro-1.0', 'vahydro-1.0'),
  'runid' = c('runid_0', 'runid_0'),
  'metric' = c('l90_Qout', 'l30_Qout'),
  'runlabel' = c('l90_Qout_base', 'l30_Qout_base')
)
baseline_data <- om_vahydro_metric_grid(
  metric = metric, runids = df_baseline, bundle = 'all', ftype = "all",
  base_url = paste(site,'entity-model-prop-level-export',sep="/"),
  ds = ds
)

#Pulling demand scenario metrics used in WA equation
df_metrics <- data.frame(
  'model_version' = c('vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0'),
  'runid' = c(runlabel, runlabel, runlabel, runlabel),
  'metric' = c('l90_Qout', 'l30_Qout', 'wd_cumulative_mgd','ps_cumulative_mgd'),
  'runlabel' = c('l90_Qout_dem', 'l30_Qout_dem', 'wd_cumulative_mgd', 'ps_cumulative_mgd')
)
metric_data <- om_vahydro_metric_grid(
  metric = metric, runids = df_metrics, bundle = 'all', ftype = "all",
  base_url = paste(site,'entity-model-prop-level-export',sep="/"),
  ds = ds
)


###--- Unit Conversions  ---###

#Convert storage from mg to mgd 
storage_data$SminL30mgd_11 <- storage_data$SminL30mg_11 / 30 #dividing by # of days in low flow period 
storage_data$SminL30mgd_13 <- storage_data$SminL30mg_13 / 30
storage_data$SminL90mgd_11 <- storage_data$SminL90mg_11 / 30
storage_data$SminL90mgd_13 <- storage_data$SminL90mg_13 / 30

#Convert cfs to mgd 
#demand scenario
metric_data$l30_Qout_dem_mgd = metric_data$l30_Qout_dem / 1.547
metric_data$l90_Qout_dem_mgd = metric_data$l90_Qout_dem / 1.547

#baseline scenario
baseline_data$l30_Qout_base_mgd = baseline_data$l30_Qout_base / 1.547
baseline_data$l90_Qout_base_mgd = baseline_data$l90_Qout_base / 1.547


###--- Join Rseg Metric Data with Storage Data  ---###

#Join storage data onto demand scenario metric data 
metric_data <- sqldf('SELECT a.*, b.SminL30mgd_11, b.SminL90mgd_11
                   FROM metric_data as a
                   LEFT OUTER JOIN storage_data as b
                   ON (a.riverseg = b.riverseg)') 

#Join baseline scenario data onto demand scenario & storage data 
metric_data <- sqldf('SELECT a.*, b.l30_Qout_base_mgd, b.l90_Qout_base_mgd
                   FROM metric_data as a
                   LEFT OUTER JOIN baseline_data as b
                   ON (a.riverseg = b.riverseg)') 


###--- Linking Riversegs with Upstream Impoundments  ---###

#Empty columns that will hold total storage
metric_data$SminL30_total <- NA 
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

#Rename original Smin metrics from VAhydro as local storage
colnames(metric_data)[which(names(metric_data) == "SminL90mgd_11")] <- "SminL30_local"
colnames(metric_data)[which(names(metric_data) == "SminL90mgd_11")] <- "SminL90_local"

metric_data[is.na(metric_data)] <- 0 #replace NA storage values with 0


###--- Solving for Water Availability ---###

#Empty columns to hold WA values
metric_data$WA_L30_mgd <- NA 
metric_data$WA_L90_mgd <- NA 

for (i in 1:nrow(metric_data)) {
  if (metric_data$l30_Qout_base_mgd[i] != 0) { #if the baseline data exists (is not 0)
    metric_data$WA_L30_mgd[i] = metric_data$l30_Qout_dem_mgd[i] - 
      0.9*metric_data$l30_Qout_base_mgd[i] + 
      metric_data$SminL30_total[i] 
    
    metric_data$WA_L90_mgd[i] = metric_data$l90_Qout_dem_mgd[i] - 
      0.9*metric_data$l90_Qout_base_mgd[i] + 
      metric_data$SminL90_total[i] 
    
  } else { #if the baseline scenario does not exist, use approximation w/ cumulative metrics 
    metric_data$WA_L30_mgd[i] = metric_data$l30_Qout_dem_mgd[i] - 
      0.9*(metric_data$l30_Qout_dem_mgd[i] + metric_data$wd_cumulative_mgd[i] - metric_data$ps_cumulative_mgd[i]) + 
      metric_data$SminL30_total[i] 
    
    metric_data$WA_L90_mgd[i] = metric_data$l90_Qout_dem_mgd[i] - 
      0.9*(metric_data$l90_Qout_dem_mgd[i] + metric_data$wd_cumulative_mgd[i] - metric_data$ps_cumulative_mgd[i]) + 
      metric_data$SminL90_total[i] 
  }
}

#WA as a % of demand scenario flow 
metric_data$pct_WA30 = (metric_data$WA_L30_mgd / metric_data$l30_Qout_dem_mgd)*100
metric_data$pct_WA90 = (metric_data$WA_L90_mgd / metric_data$l90_Qout_dem_mgd)*100


###### Comparing actual and approximated baselines
#runid_0 has actual baseline values 
#baseline approximation: Qout + wd_mgd - ps_mgd

compare_base <- data.frame(propname = metric_data$propname,
                           riverseg = metric_data$riverseg,
                           wd_cumulative_mgd = metric_data$wd_cumulative_mgd,
                           ps_cumulative_mgd = metric_data$ps_cumulative_mgd,
                           l30_Qout_dem_mgd = metric_data$l30_Qout_dem_mgd,
                           l90_Qout_dem_mgd = metric_data$l90_Qout_dem_mgd,
                           l30_Qout_base_mgd = metric_data$l30_Qout_base_mgd,
                           l90_Qout_base_mgd = metric_data$l90_Qout_base_mgd)

compare_base <- sqldf("SELECT a.* 
                      FROM compare_base as a
                      WHERE l30_Qout_base_mgd 
                      IS NOT 0")

#Solve for approx. baseline L30/L90 
compare_base$l30_Qout_base_apx_mgd <- compare_base$l30_Qout_dem_mgd + compare_base$wd_cumulative_mgd - compare_base$ps_cumulative_mgd
compare_base$l90_Qout_base_apx_mgd <- compare_base$l90_Qout_dem_mgd + compare_base$wd_cumulative_mgd - compare_base$ps_cumulative_mgd

#Difference between actual/approx baseline 
compare_base$diff_L30_base <- compare_base$l30_Qout_base_mgd - compare_base$l30_Qout_base_apx_mgd
compare_base$diff_L90_base <- compare_base$l90_Qout_base_mgd - compare_base$l90_Qout_base_apx_mgd

#Pct diff between actual/approx baseline 
compare_base <- fn_pct_diff(data = compare_base, 
                            column1 = 'l30_Qout_base_mgd', 
                            column2 = 'l30_Qout_base_apx_mgd',
                            new_col = 'pct_diff_l30_base', 
                            geom = FALSE)

compare_base <- fn_pct_diff(data = compare_base, 
                            column1 = 'l90_Qout_base_mgd', 
                            column2 = 'l90_Qout_base_apx_mgd',
                            new_col = 'pct_diff_l90_base', 
                            geom = FALSE)


######



## Could be useful:
# If WA < 0, multiply the WA (mgd) value by 30 or 90 which will give storage (in mg) needed for either a 30 or 90-day drought


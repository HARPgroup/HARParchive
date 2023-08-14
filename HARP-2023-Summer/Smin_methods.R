#comparing different methods of calculating Smin for water availability calculations 

# Load Libraries
library(stringr)
library(hydrotools)
library(zoo)
library(IHA)
library(dplyr)

basepath='/var/www/R';
source(paste(basepath,'config.R',sep='/'))
# authenticate
ds <- RomDataSource$new(site, rest_uname)
ds$get_token(rest_pw)

source(paste0(github_location,"/HARParchive/HARP-2023-Summer/fn_get_pd_min.R"),local = TRUE) #load Smin_CPL function, approx method
options(scipen = 999)

#get all impoundment features 
df_imp <- data.frame(
  'model_version' = c('vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0'),
  'runid' = c('runid_400', 'runid_400', 'runid_400', 'runid_400'),
  'metric' = c('usable_pct_p0','usable_pct_p0', 'usable_pct_p0', 'usable_pct_p0'),
  'runlabel' = c('Smin_pct_11', 'Smin_pct_perm', 'Smin_pct_prop', 'Smin_pct_800')
)
all_imp_data <- om_vahydro_metric_grid(
  metric = metric, runids = df_imp, bundle = 'all', ftype = "all",
  base_url = paste(site,'entity-model-prop-level-export',sep="/"),
  ds = ds
)

runid <- 400
runlabel <- paste0('runid_', runid)

all_imp_data$outside_pd30 <- NA
all_imp_data$outside_pd90 <- NA

for (i in 1:nrow(all_imp_data)) {
  
  pid <- all_imp_data$pid[i]
  
  token = ds$get_token(rest_pw) #needed for elid function
  elid <- om_get_model_elementid(
    base_url = site,
    mid = all_imp_data$pid[i]
  )
  rm(token)
  
  dat <- fn_get_runfile(elid, runid, site= omsite,  cached = FALSE) #get timeseries data
  mode(dat) <- 'numeric'
  
  #is the impoundment active? (imp_off = 0?)
  cols <- names(dat)
  
  if ("imp_off" %in% cols) {
    imp_off <- as.integer(median(dat$imp_off))
  } else {
    # imp_off is NOT in the cols, so impoundment must be active
    imp_off = 0
  }
  
  #different names for storage and Qin values
  names(dat)[names(dat) == 'impoundment_Storage'] <- 'Storage'
  names(dat)[names(dat) == 'local_impoundment_Storage'] <- 'Storage'
  if (!('Qin' %in% cols)) {
    names(dat)[names(dat) == 'impoundment_Qin'] <- 'Qin'
  }
  
  #find l30 and l90 years based on Qin
  flows <- zoo(dat$Qin, order.by = index(dat));
  loflows <- group2(flows, year = 'water') #gives an error when using calendar year
  
  l90 <- loflows["90 Day Min"];
  ndx = which.min(as.numeric(l90[,"90 Day Min"]));
  l90_Qout = round(loflows[ndx,]$"90 Day Min",6);
  l90_year = loflows[ndx,]$"year";
  l90_start = as.Date(paste0(l90_year,"-01-01"))
  l90_end = as.Date(paste0(l90_year,"-12-31"))
  datpd_90 <- window(
    dat,
    start = l90_start,
    end = l90_end
  )
  
  l30 <- loflows["30 Day Min"];
  ndx = which.min(as.numeric(l30[,"30 Day Min"]));
  l30_Qout = round(loflows[ndx,]$"30 Day Min",6);
  l30_year = loflows[ndx,]$"year";
  l30_start = as.Date(paste0(l30_year,"-01-01"))
  l30_end = as.Date(paste0(l30_year,"-12-31"))
  datpd_30 <- window(
    dat,
    start = l30_start,
    end = l30_end
  )
  
  ##Approximate method: Smin within low-flow years:
  Smin_L90_approx <- fn_get_pd_min(ts_data = dat, critical_pd_length = 90, 
                                   start_date = l90_start, end_date = l90_end, colname = "Storage")
  Smin_L30_approx <- fn_get_pd_min(ts_data = dat, critical_pd_length = 30, 
                                   start_date = l30_start, end_date = l30_end, colname = "Storage")
  
  all_imp_data$Smin_L90_approx_perday[i] <- Smin_L90_approx / 90
  all_imp_data$Smin_L30_approx_perday[i] <- Smin_L30_approx / 30
  
  ##Near-exact method: Smin within the L30 and L90 periods:
  
  #data for each l30 and l90 years
  l30yr_flows <- window(flows, start = l30_start, end = l30_end)
  l90yr_flows <- window(flows, start = l90_start, end = l90_end)
  
  #zoo to data frame
  l30yr_df <- as.data.frame(l30yr_flows)
  l90yr_df <- as.data.frame(l90yr_flows)
  
  yearMinRow30 <- which.min(l30yr_df$l30yr_flows)
  yearMinRow90 <- which.min(l90yr_df$l90yr_flows)
  
  l30yr_df <- l30yr_df %>% mutate(rollmean_30 = rollmean(l30yr_flows, k=30, fill=NA, align='left' ))
  l90yr_df <- l90yr_df %>% mutate(rollmean_90 = rollmean(l90yr_flows, k=90, fill=NA, align='left'))
  
  #start dates for low flow periods
  rownum_start90 <- which.min(l90yr_df$rollmean_90) 
  rownum_start30 <- which.min(l30yr_df$rollmean_30)
  
  l30pd_start <- as.Date(row.names(l30yr_df[which.min(l30yr_df$rollmean_30),]))
  l90pd_start <- as.Date(row.names(l90yr_df[which.min(l90yr_df$rollmean_90),]))
  
  #end dates for low flow periods
  rownum_end90 <- which.min(l90yr_df$rollmean_90) + 90
  rownum_end30 <- which.min(l30yr_df$rollmean_30) + 30
  
  l30pd_end <- as.Date(row.names(l30yr_df[rownum_end30, ]))
  l90pd_end <- as.Date(row.names(l90yr_df[rownum_end90, ]))
  
  #flow data for the drought periods 
  l30pd_flows <- window(dat, start = l30pd_start, end = l30pd_end)
  l90pd_flows <- window(dat, start = l90pd_start, end = l90pd_end)
  
  l30pd_df <- as.data.frame(l30pd_flows)
  l90pd_df <- as.data.frame(l90pd_flows)
  
  #Smin within the low flow periods
  Smin_L90_nearexact <- min(l90pd_df$Storage)
  Smin_L30_nearexact <- min(l30pd_df$Storage)
  
  all_imp_data$Smin_L90_nearexact_perday[i] <- Smin_L90_nearexact / 90
  all_imp_data$Smin_L30_nearexact_perday[i] <- Smin_L30_nearexact / 30
  
  ##Exact method: dividing Smin within low-flow period by # of days into that period Smin occurs 
  dayno_90 <- which.min(l90pd_df$Storage)
  dayno_30 <- which.min(l30pd_df$Storage)
  
  all_imp_data$Smin_L90_exact_perday[i] <- Smin_L90_nearexact / dayno_90
  all_imp_data$Smin_L30_exact_perday[i] <- Smin_L30_nearexact / dayno_30
  
  #Does the Smin in the low flow year (approx method) occur within low-flow period? (near-exact)
  all_imp_data$min_in_pd30[i] <- between(yearMinRow30, rownum_start30, rownum_end30)
  all_imp_data$min_in_pd90[i] <- between(yearMinRow90, rownum_start90, rownum_end90)
  
  #If not, how far outisde the low flow period?
  if (all_imp_data$min_in_pd30[i] == FALSE) {
    not_before <- rownum_start30 < yearMinRow30
    not_after <- rownum_end30 > yearMinRow30
    if (not_before == FALSE) {
      all_imp_data$outside_pd30[i] <- as.numeric(rownum_start30 - yearMinRow30)
    } else if (not_after == FALSE) {
      all_imp_data$outside_pd30[i] <- as.numeric(yearMinRow30 - rownum_end30)
    }
  }
  if (all_imp_data$min_in_pd90[i] == FALSE) {
    not_before <- rownum_start90 < yearMinRow90
    not_after <- rownum_end90 > yearMinRow90
    if (not_before == FALSE) {
      all_imp_data$outside_pd90[i] <- as.numeric(rownum_start90 - yearMinRow90)
    } else if (not_after == FALSE) {
      all_imp_data$outside_pd90[i] <- as.numeric(yearMinRow90 - rownum_end90) 
    }
  }
  
}

approx_vs_nearexact <- data.frame(propname = all_imp_data$propname,
                       riverseg = all_imp_data$riverseg,
                       Smin_L90_approx_perday = all_imp_data$Smin_L90_approx_perday,
                       Smin_L30_approx_perday = all_imp_data$Smin_L30_approx_perday,
                       Smin_L90_nearexact_perday = all_imp_data$Smin_L90_nearexact_perday,
                       Smin_L30_nearexact_perday = all_imp_data$Smin_L30_nearexact_perday,
                       min_in_pd30 = all_imp_data$min_in_pd30,
                       min_in_pd90 = all_imp_data$min_in_pd90,
                       outside_pd30 = all_imp_data$outside_pd30,
                       outside_pd90 = all_imp_data$outside_pd90)

##Getting other variables in the WA equation 
#For a L90 scenario:
#Qdem = l90_Qout(cfs)
#Qbase = l90_Qout + wd_mgd - ps_mgd 
#WA = Qdem - 0.9*Qbase + Smin/CPL = l90_Qout - 0.9*(l90_Qout + wd_mgd - ps_mgd) + Smin_perday (final units should be mgd)

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

metric_data <- sqldf('SELECT a.*, b.Smin_L30_approx_perday, b.Smin_L90_approx_perday
                   FROM metric_data as a
                   LEFT OUTER JOIN all_imp_data as b
                   ON (a.riverseg = b.riverseg)')

#replace NA storage values with 0
metric_data[is.na(metric_data)] <- 0

names(metric_data)[names(metric_data) == 'Smin_L30_approx_perday'] <- 'SminL30afd_local' #storage in units of afd
names(metric_data)[names(metric_data) == 'Smin_L90_approx_perday'] <- 'SminL90afd_local'

#We want to join impoundment data and riverseg data before using fn_extract_basin

#Linking riversegs with upstream impoundments 
for (i in 1:nrow(metric_data)) {
  ups_imp <- fn_extract_basin(data, metric_data$riverseg[i]) #find if any upstream segments have an impoundment within
  if (nrow(ups_imp) > 0) {
    ups_impsegs <- data.frame(rivseg = ups_imp$riverseg)
    ups_df <- sqldf("select a.*
                    from all_imp_data as a
                    where riverseg in (select rivseg from ups_impsegs)") #get the upstream impoundments 
    data$SminL30afd_upstream[i] <- sum(ups_df$Smin_L30_approx_perday) #sum storage available upstream (S in afd)
    data$SminL90afd_upstream[i] <- sum(ups_df$Smin_L90_approx_perday)
    
    #We want to designate whether storage is local, meaning within the same riverseg, or upstream 
    
  }
}

#convert afd to mgd 
data$SminL30_mgd = data$SminL30_afd / 3.069
data$SminL90_mgd = data$SminL90_afd / 3.069

#convert cfs to mgd 
data$l90_Qout_mgd = data$l90_Qout / 1.547
data$l30_Qout_mgd = data$l30_Qout / 1.547

#solve for WA
data$WA_L90_mgd = data$l90_Qout_mgd - 0.9*(data$l90_Qout_mgd + data$wd_cumulative_mgd - data$ps_cumulative_mgd) + data$SminL90_mgd 
data$WA_L30_mgd = data$l30_Qout_mgd - 0.9*(data$l30_Qout_mgd + data$wd_cumulative_mgd - data$ps_cumulative_mgd) + data$SminL30_mgd

#WA as a % of flow 
data$pct_WA30 = (data$WA_L30_mgd / data$l30_Qout_mgd)*100
data$pct_WA90 = (data$WA_L90_mgd / data$l90_Qout_mgd)*100

#Pulling in Smin metrics exported so far (only a few)
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


#For comparing L30 & L90 before and after batch re-calculation when Smin is exported 

#Exporting original values 
storage_byseg <- all_imp_data[grep("vahydrosw_wshed", all_imp_data$hydrocode),]

storage_byseg_og <- fread(paste0(export_path,'lowflows_impsegs.csv'))


storage_byseg <- sqldf("select a.*, b.l90_Qout, b.l30_Qout, b.l90_year, b.l30_year
                        from storage_byseg as a
                        left outer join metric_data as b 
                        on (a.riverseg = b.riverseg)")

names(storage_byseg)[names(storage_byseg) == 'l30_Qout'] <- 'l30_Qout_new'
names(storage_byseg)[names(storage_byseg) == 'l90_Qout'] <- 'l90_Qout_new'
names(storage_byseg)[names(storage_byseg) == 'l30_year'] <- 'l30_year_new'
names(storage_byseg)[names(storage_byseg) == 'l90_year'] <- 'l90_year_new'

#join sqldf
storage_joined <- sqldf('select a.*, b.l30_Qout_new, b.l90_Qout_new, b.l30_year_new, b.l90_year_new
                        from storage_byseg_og as a
                        outer left join storage_byseg as b
                        on (a.pid = b.pid)')


write.table(storage_joined,file = paste0(export_path,'lowflows_impsegs.csv'), sep = ",", row.names = FALSE) #save csv




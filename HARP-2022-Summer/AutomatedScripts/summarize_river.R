# This script will take in our hydr csv as an argument and perform analysis on 
# variables Qout, ps, wd, and demand and pushes to VAhydro.
# The values calculated are based on waterSupplyModelNode.R
basepath='/var/www/R';
source("/var/www/R/config.R")

suppressPackageStartupMessages(library(data.table)) 
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(zoo))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(caTools))
suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(IHA))
suppressPackageStartupMessages(library(PearsonDS))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(R.utils))

# establishing location on server for storing images
omsite = "http://deq1.bse.vt.edu:81"

# setwd("/Users/VT_SA/Documents/HARP") # for testing only
# hydr <- fread("PL3_5250_0001_hydr.csv") # for testing only
# river_seg <- 'PL3_5250_0001'
# input_file_path='/media/model/p532/out/river/hsp2_2022/hydr/'

# Accepting command arguments:
argst <- commandArgs(trailingOnly = T)
river_seg <- argst[1]
scenario_name <- argst[2]
hydr_file_path <- argst[3]
image_dir <- argst[4]
model_version <- argst[5]

# The hydr file columns have been modifed with a conversion script, 
# and ps and demand were added from the 'timeseries' in the h5
hydr <- fread(hydr_file_path)

### Exporting to VAHydro
## Set up currently to output all the Qout values & the graph
## From hsp_hydr_analysis

# Set up our data source
ds <- RomDataSource$new(site, rest_uname = rest_uname)
ds$get_token(rest_pw)

rseg_name=river_seg
rseg_ftype='vahydro'

riverseg<- RomFeature$new(
  ds,
  list(
    hydrocode=paste('vahydrosw_wshed_',rseg_name, sep = ''),
    ftype=rseg_ftype,
    bundle='watershed'
  ),
  TRUE
)

model <- RomProperty$new(
  ds,
  list(
    varkey="om_model_element", 
    propname=riverseg$name,
    featureid=riverseg$hydroid, 
    entity_type="dh_feature", 
    propcode=model_version
  ), 
  TRUE
)
model$save(TRUE)

model_scenario <- RomProperty$new( 
  ds,
  list(
    varkey="om_scenario", 
    featureid=model$pid, 
    entity_type="dh_properties", 
    propname=scenario_name, 
    propcode=scenario_name 
  ), 
  TRUE
)
model_scenario$save(TRUE)

# Uploading constants to VaHydro:
# entity-type specifies what we are attaching the constant to 
# Edit to more compact version???

model_constant_hydr_path <- RomProperty$new(
  ds, list(
    varkey="om_class_textField", 
    featureid=model_scenario$pid,
    entity_type='dh_properties',
    propname = 'hydr_file_path'
  ),
  TRUE
)
model_constant_hydr_path$propcode <- as.character(hydr_file_path)
model_constant_hydr_path$save(TRUE)




### ANALYSIS

## water year:
syear = min(hydr$year)
eyear = max(hydr$year)
model_run_start <- min(hydr$date)   
model_run_end <- max(hydr$date)
years <- seq(syear,eyear)

if (syear < (eyear - 2)) {
  sdate <- as.Date(paste0(syear,"-10-01"))
  edate <- as.Date(paste0((eyear-1),"-09-30")) 
  flow_year_type <- 'water'
} else {
  sdate <- as.Date(paste0(syear,"-02-01"))
  edate <- as.Date(paste0(eyear,"-12-31"))
  flow_year_type <- 'calendar'
}

hydr <- hydr %>% filter(date > sdate) %>% filter(date < edate) # New hydr table with water year start and end dates 

# dailyQout_wy <- aggregate(hydr$Qout, by = list(hydr$date), FUN='mean')
# colnames(dailyQout_wy) <- c('date','Qout')


# There is no impoundment, so it will be set to 0:

imp_off = 0
cols <- names(hydr)

## Primary Analysis on Qout, ps and wd:
# Qout
# Qbaseline
# wd_mgd
# ps_mgd
# wd_cumulative_mgd
# ps_cumulative_mgd
# ps_nextdown_mgd
# consumptive_use_frac
# daily_consumptive_use_frac
# net_consumption_mgd

# wd
wd_mgd <- mean(as.numeric(hydr$wd_mgd))

wd_imp_child_mgd <- mean(as.numeric(hydr$wd_imp_child_mgd) )
if (is.na(wd_imp_child_mgd)) {   # setting this to zero since it doesn't exist
  wd_imp_child_mgd = 0.0
}

wd_mgd <- wd_mgd + wd_imp_child_mgd   # the official wd_mgd

wd_cumulative_mgd <- mean(as.numeric(hydr$wd_cumulative_mgd) )
if (is.na(wd_cumulative_mgd)) {   # setting this to zero since it doesn't exist
  wd_cumulative_mgd = 0.0
}

# ps
ps_mgd <- mean(as.numeric(hydr$ps_mgd) )

ps_cumulative_mgd <- mean(as.numeric(hydr$ps_cumulative_mgd) )
if (is.na(ps_cumulative_mgd)) {   # setting this to zero since it doesn't exist
  ps_cumulative_mgd = 0.0
}
ps_nextdown_mgd <- mean(as.numeric(hydr$ps_nextdown_mgd) )
if (is.na(ps_nextdown_mgd)) {   # setting this to zero since it doesn't exist
  ps_nextdown_mgd = 0.0
}

# net consumption
net_consumption_mgd <- wd_cumulative_mgd - ps_cumulative_mgd
if (is.na(net_consumption_mgd)) {
  net_consumption_mgd = 0.0
}

# Qout, Q baseline
Qout <- mean(as.numeric(hydr$Qout))

hydr$Qbaseline <- hydr$Qout +
  (hydr$wd_cumulative_mgd - hydr$ps_cumulative_mgd ) * 1.547
# alter calculation to account for pump store
if (imp_off == 0) {
  if("impoundment_Qin" %in% cols) {
    if (!("ps_cumulative_mgd" %in% cols)) {
      hydr$ps_cumulative_mgd <- 0.0
    }
    hydr$Qbaseline <- hydr$impoundment_Qin +
      (hydr$wd_cumulative_mgd - hydr$ps_cumulative_mgd) * 1.547
  }
}

Qbaseline <- mean(as.numeric(hydr$Qbaseline) )
if (is.na(Qbaseline)) {   # creating Qbaseline since it doesn't exist
  Qbaseline = Qout +
    (wd_cumulative_mgd - ps_cumulative_mgd ) * 1.547
}

# Our Qout will equal Qbaseline, since we don't have cumulative wd/ps values

# The total flow method of consumptive use calculation
consumptive_use_frac <- 1.0 - (Qout / Qbaseline)
hydr$consumptive_use_frac <- 1.0 - (hydr$Qout / hydr$Qbaseline)
# This method is more appropriate for impoundments that have long
# periods of zero outflow... but the math is not consistent with elfgen
daily_consumptive_use_frac <-  mean(as.numeric(hydr$consumptive_use_frac) )
if (is.na(daily_consumptive_use_frac)) {
  daily_consumptive_use_frac <- 1.0 - (Qout / Qbaseline)
}
# Since Qout = Qbaseline, these fractions will equal 1

# datdf <- as.data.frame(dat)
# modat <- sqldf("select month, avg(wd_cumulative_mgd) as wd_mgd, avg(ps_cumulative_mgd) as ps_mgd from datdf group by month")
# barplot(wd_mgd ~ month, data=modat)
# the creation of this barplot was commented out, do we include it??


vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'net_consumption_mgd', net_consumption_mgd, ds)
vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'wd_mgd', wd_mgd, ds)
vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'wd_cumulative_mgd', wd_cumulative_mgd, ds)
vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'ps_mgd', ps_mgd, ds)
vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'ps_cumulative_mgd', ps_cumulative_mgd, ds)
vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'Qout', Qout, ds)
vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'Qbaseline', Qbaseline, ds)
vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'ps_nextdown_mgd', ps_nextdown_mgd, ds)
vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'consumptive_use_frac', consumptive_use_frac, ds)
vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'daily_consumptive_use_frac', daily_consumptive_use_frac, ds)



## Secondary Analysis that require Zoo (IHA)
# l30_Qout
# l30_year
# l90_Qout
# l90_year
# 7q10
# ml8 (alf)
# mne9_10 (sept_10) 
# unmet_demand

Qout_zoo <- zoo(dailyQout$Qout, order.by = dailyQout$date)
Qout_g2 <- data.frame(group2(Qout_zoo))
l90_Qout <- min(Qout_g2$X90.Day.Min) # cfs
l30_Qout <- min(Qout_g2$X30.Day.Min)
paste('l90_Qout:', l90_Qout)
paste('l30_Qout:', l30_Qout)

# alf
fn_iha_mlf <- function(zoots, targetmo) {
  modat <- group1(zoots,'water','min')  # IHA function that calculates minimum monthly statistics for our data by water year	 
  print(paste("Grabbing ", targetmo, " values ", sep=''))
  g1vec <- as.vector(as.matrix(modat[,targetmo]))  # gives only August statistics
  print("Performing quantile analysis")
  x <- quantile(g1vec, 0.5, na.rm = TRUE);
  return(as.numeric(x));
}
Qout_wy_z <- zoo(dailyQout_wy$Qout, order.by = dailyQout_wy$date)
alf <- fn_iha_mlf(Qout_wy_z,'August') #The median flow of the annual minumum flows in august 

# Sept. 10%
dailyQout$month <- month(dailyQout$date)
sept_flows <- subset(dailyQout, month == '9')
sept_10 <- as.numeric(round(quantile(sept_flows$Qout, 0.10),6)) # September 10th percentile value of Qout flows with quantile 

fn_iha_7q10 <- function(zoots) {
  g2 <- group2(zoots) 
  
  x <- as.vector(as.matrix(g2["7 Day Min"]))
  
  for (k in 1:length(x)) {
    if (x[k] <= 0) {
      x[k] <- 0.00000001
      print (paste("Found 0.0 average in year", g2["year"], sep = " "))
    }
  }
  x <- log(x)
  pars <- PearsonDS:::pearsonIIIfitML(x)
  x7q10 <- exp(qpearsonIII(0.1, params = pars$par))
  return(x7q10);
}
x7q10 <- fn_iha_7q10(Qout_zoo) # Avg 7-day low flow over a year period 


# vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'l90_Qout', l90_Qout, ds)
# vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'l90_year', l90_year, ds)
# vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'l30_Qout', l30_Qout, ds)
# vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'l30_year', l30_year, ds)
# vahydro_post_metric_to_scenprop(scenprop$pid, '7q10', NULL, '7q10', x7q10, ds)
# vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'ml8', alf, ds)
# vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'mne9_10', sept_10, ds)
# vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'unmet_demand_mgd', unmet_demand_mgd, ds)



## Metrics trimmed to climate change scenario timescale (Jan. 1 1990 -- Dec. 31 2000)
# l90_cc_Qout
# l90_cc_year
# l30_cc_Qout
# l30_cc_year

if (syear <= 1990 && eyear >= 2000) {
  sdate_trim <- as.Date(paste0(1990,"-10-01"))
  edate_trim <- as.Date(paste0(2000,"-09-30"))
  
  dat_trim <- window(dat, start = sdate_trim, end = edate_trim);
  # convert to daily
  dat_trim <- aggregate(
    dat_trim,
    as.POSIXct(
      format(
        time(dat_trim),
        format='%Y/%m/%d'),
      tz='UTC'
    ),
    'mean'
  )
  mode(dat_trim) <- 'numeric'
  
  flows_trim <- zoo(as.numeric(as.character( dat_trim$Qout )), order.by = index(dat_trim));
  loflows_trim <- group2(flows_trim);
  l90_trim <- loflows_trim["90 Day Min"];
  ndx_trim = which.min(as.numeric(l90_trim[,"90 Day Min"]));
  l90_Qout_trim = round(loflows_trim[ndx_trim,]$"90 Day Min",6);
  l90_year_trim = loflows_trim[ndx_trim,]$"year";
  
  if (is.na(l90_trim)) {
    l90_Qout_trim = 0.0
    l90_year_trim = 0
  }
  
  l30_trim <- loflows_trim["30 Day Min"];
  ndx_trim = which.min(as.numeric(l30_trim[,"30 Day Min"]));
  l30_Qout_trim = round(loflows_trim[ndx_trim,]$"30 Day Min",6);
  l30_year_trim = loflows_trim[ndx_trim,]$"year";
  
  if (is.na(l30_trim)) {
    l30_Qout_trim = 0.0
    l30_year_trim = 0
  }
}

vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'l90_cc_Qout', l90_Qout_trim, ds)
vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'l90_cc_year', l90_year_trim, ds)
vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'l30_cc_Qout', l30_Qout_trim, ds)
vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'l30_cc_year', l30_year_trim, ds)

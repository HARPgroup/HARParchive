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
suppressPackageStartupMessages(library(R.utils))
suppressPackageStartupMessages(library(hydroTSM))
suppressPackageStartupMessages(library(stats)) #for window()

# establishing location on server for storing images
omsite = "http://deq1.bse.vt.edu:81"

# setwd("/Users/glenncampagna/Desktop/HARPteam22/Data") # for testing only 
# setwd("/Users/VT_SA/Documents/HARP") # for testing only
# hydr <- fread("OR1_7700_7980_hydr.csv") # for testing only, final hydr csv with wd, demand, ps - Glenn
# river_seg <- 'OR1_7700_7980'
# scenario_name <- 'hsp2_2022'
# hydr_file_path <- '/media/model/p532/out/river/hsp2_2022/hydr/OR1_7700_7980_hydr.csv'

# Accepting command arguments:
argst <- commandArgs(trailingOnly = T)
river_seg <- argst[1]
scenario_name <- argst[2]
hydr_file_path <- argst[3]
model_version <- argst[4]
image_dir <- argst[5]

split <- strsplit(image_dir, split = "/")
path_list_m2 <- as.list(split[[1]][-c(1,2,3)])
path_string_m2 <- paste(path_list_m2, collapse = "/")
save_url <- paste0('http://deq1.bse.vt.edu:81/', path_string_m2)

# create a place to save an image if it does not exist
# note: we do NOT create a path for the hydr_file because it MUST exist, otherwise,
#       we would have nothing to analyze
if (!file.exists(image_dir)) {
  dir.create(file.path(image_dir)) #creates directory if one does not yet exists
}

# The hydr file columns have been modifed with a conversion script, 
# and ps and demand were added from the 'timeseries' in the h5
hydr <- fread(hydr_file_path)

# This removes the hydr file from the end of the hydr_file_path, so that later
# we can use input_file_path in order to post it on VAhydro
file_path_text = paste(hydr_file_path)
split <- strsplit(file_path_text, split = "/")
input_file_path <- gsub(split[[1]][[9]],'',file_path_text)

### Exporting to VAHydro

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
model_constant_hydr_path$propcode <- as.character(input_file_path)
model_constant_hydr_path$save(TRUE)

#Assumptions and placeholders columns 
imp_off = 1
hydr$imp_off = 1 # set to 1 meaning there will be no impoundment 

hydr$wd_imp_child_mgd = 0 #child vars used in hspf 
hydr$wd_cumulative_mgd = hydr$wd_mgd  
hydr$ps_cumulative_mgd = hydr$ps_mgd
hydr$ps_nextdown_mgd = 0 

### ANALYSIS
## water year:

syear = as.integer(min(hydr$year))
eyear = as.integer(max(hydr$year))
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

#Reverted back to using window(), which requires a ts or zoo:
hydr <- zoo(hydr, order.by = hydr$index) #Takes a little while
hydr = aggregate(
  hydr,
  as.POSIXct(
    format(
      date(hydr), 
      format='%Y/%m/%d UTC')
  ),
  'mean')

hydr <- window(hydr, start = sdate, end = edate)
#### Convert hydr to a zoo and keep it that way thorughout 

#Convert hydr to numeric: mode(dat) <- 'numeric'
mode(hydr) <- 'numeric'

#hydr <- fortify.zoo(hydrz) #Want to be able to remove 

## Primary Analysis on Qout, ps and wd:
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

ps_mgd <- mean(as.numeric(hydr$ps_mgd) )
#ps_mgd to be added to the hydr table using the conversion script,
# after ps_afd is added to hydr using the join_col script 

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

Qbaseline <- mean(as.numeric(hydr$Qbaseline))
if (is.na(Qbaseline)) {   # creating Qbaseline since it doesn't exist
  Qbaseline = Qout +
    (wd_cumulative_mgd - ps_cumulative_mgd ) * 1.547
}

#Adding unmet demand:
hydr$unmet_demand_mgd = as.numeric(hydr$demand_mgd) - as.numeric(hydr$wd_mgd)

# The total flow method of consumptive use calculation
consumptive_use_frac <- 1.0 - (Qout / Qbaseline)
hydr$consumptive_use_frac <- 1.0 - (as.numeric(hydr$Qout) / as.numeric(hydr$Qbaseline))
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

vahydro_post_metric_to_scenprop(model_scenario$pid, 'om_class_Constant', NULL, 'net_consumption_mgd', net_consumption_mgd, ds)
vahydro_post_metric_to_scenprop(model_scenario$pid, 'om_class_Constant', NULL, 'wd_mgd', wd_mgd, ds)
vahydro_post_metric_to_scenprop(model_scenario$pid, 'om_class_Constant', NULL, 'wd_cumulative_mgd', wd_cumulative_mgd, ds)
vahydro_post_metric_to_scenprop(model_scenario$pid, 'om_class_Constant', NULL, 'ps_mgd', ps_mgd, ds)
vahydro_post_metric_to_scenprop(model_scenario$pid, 'om_class_Constant', NULL, 'ps_cumulative_mgd', ps_cumulative_mgd, ds)
vahydro_post_metric_to_scenprop(model_scenario$pid, 'om_class_Constant', NULL, 'Qout', Qout, ds)
vahydro_post_metric_to_scenprop(model_scenario$pid, 'om_class_Constant', NULL, 'Qbaseline', Qbaseline, ds)
vahydro_post_metric_to_scenprop(model_scenario$pid, 'om_class_Constant', NULL, 'ps_nextdown_mgd', ps_nextdown_mgd, ds)
vahydro_post_metric_to_scenprop(model_scenario$pid, 'om_class_Constant', NULL, 'consumptive_use_frac', consumptive_use_frac, ds)
vahydro_post_metric_to_scenprop(model_scenario$pid, 'om_class_Constant', NULL, 'daily_consumptive_use_frac', daily_consumptive_use_frac, ds)

# L90 and l30 -- move this? 
#Qout_zoo <- zoo(as.numeric(hydr$Qout), order.by = hydr$index)
Qout_g2 <- data.frame(group2(hydr$Qout))
l90 <- Qout_g2["X90.Day.Min"];
ndx = which.min(as.numeric(l90[,"X90.Day.Min"]));
l90_Qout = round(Qout_g2[ndx,]$"X90.Day.Min",6);
l90_year = Qout_g2[ndx,]$"year";

if (is.na(l90)) {
  l90_Runit = 0.0
  l90_year = 0
}

l30 <- Qout_g2["X30.Day.Min"];
ndx = which.min(as.numeric(l30[,"X30.Day.Min"]));
l30_Qout = round(Qout_g2[ndx,]$"X30.Day.Min",6);
l30_year = Qout_g2[ndx,]$"year";

if (is.na(l30)) {
  l30_Runit = 0.0
  l30_year = 0
}

# alf
fn_iha_mlf <- function(zoots, targetmo) {
  modat <- group1(zoots,'water','min')  # IHA function that calculates minimum monthly statistics for our data by water year	 
  print(paste("Grabbing ", targetmo, " values ", sep=''))
  g1vec <- as.vector(as.matrix(modat[,targetmo]))  # gives only August statistics
  print("Performing quantile analysis")
  x <- quantile(g1vec, 0.5, na.rm = TRUE);
  return(as.numeric(x));
}
Qout_zoo <- zoo(hydr$Qout)
alf <- fn_iha_mlf(Qout_zoo,'August') #The median flow of the annual minumum flows in august 

#From original script:
#alf_data <- data.frame(matrix(data = NA, nrow = length(dat$thisdate), ncol = 5))
#colnames(alf_data) <- c('Qout', 'thisdate', 'year', 'month', 'day')
#alf_data$Qout <- dat$Qout
#alf_data$thisdate <- index(dat)
#alf_data$year <- year(ymd(alf_data$thisdate))
#alf_data$month <- month(ymd(alf_data$thisdate))
#alf_data$day <- day(ymd(alf_data$thisdate))
#zoo.alf_data <- zoo(alf_data$Qout, order.by = alf_data$thisdate)
#alf <- fn_iha_mlf(zoo.alf_data,'August')

# Sept. 10%
sept_flows <- subset(hydr, month == '9') 
sept_10 <- as.numeric(round(quantile(as.numeric(sept_flows$Qout), 0.10),6)) # September 10th percentile value of Qout flows with quantile 

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

# Avg 7-day low flow over a year period -- Move this? 
x7q10 <- fn_iha_7q10(hydr$Qout)  

# Unmet demand
unmet_demand_mgd <- mean(as.numeric(hydr$unmet_demand_mgd)) #Need to add unmet_demand col to hydr
if (is.na(unmet_demand_mgd)) {
  unmet_demand_mgd = 0.0
}

vahydro_post_metric_to_scenprop(model_scenario$pid, 'om_class_Constant', NULL, 'l90_Qout', l90_Qout, ds)
vahydro_post_metric_to_scenprop(model_scenario$pid, 'om_class_Constant', NULL, 'l90_year', l90_year, ds)
vahydro_post_metric_to_scenprop(model_scenario$pid, 'om_class_Constant', NULL, 'l30_Qout', l30_Qout, ds)
vahydro_post_metric_to_scenprop(model_scenario$pid, 'om_class_Constant', NULL, 'l30_year', l30_year, ds)
vahydro_post_metric_to_scenprop(model_scenario$pid, '7q10', NULL, '7q10', x7q10, ds)
vahydro_post_metric_to_scenprop(model_scenario$pid, 'om_class_Constant', NULL, 'ml8', alf, ds)
vahydro_post_metric_to_scenprop(model_scenario$pid, 'om_class_Constant', NULL, 'mne9_10', sept_10, ds)
vahydro_post_metric_to_scenprop(model_scenario$pid, 'om_class_Constant', NULL, 'unmet_demand_mgd', unmet_demand_mgd, ds)

#### FIGURE GENERATION STARTS HERE ####

message("Plotting critical flow periods")
# does this have an active impoundment sub-comp
if (imp_off == 0) {
  
  if("impoundment" %in% cols) {
    # Plot and analyze impoundment sub-comps
    hydr$storage_pct <- hydr$impoundment_use_remain_mg * 3.07 / hydr$impoundment_max_usable
    #
    storage_pct <- mean(as.numeric(hydr$storage_pct) )
    if (is.na(storage_pct)) {
      usable_pct_p0 <- 0
      usable_pct_p10 <- 0
      usable_pct_p50 <- 0
    } else {
      usable_pcts = quantile(as.numeric(hydr$storage_pct), c(0,0.1,0.5) )
      usable_pct_p0 <- usable_pcts["0%"]
      usable_pct_p10 <- usable_pcts["10%"]
      usable_pct_p50 <- usable_pcts["50%"]
    }
    impoundment_days_remaining <- mean(as.numeric(hydr$impoundment_days_remaining) )
    if (is.na(impoundment_days_remaining)) {
      remaining_days_p0 <- 0
      remaining_days_p10 <- 0
      remaining_days_p50 <- 0
    } else {
      remaining_days = quantile(as.numeric(hydr$impoundment_days_remaining), c(0,0.1,0.5) )
      remaining_days_p0 <- remaining_days["0%"]
      remaining_days_p10 <- remaining_days["10%"]
      remaining_days_p50 <- remaining_days["50%"]
    }
    
    # post em up
    vahydro_post_metric_to_scenprop(model_scenario$pid, 'om_class_Constant', NULL, 'usable_pct_p0', usable_pct_p0, ds)
    vahydro_post_metric_to_scenprop(model_scenario$pid, 'om_class_Constant', NULL, 'usable_pct_p10', usable_pct_p10, ds)
    vahydro_post_metric_to_scenprop(model_scenario$pid, 'om_class_Constant', NULL, 'usable_pct_p50', usable_pct_p50, ds)

    vahydro_post_metric_to_scenprop(model_scenario$pid, 'om_class_Constant', NULL, 'remaining_days_p0', remaining_days_p0, ds)
    vahydro_post_metric_to_scenprop(model_scenario$pid, 'om_class_Constant', NULL, 'remaining_days_p10', remaining_days_p10, ds)
    vahydro_post_metric_to_scenprop(model_scenario$pid, 'om_class_Constant', NULL, 'remaining_days_p50', remaining_days_p50, ds)

    # this has an impoundment.  Plot it up.
    # Now zoom in on critical drought period
    pdstart = as.Date(paste0(l90_year,"-06-01") )
    pdend = as.Date(paste0(l90_year, "-11-15") )
    
    hydrpd <- window(
      hydr,
      start = pdstart,
      end = pdend
    );                   # setting the fname and furl for pushing graphs to vahydro
    fname <- paste(
      image_dir,
      paste0(
        'l90_imp_storage.',
        river_seg, '.', scenario_name, '.png'
      ),
      sep = '/'
    )
    furl <- paste(
      save_url,
      paste0(
        'l90_imp_storage.',
        river_seg, '.', scenario_name, '.png'
      ),
      sep = '/'
    )
    png(fname)
    ymn <- 1
    ymx <- 100
    par(mar = c(8.8,5,0.5,5))
    plot(
      hydrpd$storage_pct * 100.0,
      ylim=c(ymn,ymx),
      ylab="Reservoir Storage (%)",
      xlab=paste("Lowest 90 Day Flow Period",pdstart,"to",pdend),
      legend=c('Storage', 'Qin', 'Qout', 'Demand (mgd)')
    )
    par(new = TRUE)
    plot(hydrpd$impoundment_Qin,col='blue', axes=FALSE, xlab="", ylab="")
    lines(hydrpd$impoundment_Qout,col='green')
    lines(hydrpd$impoundment_demand * 1.547,col='red')
    axis(side = 4)
    mtext(side = 4, line = 3, 'Flow/Demand (cfs)')
    legend("bottom",inset=-0.36, xpd=TRUE, c("Reservoir Storage","Inflow","Outflow","Demand"),
           col = c("black", "blue", "green","red"),
           lty = c(1,1,1,1),
           bg='white',cex=0.8) #ADD LEGEND
    dev.off()
    print(paste("Saved file: ", fname, "with URL", furl))
    vahydro_post_metric_to_scenprop(model_scenario$pid, 'dh_image_file', furl, 'fig.l90_imp_storage', 0.0, ds)
    
    # l90 2 year
    # this has an impoundment.  Plot it up.
    # Now zoom in on critical drought period
    pdstart = as.Date(paste0( (as.integer(l90_year) - 1),"-01-01") )
    pdend = as.Date(paste0(l90_year, "-12-31") )
    hydrpd <- window(
      hydr,
      start = pdstart,
      end = pdend
    );
    fname <- paste(
      image_dir,
      paste0(
        'l90_imp_storage.2yr.',
        river_seg, '.', scenario_name, '.png'
      ),
      sep = '/'
    )
    furl <- paste(
      save_url,
      paste0(
        'l90_imp_storage.2yr.',
        river_seg, '.', scenario_name, '.png'
      ),
      sep = '/'
    )
    png(fname)
    ymn <- 1
    ymx <- 100
    par(mar = c(8.8,5,0.5,5))
    plot(
      hydrpd$storage_pct * 100.0,
      ylim=c(ymn,ymx),
      ylab="Reservoir Storage (%)",
      xlab=paste("Lowest 90 Day Flow Period",pdstart,"to",pdend)
    )
    par(new = TRUE)
    plot(hydrpd$impoundment_Qin,col='blue', axes=FALSE, xlab="", ylab="")
    lines(hydrpd$impoundment_Qout,col='green')
    lines(hydrpd$wd_mgd * 1.547,col='red')
    axis(side = 4)
    mtext(side = 4, line = 3, 'Flow/Demand (cfs)')
    legend("bottom",inset=-0.36, xpd=TRUE, c("Reservoir Storage","Inflow","Outflow","Demand"),
           col = c("black", "blue", "green","red"),
           lty = c(1,1,1,1),
           bg='white',cex=0.8) #ADD LEGEND
    dev.off()
    print(paste("Saved file: ", fname, "with URL", furl))
    vahydro_post_metric_to_scenprop(model_scenario$pid, 'dh_image_file', furl, 'fig.l90_imp_storage.2yr', 0.0, ds)
    
    # All Periods
    # this has an impoundment.  Plot it up.
    
    # Full period Flow duration curve
    hydrpd <- hydr
    fname <- paste(
      image_dir,
      paste0(
        'fig.fdc.all.',
        river_seg, '.', scenario_name, '.png'
      ),
      sep = '/'
    )
    furl <- paste(
      save_url,
      paste0(
        'fig.fdc.all.',
        river_seg, '.', scenario_name, '.png'
      ),
      sep = '/'
    )
    png(fname)
    hydroTSM::fdc(cbind(hydrpd$impoundment_Qin, hydrpd$impoundment_Qout),ylab="Q (cfs)")
    dev.off()
    print(paste("Saved file: ", fname, "with URL", furl))
    vahydro_post_metric_to_scenprop(model_scenario$pid, 'dh_image_file', furl, 'fig.fdc.all.', 0.0, ds)
    
    
    # Full period inflow/outflow, res level
    fname <- paste(
      image_dir,
      paste0(
        'fig.imp_storage.all.',
        river_seg, '.', scenario_name, '.png'
      ),
      sep = '/'
    )
    furl <- paste(
      save_url,
      paste0(
        'fig.imp_storage.all.',
        river_seg, '.', scenario_name, '.png'
      ),
      sep = '/'
    )
    png(fname)
    ymn <- 1
    ymx <- 100
    par(mar = c(8.8,5,0.5,5))
    plot(
      hydrpd$storage_pct * 100.0,
      ylim=c(ymn,ymx),
      ylab="Reservoir Storage (%)",
      xlab=paste("Storage and Flows",sdate,"to",edate)
    )
    par(new = TRUE)
    plot(hydrpd$impoundment_Qin,col='blue', axes=FALSE, xlab="", ylab="")
    lines(hydrpd$impoundment_Qout,col='green')
    lines(hydrpd$wd_mgd * 1.547,col='red')
    axis(side = 4)
    mtext(side = 4, line = 3, 'Flow/Demand (cfs)')
    legend("bottom",inset=-0.36, xpd=TRUE, c("Reservoir Storage","Inflow","Outflow","Demand"),
           col = c("black", "blue", "green","red"),
           lty = c(1,1,1,1),
           bg='white',cex=0.8) #ADD LEGEND
    dev.off()
    print(paste("Saved file: ", fname, "with URL", furl))
    vahydro_post_metric_to_scenprop(model_scenario$pid, 'dh_image_file', furl, 'fig.imp_storage.all', 0.0, ds)
    
    # Low Elevation Period
    # hydr for Critical Period
    elevs <- zoo(hydr$storage_pct, order.by = index(hydr));
    loelevs <- group2(elevs, flow_year_type);
    l90 <- loelevs["90 Day Min"];
    ndx = which.min(as.numeric(l90[,"90 Day Min"]));
    l90_elev = round(loelevs[ndx,]$"90 Day Min",6);
    l90_elevyear = loelevs[ndx,]$"year";
    l90_elev_start = as.Date(paste0(l90_elevyear - 2,"-01-01"))
    l90_elev_end = as.Date(paste0(l90_elevyear,"-12-31"))
    elevhydrpd <- window(
      hydr,
      start = l90_elev_start,
      end = l90_elev_end
    );
    hydrpd <- elevhydrpd
    fname <- paste(
      image_dir,
      paste0(
        'elev90_imp_storage.all.',
        river_seg, '.', scenario_name, '.png'
      ),
      sep = '/'
    )
    furl <- paste(
      save_url,
      paste0(
        'elev90_imp_storage.all.',
        river_seg, '.', scenario_name, '.png'
      ),
      sep = '/'
    )
    png(fname)
    ymn <- 1
    ymx <- 100
    par(mar = c(8.8,5,01,5))
    plot(
      hydrpd$storage_pct * 100.0,cex.main=1,
      ylim=c(ymn,ymx),
      main="Minimum Modeled Reservoir Storage Period",
      ylab="Reservoir Storage (%)",
      xlab=paste("Model Time Period",l90_elev_start,"to",l90_elev_end)
    )
    par(new = TRUE)
    plot(hydrpd$impoundment_Qin,col='blue', axes=FALSE, xlab="", ylab="")
    lines(hydrpd$Qout,col='green')
    lines(hydrpd$wd_mgd * 1.547,col='red')
    axis(side = 4)
    mtext(side = 4, line = 3, 'Flow/Demand (cfs)')
    legend("bottom",inset=-0.36, xpd=TRUE, c("Reservoir Storage","Inflow","Outflow","Demand"),
           col = c("black", "blue", "green","red"),
           lty = c(1,1,1,1),
           bg='white',cex=0.8) #ADD LEGEND
    dev.off()
    print(paste("Saved file: ", fname, "with URL", furl))
    vahydro_post_metric_to_scenprop(model_scenario$pid, 'dh_image_file', furl, 'elev90_imp_storage.all', 0.0, ds)
    
  }
} else {
  
  ## End of the if-loop; this is what we will plot since imp_off is 1!!!!!!!!!!  
  
  # plot Qin, Qout of mainstem, and wd_mgd, and wd_cumulative_mgd
  # TBD
  # l90 2 year
  # this has an impoundment.  Plot it up.
  # Now zoom in on critical drought period
  pdstart = as.IDate(paste0(l90_year,"-06-01"))
  pdend = as.IDate(paste0(l90_year, "-11-15") )
  
  #Replace with window()
  #hydrpd <- with(hydr, hydr[(date >= pdstart & date <= pdend)])
  
  #hydrz <- zoo(hydr, order.by = hydr$index) #Takes a little while
  hydrpd <- window(hydr, start = pdstart, end = pdend)
  
  fname <- paste(
    image_dir,
    paste0(
      'l90_flows.2yr.',
      river_seg, '.', scenario_name, '.png'
    ),
    sep = '/'
  )
  furl <- paste(
    save_url,
    paste0(
      'l90_flows.2yr.',
      river_seg, '.', scenario_name, '.png'
    ),
    sep = '/'
  )
  png(fname)
  # Because these are zoo timeseries, they will throw an error if you use a normal DF
  # max() syntax which is OK with max(c(df1, df2))
  # instead, we cbind them instead of the default which is an implicit rbind
  # ymx <- max(hydrpd$Qbaseline, hydrpd$Qout)
  ymx <- as.numeric(max(cbind(hydrpd$Qbaseline, hydrpd$Qout)))
  plot(
    as.numeric(hydrpd$Qbaseline), ylim = c(0,ymx),  #Placeholders for xlim, come back to this and create xlim based on hydrpd
    ylab="Flow/WD/PS (cfs)",
    xlab=paste("Lowest 90 Day Flow Period",pdstart,"to",pdend)
  )
  lines(as.numeric(hydrpd$Qout,col='blue'))
  par(new = TRUE)
  # Because these are zoo timeseries, they will throw an error if you use a normal DF
  # max() syntax which is OK with max(c(df1, df2))
  # instead, we cbind them instead of the default which is an implicit rbind
  
  #as.numeric() used often because data within zoo df is of class character   
  
  
  ymx <- max(cbind(as.numeric(hydrpd$wd_cumulative_mgd) * 1.547, as.numeric(hydrpd$ps_cumulative_mgd) * 1.547))
    plot(
      hydrpd$wd_cumulative_mgd * 1.547,col='red',
      axes=FALSE, xlab="", ylab="", ylim=c(0,ymx)
    )
    lines(hydrpd$ps_cumulative_mgd * 1.547,col='green')
    axis(side = 4)
    mtext(side = 4, line = 3, 'Flow/Demand (cfs)')
    #Add title to inform why graph is blank
    dev.off()
    print(paste("Saved file: ", fname, "with URL", furl))
    vahydro_post_metric_to_scenprop(model_scenario$pid, 'dh_image_file', furl, 'fig.l90_flows.2yr', 0.0, ds)
  
  hydrpd <- hydr 
  fname <- paste(
    image_dir,
    paste0(
      'flows.all.',
      river_seg, '.', scenario_name, '.png'
    ),
    sep = '/'
  )
  furl <- paste(
    save_url,
    paste0(
      'flows.all.',
      river_seg, '.', scenario_name, '.png'
    ),
    sep = '/'
  )
  png(fname)
  ymx <- as.numeric(max(cbind(max(hydrpd$Qbaseline), max(hydrpd$Qout))))
  plot(
    as.numeric(hydrpd$Qbaseline), ylim = c(0,ymx),
    ylab="Flow/WD/PS (cfs)",
    xlab=paste("Model Flow Period",sdate,"to",edate)
  )
  lines(as.numeric(hydrpd$Qout,col='blue'))
  par(new = TRUE)
  
  #Revert these changes (loop), the graphic could be expected even if 'meaningless'
  #Have message be a part of the figure (main title of plot)
  ymx <- max(cbind(as.numeric(hydrpd$wd_cumulative_mgd) * 1.547, as.numeric(hydrpd$ps_cumulative_mgd) * 1.547))
  plot_label='WD and PS Timeseries'
  if (ymx == 0) {
    plot_label='No withdrawal or point source for this segment'
  }

  plot(
      hydrpd$wd_cumulative_mgd * 1.547,col='red',
      axes=FALSE, xlab="", ylab="", ylim=c(0,ymx), main = plot_label
    )
    lines(hydrpd$ps_cumulative_mgd * 1.547,col='green')
    axis(side = 4)
    mtext(side = 4, line = 3, 'Flow/Demand (cfs)')
    dev.off()
    print(paste("Saved file: ", fname, "with URL", furl))
    vahydro_post_metric_to_scenprop(model_scenario$pid, 'dh_image_file', furl, 'fig.flows.all', 0.0, ds)
  
}


###############################################
# RSEG FDC
###############################################
base_var <- "Qbaseline" #BASE VARIABLE USED IN FDCs AND HYDROGRAPHS
comp_var <- "Qout" #VARIABLE TO COMPARE AGAINST BASE VARIABLE, DEFAULT Qout
## ^^ Currently equal so comparison is pointless
# FOR TESTING 
# save_directory <- 'C:/Users/nrf46657/Desktop/GitHub/om/R/summarize'
#hydrpd <- hydrdf
fname <- paste(
  image_dir,
  paste0(
    'fdc.',
    river_seg, '.', scenario_name, '.png'
  ),
  sep = '/'
)
# FOR TESTING 
# save_url <- save_directory
furl <- paste(
  save_url,
  paste0(
    'fdc.',
    river_seg, '.', scenario_name, '.png'
  ),
  sep = '/'
)


#var_df <- as.data.frame(hydrpd[base_var], row.names = NULL)
#var_df$comp_var <- hydrpd[comp_var]
#colnames(var_df) <- c(base_var, comp_var)
#^This was created to replace the cbind - Glenn
hydrpd <- data.frame(hydrpd)
png(fname, width = 700, height = 700)
legend_text = c("Baseline Flow","Scenario Flow")
fdc_plot <- hydroTSM::fdc(cbind(hydrpd[names(hydrpd)== base_var], hydrpd[names(hydrpd)== comp_var]),
                          #this line is giving the first error, test with zoo
                          #Otherwise, may need to summarize data first, or sqldf
                          # yat = c(0.10,1,5,10,25,100,400),
                          # yat = c(round(min(hydrpd),0),500,1000,5000,10000),
                          yat = seq(round(min(hydrpd$Qout),0),round(max(hydrpd$Qout),0), by = 500),
                          leg.txt = legend_text,
                          main=paste("Flow Duration Curve","\n","(Model Flow Period ",sdate," to ",edate,")",sep=""),
                          ylab = "Flow (cfs)",
                          # ylim=c(1.0, 5000),
                          ylim=c(min(hydrpd$Qout), max(hydrpd$Qout)),
                          cex.main=1.75,
                          cex.axis=1.50,
                          leg.cex=2,
                          cex.sub = 1.2
)
dev.off()
#Takes forever... converting to monthly data and seeing how that changes time 


print(paste("Saved file: ", fname, "with URL", furl))
vahydro_post_metric_to_scenprop(model_scenario$pid, 'dh_image_file', furl, 'fig.fdc', 0.0, ds)

# RSEG Hydrograph (Drought Period)
# Zoom in on critical drought period
pdstart = as.Date(paste0(l90_year,"-06-01"))
pdend = as.Date(paste0(l90_year, "-11-15"))

hydrpd <- window(hydr, start = pdstart, end = pdend)
hydrpd <- data.frame(hydrpd)
#hydrpd$Date <- rownames(hydrpd)


fname <- paste(
  image_dir,
  paste0(
    'hydrograph_dry.',
    river_seg, '.', scenario_name, '.png'
  ),
  sep = '/'
)
furl <- paste(
  save_url,
  paste0(
    'hydrograph_dry.',
    river_seg, '.', scenario_name, '.png'
  ),
  sep = '/'
)

png(fname, width = 900, height = 700)
legend_text = c("Baseline Flow","Scenario Flow")
xmn <- as.Date(pdstart)
xmx <- as.Date(pdend)
ymn <- 0
#ymx <- 1000
ymx <- max(cbind(as.numeric(unlist(hydrpd[names(hydrpd)== base_var])),
                 as.numeric(unlist(hydrpd[names(hydrpd)== comp_var]))))
par(mar = c(5,5,2,5))
hydrpd$index <- as.Date(paste0(hydrpd$year,'-',hydrpd$month,'-',hydrpd$day))
hydrograph_dry <- plot(unlist(hydrpd[names(hydrpd)== base_var])~as.Date(hydrpd$index),
                       type = "l", lty=2, lwd = 1,ylim=c(ymn,ymx),xlim=c(xmn,xmx),
                       ylab="Flow (cfs)",xlab=paste("Lowest 90 Day Flow Period",pdstart,"to",pdend),
                       main = "Hydrograph: Dry Period",
                       cex.main=1.75,
                       cex.axis=1.50,
                       cex.lab=1.50
)

par(new = TRUE)
plot(as.numeric(unlist(hydrpd[names(hydrpd)== comp_var]))~as.Date(hydrpd$index),
     type = "l",col='brown3', lwd = 2, 
axes=FALSE,ylim=c(ymn,ymx),xlim=c(xmn,xmx),ylab="",xlab="")
legend("topright",legend=legend_text,col=c("black","brown3"), 
       lty=c(2,1), lwd=c(1,2), cex=1.5)
dev.off()
# Is this supposed to 
print(paste("Saved file: ", fname, "with URL", furl))
vahydro_post_metric_to_scenprop(model_scenario$pid, 'dh_image_file', furl, 'fig.hydrograph_dry', 0.0, ds)
###############################################
###############################################


###############################################
# RSEG ELFGEN
###############################################
#GET RSEG HYDROID FROM RSEG MODEL PID
#rseg <-getProperty(list(pid=pid), site)
rseg <- RomProperty$new(ds, list(pid=pid), TRUE)
rseg_hydroid<-rseg$featureid

huc_level <- 'huc8'
Dataset <- 'VAHydro-EDAS'

#elfgen_huc(scenario_name, rseg_hydroid, huc_level, hydraset, scenprop, ds, image_dir, save_url, site)

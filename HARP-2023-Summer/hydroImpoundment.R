#----------------------------------------------
site <- "http://deq2.bse.vt.edu/d.dh"    #Specify the site of interest, either d.bet OR d.dh
#----------------------------------------------
# Load Libraries
basepath='/var/www/R';
source(paste(basepath,'config.R',sep='/'))
library(stringr)
# dirs/URLs
save_directory <- "/var/www/html/data/proj3/out"
library(hydrotools)
# authenticate
ds <- RomDataSource$new(site, rest_uname)
ds$get_token(rest_pw)

#Load Smin_CPL function
source(paste0("~/HARParchive/HARP-2023-Summer/fn_get_pd_min.R"),local = TRUE)
#source(paste0(github_location,"/HARParchive/HARP-2023-Summer/fn_get_pd_min.R"),local = TRUE) #left for testing in R

# Read Args
argst <- commandArgs(trailingOnly=T)
pid <- as.integer(argst[1])
elid <- as.integer(argst[2])
runid <- as.integer(argst[3])

dat <- fn_get_runfile(elid, runid, site= omsite,  cached = FALSE)
syear = as.integer(min(dat$year))
eyear = as.integer(max(dat$year))
if (syear < (eyear - 2)) {
  sdate <- as.Date(paste0(syear,"-10-01"))
  edate <- as.Date(paste0(eyear,"-09-30"))
} else {
  sdate <- as.Date(paste0(syear,"-02-01"))
  edate <- as.Date(paste0(eyear,"-12-31"))
}
dat <- window(dat, start = sdate, end = edate);
mode(dat) <- 'numeric'
# is imp_off = 0?
cols <- names(dat)
if ("imp_off" %in% cols) {
  imp_off <- as.integer(median(dat$imp_off))
} else {
  # imp_off is NOT in the cols, so impoundment must be active
  # therefore, we assume that the impoundment is active by intention
  # and that it is a legacy that lacked the imp_off convention
  imp_off = 0
}
if ("pct_use_remain" %in% cols) {
  # nothing
} else {
  # this uses 10% dead as estimate for those that hae not been re-run
  # since reporting for pct_use_remain was enabled
  dat$pct_use_remain <- dat$use_remain_mg * 3.07 / (0.9 * dat$maxcapacity)
}

scen.propname<-paste0('runid_', runid)

# GETTING SCENARIO PROPERTY FROM VA HYDRO
sceninfo <- list(
  varkey = 'om_scenario',
  propname = scen.propname,
  featureid = pid,
  entity_type = "dh_properties",
  bundle = "dh_properties"
)
scenprop <- RomProperty$new( ds, sceninfo, TRUE)

# POST PROPERTY IF IT IS NOT YET CREATED
if (is.na(scenprop$pid) | is.null(scenprop$pid) ) {
  # create
  scenprop$save(TRUE)
}

wd_mgd <- mean(as.numeric(dat$demand) )
if (is.na(wd_mgd)) {
  wd_mgd = 0.0
}
ps_mgd <- mean(as.numeric(dat$discharge) )
if (is.na(ps_mgd)) {
  ps_mgd = 0.0
}
evap_mgd <- mean(as.numeric(dat$evap_mgd) )
if (is.na(evap_mgd)) {
  evap_mgd = 0.0
}
Qin <- mean(as.numeric(dat$Qin) )
if (is.na(Qin)) {
  Qin = 0.0
}
Qout <- mean(as.numeric(dat$Qout) )
if (is.na(Qout)) {
  Qout = 0.0
}
storage_pct <- mean(as.numeric(dat$pct_use_remain) )
if (is.na(storage_pct) || (imp_off == 1)) {
  usable_pct_p0 <- 0
  usable_pct_p10 <- 0
  usable_pct_p50 <- 0
} else {
  usable_pcts = quantile(as.numeric(dat$pct_use_remain), c(0,0.1,0.5) )
  usable_pct_p0 <- usable_pcts["0%"]
  usable_pct_p10 <- usable_pcts["10%"]
  usable_pct_p50 <- usable_pcts["50%"]
}


impoundment_days_remaining <- mean(as.numeric(dat$days_remaining) )
if (is.na(impoundment_days_remaining) || (imp_off == 1)) {
  remaining_days_p0 <- 0
  remaining_days_p10 <- 0
  remaining_days_p50 <- 0
} else {
  remaining_days = quantile(as.numeric(dat$days_remaining), c(0,0.1,0.5) )
  remaining_days_p0 <- remaining_days["0%"]
  remaining_days_p10 <- remaining_days["10%"]
  remaining_days_p50 <- remaining_days["50%"]
}
imp_enabled <- as.numeric(!(0 || imp_off))

# post em up
vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'imp_enabled', imp_enabled, ds)
vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'wd_mgd', wd_mgd, ds)
vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'ps_mgd', ps_mgd, ds)
vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'evap_mgd', evap_mgd, ds)
vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'Qout', Qout, ds)
vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'Qin', Qin, ds)
vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'usable_pct_p0', usable_pct_p0, ds)
vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'usable_pct_p10', usable_pct_p10, ds)
vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'usable_pct_p50', usable_pct_p50, ds)
vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'remaining_days_p0', remaining_days_p0, ds)
vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'remaining_days_p10', remaining_days_p10, ds)
vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'remaining_days_p50', remaining_days_p50, ds)


# Dat for Critical Period
flows <- zoo(dat$Qin, order.by = index(dat));
loflows <- group2(flows, year = 'calendar');
l90 <- loflows["90 Day Min"];
ndx = which.min(as.numeric(l90[,"90 Day Min"]));
l90_Qout = round(loflows[ndx,]$"90 Day Min",6);
l90_year = loflows[ndx,]$"year";
l90_start = as.Date(paste0(l90_year - 2,"-01-01"))
l90_end = as.Date(paste0(l90_year,"-12-31"))
datpd <- window(
  dat,
  start = l90_start,
  end = l90_end
);

# Find l30_year for calculation of Smin_L30
l30 <- loflows["30 Day Min"];
ndx = which.min(as.numeric(l30[,"30 Day Min"]));
l30_Qout = round(loflows[ndx,]$"30 Day Min",6);
l30_year = loflows[ndx,]$"year";

# Prep for Smin_CPL function
start_date_30 <- paste0(l30_year,"-01-01") # Dates for l90_year
end_date_30 <- paste0(l30_year,"-12-31")

start_date_90 <- paste0(l90_year,"-01-01") # Dates for l30_year
end_date_90 <- paste0(l90_year,"-12-31")

# Calculate Smin_CPLs using function
Smin_L30_acft <- fn_get_pd_min(ts_data = dat, critical_pd_length = 30,
                          start_date = start_date_30, end_date = end_date_30,
                          colname = "Storage")

Smin_L90_acft <- fn_get_pd_min(ts_data = dat, critical_pd_length = 90,
                          start_date = start_date_90, end_date = end_date_90,
                          colname = "Storage")

# Convert from from ac-ft to mg: 1 mg = 3.069 acre-feet
Smin_L30_mg <- round(Smin_L30_acft/3.069, digits = 3)
Smin_L90_mg <- round(Smin_L90_acft/3.069, digits = 3)

# Set Smin metrics to 0 if impoundment is not active
if (imp_off == 1) { 
  Smin_L30_mg <- 0
  Smin_L90_mg <- 0
}

# Post Smin metrics to vahydro
vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'Smin_L30_mg', Smin_L30_mg, ds)
vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'Smin_L90_mg', Smin_L90_mg, ds)

# Elevation periods

# Dat for Critical Period
elevs <- zoo(dat$pct_use_remain, order.by = index(dat));
loelevs <- group2(elevs);
l90 <- loelevs["90 Day Min"];
ndx = which.min(as.numeric(l90[,"90 Day Min"]));
l90_elev = round(loelevs[ndx,]$"90 Day Min",6);
l90_elevyear = loelevs[ndx,]$"year";
l90_elev_start = as.Date(paste0(l90_elevyear - 2,"-01-01"))
l90_elev_end = as.Date(paste0(l90_elevyear,"-12-31"))
elevdatpd <- window(
  dat,
  start = l90_elev_start,
  end = l90_elev_end
);

# Lake Plots
if (imp_off == 0) {
  fname <- paste(
    save_directory,
    paste0(
      'l90_imp_storage.',
      elid, '.', runid, '.png'
    ),
    sep = '/'
  )
  furl <- paste(
    save_url,
    paste0(
      'l90_imp_storage.',
      elid, '.', runid, '.png'
    ),
    sep = '/'
  )
  png(fname)
  ymn <- 1
  ymx <- 100
  par(mar = c(5,5,2,5))
  plot(
    datpd$pct_use_remain * 100.0,
    ylim=c(ymn,ymx),
    ylab="Reservoir Storage (%)",
    xlab=paste("Model Flow Period",l90_start,"to",l90_end)
  )
  par(new = TRUE)
  plot(datpd$Qin,col='blue', axes=FALSE, xlab="", ylab="")
  lines(datpd$Qout,col='green')
  lines(datpd$demand * 1.547,col='red')
  axis(side = 4)
  mtext(side = 4, line = 3, 'Flow/Demand (cfs)')
  dev.off()
  print(paste("Saved file: ", fname, "with URL", furl))
  vahydro_post_metric_to_scenprop(scenprop$pid, 'dh_image_file', furl, 'fig.l90_imp_storage', 0.0, ds)


  # All Periods
  # this has an impoundment.  Plot it up.
  # Now zoom in on critical drought period
  datpd <- dat
  fname <- paste(
    save_directory,
    paste0(
      'l90_imp_storage.all.',
      elid, '.', runid, '.png'
    ),
    sep = '/'
  )
  furl <- paste(
    save_url,
    paste0(
      'l90_imp_storage.all.',
      elid, '.', runid, '.png'
    ),
    sep = '/'
  )
  png(fname)
  plot(datpd$Qin, ylim=c(-0.1,15))
  lines(datpd$Qout,col='blue')
  ymn <- 1
  ymx <- 100
  par(mar = c(5,5,2,5))
  plot(
    datpd$pct_use_remain * 100.0,
    ylim=c(ymn,ymx),
    ylab="Reservoir Storage (%)",
    xlab=paste("Full Period",sdate,"to",edate)
  )
  par(new = TRUE)
  plot(datpd$Qin,col='blue', axes=FALSE, xlab="", ylab="")
  lines(datpd$Qout,col='green')
  lines(datpd$demand * 1.547,col='red')
  axis(side = 4)
  mtext(side = 4, line = 3, 'Flow/Demand (cfs)')
  dev.off()
  print(paste("Saved file: ", fname, "with URL", furl))
  vahydro_post_metric_to_scenprop(scenprop$pid, 'dh_image_file', furl, 'fig.imp_storage.all', 0.0, ds)


  # Full period Flow duration curve
  datpd <- dat
  fname <- paste(
    save_directory,
    paste0(
      'fig.fdc.all.',
      elid, '.', runid, '.png'
    ),
    sep = '/'
  )
  furl <- paste(
    save_url,
    paste0(
      'fig.fdc.all.',
      elid, '.', runid, '.png'
    ),
    sep = '/'
  )
  png(fname)
  hydroTSM::fdc(
    cbind(datpd$Qin, datpd$Qout),
    ylab="Q (cfs)",
    yaxt="n" # supress labeling till we format
  )

  #legend()
  y_ticks <- round(quantile(datpd$Qin, probs=c(0,0.1,0.25,0.5,1.0)),1)
  y_ticks_fmt <- format(y_ticks, scientific = FALSE)
  axis(2, at = y_ticks, labels = y_ticks_fmt)

  dev.off()
  print(paste("Saved file: ", fname, "with URL", furl))
  vahydro_post_metric_to_scenprop(scenprop$pid, 'dh_image_file', furl, 'fig.fdc.all.', 0.0, ds)


  # Low Elevation Period
  datpd <- elevdatpd
  fname <- paste(
    save_directory,
    paste0(
      'elev90_imp_storage.all.',
      elid, '.', runid, '.png'
    ),
    sep = '/'
  )
  furl <- paste(
    save_url,
    paste0(
      'elev90_imp_storage.all.',
      elid, '.', runid, '.png'
    ),
    sep = '/'
  )
  png(fname)
  plot(datpd$Qin, ylim=c(-0.1,15))
  lines(datpd$Qout,col='blue')
  ymn <- 1
  ymx <- 100
  par(mar = c(5,5,2,5))
  plot(
    datpd$pct_use_remain * 100.0,
    ylim=c(ymn,ymx),
    main="Minimum Modeled Reservoir Storage Period",
    ylab="Reservoir Storage (%)",
    xlab=paste("Model Time Period",l90_elev_start,"to",l90_elev_end)
  )
  par(new = TRUE)
  plot(datpd$Qin,col='blue', axes=FALSE, xlab="", ylab="")
  lines(datpd$Qout,col='green')
  lines(datpd$demand * 1.547,col='red')
  axis(side = 4)
  mtext(side = 4, line = 3, 'Flow/Demand (cfs)')
  dev.off()
  print(paste("Saved file: ", fname, "with URL", furl))
  vahydro_post_metric_to_scenprop(scenprop$pid, 'dh_image_file', furl, 'elev90_imp_storage', 0.0, ds)

}

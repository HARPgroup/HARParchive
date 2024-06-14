# Test datasets CSV
library("sqldf")
library("dataRetrieval")
library("lubridate")

gageid = '01665500' # Culpepper 01667500, Strasburg 01634000, Ruckersville 01665500
hydrocode = paste0('usgs_ws_', gageid)
prism_data <- read.csv(paste0("http://deq1.bse.vt.edu:81/files/met/", hydrocode, "-prism-all.csv"))
prism_data[,c('yr', 'mo', 'da', 'wk')] <- cbind(year(as.Date(prism_data$obs_date)), month(as.Date(prism_data$obs_date)), day(as.Date(prism_data$obs_date)), week(as.Date(prism_data$obs_date)) )

daymet_data <- read.csv(paste0("http://deq1.bse.vt.edu:81/files/met/", hydrocode, "-daymet-all.csv"))
daymet_data[,c('yr', 'mo', 'da', 'wk')] <- cbind(year(as.Date(daymet_data$obs_date)), month(as.Date(daymet_data$obs_date)), day(as.Date(daymet_data$obs_date)), week(as.Date(daymet_data$obs_date)) )

nldas2_data <- read.csv(paste0("http://deq1.bse.vt.edu:81/files/met/", hydrocode, "-nldas2-all.csv"))
nldas2_data[,c('yr', 'mo', 'da', 'wk')] <- cbind(year(as.Date(nldas2_data$obs_date)), month(as.Date(nldas2_data$obs_date)), day(as.Date(nldas2_data$obs_date)), week(as.Date(nldas2_data$obs_date)) )
nldas2_data <- sqldf(
  "select featureid, min(obs_date) as obs_date, yr, mo, da, 
     sum(precip_mm) as precip_mm, sum(precip_in) as precip_in
   from nldas2_data 
   group by yr, mo, da
   order by yr, mo, da
  "
)
# USGS
gage_info <- readNWISsite(gageid)
da <- gage_info$drain_area_va
usgs_data <- memo_readNWISdv(gageid,'00060')
usgs_data[,c('yr', 'mo', 'da')] <- cbind(year(as.Date(usgs_data$Date)), month(as.Date(usgs_data$Date)), day(as.Date(usgs_data$Date)) )


comp_data <- sqldf(
  "select a.obs_date, a.precip_in as prism_p_in, 
  a.yr, a.mo, a.da, a.wk,
  b.precip_in as daymet_p_in, d.precip_in as nldas2_p_in,
  c.X_00060_00003 as usgs_cfs
  from prism_data as a
  left outer join daymet_data as b 
  on (
    a.yr = b.yr
    and a.mo = b.mo
    and a.da = b.da
  )
  left outer join usgs_data as c 
  on (
    a.yr = c.yr
    and a.mo = c.mo
    and a.da = c.da
  )
  left outer join nldas2_data as d 
  on (
    a.yr = d.yr
    and a.mo = d.mo
    and a.da = d.da
  )
  order by a.yr, a.mo, a.da
  "
)
# lag days
comp_data$dataset_day <- index(comp_data) 
comp_data <- sqldf(
  "select a.*, b.usgs_cfs as nextday_usgs_cfs, 
  b.usgs_cfs - a.usgs_cfs as nextday_d_cfs, 
  a.usgs_cfs - c.usgs_cfs as today_d_cfs
  from comp_data as a
  left outer join comp_data as b 
  on (
    a.dataset_day = (b.dataset_day - 1)
  )
  left outer join comp_data as c 
  on (
    a.dataset_day = (c.dataset_day + 1)
  )
  order by a.dataset_day"
)

# Add a cfs Precip 
# acre-feet/day = 3.07 MGD
# cfs = 1.57 MGD
# DAsqmi * 640 ac/sqmi * p_in_per_day / 12.0 = Precip ac-ft/day
# P acft/day / 3.07 = P mgd
# P mgd * 1.572 = P cfs
# 1.572 * (DAsqmi * 640.0 * p_in / 12.0) / 3.07 
comp_data$prism_p_cfs <- 1.572 * (da * 640.0 * comp_data$prism_p_in / 12.0) / 3.07 
comp_data$daymet_p_cfs <- 1.572 * (da * 640.0 * comp_data$daymet_p_in / 12.0) / 3.07 
comp_data$nldas2_p_cfs <- 1.572 * (da * 640.0 * comp_data$nldas2_p_in / 12.0) / 3.07 
week_data <- sqldf(
  "select min(obs_date) as week_begin, yr, wk, min(dataset_day) as dataset_day_begin,
     avg(daymet_p_in) as daymet_p_in, avg(daymet_p_cfs) as daymet_p_cfs,
     avg(prism_p_in) as prism_p_in, avg(prism_p_cfs) as prism_p_cfs,
     avg(nldas2_p_in) as nldas2_p_in, avg(nldas2_p_cfs) as nldas2_p_cfs,
     avg(usgs_cfs) as usgs_cfs, avg(today_d_cfs) as today_d_cfs, 
     avg(nextday_d_cfs) as nextday_d_cfs
   from comp_data
   group by yr, wk
   order by yr, wk
  "
)
week_data$mo <- month(week_data$week_begin)

mod_prism <- lm(usgs_cfs ~ prism_p_cfs, data=comp_data)
summary(mod_prism)

mod_daymet <- lm(usgs_cfs ~ daymet_p_cfs, data=comp_data)
summary(mod_daymet)
# Weekly cfs vs P
mod_week_prism <- lm(usgs_cfs ~ prism_p_cfs, data=week_data)
summary(mod_week_prism)
plot(mod_week_prism$model$usgs_cfs ~ mod_week_prism$model$prism_p_cfs)
mod_week_daymet <- lm(usgs_cfs ~ daymet_p_cfs, data=week_data)
summary(mod_week_daymet)
plot(mod_week_daymet$model$usgs_cfs ~ mod_week_daymet$model$daymet_p_cfs)


# January only
# PRISM
mod_prism_jan <- lm(usgs_cfs ~ prism_p_cfs, data=comp_data[which(comp_data$mo == 1),])
summary(mod_prism_jan)
# daymet
mod_daymet_jan <- lm(usgs_cfs ~ daymet_p_cfs, data=comp_data[which(comp_data$mo == 1),])
summary(mod_daymet_jan)
plot(mod_daymet_jan$model$usgs_cfs ~ mod_daymet_jan$model$daymet_p_cfs)

# January, next day flow todays precip
mod_prism_jan_nd <- lm(nextday_usgs_cfs ~ prism_p_cfs, data=comp_data[which(comp_data$mo == 1),])
summary(mod_prism_jan_nd)

# next day change in flow versus todays P
mod_prism_jan_ndd <- lm(nextday_d_cfs ~ prism_p_cfs, data=comp_data[which(comp_data$mo == 1),])
summary(mod_prism_jan_ndd)
plot(mod_prism_jan_ndd$model$nextday_d_cfs ~ mod_prism_jan_ndd$model$prism_p_cfs)
mod_prism_jan_ndd$model
# next day change in flow versus todays P daymet
mod_daymet_jan_ndd <- lm(nextday_d_cfs ~ daymet_p_cfs, data=comp_data[which(comp_data$mo == 1),])
summary(mod_daymet_jan_ndd)
plot(mod_daymet_jan_ndd$model$nextday_d_cfs ~ mod_daymet_jan_ndd$model$daymet_p_cfs)
mod_daymet_jan_ndd$model

# only compare against change in flows on the same day as non-zero rain
# *** DAYMET
mod_daymet_jan_nz_ndd <- lm(nextday_d_cfs ~ daymet_p_cfs, data=comp_data[which((comp_data$mo == 1) & (comp_data$daymet_p_cfs > 0)),])
summary(mod_daymet_jan_nz_ndd)
plot(mod_daymet_jan_nz_ndd$model$nextday_d_cfs ~ mod_daymet_jan_nz_ndd$model$daymet_p_cfs)
mod_daymet_jan_nz_ndd$model
# *** PRISM
mod_prism_jan_nz_ndd <- lm(nextday_d_cfs ~ prism_p_cfs, data=comp_data[which((comp_data$mo == 1) & (comp_data$prism_p_cfs > 0)),])
summary(mod_prism_jan_nz_ndd)
plot(mod_prism_jan_nz_ndd$model$nextday_d_cfs ~ mod_prism_jan_nz_ndd$model$prism_p_cfs)
mod_prism_jan_nz_ndd$model

# only compare against change in flows on the day after it rained
# *** DAYMET
mod_daymet_jan_nz_ndd <- lm(nextday_d_cfs ~ daymet_p_cfs, data=comp_data[which((comp_data$mo == 1) & (comp_data$daymet_p_cfs > 0)),])
summary(mod_daymet_jan_nz_ndd)
plot(mod_daymet_jan_nz_ndd$model$nextday_d_cfs ~ mod_daymet_jan_nz_ndd$model$daymet_p_cfs)
mod_daymet_jan_nz_ndd$model
# *** PRISM
mod_prism_jan_nz_ndd <- lm(nextday_d_cfs ~ prism_p_cfs, data=comp_data[which((comp_data$mo == 1) & (comp_data$prism_p_cfs > 0)),])
summary(mod_prism_jan_nz_ndd)
plot(mod_prism_jan_nz_ndd$model$nextday_d_cfs ~ mod_prism_jan_nz_ndd$model$prism_p_cfs)
mod_prism_jan_nz_ndd$model

# only compare against change in flows on the day with increasing flow
# *** DAYMET
mod_daymet_jan_nz_cdd <- lm(usgs_cfs ~ daymet_p_cfs, data=comp_data[which((comp_data$mo == 1) & (comp_data$nextday_d_cfs > 0)),])
summary(mod_daymet_jan_nz_cdd)
plot(mod_daymet_jan_nz_ndd$model$nextday_d_cfs ~ mod_daymet_jan_nz_ndd$model$daymet_p_cfs)
mod_daymet_jan_nz_ndd$model
# *** PRISM
mod_prism_jan_nz_ndd <- lm(nextday_d_cfs ~ prism_p_cfs, data=comp_data[which((comp_data$mo == 1) & (comp_data$prism_p_cfs > 0)),])
summary(mod_prism_jan_nz_ndd)
plot(mod_prism_jan_nz_ndd$model$nextday_d_cfs ~ mod_prism_jan_nz_ndd$model$prism_p_cfs)


# *** PRISM - demo february
mod_prism_mon_nz_ndd <- lm(nextday_d_cfs ~ prism_p_cfs, data=comp_data[which((comp_data$mo == 2) & (comp_data$nextday_d_cfs > 0)),])
summary(mod_prism_mon_nz_ndd)
plot(mod_prism_mon_nz_ndd$model$nextday_d_cfs ~ mod_prism_mon_nz_ndd$model$prism_p_cfs)


# Test datasets CSV
library("sqldf")
library("dataRetrieval")
library("lubridate")
library("R6")

basepath='/var/www/R';
source("/var/www/R/config.R")
source("https://raw.githubusercontent.com/HARPgroup/hydro-tools/master/R/fac_utils.R")
source("https://raw.githubusercontent.com/HARPgroup/HARParchive/master/HARP-2024-2025/functions/lm_analysis_plots.R")

gageid = '01613900' # Culpepper 01667500, Strasburg 01634000, Ruckersville 01665500, Mechums 02031000
hydrocode = paste0('usgs_ws_', gageid)
prism_data <- read.csv(paste0("http://deq1.bse.vt.edu:81/met/PRISM/precip/", hydrocode, "_precip_daily.csv"))
prism_data[,c('yr', 'mo', 'da', 'wk')] <- cbind(year(as.Date(prism_data$obs_date)), month(as.Date(prism_data$obs_date)), day(as.Date(prism_data$obs_date)), week(as.Date(prism_data$obs_date)) )

daymet_data <- read.csv(paste0("http://deq1.bse.vt.edu:81/met/daymet/precip/", hydrocode, "_precip_daily.csv"))
daymet_data[,c('yr', 'mo', 'da', 'wk')] <- cbind(year(as.Date(daymet_data$obs_date)), month(as.Date(daymet_data$obs_date)), day(as.Date(daymet_data$obs_date)), week(as.Date(daymet_data$obs_date)) )

nldas2_data <- read.csv(paste0("http://deq1.bse.vt.edu:81/met/nldas2/precip/", hydrocode, "_precip_daily.csv"))
nldas2_data[,c('yr', 'mo', 'da', 'wk')] <- cbind(year(as.Date(nldas2_data$obs_date)), month(as.Date(nldas2_data$obs_date)), day(as.Date(nldas2_data$obs_date)), week(as.Date(nldas2_data$obs_date)) )
nldas2_data <- sqldf(
  "select min(obs_date) as obs_date, yr, mo, da, 
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

mon_lm_stats(week_data, "nldas2_p_cfs", "usgs_cfs", "mo")
nldas2_lm <- mon_lm(week_data, "nldas2_p_cfs", "usgs_cfs", "mo", "nldas2", gageid)
nldas2_lm$plot
mo_data=week_data[which((week_data[,"mo"] == 1)),]
weekmo_data <- lm(mo_data[,"usgs_cfs"] ~ mo_data[,"nldas2_p_cfs"])
dsum <- summary(weekmo_data)
week_data$model <- dsum$coefficients[1,1] + dsum$coefficients[2,1] * week_data$nldas2_p_cfs

plot(weekmo_data)
plot_out$atts$lms[[i]] <- weekmo_data

plot(week_data$usgs_cfs ~ week_data$nldas2_p_cfs)
abline(dsum)

aav <- sqldf(
  "select yr,avg(nldas2_p_cfs) as nldas2, avg(usgs_cfs) as usgs,
   avg(model) as model
   from week_data
   group by yr"
)

barplot(as.matrix(aav[,c('usgs', 'nldas2', 'model')]) ~ aav$yr )

barplot(aav$usgs)
barplot(aav$nldas2, add = TRUE, col = "red")
barplot(aav$nldas2, col = "red")


prism_lm <- mon_lm(week_data, "prism_p_cfs", "usgs_cfs", "mo", "prism", gageid)
prism_lm$plot
# ex: show the July lm for prism
summary(prism_lm$atts$lms[[7]])

daymet_lm <- mon_lm(week_data, "daymet_p_cfs", "usgs_cfs", "mo", "daymet", gageid)
daymet_lm$atts
daymet_lm$plot

all_stats <- cbind(nldas2_lm$atts$stats, prism_lm$atts$stats[,prism_lm$r_col], daymet_lm$atts$stats[,daymet_lm$r_col])
names(all_stats) <- c('mo', 'r_nldas2', 'r_prism', 'r_daymet')
all_ratings <- sqldf(
  "select a.*, 
   CASE
     WHEN r_prism > r_nldas2 and r_prism > r_daymet THEN 'prism'
     WHEN r_nldas2 > r_prism and r_nldas2 > r_daymet THEN 'nldas2'
     WHEN r_daymet > r_nldas2 and r_daymet > r_prism THEN 'daymet'
     ELSE 'nldas2'
   END as best_method
   from all_stats as a
   order by mo
  "
)
# assemble a list of the best datasource for this watershed, monthly
# based on the best fit weekly lm()
best_fit_catch <- sqldf(
  "select a.yr, a.mo, a.da, a.wk, b.best_method,
   CASE 
     WHEN b.best_method = 'prism' THEN a.prism_p_in
     WHEN b.best_method = 'daymet' THEN a.daymet_p_in
     ELSE a.nldas2_p_in
   END as precip_in
   from comp_data as a
   left outer join all_ratings as b
   on (
     a.mo = b.mo
   )
   order by a.yr, a.mo, a.da
  "
)

week_best_fit_catch <- sqldf(
  "select a.yr, a.mo, a.wk, b.best_method,
   CASE 
     WHEN b.best_method = 'prism' THEN a.prism_p_in
     WHEN b.best_method = 'daymet' THEN a.daymet_p_in
     ELSE a.nldas2_p_in
   END as precip_in
   from week_data as a
   left outer join all_ratings as b
   on (
     a.mo = b.mo
   )
   where a.nldas2_p_in is not NULL
   order by a.yr, a.mo, a.wk
  "
)
write.table(
  week_best_fit_catch, 
  paste0(export_path,"/met_weekly_ratings_", gageid, ".csv"), 
  row.names = FALSE,
  sep=","
)

# Now, best_fit_catch has all daily records with an indication of best fit dataset
# How to regenerate?
# Maybe we generate a landseg dataset for prism and daymet, 
# using various disaggregation methods (daily to hourly)
# then, we can just load all three datasets, and 


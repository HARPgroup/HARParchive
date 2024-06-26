# Test datasets CSV
library("sqldf")
library("dataRetrieval")
library("lubridate")

gageid = '02031000' # Culpepper 01667500, Strasburg 01634000, Ruckersville 01665500, Mechums 02031000
hydrocode = paste0('usgs_ws_', gageid)
prism_data <- read.csv(paste0("http://deq1.bse.vt.edu:81/met/PRISM/out/", hydrocode, "-PRISM-all.csv"))
prism_data[,c('yr', 'mo', 'da', 'wk')] <- cbind(year(as.Date(prism_data$obs_date)), month(as.Date(prism_data$obs_date)), day(as.Date(prism_data$obs_date)), week(as.Date(prism_data$obs_date)) )

daymet_data <- read.csv(paste0("http://deq1.bse.vt.edu:81/met/daymet/out/", hydrocode, "-daymet-all.csv"))
daymet_data[,c('yr', 'mo', 'da', 'wk')] <- cbind(year(as.Date(daymet_data$obs_date)), month(as.Date(daymet_data$obs_date)), day(as.Date(daymet_data$obs_date)), week(as.Date(daymet_data$obs_date)) )

nldas2_data <- read.csv(paste0("http://deq1.bse.vt.edu:81/met/out/", hydrocode, "-nldas2-all.csv"))
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

# across all months of the year
# do all months and assemble a barplot of R^2
plotBin <- R6Class(
  "plotBin", 
  public = list(
    plot = NULL, data=list(), atts=list(), r_col='',
    initialize = function(plot = NULL, data = list()){ 
      self.plot = plot; self.data=data; 
    }
  )
)
# Week
mon_lm <- function(sample_data, y_var, x_var, mo_var, data_name, label_name){
  plot_out <- plotBin$new(data = sample_data)
  plot_out$atts$lms <- list()
  nwd_stats <- data.frame(row.names=c('month', 'rsquared_a'))
  for (i in 1:12) {
    mo_data=sample_data[which((sample_data[,mo_var] == i)),]
    weekmo_data <- lm(mo_data[,y_var] ~ mo_data[,x_var])
    plot_out$atts$lms[[i]] <- weekmo_data
    dsum <- summary(weekmo_data)
    nwd_stats <- rbind(nwd_stats, data.frame(i, dsum$adj.r.squared))
  }
  plot_out$atts$stats <- nwd_stats
  bp <- barplot(
    nwd_stats$dsum.adj.r.squared ~ nwd_stats$i,
    ylim=c(0,1.0),
    main=paste("lm(Q ~ P), monthly,",data_name,label_name)
  )
  plot_out$r_col <- paste0('r_', data_name)
  names(plot_out$atts$stats) <- c('mo', plot_out$r_col)
  plot_out$plot <- recordPlot()
  return(plot_out)
}
nldas2_lm <- mon_lm(week_data, "nldas2_p_cfs", "usgs_cfs", "mo", "nldas2", gageid)
nldas2_lm$plot

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


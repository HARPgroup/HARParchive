gageid = '01667500'
prism_data <- read.csv("http://deq1.bse.vt.edu:81/files/met/usgs_ws_01667500-prism-all.csv")
prism_data[,c('yr', 'mo', 'da', 'wk')] <- cbind(year(as.Date(prism_data$obs_date)),
                                                month(as.Date(prism_data$obs_date)),
                                                day(as.Date(prism_data$obs_date)),
                                                week(as.Date(prism_data$obs_date)))
gage_info <- readNWISsite(gageid)
da <- gage_info$drain_area_va
usgs_data <- readNWISdv(gageid,'00060')
usgs_data[,c('yr', 'mo', 'da')] <- cbind(year(as.Date(usgs_data$Date)),
                                         month(as.Date(usgs_data$Date)),
                                         day(as.Date(usgs_data$Date)))

comp_data <- sqldf(
  "select a.obs_date, a.precip_in as prism_p_in, 
  a.yr, a.mo, a.da, a.wk, b.X_00060_00003 as usgs_cfs
  from prism_data as a
  left outer join usgs_data as b 
  on (
    a.yr = b.yr
    and a.mo = b.mo
    and a.da = b.da
  )
  "
)

comp_data$dataset_day <- index(comp_data)

comp_data$nextday_usgs_cfs <- c(comp_data$usgs_cfs[2:nrow(comp_data)],NA)
comp_data$nextday_d_cfs <- comp_data$nextday_usgs_cfs - comp_data$usgs_cfs
comp_data$today_d_cfs <- comp_data$usgs_cfs - c(NA,comp_data$usgs_cfs[1:(nrow(comp_data) - 1)])

comp_data$prism_p_cfs <- 1.572 * (da * 640.0 * comp_data$prism_p_in / 12.0) / 3.07


week_data <- sqldf(
  "select min(obs_date) as week_begin, yr, wk, mo, min(dataset_day) as dataset_day_begin,
     avg(prism_p_in) as prism_p_in, avg(prism_p_cfs) as prism_p_cfs,
     avg(usgs_cfs) as usgs_cfs, avg(today_d_cfs) as today_d_cfs, 
     avg(nextday_d_cfs) as nextday_d_cfs
   from comp_data
   group by yr, wk
   order by yr, wk
  "
)
#Pretty much Everything before this point is just getting the data into this format
#########
week_data$mo <- month(week_data$week_begin)
last_week_data <- sqldf(
  "select min(week_begin) as week_begin, yr, wk, mo, min(dataset_day_begin) as dataset_day_begin,
     Lag(prism_p_in,1) OVER(ORDER BY dataset_day_begin) as lastweek_prism_p_in, 
     Lag(prism_p_cfs,1) OVER(ORDER BY dataset_day_begin) as lastweek_prism_p_cfs,
     usgs_cfs as usgs_cfs, today_d_cfs as today_d_cfs, 
     nextday_d_cfs as nextday_d_cfs
   from week_data
   group by yr, wk
   order by yr, wk
  "
)
last_week_data = last_week_data[-1,]

las_week_ndd_stats <- data.frame(row.names=c('month', 'rsquared_a'))
for (i in 1:12) {
  las_week_prism_mon_nz_ndd <- lm(usgs_cfs ~ lastweek_prism_p_cfs, data=last_week_data[which((last_week_data$mo == i) & (last_week_data$usgs_cfs > 0)),])
  las_week_dsum <- summary(las_week_prism_mon_nz_ndd)
  plot(las_week_prism_mon_nz_ndd$model$usgs_cfs ~ las_week_prism_mon_nz_ndd$model$lastweek_prism_p_cfs)
  las_week_ndd_stats <- rbind(las_week_ndd_stats, data.frame(i, las_week_dsum$adj.r.squared))
}
barplot(las_week_ndd_stats$las_week_dsum.adj.r.squared ~ las_week_ndd_stats$i)
summary(las_week_prism_mon_nz_ndd)

mon_lm <- function(sample_data, y_var, x_var, mo_var, data_name){
  plot_out <- plotBin$new(data = sample_data)
  nwd_stats <- data.frame(row.names=c('month', 'rsquared_a'))
  for (i in 1:12) {
    mo_data=sample_data[which((sample_data[,mo_var] == i)),]
    weekmo_data <- lm(mo_data[,y_var] ~ mo_data[,x_var])
    dsum <- summary(weekmo_data)
    nwd_stats <- rbind(nwd_stats, data.frame(i, dsum$adj.r.squared))
  }
  plot_out$atts[['stats']] <- nwd_stats
  barplot(
    nwd_stats$dsum.adj.r.squared ~ nwd_stats$i,
    ylim=c(0,1.0),
    main=paste("lm(Q ~ P), monthly,",data_name)
  )
  plot_out$plot <- recordPlot()
  return(plot_out)
}

lasweek_lm <- mon_lm(last_week_data, "lastweek_prism_p_cfs", "usgs_cfs", "mo", "prism")
lasweek_lm$atts
lasweek_lm$plot
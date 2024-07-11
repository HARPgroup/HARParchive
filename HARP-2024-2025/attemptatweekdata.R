
#This dataset takes in the gageid, dataset, and file location and outputs
#a csv of the dataset's weekly data
dataset_creation <- function(gageid, dataset, write_file_location){
#takes in datset and gage id and created the precip_data dataset
precip_data <- read.csv(paste0("http://deq1.bse.vt.edu:81/met/", dataset, "/out/usgs_ws_", gageid, "-", dataset, "-all.csv"))
precip_data[,c('yr', 'mo', 'da', 'wk')] <- cbind(year(as.Date(precip_data$obs_date)),
                                                month(as.Date(precip_data$obs_date)),
                                                day(as.Date(precip_data$obs_date)),
                                                week(as.Date(precip_data$obs_date)))
#havent been able to test because can't use nldas, but changes nldas to daily
if (dataset == "nldas2"){
  precip_data <- sqldf(
    "select featureid, min(obs_date) as obs_date, yr, mo, da, 
     sum(precip_mm) as precip_mm, sum(precip_in) as precip_in
   from precip_data 
   group by yr, mo, da
   order by yr, mo, da
  "
  )
}


gage_info <- readNWISsite(gageid)
#Extract the drainage area of the gage
da <- gage_info$drain_area_va
#Get daily, mean streamflow (pcode 000060, see dataRetrieval::parameterCdFile)
#from the gage specified in gageid
usgs_data <- readNWISdv(gageid,'00060')
#Extract date information from the gage using lubridate as above
usgs_data[,c('yr', 'mo', 'da')] <- cbind(year(as.Date(usgs_data$Date)),
                                         month(as.Date(usgs_data$Date)),
                                         day(as.Date(usgs_data$Date)))

comp_data <- sqldf(
  "select a.obs_date, a.precip_in as  dataset_p_in, 
  a.yr, a.mo, a.da, a.wk,
  b.X_00060_00003 as usgs_cfs
  from precip_data as a
  left outer join usgs_data as b 
  on (
    a.yr = b.yr
    and a.mo = b.mo
    and a.da = b.da
  )
  order by a.yr, a.mo, a.da
  "
)

comp_data$dataset_day <- index(comp_data) 

comp_data <- sqldf(
  "select b.*,
  b.nextday_usgs_cfs - b.usgs_cfs as nextday_d_cfs,
  b.usgs_cfs - b.yesterday_usgs_cfs as today_d_cfs
  FROM (
    select 
    a.*,
    Lag(a.usgs_cfs,1) OVER(ORDER BY a.dataset_day) as yesterday_usgs_cfs,
    Lag(a.usgs_cfs,-1) OVER(ORDER BY a.dataset_day) as nextday_usgs_cfs
    from comp_data as a
    order by a.dataset_day
  ) as b
  "
)
# Add a cfs Precip column using the following conversions for EACH of the precip
# data sources:
# acre-feet/day = 3.07 MGD
# cfs = 1.57 MGD
# DAsqmi * 640 ac/sqmi * p_in_per_day / 12.0 = Precip ac-ft/day
# P acft/day / 3.07 = P mgd
# P mgd * 1.572 = P cfs
# 1.572 * (DAsqmi * 640.0 * p_in / 12.0) / 3.07 
comp_data$precip_cfs <- 1.572 * (da * 640.0 * comp_data$dataset_p_in / 12.0) / 3.07 

#converts our daily data into weekly
week_data <- sqldf(
  "select min(obs_date) as week_begin, yr, wk, min(dataset_day) as dataset_day_begin,
     avg(dataset_p_in) as dataset_p_in, avg(precip_cfs) as dataset_p_cfs,
     avg(usgs_cfs) as usgs_cfs, avg(today_d_cfs) as today_d_cfs, 
     avg(nextday_d_cfs) as nextday_d_cfs
   from comp_data
   group by yr, wk
   order by yr, wk
  "
)
#Base the month column off of the min day oindex for that week, or the day that
#week began
week_data$mo <- month(week_data$week_begin)


write.csv(week_data,write_file_location)

}


dataset_creation(gageid="01660400", dataset="PRISM",
                 write_file_location= "~/HarpData/HARParchive/HARP-2024-2025/attempt.csv")                                    



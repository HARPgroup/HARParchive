# Test datasets CSV
library("sqldf")
library("dataRetrieval")

gageid = '01634000'
hydrocode = paste0('usgs_ws_', gageid)
prism_data <- read.csv("http://deq1.bse.vt.edu:81/files/met/usgs_ws_01634000-prism-2022-2023.csv")
prism_data[,c('yr', 'mo', 'da')] <- cbind(year(as.Date(prism_data$obs_date)), month(as.Date(prism_data$obs_date)), day(as.Date(prism_data$obs_date)) )

daymet_data <- read.csv("http://deq1.bse.vt.edu:81/files/met/usgs_ws_01634000-daymet-2022-2023.csv")
daymet_data[,c('yr', 'mo', 'da')] <- cbind(year(as.Date(daymet_data$obs_date)), month(as.Date(daymet_data$obs_date)), day(as.Date(daymet_data$obs_date)) )
# USGS
gage_info <- memo_readNWISsite(gageid)
da <- gage_info$drain_area_va
usgs_data <- memo_readNWISdv(gageid,'00060')
usgs_data[,c('yr', 'mo', 'da')] <- cbind(year(as.Date(usgs_data$Date)), month(as.Date(usgs_data$Date)), day(as.Date(usgs_data$Date)) )

comp_data <- sqldf(
  "select a.obs_date, a.precip_in as prism_p_in, 
  b.precip_in as daymet_p_in, c.X_00060_00003 as usgs_cfs
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
  "
)

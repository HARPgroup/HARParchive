#Function to calculate a minimum value in timeseries data over a period of time 
library(zoo)
library(stats)

#ts_data : timeseries data, either a df or zoo
#colname : column name of interest in ts_data 
#start/end_date : dates to trim ts_data by 
#timestep in ts_data = unit of critical_pd_length
#critical_pd_length: not used currently

fn_get_pd_min <- function(ts_data, critical_pd_length, start_date, end_date, colname) {

  #format dates given 
  start_date <- as.POSIXct(start_date)
  end_date <- as.POSIXct(end_date)  
  
  data_class <- class(ts_data) #get class of ts data
  
  if (data_class != "zoo") { 
    ts_zoo <- zoo(ts_data, order.by = index(ts_data)) #transform data to zoo if not already 
  } else {
    ts_zoo <- ts_data
  }
  
  ts_crop <- window(ts_zoo, start = start_date, end = end_date) #trim timeseries data by start and end dates
  
  ts_crop <- as.data.frame(ts_crop) #convert to df from zoo
  
  class(ts_crop[,colname]) <- "numeric" 
  
  #get minimum value within trimmed timeseries data 
  minval <- sqldf(paste0("SELECT min(", colname ,")
                      FROM ts_crop"))
  
  minval <- as.numeric(minval[1,1])

  return(minval)
}
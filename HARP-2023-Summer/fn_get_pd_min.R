#Function to calculate a minimum value in timeseries data over a period of time 
library(zoo)
library(stats) #for window
library(sqldf)

#' fn_get_pd_min()
#' @description Calculates a minimum value of a specified column in timeseries data over a period of time
#' @param ts_data Timeseries data, either a data frame or zoo
#' @param start_date Start of period to trim ts_data by
#' @param end_date End of period to trim ts_data by
#' @param colname Column name of interest in ts_data, which minimum will be found for 
#' @return Value for volume in acre-feet
#' @import zoo, stats, sqldf
#' @export fn_get_pd_min
fn_get_pd_min <- function(ts_data, start_date, end_date, colname) {

  #format dates provided
  start_date <- as.POSIXct(start_date)
  end_date <- as.POSIXct(end_date)  
  
  #change data class to zoo if needed
  data_class <- class(ts_data) 
  if (data_class != "zoo") { 
    ts_zoo <- zoo(ts_data, order.by = index(ts_data))
  } else {
    ts_zoo <- ts_data
  }
  
  #trim data by start and end dates
  ts_crop <- window(ts_zoo, start = start_date, end = end_date) 
  ts_crop <- as.data.frame(ts_crop) #convert to df from zoo
  class(ts_crop[,colname]) <- "numeric" 
  
  #get minimum value within trimmed data 
  minval <- sqldf(paste0("SELECT min(", colname ,")
                      FROM ts_crop"))
  minval <- as.numeric(minval[1,1])

  return(minval)
}
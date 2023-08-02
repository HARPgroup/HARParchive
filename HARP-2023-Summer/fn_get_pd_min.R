#Function to calculate Smin_CPL
library(zoo)
library(stats)

fn_get_pd_min <- function(ts_data, critical_pd_length, date_filter, colname) {
#timestep in ts_data = unit of critical_pd_length
  
#format dates 
startdate <- as.POSIXct(date_filter[1])
enddate <- as.POSIXct(date_filter[2])  

ts_zoo <- zoo(ts_data, order.by = index(ts_data)) #create zoo to filter by date 

ts_crop <- window(ts_zoo, start = startdate, end = enddate) #crop by date - stats package

ts_crop <- as.data.frame(ts_data) #convert to df from zoo

class(ts_crop[,colname]) <- "numeric" #data for rollmean needs to be numeric

rollmeans <- rollmean(ts_crop[,colname], k = critical_pd_length) #get all x-day averages using rollmean
min_pd <- min(rollmeans) #get lowest of all x-day means
    
return(min_pd)
}
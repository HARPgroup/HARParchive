#Function to calculate Smin_CPL
library(zoo)
library(stats)

fn_get_pd_min <- function(ts_data, critical_pd_length, date_filter, colname) {

#format dates 
startdate <- as.POSIXct(date_filter[1])
enddate <- as.POSIXct(date_filter[2])  

#create zoo to filter by date  
ts_zoo <- zoo(ts_data, order.by = index(ts_data))

#filter by date
ts_filtered <- window(ts_zoo, start = startdate, end = enddate)

ts_filtered <- as.data.frame(ts_data)

#need to make sure data for mean is numeric
class(ts_filtered[,colname]) <- "numeric"

rollmeans <- rollmean(ts_filtered[,colname], k = critical_pd_length) #get all x-day averages using rollmean
min_pd <- min(rollmeans) #get lowest of all x-day means
    
return(min_pd)
}
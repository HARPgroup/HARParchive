#Function to calculate Smin_CPL
library(zoo)
library(stats)

#start_date and end_date will be set according to the low-flow year l90_year/l30_year before this function will be called 
#so, we'll be finding the Smin over the time period between start and end dates 
fn_get_pd_min <- function(ts_data, critical_pd_length, start_date, end_date, colname) {
#timestep in ts_data = unit of critical_pd_length
  
#format dates 
start_date <- as.POSIXct(start_date)
end_date <- as.POSIXct(end_date)  

data_class <- class(ts_data) #get class of ts data

if (data_class != "zoo") { #transform to zoo if not already
  ts_zoo <- zoo(ts_data, order.by = index(ts_data)) #create zoo to filter by date 
} else {
  ts_zoo <- ts_data
}

ts_crop <- window(ts_zoo, start = start_date, end = end_date) #crop by start and end dates

ts_crop <- as.data.frame(ts_crop) #convert to df from zoo

class(ts_crop[,colname]) <- "numeric" #data for rollmean needs to be numeric

#use sqldf to get minimum storage within the entire ts_crop, which is cropped by date 
Smin_CPL <- sqldf(paste0("SELECT min(", colname ,")
                    FROM ts_crop"))

assign(paste0('Smin_L', critical_pd_length), as.numeric(Smin_CPL$`min(Storage)`[1]))

return(get(paste0('Smin_L', critical_pd_length)))
}
# Function to classify flows for a selected period as 
# either OVER or UNDER the mean min from X number of days min flow for that period
# e.g. if mean 90 day min flow = 30 cfs, any flow over 30 is marked as over
# gageid = USGS Gage to be classified
# daynum = numeric (1, 3, 7, 30, or 90)
# startDate = start date of period as "YYYY-MM-DD"
# endDate = end date of period as "YYYY-MM-DD"
classify.drought <- function(gageid, daynum, startDate="1925-01-01", endDate="2024-12-31"){
# Require packages
  require(dataRetrieval)
  require(lubridate)
  require(zoo)
  require(hydrotools)
  require(sqldf)
  
# Get data from data retrieval
flow_data <-  dataRetrieval::readNWISdv(gageid,
                                        parameterCd = "00060",
                                        startDate = startDate,
                                        endDate = endDate)
flow_data <- dataRetrieval::renameNWISColumns(flow_data)

# Add helpful date columns
flow_data$Month <- month(flow_data$Date)
flow_data$Year <- year(flow_data$Date)

# Convert flows to zoo
flows_zoo <- zoo::as.zoo(x=flow_data$Flow)
zoo::index(flows_zoo) <- flow_data$Date
# Use group 2 to get low and high flows
flows <- as.data.frame(hydrotools::group2(flows_zoo,"water",mimic.tnc = TRUE))

# Get averget()# Get average from each min column
min_1 = mean(flows$`1 Day Min`)
min_3 = mean(flows$`3 Day Min`)
min_7 =  mean(flows$`7 Day Min`)
min_30 = mean(flows$`30 Day Min`)
min_90 = mean(flows$`90 Day Min`)

# correctly set num vairable
daynum <- ifelse(daynum==1, min_1,
                 ifelse(daynum==3, min_3,
                        ifelse(daynum==7, min_7,
                               ifelse(daynum==30, min_30,
                                      ifelse(daynum==90, min_90)))))

# Creete dat frame
class <- sqldf(sprintf(
  "select Date, Year, Month, Flow, 'Under' as Class 
  from flow_data where Flow < %f
  union all
  select Date, Year, Month, Flow, 'Over' as Class
  from flow_data where Flow > %f
  order by Date
  ", daynum, daynum))

return(class)

}

# Function to create a summary table of periods determined as a drought from classify.drought
# result = dataframe output from classify.drought
# max_drought_days = number of days with "drought" conditions in a row considered to be real drought
#   e.g. max_drought_days = 7 would remove any "drought" periods less than 7 days long
create.drought.sum <- function(classify_result, max_drought_days){
# Require packages
  require(sqldf)

# identify oeriods where class is under
droughts <- rle(classify_result$Class == "Under")

# Create  event IDs
event_ids <- cumsum(c(1, diff(which(classify_result$Class == "Under")) != 1))

# Assign drought ids
classify_result$Drought_ID <- NA
classify_result$Drought_ID[which(classify_result$Class == "Under")] <- event_ids

# get subset with only dorught event data
drought_data <- subset(classify_result, !is.na(Drought_ID))

drought_event_sum <- data.frame(matrix(,nrow=max(result$Drought_ID, na.rm=T), ncol=0))

# create columns from drought data
start_dates <- aggregate(drought_data$Date ~ drought_data$Drought_ID, FUN = min)
end_dates <- aggregate(drought_data$Date ~ drought_data$Drought_ID, FUN = max)
durations <- aggregate(drought_data$Date ~ drought_data$Drought_ID, FUN = length)
mean_flows <- aggregate(drought_data$Flow ~ drought_data$Drought_ID, FUN = mean)

# Format aggregates into the dataframe
drought_event_sum$drought_id <- start_dates$`drought_data$Drought_ID`
drought_event_sum$start_date <- start_dates$`drought_data$Date`
drought_event_sum$end_date <- end_dates$`drought_data$Date`
drought_event_sum$ndays <- durations$`drought_data$Date`
drought_event_sum$mean_flow <- mean_flows$`drought_data$Flow`

#Add helpfull date columns
drought_event_sum$period_drought_days <- sum(drought_event_sum$ndays)

# Remove any droughts lasting less than input number of days
drought_event_sum <- sqldf(sprintf(
  "select * from drought_event_sum where ndays > %f
  ", max_drought_days))


return(drought_event_sum)
}

# Function which performs gorup2 funcitons on a gage and 
# normalizes columns of choice fo watershed drainage area
# gageid = gage to perform group 2 function on
# col_to_norm =  group 2 column to be normalized "X Day Min"
perform.group2 <- function(gageid, col_to_norm){
  # Get data from input site
  flows <- dataRetrieval::readNWISdv(gageid,
                                     parameterCd = "00060")
  flows <- dataRetrieval::renameNWISColumns(flows)
  
  # Get site info
  info <- dataRetrieval::readNWISsite(gageid)
  # Extract Drainage area
  da <- info$drain_area_va
  # convert da to ft2 from mi2
  da <- 5280*5280*da
  
  # Convert flows to zoo
  flows_zoo <- zoo::as.zoo(x=flows$Flow)
  zoo::index(flows_zoo) <- flows$Date
  # Use group 1 to get minimum monthly flows
  flows <- as.data.frame(hydrotools::group2(flows_zoo,"water",mimic.tnc = TRUE))
  
  
  query <- paste0("select year, \"", col_to_norm, "\" as reg_flow from flows")
  
  flows <- sqldf::sqldf(query)
  
  flows$specific_flow <- flows$reg_flow/da
  
  #convert specific flow from ft/s to in/day
  flows$specific_flow <- flows$specific_flow*86400*12
  
  # create column for monthly and yearly values
  flows$monthly <- flows$specific_flow*30
  flows$yearly <- flows$specific_flow*365
  
  
  return(flows)
}

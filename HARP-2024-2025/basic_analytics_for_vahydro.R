#Creates Summary analytic functions for workflow
#Use for VA HYDRO metrics
library(tidyr)
library(sqldf)

## For PRISM data

prism_summary_analytics <- function(df){
df <- 
  separate (data = df, 
            col = obs_date, 
            into = c("obs_date","obs_time"),
            sep = " ")
df <- 
  separate (data = df, 
            col = obs_date, 
            into = c("obs_year","obs_month","obs_day"), 
            sep = "-")

#creating a yearly summary with each year and its total precip
yearly.summary <- 
  sqldf(
  "SELECT obs_year, SUM(precip_in) AS total_precip
  FROM df
  GROUP BY obs_year"
)

#summary analytics

precip_annual_max_in <- 
  max(yearly.summary$total_precip)

precip_annual_max_year <- 
  yearly.summary$obs_year[which.max(yearly.summary$total_precip)]

precip_annual_mean_in <- 
  mean(yearly.summary$total_precip)


#For min values and years, we can exclude the last row since the current year 
#is incomplete data

precip_annual_min_in <- 
  min(yearly.summary$total_precip[-nrow(yearly.summary)])
precip_annual_min_year <- 
  yearly.summary$obs_year[which.min
                          (yearly.summary$total_precip[-nrow
                                                       (yearly.summary)])]


precip_daily_max_in <- max(df$precip_in)
precip_hourly_max_in <- max(df$precip_in)/24 
#precip_hourly_max_in done using interpolation (not super realistic). 
#Alternatively, we could look at the rainfall distribution table for a 
#type II storm and multiply by the max P(t)/P(24) value.

#not sure about last metric, 190_precip_in. 
print(c(precip_annual_max_in, precip_annual_max_year, precip_annual_mean_in,
        precip_annual_min_in, precip_annual_min_year, precip_daily_max_in,
        precip_hourly_max_in)) #move into dataframe with 7 columns and 1 row
}

# For Daymet

daymet_summary_analytics <- function(df){
  df <- 
    separate (data = df, 
              col = obs_date, 
              into = c("obs_date","obs_time"),
              sep = " ")
  df <- 
    separate (data = df, 
              col = obs_date, 
              into = c("obs_year","obs_month","obs_day"), 
              sep = "-")
  
  #creating a yearly summary with each year and its total precip
  yearly.summary <- 
    sqldf(
      "SELECT obs_year, SUM(precip_in) AS total_precip
  FROM df
  GROUP BY obs_year"
    )
  
  #summary analytics
  
  precip_annual_max_in <- 
    max(yearly.summary$total_precip)
  
  precip_annual_max_year <- 
    yearly.summary$obs_year[which.max(yearly.summary$total_precip)]
  
  precip_annual_mean_in <- 
    mean(yearly.summary$total_precip)
  
  
  #For min values and years, we can exclude the last row since the current year 
  #is incomplete data
  
  precip_annual_min_in <- 
    min(yearly.summary$total_precip[-nrow(yearly.summary)])
  precip_annual_min_year <- 
    yearly.summary$obs_year[which.min
                            (yearly.summary$total_precip[-nrow
                                                         (yearly.summary)])]
  
  
  precip_daily_max_in <- max(df$precip_in)
  precip_hourly_max_in <- max(df$precip_in)/24 
  #precip_hourly_max_in done using interpolation (not super realistic). 
  #Alternatively, we could look at the rainfall distribution table for a 
  #type II storm and multiply by the max P(t)/P(24) value.
  
  #not sure about last metric, 190_precip_in. 
  print(c(precip_annual_max_in, precip_annual_max_year, precip_annual_mean_in,
          precip_annual_min_in, precip_annual_min_year, precip_daily_max_in,
          precip_hourly_max_in))
}

# For nldas2

nldas2_summary_analytics <- function(df){
  df <- 
    separate (data = df, 
              col = obs_date, 
              into = c("obs_date","obs_time"),
              sep = " ")
  df <- 
    separate (data = df, 
              col = obs_date, 
              into = c("obs_year","obs_month","obs_day"), 
              sep = "-")
  
  #creating a yearly summary with each year and its total precip
  yearly.summary <- 
    sqldf(
      "SELECT obs_year, SUM(precip_in) AS total_precip
  FROM df
  GROUP BY obs_year"
    )
  
  #summary analytics
  
  precip_annual_max_in <- 
    max(yearly.summary$total_precip)
  
  precip_annual_max_year <- 
    yearly.summary$obs_year[which.max(yearly.summary$total_precip)]
  
  precip_annual_mean_in <- 
    mean(yearly.summary$total_precip)
  
  
  #For min values and years, we can exclude the last row since the current year 
  #is incomplete data
  
  precip_annual_min_in <- 
    min(yearly.summary$total_precip[c(-nrow(yearly.summary),-1)])
  precip_annual_min_year <- 
    yearly.summary$obs_year[which.min
                            (yearly.summary$total_precip[c(-nrow
                                                         (yearly.summary),-1)])]
  daily.summary <- 
    sqldf(
      "SELECT obs_year, obs_month, obs_day, SUM(precip_in) AS total_precip
  FROM df
  GROUP BY obs_year, obs_month, obs_day"
    )
  
  precip_daily_max_in <- max(daily.summary$total_precip[-1])
  
  precip_hourly_max_in <- max(df$precip_in)

  
  #not sure about last metric, 190_precip_in. 
  print(c(precip_annual_max_in, precip_annual_max_year, precip_annual_mean_in,
          precip_annual_min_in, precip_annual_min_year, precip_daily_max_in,
          precip_hourly_max_in))
}


metric <- c("precip_annual_max_in", 'precip_annual_max_year', 'precip_annual_mean_in',
            'precip_annual_min_in', 'precip_annual_min_year', 'precip_hourly_max_in',
            'precip_daily_max_in')
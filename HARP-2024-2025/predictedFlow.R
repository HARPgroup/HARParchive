# Load necessary packages 
suppressPackageStartupMessages(library(R6))
suppressPackageStartupMessages(library(sqldf))

# Source functions
source("https://raw.githubusercontent.com/HARPgroup/meta_model/master/scripts/precip/lm_analysis_plots.R")

# Import model json from server
gage_ratings_ex <- plotBin$new()
gage_ratings_ex$fromJSON("http://deq1.bse.vt.edu:81/met/stormVol_prism/stats/usgs_ws_01615000-PRISM-storm_volume-model.json", TRUE)

# Import precip data from server
precip_data_ex <- read.csv("http://deq1.bse.vt.edu:81/met/stormVol_prism/precip/usgs_ws_01615000-PRISM-all.csv")

# Import flow data from server
flow_data_ex <- read.csv("http://deq1.bse.vt.edu:81/met/stormVol_prism/flow/usgs_ws_01615000-flow.csv")



# Attempt for predicted flow
predict.flow <- function(precip_data, gage_ratings, month){
  # Required Packages
  require(R6)
  require(sqldf)
  #Extracting coefficients
  print("Extracting coefficients")
  month <- as.numeric(month)
  coefficients <- gage_ratings$atts$lms[[month]]$coefficients
  intercept <- coefficients[1]
  mo_data <- coefficients[2]
  # Getting Data from the correct month
  print("Obtaining data from input month")
  precip_data <- subset(precip_data, mo %in% month )
  # Inserting predicted flow into precip data frame (guessing column name? Units?)
  print("Calculating predicted flow")
  precip_data$predicted_flow <- mo_data*precip_data$precip_in + intercept
  # New Data frame with desired columns
  print("Creating dataframe with predicted flow")
  predicted_data <- sqldf(
    "Select a.obs_date, a.mo, a.precip_in, a.predicted_flow
    from precip_data as a"
  )
  return(predicted_data)
}

# Test with different months

jan_predicted <- predict.flow(precip_data_ex, gage_ratings_ex, 1)

feb_predicted <- predict.flow(precip_data_ex, gage_ratings_ex, 2)

# Attempt including Error
predict.flow.error <- function(precip_data, flow_data, gage_ratings, month){
  # Required Packages
  require(R6)
  require(sqldf)
  require(lubridate)
  # Cleaning data
  precip_data$obs_date <- as.Date(precip_data$obs_date)
  flow_data$obs_date <- as.Date(flow_data$obs_date)
  #Extracting coefficients
  month <- as.numeric(month)
  coefficients <- gage_ratings$atts$lms[[month]]$coefficients
  intercept <- coefficients[1]
  mo_data <- coefficients[2]
  # Getting Data from the correct month
  precip_data <- subset(precip_data, mo %in% month )
  # Inserting predicted flow into precip data frame (guessing column name? Units?)
  precip_data$predicted_flow <- mo_data*precip_data$precip_in + intercept
  # New Data frame with desired columns
  predicted_data <- sqldf(
    "Select a.obs_date, a.mo, a.precip_in, 
    a.predicted_flow, b.obs_flow
    from precip_data as a
    left outer join flow_data as b
    on(
      a.obs_date = b.obs_date
    )
    "
  )
  # Create column for error and rating
  predicted_data$error <- predicted_data$predicted_flow - predicted_data$obs_flow
  predicted_data$rating <- 1 - ((abs(predicted_data$obs_flow - predicted_data$predicted_flow))/predicted_data$obs_flow)
  
  return(predicted_data)
}

flow_error_ex <- predict.flow.error(precip_data_ex, flow_data_ex, gage_ratings_ex, 1)

mean(flow_error_ex$rating)

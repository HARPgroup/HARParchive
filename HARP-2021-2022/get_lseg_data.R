## This function downloads NLDAS land segment data from deq4
## It generates data frames for temperature, precipitation, and solar radiation data
## @input: A land segment
## @output: A list with 3 data frames of time series NLDAS data for inputted land segment, one for each metric
## The dataframes follow column format year, month, day, hour, value, date (yyyy-mm-dd)

## HARP group
## Last updated: 7/27/2021


# load necessary packages
library(sqldf)
library(lubridate)
library(data.table)

get_lseg_data_met <- function(landseg,met_comp,date_range="all_data", metsite="http://deq1.bse.vt.edu:81") {
  # read in land segment radiation data
  dfDAT <- data.table::fread(
    paste0(metsite,"/met/out/lseg_csv/", date_range, "/", landseg, ".", met_comp), 
    header = FALSE, sep = ","
  )
  colnames(dfDAT) = c("year","month","day","hour",met_comp)
  dfDAT$date <- as.Date(paste(dfDAT$year,dfDAT$month,dfDAT$day, sep="-"))
  return(dfDAT)
}

# creating get_lseg_data function
get_lseg_data <- function(landseg, date_range="all_data", metsite="http://deq1.bse.vt.edu:81"){
    
  # read in land segment radiation data
  dfRAD <- get_lseg_data_met(landseg,"RAD", date_range,metsite)

  # read in land segment temperature data
  dfTMP <- get_lseg_data_met(landseg,"TMP", date_range,metsite)

  # read in land segment precipitation data
  dfPRC <- get_lseg_data_met(landseg,"PRC", date_range,metsite)
    
  # returning data frames in list format
  lseg_data <- list(
    dfRAD = dfRAD, 
    dfTMP = dfTMP, 
    dfPRC = dfPRC
  )

  return(lseg_data)
  
}

# use
# dfRAD, dfTMP, and dfPRC are used as inputs to the get_lseg_summary_stats() function
landseg <- "A51197"
lseg_data <- get_lseg_data(landseg = landseg)
dfRAD <- lseg_data$dfRAD
dfTMP <- lseg_data$dfTMP
dfPRC <- lseg_data$dfPRC

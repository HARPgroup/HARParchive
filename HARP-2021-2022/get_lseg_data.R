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

# creating get_lseg_data function
get_lseg_data <- function(landseg){
    
    # read in land segment radiation data
    dfRAD <- read.table(paste0("http://deq1.bse.vt.edu:81/met/out/lseg_csv/1984010100-2020123123/",landseg,".RAD"), header = FALSE, sep = ",")
    colnames(dfRAD) = c("year","month","day","hour","RAD")
    dfRAD$date <- as.Date(paste(dfRAD$year,dfRAD$month,dfRAD$day, sep="-"))
    
    # read in land segment temperature data
    dfTMP <- read.table(paste0("http://deq1.bse.vt.edu:81/met/out/lseg_csv/1984010100-2020123123/",landseg,".TMP"), header = FALSE, sep = ",")
    colnames(dfTMP) = c("year","month","day","hour","temp")
    dfTMP$date <- as.Date(paste(dfTMP$year,dfTMP$month,dfTMP$day, sep="-"))
    
    # read in land segment precipitation data
    dfPRC <- read.table(paste0("http://deq1.bse.vt.edu:81/met/out/lseg_csv/1984010100-2020123123/",landseg,".PRC"), header = FALSE, sep = ",")
    colnames(dfPRC) = c("year","month","day","hour","precip")
    dfPRC$date <- as.Date(paste(dfPRC$year,dfPRC$month,dfPRC$day, sep="-"))
    
    # returning data frames in list format
    lseg_data <- list(dfRAD = dfRAD, 
                      dfTMP = dfTMP, 
                      dfPRC = dfPRC)
    return(lseg_data)
  
}

# use
# dfRAD, dfTMP, and dfPRC are used as inputs to the get_lseg_summary_stats() function
landseg <- "A51197"
lseg_data <- get_lseg_data(landseg = landseg)
dfRAD <- lseg_data$dfRAD
dfTMP <- lseg_data$dfTMP
dfPRC <- lseg_data$dfPRC

# Download hourly data
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(sqldf))
suppressPackageStartupMessages(library(ggplot2))

# H51165 <- read.csv("http://deq1.bse.vt.edu:81/p6/out/land/subsheds2/pwater/forH51165_pwater.csv")
# N51165 <- read.csv("http://deq1.bse.vt.edu:81/p6/out/land/subsheds2/pwater/forN51165_pwater.csv")
# N51171 <- read.csv("http://deq1.bse.vt.edu:81/p6/out/land/subsheds2/pwater/forN51171_pwater.csv")
# N54031 <- read.csv("http://deq1.bse.vt.edu:81/p6/out/land/subsheds2/pwater/forN54031_pwater.csv")

# Make daily ----
make.model.daily <- function(data, datecol){
  # Packages
  require(lubridate)
  require(sqldf)
  
  rates <- c("AGWET", "AGWI", "AGWLI", "AGWO", "BASET", "CEPE",
             "IFWI", "IFWLI", "IFWO", "IGWI", "INFIL", "LZET",
             "LZI", "LZLI", "PERC", "PERO", "PET", "SUPY", "SURI",
             "SURLI", "SURO", "TAET", "UZET", "UZI", "UZLI")
  
  lengths_etc <- c("AGWS", "CEPS", "GWVS", "IFWS", "INFFAC", 
                   "LZS", "PERS", "PETADJ", "SURS", "TGWS", "UZS",
                   "year", "month", "day")
  
  # create date column to be used
  if (datecol %in% colnames(data)){
    data$date <- as_date(data[[datecol]])
  } else {
    print("Could not find input date column")
  }
  
  # Apply different functions tot he columns to aggregate to daily
  print("Aggregating rates into daily values")
  rates_daily <- aggregate(data[rates], by = list(Date = data$date), FUN = sum)
  
  print("Aggregating lengths and constants into daily values")
  lengths_etc_daily <- aggregate(data[lengths_etc], by = list(Date = data$date), FUN = mean)
  
  # Combine rates and lengths into one dat frame
  print("Creating new daily data frame")
  data_daily <- sqldf(
    "select * from rates_daily as a
    inner join lengths_etc_daily as b 
    on (
      a.date = b.date
    )
    ")
  
  # Put back in alphabetical order with Date as first column
  data_daily <- data_daily[c("Date", sort(setdiff(names(data_daily), "Date")))]
  
  return(data_daily)
}

# H51165_daily <- make.model.daily(H51165, "index")
# N51165_daily <- make.model.daily(N51165, "index")
# N51171_daily <- make.model.daily(N51171, "index")
# N54031_daily <- make.model.daily(N54031, "index")

# Calculation for 0s in data
model_data <- subset(H51165, year > 1999  & year < 2006)

model_data$index <- as_datetime(model_data$index)

model_precalc <- model_data

# create column for change in storage
model_data$dAGWS <- c(NA, diff(model_data$AGWS))

# Use outflows and inflows to calculate Lateral inflow
model_data$AGWI <- model_data$dAGWS + model_data$AGWO + model_data$AGWET

# Change any calculated values < 0 to 0
model_data$AGWI[model_data$AGWI < 0] <- 0

# Repeat for Upper and Lower Zones
model_data$dLZS <- c(NA, diff(model_data$LZS))

model_data$LZI <- model_data$dLZS + model_data$LZET - model_data$PERC

model_data$LZI[model_data$LZI < 0] <- 0

# Upper
model_data$dUZS <- c(NA, diff(model_data$UZS))

model_data$UZI <- model_data$dUZS + model_data$UZET + model_data$PERC

model_data$UZI[model_data$UZI < 0 ] <- 0 
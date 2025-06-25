# Download hourly data
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(sqldf))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggpubr))
suppressPackageStartupMessages(library(gridExtra))

H51165 <- read.csv("http://deq1.bse.vt.edu:81/p6/out/land/subsheds2/pwater/forH51165_pwater.csv")
N51165 <- read.csv("http://deq1.bse.vt.edu:81/p6/out/land/subsheds2/pwater/forN51165_pwater.csv")
N51171 <- read.csv("http://deq1.bse.vt.edu:81/p6/out/land/subsheds2/pwater/forN51171_pwater.csv")
N54031 <- read.csv("http://deq1.bse.vt.edu:81/p6/out/land/subsheds2/pwater/forN54031_pwater.csv")

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
    "
  )
  
  # Put back in alphabetical order with Date as first column
  data_daily <- data_daily[c("Date", sort(setdiff(names(data_daily), "Date")))]
  
  return(data_daily)
}

H51165_daily <- make.model.daily(H51165, "index")
N51165_daily <- make.model.daily(N51165, "index")
N51171_daily <- make.model.daily(N51171, "index")
N54031_daily <- make.model.daily(N54031, "index")



#----

# Calculation for 0s in data
H_practice <- subset(H51165, year > 1999  & year < 2006)

H_practice$index <- as_datetime(H_practice$index)

H_precalc <- H_practice


# create column for change in storage
H_practice$dAGWS <- c(NA, diff(H_practice$AGWS))

# Use outflows and inflows to calculate Lateral inflow
H_practice$AGWI <- H_practice$dAGWS + H_practice$AGWO + H_practice$AGWET

# Change any calculated values < 0 to 0
H_practice$AGWI[H_practice$AGWI < 0] <- 0

# Repeat for Upper and Lower Zones

H_practice$dLZS <- c(NA, diff(H_practice$LZS))

H_practice$LZI <- H_practice$dLZS + H_practice$LZET - H_practice$PERC

H_practice$LZI[H_practice$LZI < 0] <- 0


# Upper

H_practice$dUZS <- c(NA, diff(H_practice$UZS))

H_practice$UZI <- H_practice$dUZS + H_practice$UZET + H_practice$PERC

H_practice$UZI[H_practice$UZI < 0 ] <- 0 


# Make summary table for each zones non-zero values ----
summaries <- list(
  `UZ Pre`  = summary(H_precalc$UZI == 0),
  `UZ Post` = summary(H_practice$UZI == 0),
  `LZ Pre`  = summary(H_precalc$LZI == 0),
  `LZ Post` = summary(H_practice$LZI == 0),
  `GW Pre`  = summary(H_precalc$AGWI == 0),
  `GW Post` = summary(H_practice$AGWI == 0)
)

sum_table <- do.call(rbind, lapply(names(summaries), function(name) {
  counts <- summaries[[name]]
  # make sur all names exist
  full_counts <- c(`FALSE` = 0, `TRUE` = 0, `NA's` = 0)
  full_counts[names(counts)] <- counts
  data.frame(Condition = name, as.list(full_counts), check.names = FALSE)
}))

sum_table <- sqldf(
  "select Condition, FALSE, TRUE from sum_table
  "
)

names(sum_table)[names(sum_table) == "FALSE"] <- "Non-zeros"
names(sum_table)[names(sum_table) == "TRUE"] <- "Zeros"

tab_sum <- flextable::flextable(sum_table)
tab_sum <- flextable::autofit(tab_sum)
tab_sum <- flextable::set_caption(tab_sum, "Count of 0-Values Before and After Recalculating Inflows (H51165, 2000-2002)")
tab_sum <- set_table_properties(tab_sum,width = 1,layout = "autofit")
tab_sum


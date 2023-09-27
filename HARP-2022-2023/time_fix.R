# For the purpose of converting 24th hours (hspf) to 0th hours of the next day

#setwd("/Users/glenncampagna/Desktop/HARPteam22/Data") # for testing only (Glenn)
#df <- fread("ps_sep_div_ams_vadeq_2021_JL1_6770_6850_3000.csv") # for testing only

suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(lubridate))

argst <- commandArgs(trailingOnly = T)
csv <- argst[1]
format <- argst[2] # either wdm or header depending on csv2
timestep <- argst[3] # either hour or day 

df <- fread(csv)

if (format == 'wdm') {
  if (timestep == 'day') {
    colnames(df) <- c('year','month','day','values')
  }
  if (timestep == 'hour') {
    colnames(df) <- c('year','month','day','hour','values')
  }
}

if (timestep == 'day') {
  df$index <- as.POSIXct(make_datetime(df$year,df$month,df$day))
  df$day <- day(df$index)
  df$month <- month(df$index)
  df$year <- year(df$index)
}

if (timestep == 'hour') {
  df$index <- as.POSIXct(make_datetime(df$year,df$month,df$day,df$hour))
  df$hour <- hour(df$index)
  df$day <- day(df$index)
  df$month <- month(df$index)
  df$year <- year(df$index)
}

#If file came in as wdm-format we want to restore that format (and no headers)
if (format == 'wdm') {
  df <- subset(df, select = -c(index))# remove the added index col
  df <- as.matrix(df) #convert to matrix for header removal
  df <- unname(df) #removing headers
}

write.table(df,file = csv, sep = ",", row.names = FALSE)

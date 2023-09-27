#Will join a column of 1 table to a different table 
#Assuming: csv1 is hourly data and does not yet have columns for hour, day, etc. 
#  and csv2 either has headers with an index column or is in wdm format (no headers but set column order)
suppressPackageStartupMessages(library(data.table)) 
suppressPackageStartupMessages(library(sqldf))
suppressPackageStartupMessages(library(lubridate))

#setwd("/Users/glenncampagna/Desktop/HARPteam22/Data") # for testing only (Glenn)
#df1<- fread("JL1_6770_6850_hydr.csv") # for testing only 
#df2 <- fread("ps_sep_div_ams_vadeq_2021_JL1_6770_6850_3000.csv") # for testing only

argst <- commandArgs(trailingOnly = T)
csv1 <- argst[1]
csv2 <- argst[2]
old_col <- argst[3] #should be 'values' when wdm is used 
new_col <- argst[4]
format <- argst[5] # either wdm or header depending on csv2
timestep <- argst[6] # either hour or day 
mod <- argst[7] # either hsp2 or hspf

#for testing, should be commented before actual use 
#old_col <- 'values'
#new_col <- 'divr_cfs'

df1 <- fread(csv1)
df2 <- fread(csv2)

origin <- "1970-01-01"
df1$date <- as.Date(df1$index, origin = origin, tz = "UTC") 
df1$hour <- hour(df1$index)
df1$day <- day(df1$index)
df1$month <- month(df1$index)
df1$year <- year(df1$index)
index_seq <- df1$index

if (format == 'wdm') {
  if (timestep == 'day') {
    colnames(df2) <- c('year','month','day','values')
    df2$date = as.Date(paste0(df2$year,'-',df2$month,'-',df2$day), origin = origin, tz = "UTC") 
  }
  if (timestep == 'hour') {
    colnames(df2) <- c('year','month','day','hour','values')
  }
}

# index/timestamp adjustment if data to be joined is hspf
if (mod == 'hspf') {
  df2$index <- as.POSIXct(make_datetime(df2$year,df2$month,df2$day,df2$hour))
  df2$date <- as.Date(df2$index, origin = origin, tz = "UTC") 
  df2$hour <- hour(df2$index)
  df2$day <- day(df2$index)
  df2$month <- month(df2$index)
  df2$year <- year(df2$index)
}


#df2 can have headers or be in wdm format (no headers)
if (format == 'header') {
  df2$date <- as.Date(df2$index, origin = origin, tz = "UTC")
  df2$day <- day(df2$date)
  df2$month <- month(df2$date)
  df2$year <- year(df2$date)
  if (timestep == 'hour') {
    df2$hour <- hour(df2$date)
  }
}


#this sqldf syntax selects a as primary table and b to be joined, we capitalized sqldf operator words to exclude syntax errors

if (timestep == 'hour') {
  df1_joined <- sqldf(
    paste0("SELECT a.*, b.'", old_col,"' AS '", new_col, "'
        FROM df1 AS a 
         LEFT OUTER JOIN df2 AS b ON (a.year = b.year AND a.month = b.month AND a.day = b.day AND a.hour = b.hour)
         ORDER BY a.year,a.month,a.day,a.hour"
    )
  ) 
}

if (timestep == 'day') {
  df1_joined <- sqldf(
    paste0("SELECT a.*, b.'", old_col,"' AS '", new_col, "'
        FROM df1 AS a 
         LEFT OUTER JOIN df2 AS b ON (a.date = b.date)
         ORDER BY a.date"
    )
  )
}

#Comparing lengths of table and column to be joined
rows_df1 <- nrow(df1)
rows_df1j <- nrow(df1_joined)
if (rows_df1 != rows_df1j) {
  stop('Table and column are different lengths, unable to join')
}

#Replace index col with original sequence of timestamps including hour
df1_joined$index <- index_seq

write.table(df1_joined,file = csv1, sep = ",", row.names = FALSE)
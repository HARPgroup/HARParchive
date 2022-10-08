#Will mimmick function of join_col.R but is for use with wdm format csvs (no column headers)
#Assumes the table/csv having the column added to it contains hour/day/month/year columns 
#Script also assumes the table being added to has hourly data for ordering in sqldf 

suppressPackageStartupMessages(library(data.table)) 
suppressPackageStartupMessages(library(sqldf))
suppressPackageStartupMessages(library(lubridate))

setwd("/Users/glenncampagna/Desktop/HARPteam22/Data") # for testing only (Glenn)
df1<- fread("OR1_7700_7980_hydr.csv") # for testing only 
df2 <- fread("PL1_5370_5470_0111.csv") # for testing only

argst <- commandArgs(trailingOnly = T)
csv1 <- argst[1] #table/csv to have the column added 
csv2 <- argst[2] #wdm-format csv
new_col <- argst[3] #iname of new col 
timestep <- argst[4] # either 'hour' or 'day'

new_col <- 'ROVOL'
timestep <- 'hour' #for testing 

df1 <- fread(csv1)
df2 <- fread(csv2)

#for testing, this script assumes these columns will already exist, needs commenting before use 
#df1$date <- as.Date(df1$index, format = "%m/%d/%Y %H:%M")
#df1$hour <- hour(df1$index)
#df1$day <- day(df1$date)
#df1$month <- month(df1$date)
#df1$year <- year(df1$date)

#Adding temporary column headers to the wdm format table for joining 
if (timestep == 'hour') {
  colnames(df2) <- c('year', 'month', 'day', 'hour', 'value')
}
if (timestep == 'day') {
  colnames(df2) <- c('year', 'month', 'day', 'value')
}

# this syntax selects a as primary table and b to be joined 
# we capitalized sqldf operator words to exclude syntax errors

#Add loops for different types of joins depending on timestep  

if (timestep == 'hour') {
  df1_joined <- sqldf(
    paste0("SELECT a.*, b.value AS '", new_col, "'
        FROM df1 AS a 
         LEFT OUTER JOIN df2 AS b ON (a.year = b.year AND a.month = b.month AND a.day = b.day AND a.hour = b.hour)
         ORDER BY a.year,a.month,a.day,a.hour"
    )
  )  
}
#join correct for hourly data 

if (timestep == 'day') {
  df1_joined <- sqldf(
    paste0("SELECT a.*, b.value AS '", new_col, "'
        FROM df1 AS a 
         LEFT OUTER JOIN df2 AS b ON (a.year = b.year AND a.month = b.month AND a.day = b.day)
         ORDER BY a.year,a.month,a.day,a.hour"
    )
  )  
}

rows_df1 <- nrow(df1)
rows_df1j <- nrow(df1_joined)

if (rows_df1 != rows_df1j) {
  stop('Table and column are different lengths, unable to join')
}

write.table(df1,file = csv1, sep = ",", row.names = FALSE)

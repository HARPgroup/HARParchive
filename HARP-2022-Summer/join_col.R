#Will join a column of 1 table to a different table 

suppressPackageStartupMessages(library(data.table)) 
suppressPackageStartupMessages(library(sqldf))
suppressPackageStartupMessages(library(lubridate))

#setwd("/Users/glenncampagna/Desktop/HARPteam22/Data") # for testing only (Glenn)
#df1<- fread("OR1_7700_7980_hydr.csv") # for testing only 
#df2 <- fread("OR1_7700_7980_divr.csv") # for testing only

argst <- commandArgs(trailingOnly = T)
csv1 <- argst[1]
csv2 <- argst[2]
old_col <- argst[3]
new_col <- argst[4]

#for testing, should be commented before actual use 
#old_col <- 'values'
#new_col <- 'divr_cfs'

df1 <- fread(csv1)
df2 <- fread(csv2)

#for testing, this script assumes these columns will already exist, needs commenting before use 
#df1$date <- as.Date(df1$index, format = "%m/%d/%Y %H:%M")
#df1$hour <- hour(df1$index)
#df1$day <- day(df1$date)
#df1$month <- month(df1$date)
#df1$year <- year(df1$date)
#df2$date <- as.Date(df2$index, format = "%m-%d-%Y")
#df2$day <- day(df2$index)
#df2$month <- month(df2$index)
#df2$year <- year(df2$index)

#names(df2)[names(df2) == old_col] <- 'join_col' #Renames column of interest to join_col for sqldf function 

df1 <- sqldf(
  paste0("select a.*, b.", old_col, " as ", new_col, 
         "from df1 as a left outer join df2 as b
         on (  
         a.year = b.year
         and a.month = b.month
         and a.day = b.day)
         order by a.year,a.month,a.day,a.hour"
         )
  )

#names(df1)[names(df1) == 'join_col'] <- new_col #Renames joined column to what is given as an argument

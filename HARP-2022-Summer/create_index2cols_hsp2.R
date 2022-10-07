#Creating a function to convert an hsp2 'index' date column of the format ___ to individual columns 
#df (df1 fir testing) is the table with the index col and df_ is the final modified table 

library(lubridate)
library(data.table)

setwd("/Users/glenncampagna/Desktop/HARPteam22/Data") # for testing only 
df1 <- fread("OR1_7700_7980_hydr.csv") # for testing only

index2cols_hsp2 <- function(df) {
  df$date <- as.Date(df$index, format = "%m/%d/%Y %H:%M")
  df$hour <- hour(df$index)
  df$day <- day(df$index)
  df$month <- month(df$index)
  df$year <- year(df$index)
  return(df)
}

df_ <- as.data.frame(index2cols_hsp2(df1))

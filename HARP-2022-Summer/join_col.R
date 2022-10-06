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
df1$date <- as.Date(df1$index, format = "%m/%d/%Y %H:%M")
df1$hour <- hour(df1$index)
df1$day <- day(df1$date)
df1$month <- month(df1$date)
df1$year <- year(df1$date)
df2$day <- day(df2$index)
df2$month <- month(df2$index)
df2$year <- year(df2$index)

# selecting a as primary table 
# capitalized sqldf operators 

df1_joined <- sqldf(
  paste0("SELECT a.*, b.'", old_col,"' AS '", new_col, "'
        FROM df1 AS a 
         LEFT OUTER JOIN df2 AS b ON (a.year = b.year AND a.month = b.month AND a.day = b.day)
         ORDER BY a.year,a.month,a.day,a.hour"
         )
  ) 

rows_df1 <- nrow(df1)
rows_df1j <- nrow(df1_joined)

if (rows_df1 != rows_df1j) {
  stop('Table and column are different lengths, unable to join')
}

write.table(df1,file = csv1, sep = ",", row.names = FALSE)


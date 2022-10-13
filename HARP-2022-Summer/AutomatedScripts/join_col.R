#Will join a column of 1 table to a different table 

suppressPackageStartupMessages(library(data.table)) 
suppressPackageStartupMessages(library(sqldf))
suppressPackageStartupMessages(library(lubridate))

#setwd("/Users/glenncampagna/Desktop/HARPteam22/Data") # for testing only (Glenn)
#df1<- fread("OR1_7700_7980_hydr.csv") # for testing only 
#df2 <- fread("OR1_7700_7980_divr.csv") # for testing only
#add a file path in wdm format for testing (daily)

argst <- commandArgs(trailingOnly = T)
csv1 <- argst[1]
csv2 <- argst[2]
old_col <- argst[3] #should be 'values' when wdm is used 
new_col <- argst[4]
format <- argst[5] # either wdm or header depending on csv2

#for testing, should be commented before actual use 
#old_col <- 'values'
#new_col <- 'divr_cfs'

df1 <- fread(csv1)
df2 <- fread(csv2)

df1$date <- as.Date(df1$index) 

#df2 can have headers or be in wdm format (no headers)
if (format == 'header') {
  df2$date <- as.Date(df2$index)
}

if (format == 'wdm') {
  colnames(df2) <- c('year','month','day','values')
  origin <- "1970-01-01"
  df2$date = as.Date(paste0(df2$year,'-',df2$month,'-',df2$day), origin = origin, tz = "UTC")
}

# this syntax selects a as primary table and b to be joined 
# we capitalized sqldf operator words to exclude syntax errors

#df1_joined <- sqldf(
#  paste0("SELECT a.*, b.'", old_col,"' AS '", new_col, "'
#        FROM df1 AS a 
#         LEFT OUTER JOIN df2 AS b ON (a.year = b.year AND a.month = b.month AND a.day = b.day)
#         ORDER BY a.year,a.month,a.day,a.hour"
#         )
#  ) 

#Simpler join by only date column:
df1_joined <- sqldf(
  paste0("SELECT a.*, b.'", old_col,"' AS '", new_col, "'
        FROM df1 AS a 
         LEFT OUTER JOIN df2 AS b ON (a.date = b.date)
         ORDER BY a.date"
  )
)

rows_df1 <- nrow(df1)
rows_df1j <- nrow(df1_joined)

if (rows_df1 != rows_df1j) {
  stop('Table and column are different lengths, unable to join')
}

write.table(df1_joined,file = csv1, sep = ",", row.names = FALSE)


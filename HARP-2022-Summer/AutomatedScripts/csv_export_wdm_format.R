# creating a csv with wanted col and wdm format

basepath='/var/www/R';
source("/var/www/R/config.R")

suppressPackageStartupMessages(library(data.table)) 
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(R.utils))

omsite = "http://deq1.bse.vt.edu:81"

#setwd("/Users/VT_SA/Documents/HARP") #for testing
#hydr <- fread("OR1_7700_7980_hydr.csv") #for testing

argst <- commandArgs(trailingOnly = T)
hydr_path <- argst[1]
#hydr_file <- ('OR1_7700_7980_hydr.csv')
rovol_file <- argst[2]
#rovol_file <- ('OR1_7700_7980_rovol.csv')
column <- argst[3]
#column <- 'ROVOL'


hydr <- fread(hydr_path)

hydr %>% select(column) -> hydr_column

# the hydr.csv contains the year and month columns already
hydr$hour <- hour(hydr$index)
hydr$day <- day(hydr$index)
hydr$month <- month(hydr$index)
hydr$year <- year(hydr$index)


# creating tables with OVOL3 and ROVOL
hydr_df <- data.frame(hydr$year, hydr$month, hydr$day, hydr$hour, hydr_column)

# exporting the tables
write.table(hydr_df, file = rovol_path, sep = ',', row.names = FALSE, col.names = FALSE, quote = FALSE)


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
hydr_file <- argst[1]
#hydr_file <- ('OR1_7700_7980_hydr.csv')
rovol_file <- argst[2]
#rovol_file <- ('OR1_7700_7980_rovol.csv')
column <- argst[3]
#column <- ('Qout')

hydr_path=paste('/media/model/p532/out/river/hsp2_2022/hydr/', hydr_file, sep = '')
rovol_path=paste('/media/model/p532/out/river/hsp2_2022/rovol/', rovol_file, sep = '')

hydr <- fread(hydr_path)

hydr %>% select(column) -> hydr_column

# the hydr.csv contains the year and month columns already
hydr$day <- day(hydr$date)
hydr$hour <- hour(hydr$index)

# adding commas after the time columns
year_comma <- paste0(as.character(hydr$year), ',') 
month_comma <- paste0(as.character(hydr$month), ',')
day_comma <- paste0(as.character(hydr$day), ',')
hour_comma <- paste0(as.character(hydr$hour), ',')

# creating tables with OVOL3 and ROVOL
hydr_df <- data.frame(year_comma, month_comma, day_comma, hour_comma, hydr_column)

# exporting the tables
write.table(hydr_df, file = rovol_path, row.names = FALSE, col.names = FALSE, quote = FALSE)


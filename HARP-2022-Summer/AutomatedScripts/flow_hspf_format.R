# creating a csv with just OVOL3 and ROVOL in the hspf file format
# in hspf (correct order): year, month, day, hour, OVOL3/ROVOL
# in hydr.csv: date, week, month, year, Qout (ROVOL_cfs), OVOL3

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
river_seg <- argst[1]
#river_seg <- ('OR1_7700_7980')
file_path <- argst[2]
#file_path <- ('/media/model/p532/out/river/hsp2_2022')

hydr_file=paste(file_path,'/hydr/', river_seg, '_hydr.csv', sep = '')

hydr <- fread(hydr_file)

# the hydr.csv contains the year and month columns already
hydr$day <- day(hydr$date)
hydr$hour <- hour(hydr$date)

# adding commas after the time columns
year_comma <- paste0(as.character(hydr$year), ',') 
month_comma <- paste0(as.character(hydr$month), ',')
day_comma <- paste0(as.character(hydr$day), ',')
hour_comma <- paste0(as.character(hydr$hour), ',')

ovol3_cfs <- hydr$OVOL3*12.1 # ovol had to be converted to cfs from ac.ft/hr

# creating tables with OVOL3 and ROVOL
hydr_ovol3 <- data.frame(year_comma, month_comma, day_comma, hour_comma, ovol3_cfs)
colnames(hydr_ovol3) <- c('year', 'month', 'day', 'hour', 'OVOL3')

hydr_rovol <- data.frame(year_comma, month_comma, day_comma, hour_comma, hydr$Qout)
colnames(hydr_ovol3) <- c('year', 'month', 'day', 'hour', 'ROVOL')

# exporting the tables
output_file_ovol3=paste(file_path,'/fortran/', river_seg, '_ovol3.csv', sep = '')
output_file_rovol=paste(file_path,'/fortran/', river_seg, '_rovol.csv', sep = '')

write.table(hydr_ovol3, file = output_file_ovol3, sep = ",", row.names = FALSE)
write.table(hydr_rovol, file = output_file_rovol, sep = ",", row.names = FALSE)



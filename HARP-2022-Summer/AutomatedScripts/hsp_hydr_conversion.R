basepath='/var/www/R';
source("/var/www/R/config.R") # will need file in same folder/directory

suppressPackageStartupMessages(library(data.table)) 
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(zoo))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(PearsonDS))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(R.utils))
suppressPackageStartupMessages(library(sqldf))

#setwd("/Users/glenncampagna/Desktop/HARPteam22/Data") # for testing only (Glenn) 
#setwd("/Users/VT_SA/Documents/HARP") # for testing only (Julia)
#hydr <- fread("OR1_7700_7980_hydr.csv") # for testing only 
#divr <- fread("OR1_7700_7980_divr.csv") # for testing only
#ps <- fread("OR1_7700_7980_psflow.csv") # for testing only

# establishing location on server for storing images
omsite = "http://deq1.bse.vt.edu:81"

# Accepting command arguments:
argst <- commandArgs(trailingOnly = T)
riverseg <- argst[1]
output_file_path <- argst[2]
#riverseg <- 'OR1_7700_7980' #for testing
#output_file_path='/media/model/p532/out/river/hsp2_2022' #for testing

hydr_file_path=paste(output_file_path,'/hydr/', riverseg, '_hydr.csv', sep = '')
#divr_file_path=paste(output_file_path,'/divr/', riverseg, '_divr.csv', sep = '')
ps_file_path=paste(output_file_path,'/ps_flow/', riverseg, '_psflow.csv', sep = '')

hydr <- fread(hydr_file_path)
#divr <- fread(divr_file_path) #divr in units of cfs 
ps <- fread(ps_file_path) #ps in units of ac-ft/day 

#colnames(divr) = c('date', 'divr_cfs')
colnames(ps) = c('date', 'ps_afd')

hydr$date <- as.Date(hydr$index, format = "%m/%d/%Y %H:%M")
hydr$hour <- hour(hydr$index)
hydr$day <- day(hydr$date)
hydr$month <- month(hydr$date)
hydr$year <- year(hydr$date)

#divr$date <- as.Date(divr$date, format = "%m-%d-%Y")
#divr$day <- day(divr$date)
#divr$month <- month(divr$date)
#divr$year <- year(divr$date)

ps$date <- as.Date(ps$date, format = "%m-%d-%Y")
ps$day <- day(ps$date)
ps$month <- month(ps$date)
ps$year <- year(ps$date)

#Converting ps from ac-ft/d to mgd 
ps$ps_mgd <- ps$ps_afd*0.3258

#final units for joining need to be mgd
#Using sqldf to join tables
#hydr <- sqldf( #adding divr
#  "select a.*, b.divr_cfs 
#from hydr as a
#left outer join divr as b
#on (  
#a.year = b.year
#and a.month = b.month
#and a.day = b.day
#)
#order by a.year,a.month,a.day,a.hour
#")

hydr <- sqldf( #adding ps
  "select a.*, b.ps_afd 
from hydr as a
left outer join ps as b
on (  
a.year = b.year
and a.month = b.month
and a.day = b.day
)
order by a.year,a.month,a.day,a.hour
")

# Converting from ac-ft/hr (OVOL3) to cfs : 1 ac-ft/hr = 12.1 cfs
hydr$Qout <- hydr$OVOL3*12.1 #Qout in units of cfs

# Exporting the modified csv files into the output_file_path:
write.table(hydr,file = hydr_file_path, sep = ",", row.names = FALSE)

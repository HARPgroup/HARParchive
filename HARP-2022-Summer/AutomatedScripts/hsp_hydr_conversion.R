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
ps_file_path=paste(output_file_path,'/ps_flow/', riverseg, '_psflow.csv', sep = '')
divr_file_path=paste(output_file_path,'/divr/', riverseg, '_divr.csv', sep = '')
diva_file_path=paste(output_file_path,'/diva/', riverseg, '_diva.csv', sep = '')

hydr <- fread(hydr_file_path)
ps <- fread(ps_file_path) #ps in units of ac-ft/day 
divr <- fread(divr_file_path) #divr in units of cfs 
diva <- fread(diva_file_path) #diva in units of cfs 

colnames(ps) = c('date', 'ps_afd')
colnames(divr) = c('date', 'divr_cfs')
colnames(diva) = c('date', 'diva_cfs')

#Adding date information to the tables:
hydr$date <- as.Date(hydr$index, format = "%m/%d/%Y %H:%M")
hydr$hour <- hour(hydr$index)
hydr$day <- day(hydr$date)
hydr$month <- month(hydr$date)
hydr$year <- year(hydr$date)

ps$date <- as.Date(ps$date, format = "%m-%d-%Y")
ps$day <- day(ps$date)
ps$month <- month(ps$date)
ps$year <- year(ps$date)

divr$date <- as.Date(divr$date, format = "%m-%d-%Y")
divr$day <- day(divr$date)
divr$month <- month(divr$date)
divr$year <- year(divr$date)

diva$date <- as.Date(diva$date, format = "%m-%d-%Y")
diva$day <- day(diva$date)
diva$month <- month(diva$date)
diva$year <- year(diva$date)

#Converting ps from ac-ft/d to mgd 
ps$ps_mgd <- ps$ps_afd*0.3258

#Using sqldf to join ps with hydr:
hydr <- sqldf( #adding ps
  "select a.*, b.ps_mgd 
from hydr as a
left outer join ps as b
on (  
a.year = b.year
and a.month = b.month
and a.day = b.day
)
order by a.year,a.month,a.day,a.hour
")


#Summing divr and diva together as demand
  #Adding a new 'demand' column to the divr table in order to merge that column
  #with the hydr table later

divr$demand <- (as.numeric(divr$divr_cfs)) + (as.numeric(diva$diva_cfs))

#Converting demand from cfs to mgd

divr$demand_mgd <- divr$demand*0.64632

#Using sqldf to join demand with hydr:
hydr <- sqldf( #adding divr
  "select a.*, b.demand_mgd 
from hydr as a
left outer join divr as b
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

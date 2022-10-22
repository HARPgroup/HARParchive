# This script will convert the hydr csv to a data table and perform analysis & generate graphs 
#install.packages("IHA", repos="http://R-Forge.R-project.org")
#install_github("HARPGroup/hydro-tools", force=TRUE)

suppressPackageStartupMessages(library(data.table)) 
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(zoo))
suppressPackageStartupMessages(library(plyr))
#suppressPackageStartupMessages(library(PearsonDS))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(R.utils))

#setwd("/Users/glenncampagna/Desktop/HARPteam22/Data") # for testing only (Glenn) 
#setwd("/Users/VT_SA/Documents/HARP") # for testing only (Julia)
#hydr <- fread("OR1_7700_7980_hydr.csv") # for testing only 
#divr <- fread("OR1_7700_7980_divr.csv") # for testing only
#ps_flow <- fread("OR1_7700_7980_psflow.csv") # for testing only

# establishing location on server for storing images
omsite = "http://deq1.bse.vt.edu:81"

# Accepting command arguments:
argst <- commandArgs(trailingOnly = T)
hydr_file_path <- argst[1]  
#output_file_path='/media/model/p532/out/river/hsp2_2022/'

#divr_file_path=paste(output_file_path,'/divr/', river_segment_name, '_divr.csv', sep = '')
#ps_file_path=paste(output_file_path,'/ps_flow/', river_segment_name, '_psflow.csv', sep = '')

hydr <- fread(hydr_file_path)
#divr <- fread(divr_file_path) # divr in units of cfs
#ps_flow <- fread(ps_file_path) # ps in units of ac-ft/hr
origin <- "1970-01-01"
hydr$date <- as.Date(hydr$index, format = "%m/%d/%Y %H:%M", origin = origin)
#hydr$hour <- hour(hydr$index)
#hydr$day <- day(hydr$date)
#hydr$month <- month(hydr$date)
#hydr$year <- year(hydr$date)

# Converting from ac-ft/hr (ROVOL) to cfs : 1 ac-ft/hr = 12.1 cfs
hydr$Qout <- hydr$OVOL3*12.1 #Qout in units of cfs
hydr$wd_mgd <- (hydr$RO - hydr$O3) /1.5472 # withdrawal cfs converted to mgd

hydr$ps_mgd <- hydr$ps_afd*0.32585

# Converting to mgd:
#colnames(divr) = c('date','divr_achfth')
#colnames(ps_flow) = c('date','ps_cfs')

#divr$divr_mgd=divr$divr_achfth*7.820434
#ps_flow$ps_mgd=ps_flow$ps_cfs*1.547

# Exporting the modified csv files into the output_file_path:
write.table(hydr,file = hydr_file_path, sep = ",", row.names = FALSE)
#write.table(divr,file = divr_file_path, sep = ",", row.names = FALSE)
#write.table(ps_flow,file = ps_file_path, sep = ",", row.names = FALSE)

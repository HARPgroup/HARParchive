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
hydr$demand_mgd <- (hydr$divr_cfs + hydr$diva_cfs)/1.5472 #demand cfs summed and coverted to mgd 

#Qbaseline = Qout + (wd_cum_mgd - ps_cum_mgd)*1.547
hydr$Qbaseline <- hydr$Qout + (hydr$wd_mgd - hydr$ps_mgd)*1.5472

# Exporting the modified csv files into the output_file_path:
write.table(hydr,file = hydr_file_path, sep = ",", row.names = FALSE)
#write.table(divr,file = divr_file_path, sep = ",", row.names = FALSE)
#write.table(ps_flow,file = ps_file_path, sep = ",", row.names = FALSE)


## Creating a DAILY dataset for analysis:

# syear = as.integer(min(hydr$year))
# eyear = as.integer(max(hydr$year))
# model_run_start <- min(hydr$date)   
# model_run_end <- max(hydr$date)
# years <- seq(syear,eyear)
# 
# if (syear < (eyear - 2)) {
#   sdate <- as.Date(paste0(syear,"-10-01"))
#   edate <- as.Date(paste0((eyear-1),"-09-30")) 
#   flow_year_type <- 'water'
# } else {
#   sdate <- as.Date(paste0(syear,"-02-01"))
#   edate <- as.Date(paste0(eyear,"-12-31"))
#   flow_year_type <- 'calendar'
# }
# 
# #Reverted back to using window(), which requires a ts or zoo:
# hydr <- zoo(hydr, order.by = hydr$index) #Takes a little while
# hydr = aggregate(
#   hydr,
#   as.POSIXct(
#     format(
#       date(hydr), 
#       format='%Y/%m/%d UTC')
#   ),
#   'mean')
# 
# hydr <- window(hydr, start = sdate, end = edate)
# #### Convert hydr to a zoo and keep it that way thorughout 
# 
# #Convert hydr to numeric: mode(dat) <- 'numeric'
# mode(hydr) <- 'numeric'
# 
# file_text = paste(hydr_file_path)
# split <- strsplit(file_text, split = "_")
# name <- paste0(split[[1]][[1]],"_",split[[1]][[2]],"_",split[[1]][[3]],"_",
#                "daily","_",split[[1]][[4]], collapse = ",")
# 
# write.table(hydr, file = name, sep = ",", row.names = FALSE)

# This script will convert the hydr csv to a data table and perform analysis & generate graphs 
#install.packages("IHA", repos="http://R-Forge.R-project.org")
#install_github("HARPGroup/hydro-tools", force=TRUE)
basepath='/var/www/R';
source("/var/www/R/config.R") # will need file in same folder/directory

suppressPackageStartupMessages(library(data.table)) 
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(zoo))
suppressPackageStartupMessages(library(plyr))
#suppressPackageStartupMessages(library(caTools))
#suppressPackageStartupMessages(library(RColorBrewer))
#suppressPackageStartupMessages(library(IHA))
suppressPackageStartupMessages(library(PearsonDS))
#suppressPackageStartupMessages(library(ggplot2))
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
river_segment_name <- argst[1]
#river_segment_name <-'OR1_7700_7980' #for testing only 
scenario_name <- argst[2]
output_file_path <- argst[3]  
#output_file_path='/media/model/p532/out/river/hsp2_2022' # for testing only 

hydr_file_path=paste(output_file_path,'/hydr/', river_segment_name, '_hydr.csv', sep = '')
divr_file_path=paste(output_file_path,'/divr/', river_segment_name, '_divr.csv', sep = '')
ps_file_path=paste(output_file_path,'/ps_flow/', river_segment_name, '_psflow.csv', sep = '')

# Reading in the tables
hydr <- fread(hydr_file_path)
divr <- fread(divr_file_path) # divr in units of cfs
ps_flow <- fread(ps_file_path) # ps in units of ac-ft/hr

hydr$date <- as.Date(hydr$index, format = "%m/%d/%Y %H:%M")
hydr$week <- week(hydr$date)
hydr$month <- month(hydr$date)
hydr$year <- year(hydr$date)

# Converting from ac-ft/hr (ROVOL) to cfs : 1 ac-ft/hr = 12.1 cfs
hydr$ROVOL_cfs = hydr$ROVOL*12.1 

# Converting to mgd:
colnames(divr) = c('date','divr_cfs')
colnames(ps_flow) = c('date','ps_cfs')
#colnames(ps_flow) = c('date','ps_acfth')   #if ps was in acft/hr instead!

divr$divr_mgd=divr$divr_cfs*1.547
ps_flow$ps_mgd=ps_flow$ps_cfs*1.547
#ps_flow$ps_mgd=ps_flow$ps_acfth*7.820434   #if ps was in acft/hr instead!

# Creating tables with just wanted columns:
rovol <- data.frame(hydr$date, hydr$week, hydr$month, hydr$year, hydr$ROVOL_cfs)
colnames(rovol) <- c("date","week","month","year","ROVOL_cfs")

divr_mgd <- data.frame(divr$index, divr$divr_mgd)
colnames(divr_mgd) <- c("index","divr_mgd")

ps_mgd <- data.frame(ps_flow$index, ps_flow$ps_mgd)
colnames(ps_mgd) <- c("index", "ps_mgd")

# Exporting the modified csv files into the output_file_path:

rovol_file_path=paste(output_file_path,'/hydr/', river_segment_name, '_rovol_cfs.csv', sep = '')
divr_mgd_file_path=paste(output_file_path,'/divr/', river_segment_name, '_divr_mgd.csv', sep = '')
ps_mgd_file_path=paste(output_file_path,'/ps_flow/', river_segment_name, '_psflow_mgd.csv', sep = '')

write.table(rovol,file = rovol_file_path, sep = ",", row.names = FALSE)
write.table(divr_mgd,file = divr_mgd_file_path, sep = ",", row.names = FALSE)
write.table(ps_mgd,file = ps_mgd_file_path, sep = ",", row.names = FALSE)



## Converting to mgd (instead of cfs):
#hydr$ROVOL_mgd <- hydr$ROVOL*7.820434

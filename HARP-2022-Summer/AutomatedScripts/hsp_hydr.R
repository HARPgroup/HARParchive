# This script will convert the hydr csv to a data table and perform analysis & generate graphs 
#install.packages("IHA", repos="http://R-Forge.R-project.org")
#install_github("HARPGroup/hydro-tools", force=TRUE)
basepath='/var/www/R';
source("/var/www/R/config.R") # will need file in same folder/directory

suppressPackageStartupMessages(library(data.table)) 
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(zoo))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(caTools))
suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(IHA))
suppressPackageStartupMessages(library(PearsonDS))
#suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(R.utils))

#setwd("/Users/glenncampagna/Desktop/HARPteam22/Data") # for testing only 
#hydr <- fread("OR1_7700_7980_hydr.csv") # for testing only 
#divr <- fread("OR1_7700_7980_divr.csv") # for testing only
#ps_flow <- fread("OR1_7700_7980_psflow.csv") # for testing only

# establishing location on server for storing images
#omsite = "http://deq1.bse.vt.edu:81"


# Accepting command arguments:
argst <- commandArgs(trailingOnly = T)
river_segment_name <- argst[1]
#river_segment_name <-'OR1_7700_7980' #for testing only 
scenario_name <- argst[2]
output_file_path <- argst[4]  
#output_file_path='/media/model/p532/out/river/hsp2_2022' # for testing only 
#image_directory_path <- argst[5] # '/media/model/p532/out/river/p532sova_2021/images'

hydr_file_path=paste(output_file_path,'/hydr/', river_segment_name, '_hydr.csv', sep = '')
divr_file_path=paste(output_file_path,'/divr/', river_segment_name, '_divr.csv', sep = '')
ps_file_path=paste(output_file_path,'/ps_flow/', river_segment_name, '_psflow.csv', sep = '')

# Reading in the tables
hydr <- fread(hydr_file_path)
divr <- fread(divr_file_path) # divr in units of cfs
ps_flow <- fread(ps_file_path) # ps in units of ac-ft/hr


colnames(divr) = c('date','divr_cfs')
colnames(ps_flow) = c('date','ps_cfs')
divr$divr_mgd=divr$divr_cfs*1.55
convert_acfthr_mgd = 7.820434 
ps_flow$ps_mgd=ps_flow$ps_cfs*convert_acfthr_mgd

#Coverting from cfs to mgd:


hydr$date <- as.Date(hydr$index, format = "%m/%d/%Y %H:%M")
hydr$week <- week(hydr$date)
hydr$month <- month(hydr$date)
hydr$year <- year(hydr$date)

# We don't know what units divr and ps_flow will come in as (cfs or ac-ft/hr)


# Converting units to mgd from ac.ft/ivld

     
hydr$ROVOL_mgd <- hydr$ROVOL*convert_acfthr_mgd


dailyQout <- aggregate(hydr$ROVOL_mgd, by = list(hydr$date), FUN='mean')  # ROVOL_mgd represents Qout
colnames(dailyQout) <- c('date','Qout') # Qout in units of mgd
monthlyQout <- aggregate(hydr$ROVOL_mgd, by = list(hydr$month, hydr$year), FUN = "mean")
colnames(monthlyQout) <- c("month", "year", "Qout") # ROVOL in units of mgd


# Conversion to water-year ???????????????/

# From: waterSupplyModelNode.R

#syear = as.integer(min(hydr$year))
#eyear = as.integer(max(hydr$year))
#model_run_start <- min(hydr$date)   # not sure about the "date"
#model_run_end <- max(hydr$date)
#if (syear < (eyear - 2)) {
#  sdate <- as.Date(paste0(syear,"-10-01"))
#  edate <- as.Date(paste0(eyear,"-09-30"))
#  flow_year_type <- 'water'
#} else {
#  sdate <- as.Date(paste0(syear,"-02-01"))
#  edate <- as.Date(paste0(eyear,"-12-31"))
#  flow_year_type <- 'calendar'
#}
#hydr <- window(hydr, start = sdate, end = edate);   # not sure what this does
#mode(hydr) <- 'numeric'
#scen.propname<-paste0('runid_', runid)  # not sure what this does/what to input instead of runid


        # this would then be used in the values below instead of "hydr"??


# Mean values for outflow amount and rate, and inflow amount

Qout_mean <- mean(as.numeric(dailyQout$Qout)) # mgd
paste('Qout_mean:', Qout_mean)
# l90 and l30 from RO (IHA metric - group 2)
# l90 Runit???

Qout_zoo <- zoo(dailyQout$Qout, order.by = dailyQout$date)
Qout_g2 <- data.frame(group2(Qout_zoo))
l90_Qout <- min(Qout_g2$X90.Day.Min) # mgd
l30_Qout <- min(Qout_g2$X30.Day.Min)
paste('l90_Qout:', l90_Qout)
paste('l30_Qout:', l30_Qout)
# Exporting to VAHydro

# Set up our data source
ds <- RomDataSource$new(site, rest_uname = rest_uname)
ds$get_token(rest_pw)

rseg_name=river_segment_name
rseg_ftype="cbp532_riverseg"

riverseg<- RomFeature$new(
  ds,
  list(
    hydrocode=rseg_name, 
    ftype=rseg_ftype,
    bundle='riverunit'
  ), 
  TRUE
)
riverseg$save(TRUE)


model <- RomProperty$new(
  ds,
  list(
    varkey="om_model_element", 
    propname=riverseg$name,
    featureid=riverseg$hydroid, 
    entity_type="dh_feature", 
    propcode="cbp-5.3.2" 
  ), 
  TRUE
)
model$save(TRUE)

model_scenario <- RomProperty$new( 
  ds,
  list(
    varkey="om_scenario", 
    featureid=model$pid, 
    entity_type="dh_properties", 
    propname=scenario_name, 
    propcode=scenario_name 
  ), 
  TRUE
)
model_scenario$save(TRUE)


# Uploading constants to VaHydro:
# entity-type specifies what we are attaching the constant to 


model_constant_l90_Qout <- RomProperty$new(
  ds, list(
    varkey="om_class_Constant",
    featureid=model_scenario$pid,
    entity_type='dh_properties',
    propname = 'l90_Qout_mgd'
  ),
  TRUE
)
model_constant_l90_Qout$propvalue <- as.numeric(l90_Qout)
model_constant_l90_Qout$save(TRUE)



model_constant_l30_Qout <- RomProperty$new(
  ds, list(
    varkey="om_class_Constant",
    featureid=model_scenario$pid,
    entity_type='dh_properties',
    propname = 'l30_Qout_mgd'
  ),
  TRUE
)
model_constant_l30_Qout$propvalue <- as.numeric(l30_Qout)
model_constant_l30_Qout$save(TRUE)


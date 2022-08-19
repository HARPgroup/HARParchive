# This script will generate summary statistics of the flow in the hspf 0111.csv file
# the scenario: p532sova_2021

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
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(R.utils))


#setwd("/Users/VT_SA/Documents/HARP")
#flow <- fread('OR1_7700_7980_0111.csv')

# establishing location on server for storing images
omsite = "http://deq1.bse.vt.edu:81"


# Accepting command arguments:
argst <- commandArgs(trailingOnly = T)
river_segment_name <- argst[1]
#river_segment_name <- 'OR1_7700_7980' #for testing only 
scenario_name <- argst[2]
#scenario_name <- 'p532sova_2021'
input_file_path <- argst[3] 
#input_file_path <- '/media/model/p532/out/river/p532sova_2021/stream/' #for testing 
model_version <- argst[4]
#model_version <- 'cbp-5.3.2'

file_path=paste(input_file_path, river_segment_name, '_0111.csv', sep ='')

flow <- fread(file_path)
colnames(flow) <- c('year','month','day','hour','Qout')

# Calculating the mean flow
Qout_mean <- mean(as.numeric(flow$Qout)) # cfs
paste('Qout_mean:', Qout_mean)

# Adding a date column
flow$date <- seq(ymd_hm("1984-1-1 0:00"), ymd_hm("2020-12-31 23:00"), by = "hour") 

# Calculating the 30 and 90 day low flows
Qout_zoo <- zoo(flow$Qout, order.by = flow$date)
Qout_g2 <- data.frame(group2(Qout_zoo))
l90_Qout <- min(Qout_g2$X90.Day.Min) # cfs
l30_Qout <- min(Qout_g2$X30.Day.Min)
paste('l90_Qout:', l90_Qout)
paste('l30_Qout:', l30_Qout)


# Exporting to VAHydro

# Set up our data source
ds <- RomDataSource$new(site, rest_uname = rest_uname)
ds$get_token(rest_pw)

rseg_name=river_segment_name
rseg_ftype='vahydro'

riverseg<- RomFeature$new(
  ds,
  list(
    hydrocode=paste('vahydrosw_wshed_',rseg_name, sep = ''),
    ftype=rseg_ftype,
    bundle='watershed'
  ),
  TRUE
)

model <- RomProperty$new(
  ds,
  list(
    varkey="om_model_element", 
    propname=riverseg$name,
    featureid=riverseg$hydroid, 
    entity_type="dh_feature", 
    propcode=model_version
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


# Uploading constants

model_constant_file_path <- RomProperty$new(
  ds, list(
    varkey="om_class_textField", 
    featureid=model_scenario$pid,
    entity_type='dh_properties',
    propname = 'file_path'
  ),
  TRUE
)
model_constant_file_path$propcode <- as.character(file_path)
model_constant_file_path$save(TRUE)


model_constant_Qout <- RomProperty$new(
  ds, list(
    varkey="om_class_Constant",
    featureid=model_scenario$pid,
    entity_type='dh_properties',
    propname = 'Qout'
  ),
  TRUE
)
model_constant_Qout$propvalue <- as.numeric(Qout_mean)
model_constant_Qout$save(TRUE)

model_constant_l90_Qout <- RomProperty$new(
  ds, list(
    varkey="om_class_Constant",
    featureid=model_scenario$pid,
    entity_type='dh_properties',
    propname = 'l90_Qout'
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
    propname = 'l30_Qout'
  ),
  TRUE
)
model_constant_l30_Qout$propvalue <- as.numeric(l30_Qout)
model_constant_l30_Qout$save(TRUE)

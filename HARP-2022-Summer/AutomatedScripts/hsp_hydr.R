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
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(R.utils))

# establishing location on server for storing images
omsite = "http://deq1.bse.vt.edu:81"


# Accepting command arguments:
argst <- commandArgs(trailingOnly = T)
land_segment_name <- argst[1]
scenario_name <- argst[2]
landuse <- as.character(argst[3]) # don't need quotes around landuse argument anymore
hydr_file_path <- argst[4] 
image_directory_path <- argst[5] # '/media/model/p532/out/river/p532sova_2021/images'

image_path_split <- strsplit(image_directory_path, split = '/')
# print(image_path_split[[1]][2]) # this is how to call items of a list

path_list_m2 <- as.list(image_path_split[[1]][-c(1,2,3)])
path_string_m2 <- paste(path_list_m2, collapse = "/")

# Reading in the table

hydr <- fread(hydr_file_path)
hydr$date <- as.Date(hydr$index, format = "%m/%d/%Y %H:%M")
hydr$week <- week(hydr$date)
hydr$month <- month(hydr$date)
hydr$year <- year(hydr$date)

dailyRO <- aggregate(hydr$RO, by = list(hydr$date), FUN='mean')
colnames(dailyRO) <- c('date','RO')
monthlyRO <- aggregate(hydr$RO, by = list(hydr$month, hydr$year), FUN = "mean")
colnames(monthlyAGWS) <- c("month", "year", "RO")


# Conversion to water-year ???????????????/

# From: waterSupplyModelNode.R

syear = as.integer(min(hydr$year))
eyear = as.integer(max(hydr$year))
model_run_start <- min(hydr$date)   # not sure about the "date"
model_run_end <- max(hydr$date)
if (syear < (eyear - 2)) {
  sdate <- as.Date(paste0(syear,"-10-01"))
  edate <- as.Date(paste0(eyear,"-09-30"))
  flow_year_type <- 'water'
} else {
  sdate <- as.Date(paste0(syear,"-02-01"))
  edate <- as.Date(paste0(eyear,"-12-31"))
  flow_year_type <- 'calendar'
}
hydr <- window(hydr, start = sdate, end = edate);   # not sure what this does
mode(hydr) <- 'numeric'
scen.propname<-paste0('runid_', runid)  # not sure what this does/what to input instead of runid


        # this would then be used in the values below instead of "hydr"??


# Converting units to mgd from ac.ft/ivld

convert_mgd = 7.8204342682083             #proof?
hydr$ROVOL_mgd <- hydr$ROVOL*convert_mgd
hydr$IVOL_mgd <- hydr$IVOL*convert_mgd

dailyROVOL <- aggregate(hydr$ROVOL_mgd, by = list(hydr$date), FUN='mean')
colnames(dailyRO) <- c('date','RO')
dailyIVOL <- aggregate(hydr$IVOL_mgd, by = list(hydr$date), FUN='mean')
colnames(dailyRO) <- c('date','RO')

# Mean values for outflow amount and rate, and inflow amount

ROVOL_mean <- mean(as.numeric(dailyROVOL$ROVOL_mgd))
IVOL_mean <- mean(as.numeric(dailyIVOL$IVOL_mgd))
RO_mean <- mean(as.numeric(dailyRO$RO))

# l90 and l30 from RO (IHA metric - group 2)
# l90 Runit???

RO_zoo <- zoo(dailyRO$RO, order.by = dailyRO$date)
RO_g2 <- data.frame(group2(RO_zoo))
l90_RO_Runit <- min(RO_g2$X90.Day.Min)
l30_RO_Runit <- min(RO_g2$X30.Day.Min)




# Exporting to VAHydro

      # From hsp_pwater => not modified yet

# Set up our data source
ds <- RomDataSource$new(site, rest_uname = rest_uname)
ds$get_token(rest_pw)

# TBD: get inputs from the comand line
#  For now we just load some samples
lseg_name=land_segment_name
lseg_ftype="cbp532_landseg"

landseg<- RomFeature$new(
  ds,
  list(
    hydrocode=lseg_name, 
    ftype=lseg_ftype,
    bundle='landunit'
  ), 
  TRUE
)

model <- RomProperty$new(
  ds,
  list(
    varkey="om_model_element", 
    propname=landseg$name,
    featureid=landseg$hydroid, 
    entity_type="dh_feature", 
    propcode="cbp-5.3.2" 
  ), 
  TRUE
)
model$save(TRUE)

model_scenario <- RomProperty$new( #Re-ordered scenario to be within the model element and the land use within the scenario
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

lu <- RomProperty$new(
  ds,
  list(
    varkey="om_hspf_landuse", 
    propname=landuse,
    featureid=model_scenario$pid, 
    entity_type="dh_properties", 
    propcode=landuse 
  ), 
  TRUE
)
lu$save(TRUE)

# Create/Load a model scenario property
# tstime = the run time 
# note: do not set tstime when retrieving since if we have a previous
#       timesereies event already set, we want to gt it and may not know the tstime
# 


# Uploading constants to VaHydro:
# entity-type specifies what we are attaching the constant to 


model_constant_Runit <- RomProperty$new(
  ds, list(
    varkey="om_class_Constant",
    featureid=lu$pid,
    entity_type='dh_properties',
    propname = 'l90_Runit'
  ),
  TRUE
)
model_constant_Runit$propvalue <- as.numeric(l90_Runit)
model_constant_Runit$save(TRUE)




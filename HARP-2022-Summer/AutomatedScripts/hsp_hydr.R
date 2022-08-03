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
omsite = "http://deq1.bse.vt.edu:81"


# Accepting command arguments:
argst <- commandArgs(trailingOnly = T)
river_segment_name <- argst[1]
#river_segment_name <-'OR1_7700_7980' #for testing only 
scenario_name <- argst[2]
output_file_path <- argst[3]  
image_directory_path <- argst[4] 
#image_directory_path <- '/media/model/p532/out/river/hsp2_2022/images' # for testing only 
#output_file_path='/media/model/p532/out/river/hsp2_2022' # for testing only 
#image_directory_path <- argst[5] # '/media/model/p532/out/river/p532sova_2021/images'

hydr_file_path=paste(output_file_path,'/hydr/', river_segment_name, '_hydr.csv', sep = '')
divr_file_path=paste(output_file_path,'/divr/', river_segment_name, '_divr.csv', sep = '')
ps_file_path=paste(output_file_path,'/ps_flow/', river_segment_name, '_psflow.csv', sep = '')

image_path_split <- strsplit(image_directory_path, split = '/')
# print(image_path_split[[1]][2]) # this is how to call items of a list

path_list_m2 <- as.list(image_path_split[[1]][-c(1,2,3)])
path_string_m2 <- paste(path_list_m2, collapse = "/")

# Reading in the tables
hydr <- fread(hydr_file_path)
divr <- fread(divr_file_path) # divr in units of cfs
ps_flow <- fread(ps_file_path) # ps in units of ac-ft/hr

convert_acfthr_mgd = 7.820434 

colnames(divr) = c('date','divr_cfs')
colnames(ps_flow) = c('date','ps_cfs')
divr$divr_mgd=divr$divr_cfs*1.55
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
colnames(monthlyQout) <- c("month", "year", "Qout") # Qout in units of mgd


# Conversion to water-year ???????????????/

# From: waterSupplyModelNode.R

syear = min(hydr$year)
eyear = max(hydr$year)
model_run_start <- min(hydr$date)   # not sure about the "date"
model_run_end <- max(hydr$date)
years <- seq(syear,eyear)

if (syear < (eyear - 2)) {
  sdate <- as.Date(paste0(syear,"-10-01"))
  edate <- as.Date(paste0((eyear-1),"-09-30")) # the change i made to eyear
  flow_year_type <- 'water'
} else {
  sdate <- as.Date(paste0(syear,"-02-01"))
  edate <- as.Date(paste0(eyear,"-12-31"))
  flow_year_type <- 'calendar'
}

hydr_wy <- hydr %>% filter(date > sdate) %>% filter(date < edate) # New hydr table with water year start and end dates 

dailyQout_wy <- aggregate(hydr_wy$ROVOL_mgd, by = list(hydr_wy$date), FUN='mean')
colnames(dailyQout_wy) <- c('date','Qout')
# not sure what this does
#mode(hydr) <- 'numeric'
#scen.propname<-paste0('runid_', runid)  # not sure what this does/what to input instead of runid


        # this would then be used in the values below instead of "hydr"?? 


# Mean values for outflow amount and rate, and inflow amount

Qout_mean <- mean(as.numeric(dailyQout$Qout)) # mgd
paste('Qout_mean:', Qout_mean)


Qout_zoo <- zoo(dailyQout$Qout, order.by = dailyQout$date)
Qout_g2 <- data.frame(group2(Qout_zoo))
l90_Qout <- min(Qout_g2$X90.Day.Min) # mgd
l30_Qout <- min(Qout_g2$X30.Day.Min)
paste('l90_Qout:', l90_Qout)
paste('l30_Qout:', l30_Qout)
# Exporting to VAHydro

fn_iha_mlf <- function(zoots, targetmo) {
  modat <- group1(zoots,'water','min')  # IHA function that calculates minimum monthly statistics for our data by water year	 
  print(paste("Grabbing ", targetmo, " values ", sep=''))
  g1vec <- as.vector(as.matrix(modat[,targetmo]))  # gives only August statistics
  print("Performing quantile analysis")
  x <- quantile(g1vec, 0.5, na.rm = TRUE);
  return(as.numeric(x));
}
Qout_wy_z <- zoo(dailyQout_wy$Qout, order.by = dailyQout_wy$date)
alf <- fn_iha_mlf(Qout_wy_z,'August') #The median flow of the annual minumum flows in august 

# Sept. 10%
dailyQout$month <- month(dailyQout$date)
sept_flows <- subset(dailyQout, month == '9')
sept_10 <- as.numeric(round(quantile(sept_flows$Qout, 0.10),6)) # September 10th percentile value of Qout flows with quantile 

fn_iha_7q10 <- function(zoots) {
  g2 <- group2(zoots) 
  #print("Group 2, 7-day low flow results ")
  #print(g2["7 Day Min"])
  x <- as.vector(as.matrix(g2["7 Day Min"]))
  # fudge 0 values
  # correct for zeroes?? If so, use this loop:
  # This is not an "approved" method - we need to see how the GS/other authorities handles this
  for (k in 1:length(x)) {
    if (x[k] <= 0) {
      x[k] <- 0.00000001
      print (paste("Found 0.0 average in year", g2["year"], sep = " "))
    }
  }
  x <- log(x)
  pars <- PearsonDS:::pearsonIIIfitML(x)
  x7q10 <- exp(qpearsonIII(0.1, params = pars$par))
  return(x7q10);
}
x7q10 <- fn_iha_7q10(Qout_zoo) # Avg 7-day low flow over a year period 

#For graphing purposes:
len_Qmon <- length(monthlyQout$year)


# Set up our data source
ds <- RomDataSource$new(site, rest_uname = rest_uname)
ds$get_token(rest_pw)

rseg_name=river_segment_name
rseg_ftype='cbp532' # "vahydro" 

riverseg<- RomFeature$new(
  ds,
  list(
    hydrocode=rseg_name,
    ftype=rseg_ftype,
    bundle='watershed'
  ),
  TRUE
)
#riverseg$save(TRUE)

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

model_constant_hydr_path <- RomProperty$new(
  ds, list(
    varkey="om_class_textField",  # change var key since it's not a constant 
    featureid=model_scenario$pid,
    entity_type='dh_properties',
    propname = 'hydr_file_path'
  ),
  TRUE
)
model_constant_hydr_path$propcode <- as.character(hydr_file_path)
model_constant_hydr_path$save(TRUE)

#Creating Qout container;
model_constant_Qout_cont<- RomProperty$new(
  ds, list(
    varkey="om_class_Constant",
    featureid=model_scenario$pid,
    entity_type='dh_properties',
    propname = 'Qout_stats'
  ),
  TRUE
)

model_constant_Qout_cont$propcode <- paste("River seg Outflow")
model_constant_Qout_cont$save(TRUE)

model_constant_Qout <- RomProperty$new(
  ds, list(
    varkey="om_class_Constant",
    featureid=model_constant_Qout_cont$pid,
    entity_type='dh_properties',
    propname = 'Qout_mgd'
  ),
  TRUE
)
model_constant_Qout$propvalue <- as.numeric(Qout_mean)
model_constant_Qout$save(TRUE)


model_constant_l90_Qout <- RomProperty$new(
  ds, list(
    varkey="om_class_Constant",
    featureid=model_constant_Qout_cont$pid,
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
    featureid=model_constant_Qout_cont$pid,
    entity_type='dh_properties',
    propname = 'l30_Qout_mgd'
  ),
  TRUE
)
model_constant_l30_Qout$propvalue <- as.numeric(l30_Qout)
model_constant_l30_Qout$save(TRUE)


model_constant_sept10 <- RomProperty$new(
  ds, list(
    varkey="om_class_Constant",
    featureid=model_scenario$pid,
    entity_type='dh_properties',
    propname = 'sept_10'
  ),
  TRUE
)
model_constant_sept10$propvalue <- as.numeric(sept_10)
model_constant_sept10$save(TRUE)


model_constant_alf <- RomProperty$new(
  ds, list(
    varkey="om_class_Constant",
    featureid=model_scenario$pid,
    entity_type='dh_properties',
    propname = 'alf'
  ),
  TRUE
)
model_constant_alf$propvalue <- as.numeric(alf)
model_constant_alf$save(TRUE)



model_constant_x7q10 <- RomProperty$new(
  ds, list(
    varkey="om_class_Constant",
    featureid=model_scenario$pid,
    entity_type='dh_properties',
    propname = 'x7q10'
  ),
  TRUE
)
model_constant_x7q10$propvalue <- as.numeric(x7q10)
model_constant_x7q10$save(TRUE)

#Exporting graph of Qout to Vahydro
save_url = paste(omsite, '/', path_string_m2, sep ='')

fname <- paste(
  image_directory_path,paste0( river_segment_name, '.','fig.Qout','.png'), # building file name
  sep = '/'
)
furl <- paste(
  save_url,paste0( river_segment_name,'.','fig.Qout', '.png'),
  sep = '/'
)
png(fname) #fname is a character string with file name
plot(monthlyQout$Qout, type = 'l', col = 'blue', ylab = 'Qout (mgd)', xaxt = 'n', xlab = NA,)
title(main = 'Outflow from the River Segment', sub = 'Monthly average values are plotted')
axis(1, at = seq(0,len_Qmon,12), labels = years)
dev.off()
print(paste("Saved file: ", fname, "with URL", furl))
model_graph1 <- RomProperty$new(
  ds, list(
    varkey="dh_image_file",
    featureid=model_constant_Qout_cont$pid,
    entity_type='dh_properties',
    propcode = furl,
    propname = 'fig.Qout'
  ),
  TRUE
)
model_graph1$save(TRUE)






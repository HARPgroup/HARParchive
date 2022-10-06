# This script will perform analysis & generate graphs of river segment data
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
input_file_path <- argst[3] 
#input_file_path='/media/model/p532/out/river/hsp2_2022/hydr/' #for testing 
image_directory_path <- argst[4]
#image_directory_path <- '/media/model/p532/out/river/hsp2_2022/images/' # for testing only 
model_version <- argst[5]

if (!file.exists(input_file_path)) {
  dir.create(file.path(input_file_path)) #creates directory if one does not yet exists
}
if (!file.exists(image_directory_path)) {
  dir.create(file.path(image_directory_path)) #creates directory if one does not yet exists
}

image_path_split <- strsplit(image_directory_path, split = '/')

path_list_m2 <- as.list(image_path_split[[1]][-c(1,2,3)])
path_string_m2 <- paste(path_list_m2, collapse = "/")

# Reading in the tables
message(paste("loading", input_file_path))
hydr <- fread(input_file_path)
#divr <- fread(divr_file_path) # divr in units of mgd
#ps_flow <- fread(ps_file_path) # ps in units of mgd

# Adding columns for daily and monthly values
dailyQout <- aggregate(hydr$Qout, by = list(hydr$date), FUN='mean')  
colnames(dailyQout) <- c('date','Qout') # Qout in units of cfs
monthlyQout <- aggregate(hydr$Qout, by = list(hydr$month, hydr$year), FUN = "mean")
colnames(monthlyQout) <- c("month", "year", "Qout") # Qout in units of cfs


# From: waterSupplyModelNode.R

syear = min(hydr$year)
eyear = max(hydr$year)
model_run_start <- min(hydr$date)   
model_run_end <- max(hydr$date)
years <- seq(syear,eyear)

if (syear < (eyear - 2)) {
  sdate <- as.Date(paste0(syear,"-10-01"))
  edate <- as.Date(paste0((eyear-1),"-09-30")) 
  flow_year_type <- 'water'
} else {
  sdate <- as.Date(paste0(syear,"-02-01"))
  edate <- as.Date(paste0(eyear,"-12-31"))
  flow_year_type <- 'calendar'
}

hydr_wy <- hydr %>% filter(date > sdate) %>% filter(date < edate) # New hydr table with water year start and end dates 

dailyQout_wy <- aggregate(hydr_wy$Qout, by = list(hydr_wy$date), FUN='mean')
colnames(dailyQout_wy) <- c('date','Qout')

# Mean values for outflow amount and rate, and inflow amount
Qout_mean <- mean(as.numeric(dailyQout$Qout)) # cfs
paste('Qout_mean:', Qout_mean)

Qout_zoo <- zoo(dailyQout$Qout, order.by = dailyQout$date)
Qout_g2 <- data.frame(group2(Qout_zoo))
l90_Qout <- min(Qout_g2$X90.Day.Min) # cfs
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
  
  x <- as.vector(as.matrix(g2["7 Day Min"]))
  
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

# Uploading constants to VaHydro:
# entity-type specifies what we are attaching the constant to 

model_constant_hydr_path <- RomProperty$new(
  ds, list(
    varkey="om_class_textField", 
    featureid=model_scenario$pid,
    entity_type='dh_properties',
    propname = 'hydr_file_path'
  ),
  TRUE
)
model_constant_hydr_path$propcode <- as.character(input_file_path)
model_constant_hydr_path$save(TRUE)


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
#For graphing purposes:
len_Qmon <- length(monthlyQout$year)
save_url = paste(omsite, '/', path_string_m2, sep ='')
fname <- paste(
  image_directory_path,paste0( river_segment_name, '.','fig.Qout','.png'), # building file name
  sep = '/'
)
furl <- paste(
  save_url,paste0( river_segment_name,'.','fig.Qout', '.png'),
  sep = '/'
)
png(fname) 
plot(monthlyQout$Qout, type = 'l', col = 'blue', ylab = 'Qout (cfs)', xaxt = 'n', xlab = NA,)
title(main = 'Outflow from the River Segment', sub = 'Monthly average values are plotted')
axis(1, at = seq(0,len_Qmon,12), labels = years)
dev.off()
print(paste("Saved file: ", fname, "with URL", furl))
model_graph1 <- RomProperty$new(
  ds, list(
    varkey="dh_image_file",
    featureid=model_scenario$pid,
    entity_type='dh_properties',
    propcode = furl,
    propname = 'fig.Qout'
  ),
  TRUE
)
model_graph1$save(TRUE)


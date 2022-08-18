# This script will convert the pwater csv to a data table and perform 
# time series and trend analysis by generating graphs and summary statistics.
#install.packages("IHA", repos="http://R-Forge.R-project.org")
#install_github("HARPGroup/hydro-tools", force=TRUE)
basepath='/var/www/R';
source("/var/www/R/config.R") # will need file in same folder/directory

library(data.table)
library(lubridate)
library(zoo)
library(PearsonDS)
library(dplyr)
library(R.utils)


#message(R_TempDir)
# establishing location on server for storing images
omsite = "http://deq1.bse.vt.edu:81"
save_directory <-  "/var/www/html/data/proj3/out"
#landuse <- 'for' # needs to be commented when running on the server 
#land_segment_name <- 'A51800' # need to remove before using on server 
#scenario_name <- 'hsp2_2022'# need to remove before using on server 

# Accepting command arguments:
argst <- commandArgs(trailingOnly = T)
land_segment_name <- argst[1]
scenario_name <- argst[2]
landuse <- as.character(argst[3]) 
pwater_file_path <- argst[4] 
image_directory_path <- argst[5] # '/media/model/p532/out/land/hsp2_2022/images'
#image_directory_path <- '/media/model/p532/out/land/hsp2_2022/images' # needs to be commented when running on the server 
model_version <- argst[5]


image_path_split <- strsplit(image_directory_path, split = '/')
# print(image_path_split[[1]][2]) # this is how to call items of a list

path_list_m2 <- as.list(image_path_split[[1]][-c(1,2,3)])
path_string_m2 <- paste(path_list_m2, collapse = "/")


pwater <- fread(pwater_file_path)
pwater$date <- as.Date(pwater$index, format = "%m/%d/%Y %H:%M")
pwater$week <- week(pwater$date)
pwater$month <- month(pwater$date)
pwater$year <- year(pwater$date)

monthlyAGWS <- aggregate(pwater$AGWS, by = list(pwater$month, pwater$year), FUN = "mean")
colnames(monthlyAGWS) <- c("month","year", "AGWS")
yearlyAGWS <- aggregate(pwater$AGWS, by = list(pwater$year), FUN = "mean")
colnames(yearlyAGWS) <- c("year", "AGWS")


# 1. Decomposition: 
# response = trend + seasonal + random
# $trend, $seasonal, and $random can be individually plotted from the stacked plot

AGWS_ts <- ts(monthlyAGWS$AGWS, start = c(1984,1), end = c(2020,12), frequency = 12)

agws_decomp <- decompose(AGWS_ts, type = "multiplicative") #multiplicative seasonality was chosen

# 2. Yearly Median - stats

AGWS_median <- aggregate(pwater$AGWS, by = list(pwater$year), FUN = "median")
colnames(AGWS_median) <- c("year", "median")

median_lm <- lm(median~year, data = AGWS_median)

slope <- summary(median_lm)$coefficients[2]
rsquared <- summary(median_lm)$r.squared
p <- summary(median_lm)$coefficients[2,4]

# 3. 25th percentile Yearly Median - stats

quan_fun <- function(pwater) {   #Creating a function to use in aggregate function for 25th percentile 
  quantile(pwater, probs = .25)
}
quan_ag <- aggregate(pwater$AGWS, by = list(pwater$year), FUN = quan_fun)
colnames(quan_ag) <- c('year','q1')
AGWS_median$q1 <- quan_ag$q1

min_lim <- min(AGWS_median$q1)
max_lim <- max(AGWS_median$median)

lm_25 <- lm(q1~year, data = AGWS_median)

slope_25th <- summary(lm_25)$coefficients[2]
rsquared_25th <- summary(lm_25)$r.squared
p_25th <- summary(lm_25)$coefficients[2,4]


# Exporting to VAHydro

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
#Implementing containers for VAHydro

model_constant_median_cont<- RomProperty$new(
  ds, list(
    varkey="om_class_Constant",
    featureid=lu$pid,
    entity_type='dh_properties',
    propname = 'AGWSmedian'
  ),
  TRUE
)

model_constant_median_cont$propcode <- paste("Median AGWS summary stats")
model_constant_median_cont$save(TRUE)

model_constant_25perc_cont<- RomProperty$new(
  ds, list(
    varkey="om_class_Constant",
    featureid=lu$pid,
    entity_type='dh_properties',
    propname = 'AGWS25perc'
  ),
  TRUE
)

model_constant_25perc_cont$propcode<- paste("25th Percentile AGWS summary stats")
model_constant_25perc_cont$save(TRUE)


model_constant_rsq50 <- RomProperty$new(
  ds, list(
    varkey="om_class_Constant",
    featureid=model_constant_median_cont$pid,
    entity_type='dh_properties',
    propname = 'rsquared_med'
  ),
  TRUE
)
model_constant_rsq50$propvalue <- as.numeric(rsquared)
model_constant_rsq50$save(TRUE)

model_constant_slope50 <- RomProperty$new(
  ds, list(
    varkey="om_class_Constant",
    featureid=model_constant_median_cont$pid,
    entity_type='dh_properties',
    propname = 'slope_med'
  ),
  TRUE
)
model_constant_slope50$propvalue <- as.numeric(slope)
model_constant_slope50$save(TRUE)

model_constant_p50 <- RomProperty$new(
  ds, list(
    varkey="om_class_Constant",
    featureid=model_constant_median_cont$pid,
    entity_type='dh_properties',
    propname = 'p_med'
  ),
  TRUE
)
model_constant_p50$propvalue <- as.numeric(p) 
model_constant_p50$save(TRUE)

model_constant_rsq25 <- RomProperty$new(
  ds, list(
    varkey="om_class_Constant",
    featureid=model_constant_25perc_cont$pid,
    entity_type='dh_properties',
    propname = 'rsquared_25th'
  ),
  TRUE
)
model_constant_rsq25$propvalue <- as.numeric(rsquared_25th)
model_constant_rsq25$save(TRUE)

model_constant_slope25 <- RomProperty$new(
  ds, list(
    varkey="om_class_Constant",
    featureid=model_constant_25perc_cont$pid,
    entity_type='dh_properties',
    propname = 'slope_25th'
  ),
  TRUE
)
model_constant_slope25$propvalue <- as.numeric(slope_25th)
model_constant_slope25$save(TRUE)

model_constant_p25 <- RomProperty$new(
  ds, list(
    varkey="om_class_Constant",
    featureid=model_constant_25perc_cont$pid,
    entity_type='dh_properties',
    propname = 'p_25th'
  ),
  TRUE
)
model_constant_p25$propvalue <- as.numeric(p_25th)
model_constant_p25$save(TRUE)

# Add code here to export graphs 
save_url = paste(omsite, '/', path_string_m2, sep ='')
# For graph 1
fname <- paste(
  image_directory_path,paste0(landuse,'',land_segment_name,'.', 'fig.AGWSdecomp', '.png'), # building file name
  sep = '/'
)
furl <- paste(
  save_url,paste0(landuse,'',land_segment_name,'.', 'fig.AGWSdecomp', '.png'),
  sep = '/'
)
png(fname) 
plot(agws_decomp)
dev.off()
print(paste("Saved file: ", fname, "with URL", furl))
model_graph1 <- RomProperty$new(
  ds, list(
    varkey="dh_image_file",
    featureid=lu$pid,
    entity_type='dh_properties',
    propcode = furl,
    propname = 'fig.AGWSdecomp'
  ),
  TRUE
)
model_graph1$save(TRUE)

# For graph 2
fname2 <- paste(
  image_directory_path,paste0(landuse,'',land_segment_name,'.', 'fig.AGWSmedian', '.png'), # building file name
  sep = '/'
)
furl2 <- paste(
  save_url,paste0(landuse,'',land_segment_name,'.', 'fig.AGWSmedian', '.png'),
  sep = '/'
)
png(fname2)
plot(AGWS_median$year, AGWS_median$median, type = 'l', col = 'blue', ylab = "AGWS Median (in)", xlab = NA)
title(main = "Annual Active Groundwater Storage")
abline(lm(AGWS_median$median ~ AGWS_median$year), col='red')
dev.off()
print(paste("Saved file: ", fname2, "with URL", furl2))
model_graph2 <- RomProperty$new(
  ds, list(
    varkey="dh_image_file",
    featureid=model_constant_median_cont$pid,
    entity_type='dh_properties',
    propcode = furl2,
    propname = 'fig.AGWSmedian'
  ),
  TRUE
)
model_graph2$save(TRUE)

# For graph 3
fname3 <- paste(
  image_directory_path,paste0(landuse,'',land_segment_name,'.', 'fig.AGWS25perc', '.png'), # building file name
  sep = '/'
)
furl3 <- paste(
  save_url,paste0(landuse,'',land_segment_name,'.', 'fig.AGWS25perc', '.png'),
  sep = '/'
)
png(fname3)

plot(AGWS_median$year, AGWS_median$median, type = 'l', col = 'blue', ylab = "AGWS Median (in)", xlab = NA, ylim = c(min_lim,max_lim))
lines(AGWS_median$year, AGWS_median$q1 , type = 'l', col = 'forestgreen')
abline(lm(AGWS_median$q1 ~ AGWS_median$year), col='purple')
abline(lm(AGWS_median$median ~ AGWS_median$year), col='red')
legend(x = "topright", legend = c('Median', '25th Percentile'), fill = c('blue','forestgreen'), bty = 'n')
title(main = "Annual Active Groundwater Storage")
dev.off()
print(paste("Saved file: ", fname3, "with URL", furl3))
model_graph3 <- RomProperty$new(
  ds, list(
    varkey="dh_image_file",
    featureid=model_constant_25perc_cont$pid,
    entity_type='dh_properties',
    propcode = furl3,
    propname = 'fig.AGWS25perc'
  ),
  TRUE
)
model_graph3$save(TRUE)





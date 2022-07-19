# This script will convert the pwater csv to a data table and perform 
# time series and trend analysis by generating graphs and summary statistics.

#install.packages("IHA", repos="http://R-Forge.R-project.org")
#install_github("HARPGroup/hydro-tools", force=TRUE)
library(data.table)
library(lubridate)
library(zoo)
library(plyr)
library(caTools)
library(RColorBrewer)
library(IHA)
library(PearsonDS)
library(ggplot2)
library(dplyr)
library(stats)
library(R.utils)
library(hydrotools)

#message(R_TempDir)
basepath='/var/www/R';
source("/var/www/R/config.R") # will need file in same folder/directory
# establishing location on server for storing images
omsite = "http://deq1.bse.vt.edu:81"
# save_directory <-  "/var/www/html/data/proj3/out"
#landuse <- 'for' # needs to be commented when running on the server 
#land_segment_name <- 'A51800' # need to remove before using on server 
#scenario_name <- 'p532sova_2021'# need to remove before using on server 

# Accepting command arguments:
argst <- commandArgs(trailingOnly = T)
land_segment_name <- argst[1]
scenario_name <- argst[2]
landuse <- as.character(argst[3]) # don't need quotes around landuse argument anymore
pwater_file_path <- argst[4] 
image_directory_path <- argst[5] # '/media/model/p532/out/land/p532sova_2021/images'
#image_directory_path <- '/media/model/p532/out/land/p532sova_2021/images' # needs to be commented when running on the server 
save_directory <-  image_directory_path


image_path_split <- strsplit(image_directory_path, split = '/')
# print(image_path_split[[1]][2]) # this is how to call items of a list

pwater <- fread(pwater_file_path)
pwater$date <- as.Date(pwater$index, format = "%m/%d/%Y %H:%M")
pwater$week <- week(pwater$date)
pwater$month <- month(pwater$date)
pwater$year <- year(pwater$date)
yearlyAGWS <- aggregate(pwater$AGWS, by = list(pwater$year), FUN = "mean")
colnames(yearlyAGWS) <- c("year", "AGWS")


# 1. Decomposition: 
# response = trend + seasonal + random
# $trend, $seasonal, and $random can be individually plotted from the stacked plot

AGWS_ts <- ts(monthlyAGWS$AGWS, start = c(1984,1), end = c(2020,12), frequency = 12)

agws_decompM <- decompose(AGWS_ts, type = "multiplicative") #multiplicative seasonality was chosen
plot(agws_decompM) #to be pushed to VAHydro


# 2. Yearly Median

AGWS_median <- aggregate(pwater$AGWS, by = list(pwater$year), FUN = "median")
colnames(AGWS_median) <- c("year", "median")

plot(AGWS_median, type = 'l', col = 'blue', ylab = "AGWS median (in)", xlab = NA)
title(main = "Active Groundwater Storage Yearly Median")
abline(lm(AGWS_median$median ~ AGWS_median$year), col='red')

median_lm <- lm(AGWS_median~year, data = AGWS_yearly_median)
slope <- summary(median_lm)$coefficients[2]
Rsq <- summary(median_lm)$r.squared
p <- summary(median_lm)$coefficients[2,4]


# 3. 25th percentile Yearly Median

AGWS_25 <- quantile(yearlyAGWS$AGWS, probs = .25)

AGWS_perc <- AGWS_median$median - AGWS_25
perc_df <- data.frame(AGWS_median$year, AGWS_perc)
colnames(perc_df) <- c("year", "median_25")

plot(AGWS_median$year, AGWS_median$median, type = 'l', col = 'blue', ylab = "AGWS Median (in)", xlab = NA, ylim = c(-0.2,0.4))
lines(perc_df$year, perc_df$median_25 , type = 'l', col = 'forestgreen')
abline(lm(perc_df$median_25 ~ perc_df$year), col='purple')
abline(lm(AGWS_median$median ~ AGWS_median$year), col='red')
legend(x = 2010,y = 0.35, legend = c('Median', '25th Percentile'), fill = c('blue','forestgreen'), bty = 'n')
title(main = "Yearly Active Groundwater Storage")

lm_25 <- lm(median_25~year, data = perc_df)
slope_25 <- summary(lm_25)$coefficients[2]
Rsq_25 <- summary(lm_25)$r.squared
p_25 <- summary(lm_25)$coefficients[2,4]


# Exporting to VAHydro            (=> decomp.fig, )

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
    propname = 'l90_Runit',
    propvalue= l90_Runit
  ),
  TRUE
)
model_constant_Runit$save(TRUE)

model_constant_agwo_Runit <- RomProperty$new(
  ds, list(
    varkey="om_class_Constant",
    featureid=lu$pid,
    entity_type='dh_properties',
    propname = 'l90_agwo_Runit',
    propvalue= l90_agwo_Runit
  ),
  TRUE
)
model_constant_agwo_Runit$save(TRUE)


# Add code here to export graphs 
save_url = paste(omsite,image_path_split[[1]][4],image_path_split[[1]][5],image_path_split[[1]][6],image_path_split[[1]][7],image_path_split[[1]][8],sep ='/')
# For graph 1
fname <- paste(
  save_directory,paste0(landuse,'',land_segment_name,'.', 'fig.AGWS', '.png'), # building file name
  sep = '/'
)
furl <- paste(
  save_url,paste0(landuse,'',land_segment_name,'.', 'fig.AGWS', '.png'),
  sep = '/'
)
png(fname) #fname is a character string with file name
years <- seq(1984,2020,1)
plot(monthlyAGWS$AGWS, type ='l', ylab = 'AGWS (in)', xaxt = 'n', xlab = NA, col = 'blue')
axis(1, at = seq(6,438,12), labels = years) 
title(main = 'Active groundwater storage', sub = 'Monthly average values are plotted')
dev.off()
print(paste("Saved file: ", fname, "with URL", furl))
model_graph1 <- RomProperty$new(
  ds, list(
    varkey="dh_image_file",
    featureid=lu$pid,
    entity_type='dh_properties',
    propcode = furl,
    propname = 'fig.AGWS'
  ),
  TRUE
)
model_graph1$save(TRUE)

# For graph 2
fname2 <- paste(
  save_directory,paste0(landuse,'',land_segment_name,'.', 'fig.totalOut', '.png'), # building file name
  sep = '/'
)
furl2 <- paste(
  save_url,paste0(landuse,'',land_segment_name,'.', 'fig.totalOut', '.png'),
  sep = '/'
)
png(fname2)
ggplot(monthlyAGWO, aes(date, AGWO)) + geom_line(aes(col = 'blue'), size = 0.25)  + 
  geom_line(aes(y=SURO, col = 'red'), size = 0.25) +
  geom_line(aes(y=IFWO, col = 'dark green'), size = 0.25) +
  labs (x = NULL, y = 'Flow (cfs/sq mi)') + 
  ggtitle('Elements of total outflow from the land segment ') +
  scale_color_identity(name = NULL, breaks=c('red','dark green','blue'), labels = c('Runoff', 'Interflow', 'Baseflow'), guide = 'legend') +
  theme(legend.position = 'bottom')
dev.off()
print(paste("Saved file: ", fname2, "with URL", furl2))
model_graph2 <- RomProperty$new(
  ds, list(
    varkey="dh_image_file",
    featureid=lu$pid,
    entity_type='dh_properties',
    propcode = furl2,
    propname = 'fig.totalFlowOut'
  ),
  TRUE
)
model_graph2$save(TRUE)






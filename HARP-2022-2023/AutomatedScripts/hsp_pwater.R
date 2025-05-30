# This script will convert the pwater csv to a data table and perform analysis & generate graphs 
#install.packages("IHA", repos="http://R-Forge.R-project.org")
#install_github("HARPGroup/hydro-tools", force=TRUE)
basepath='/var/www/R';
source("/var/www/R/config.R")

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
library(R.utils)


#message(R_TempDir)

# establishing location on server for storing images
omsite = "http://deq1.bse.vt.edu:81"
#landuse <- 'for' # needs to be commented when running on the server 
#land_segment_name <- 'A51019' # need to comment before using on server 
#scenario_name <- 'hsp2_2022'# need to comment before using on server 

# Accepting command arguments:
argst <- commandArgs(trailingOnly = T)
land_segment_name <- argst[1]
scenario_name <- argst[2]
landuse <- as.character(argst[3]) # don't need quotes around landuse argument anymore
pwater_file_path <- argst[4] 
image_directory_path <- argst[5] # '/media/model/p532/out/land/p532sova_2021/images'
model_version <- argst[6]


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
colnames(monthlyAGWS) <- c("month", "year", "AGWS")

#Creating column in pwater for baseflow in units of cfs/sq mi
convert_cfs_sqm = 645.3333333
pwater$AGWO_ <- pwater$AGWO*convert_cfs_sqm # AGWO_ has units of cfs/sq mi

dailyAGWO_ <- aggregate(pwater$AGWO_, by=list(pwater$date), FUN='mean')
colnames(dailyAGWO_) <- c('date','AGWO')
dailyAGWO_$month <- month(dailyAGWO_$date)
dailyAGWO_$year <- year(dailyAGWO_$date)
monthlyAGWO <- aggregate(dailyAGWO_$AGWO, by = list(dailyAGWO_$month, dailyAGWO_$year), FUN = "mean")
colnames(monthlyAGWO) <- c('month', 'year', 'AGWO')
monthlyAGWO$date <-  as.Date(paste(monthlyAGWO$month, monthlyAGWO$year, '15'), '%m %Y %d')

# For graph 2
monthlySURO <- aggregate(pwater$SURO, by = list(pwater$month, pwater$year), FUN = 'mean')
colnames(monthlySURO) <- c('month','year','SURO')
monthlySURO$SURO_ <- monthlySURO$SURO*convert_cfs_sqm # the SURO_ column is in units of cfs/sq mi
monthlyIFWO <- aggregate(pwater$IFWO, by = list(pwater$month, pwater$year), FUN = 'mean')
colnames(monthlyIFWO) <- c('month','year','IFWO')
monthlyIFWO$IFWO_ <- monthlyIFWO$IFWO*convert_cfs_sqm # the IFWO_ column is in units of cfs/sq mi
monthlyAGWO$SURO <- monthlySURO$SURO_
monthlyAGWO$IFWO <- monthlyIFWO$IFWO_


#IHA group 2 metrics
pwater$sum <- pwater$AGWO+pwater$IFWO+pwater$SURO
pwater$sum_ <- pwater$sum*convert_cfs_sqm         
monthlySum <- aggregate(pwater$sum_, by = list(pwater$month, pwater$year), FUN = 'mean')
colnames(monthlySum) <- c('month','year','sum')
monthlySum$date <- monthlyAGWO$date
monthlyAGWO$sum <- monthlySum$sum # maybe not needed?

dailySum <- aggregate(pwater$sum_, by = list(pwater$date), FUN = 'mean')
colnames(dailySum) <- c('date','sum')
dailyAGWO_$sum <- dailySum$sum
dailyAGWOz <- zoo(dailyAGWO_$AGWO, order.by = dailyAGWO_$date)
dailySumz <- zoo(dailyAGWO_$sum, order.by = dailyAGWO_$date)
sum_g2 <- data.frame(group2(dailySumz))
l90_Runit <- min(sum_g2$X90.Day.Min)
AGWO_g2 <- data.frame(group2(dailyAGWOz))
l90_agwo_Runit <- min(AGWO_g2$X90.Day.Min)

#Exporting to VAHydro

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

model_constant_agwo_Runit <- RomProperty$new(
  ds, list(
    varkey="om_class_Constant",
    featureid=lu$pid,
    entity_type='dh_properties',
    propname = 'l90_agwo_Runit'
  ),
  TRUE
)
model_constant_agwo_Runit$propvalue <- as.numeric(l90_agwo_Runit)
model_constant_agwo_Runit$save(TRUE)


# Add code here to export graphs 
save_url = paste(omsite, '/', path_string_m2, sep ='')
# For graph 1
fname <- paste(
  image_directory_path,paste0(landuse,'',land_segment_name,'.', 'fig.AGWS', '.png'), # building file name
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
  image_directory_path,paste0(landuse,'',land_segment_name,'.', 'fig.totalOut', '.png'), # building file name
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
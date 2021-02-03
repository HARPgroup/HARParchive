# DESCRIPTION -----
# This code is used to create quick maps for spatial analysis of the percent error of metrics associated with river segments

# LIBRARIES -----
library(rgdal)
library(raster)

# USER INPUTS -----

# Address of "DEQ_Model_vs_USGS_Comparison" folder
# Include "DEQ_Model_vs_USGS_Comparison" in address!
container <-'C:\\Users\\Daniel\\Documents\\HARP\\GitHub\\hydro-tools\\HARP-2018\\DEQ_Model_vs_USGS_Comparison';

# Should new or original data be used?
new.or.original <- "new"

# CARRYOVER IF MASTER IS BEING RUN ----------------------------------------
if (exists("container.master") == TRUE) {
  container <- container.master
  siteNo <- siteNo.master
  new.or.original <- new.or.original.master
}

# NEW OR ORIGINAL DATA SWITCH ---------------------------------------------

if (new.or.original == "new") {
  container.cont <- "\\spatial_analysis\\user's_results\\"
} else if (new.or.original == "original") {
  container.cont <- "\\spatial_analysis\\harp_analysts'_results\\"
} else {
  print("ERROR: neither new or original data specified")
}

# IMPORT DATA -----
#The projection that you want the map to be output in requires a Proj4 code. 
#Search the projection you want (in this case WGS 1984 was used becuase it is compatible with VA Hydro) and choose the link for 'spatialreference.org'
#Once on the website click on the Proj4 link and copy and paste the provided line in quotes after its associated variable name 
Projection<- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs' 

#Importing base map
#The base map for this code is the outline of the states in the US
States<- readOGR(paste0(container, '\\spatial_analysis\\gis_layers\\cb_2017_us_state_5m'),'cb_2017_us_state_5m' )  #Pull the (location of GIS shapefile, desired shapefile)
States<- spTransform(States, CRS=Projection)                                        #Put shapefile in correct projection/coordinated system

#Importing desired area
#The desired area for this code are the river segements associated with the P532 Chesapeake Bay Model
RivSeg<- readOGR(paste0(container, '\\spatial_analysis\\gis_layers\\BaseMap'), 'AlteredRiverSegs')             #Pull the (location of GIS shapefile, desired shapefile)
RivSeg<- spTransform(RivSeg, CRS=Projection)                                     #Put shapefile in correct projection/coordinated system

#Importing desired data
#The desired data for this code is the metrics that have been run for the river segments located outside of the Cheasapeake Bay Watershed
#Metrics<- read.csv(paste0(container,'\\results\\all.segments.pct.error.csv'))    #Pull the (location of desired data table)
Metrics <- read.csv(file=paste(container,'\\spatial_analysis\\results\\all.segments.pct.error.csv',sep=""), header=TRUE, sep=",")    #Pull the (location of desired data table)

#Determining number of metrics
num.metrics <- length(Metrics) - 1
# This eliminates the gage number, river segment ID, or X value from contributing to the count

# Initiating counter
ctr <- 1

# Determining metric column names
metric.names <- as.character(colnames(Metrics))

for (ctr in 1:num.metrics) {

DesiredMetric<- metric.names[ctr+1]

# SETTING UP FOR LOOP -----
#Create an empty column for the desired data you would like to be applied to your river segments
#The metrics will be added to the shapefile for the river segments as seprate columns and the desired metric will be given its own column
RivSeg@data$RiverSeg<-as.character(RivSeg@data$RiverSeg)
RivSeg@data$Metric<-NA

Metrics[,paste0("DesiredMetric")] <- Metrics[DesiredMetric]

# LOOP TO ASSOCIATE RIVER SEGMENT AND DATA -----
#The loop will run and add the desired metrics column to any segment that has a matching river segment ID with that metric
for (i in 1:length(RivSeg@data$RiverSeg)){
  if (RivSeg@data$RiverSeg[i]%in%Metrics$X){ #if the river segment ID is in the metrics file make it true, if not make it false
    RivSeg@data$Metric[i]<- Metrics$DesiredMetric[Metrics$X==RivSeg@data$RiverSeg[i]]
  }
}

# GRAPHING -----
title<-paste0((DesiredMetric),' Percent Error')

dir.create(paste0(container,"\\spatial_analysis\\user's_results\\error_maps"), showWarnings = FALSE);
png(filename=paste0(container,"\\spatial_analysis\\user's_results\\error_maps\\", DesiredMetric ,"- Error.png"), 
    width=1400, height=950, units="px")

RivSeg@data$color<- cut(RivSeg@data$Metric,c(-Inf,-50,-20,-5,5,20,50,Inf), labels=c('firebrick4', 'firebrick3', 'firebrick1', 'white', 'chartreuse1','chartreuse3', 'chartreuse4'))
SouthernRivers<- RivSeg[!is.na (RivSeg@data$color),]
plot(SouthernRivers, col=paste0(SouthernRivers@data$color))
plot(States, add=TRUE, col='gray')
lines(States, col='white')
plot(SouthernRivers, col=paste0(SouthernRivers@data$color), add=T)
legend("bottomright", legend=c('< -50', '-50 to -20', '-20 to -5', '-5 to 5', '5 to 20','20 to 50', '> 50'), col=c('firebrick4', 'firebrick3', 'firebrick1', 'white', 'chartreuse1','chartreuse3', 'chartreuse4'), lty=0, pch=15, pt.cex=7, bty='n', y.intersp=0.75, x.intersp=0.3, cex=4, lwd=2)
legend("topleft", legend=c(title), lty=0, pt.cex=3, bty='n', y.intersp=0.75, x.intersp=0.3, cex=5, lwd=2)

dev.off()

# Incrementing counter
ctr <- ctr + 1
}
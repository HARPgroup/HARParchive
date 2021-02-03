# DESCRIPTION -----
#Compiled by Haiely Alspaugh 
#6/28/18
#This code is used to create quick maps for spatial analysis of the percent error of metrics associated with river segments
#Adjustments will need to be made for the desired metric (line 10) and file locations (lines 19,24 and 29)

# OPEN LIBRARIES -----
library(rgdal)
library(raster)

# USER INPUTS -----
DesiredMetric<- 'AugLowFlow'
folder_location<- 'C:\\Users\\HaileyMae\\Downloads'

# IMPORT DATA -----
#The projection that you want the map to be output in requires a Proj4 code. 
#Search the projection you want (in this case WGS 1984 was used becuase it is compatible with VA Hydro) and choose the link for 'spatialreference.org'
#Once on the website click on the Proj4 link and copy and paste the provided line in quotes after its associated variable name 
Projection<- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs' 

#Importing base map
#The base map for this code is the outline of the states in the US
States<- readOGR('E:\\SouthernRivers\\cb_2017_us_state_5m','cb_2017_us_state_5m' )  #Pull the (location of GIS shapefile, desired shapefile)
States<- spTransform(States, CRS=Projection)                                        #Put shapefile in correct projection/coordinated system

#Importing desired area
#The desired area for this code are the river segements associated with the P532 Chesapeake Bay Model
RivSeg<-readOGR ('E:\\SouthernRivers\\BaseMap', 'AlteredRiverSegs')             #Pull the (location of GIS shapefile, desired shapefile)
RivSeg<-spTransform(RivSeg, CRS=Projection)                                     #Put shapefile in correct projection/coordinated system

#Importing desired data
#The desired data for this code is the metrics that have been run for the river segments located outside of the Cheasapeake Bay Watershed
Metrics<- read.csv('E:\\SouthernRivers\\Percent Error All Metrics.csv')    #Pull the (location of desired data table)

# SETTING UP FOR LOOP -----
#Create an empty column for the desired data you would like to be applied to your river segments
#The metrics will be added to the shapefile for the river segments as seprate columns and the desired metric will be given its own column
RivSeg@data$RiverSeg<-as.character(RivSeg@data$RiverSeg)
RivSeg@data$Metric<-NA

Metrics[,paste0("DesiredMetric")] <- Metrics[DesiredMetric]

# LOOP TO ASSOCIATE RIVER SEGMENT AND DATA -----
#The loop will run and add the desired metrics column to any segment that has a matching river segment ID with that metric
for (i in 1:length(RivSeg@data$RiverSeg)){
  if (RivSeg@data$RiverSeg[i]%in%Metrics$river_seg){ #if the river segment ID is in the metrics file make it true, if not make it false
    RivSeg@data$Metric[i]<- Metrics$DesiredMetric[Metrics$river_seg==RivSeg@data$RiverSeg[i]]
  }
}

# GRAPHING -----
title<-paste0((DesiredMetric),' Percent Error')

dir.create(paste0(folder_location,"\\USGStoModel\\OUTPUT\\SpatialComparisons"), showWarnings = FALSE);
png(filename=paste0(folder_location,"\\USGStoModel\\OUTPUT\\SpatialComparisons\\", DesiredMetric ,"- Error.png"), 
    width=1400, height=950, units="px")

RivSeg@data$color<- cut(RivSeg@data$Metric,c(-Inf,-50,-20,20,50,Inf), labels=c('red', 'palegreen', 'green4', 'yellowgreen', 'orange'))
SouthernRivers<- RivSeg[!is.na (RivSeg@data$color),]
plot(SouthernRivers, col=paste0(SouthernRivers@data$color))
plot(States, add=TRUE, col='gray')
lines(States, col='white')
plot(SouthernRivers, col=paste0(SouthernRivers@data$color), add=T)
legend("bottomright", legend=c('< -50', '-50 to -20', '-20 to 20', '20 to 50', '> 50'), col=c('red', 'palegreen', 'green4', 'yellowgreen', 'orange'), lty=0, pch=15, pt.cex=7, bty='n', y.intersp=0.75, x.intersp=0.3, cex=4, lwd=2)
legend("topleft", legend=c(title), lty=0, pt.cex=3, bty='n', y.intersp=0.75, x.intersp=0.3, cex=5, lwd=2)

dev.off()
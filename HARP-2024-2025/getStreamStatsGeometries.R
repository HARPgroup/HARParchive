library(hydrotools)
basepath='/var/www/R'
source('/var/www/R/config.R')
library(sqldf)
library(sf)
source("https://raw.githubusercontent.com/EmmaVJones/DEQprojects/refs/heads/main/functions/StreamStatsAutoDelineation.R")

library(dataRetrieval)
test <- dataRetrieval::readNWISsite(siteNumber  = "01634000")

testWshed <- streamStats_Delineation_single(state = 'VA', # StreamStats state info e.g. 'VA'
                                           longitude = test$dec_long_va, # longitude value, numeric
                                           latitude = test$dec_lat_va, # latitude value, numeric
                                           uid = "01634000" # Unique station identifier to append to dataset
)


#All sites in CB watershed states (whole state) that have daily average
#streamflow
statesOfInterest <- c('VA','MD','WV','DE','PA','NY')
stateSites <- list()
for (i in statesOfInterest){
  print(paste0("Accessing whatNWISdata for ",i))
  stateSite <- whatNWISdata(stateCd  = i,
                          parameterCd = '00060', statCd = '00003',
                          service = 'dv')
  stateSite$stateCd <- i
  stateSites[[i]] <- stateSite
}
#Combine the sites from all states
allSites <- do.call(rbind,stateSites)
#Write out for convenvient loading
write.csv(allSites,"allCBPNWISSites.csv",row.names = FALSE)

#Get all the USGS gages currently in dbase2
dbaseGages <- sqldf(connection = ds$connection,"
  SELECT DISTINCT hydrocode
  FROM dh_feature
  WHERE bundle = 'watershed'
  AND ftype = 'usgs_full_drainage'
")
#Convert the hydrocodes to USGS gage numbers
dbaseGages <- gsub('usgs_ws_([0-9]+)','\\1',dbaseGages$hydrocode)
#Exclude any gages that had non-standard names (e.g. not in the form of
#usgs_ws_01664400)
dbaseGages <- dbaseGages[grepl('^[0-9]',dbaseGages)]

#Remove any gages in dbase2 from newSites
newSites <- allSites[!(allSites$site_no %in% dbaseGages),]
#Only keep unique combinations of site_no and lat/long. There are otherwise some
#duplicate site numbers as the loc_web_ds has changed
newSites <- newSites[!duplicated(paste0(newSites$site_no,newSites$dec_lat_va,newSites$dec_long_va)),]

#Find the geometry associated with each gage. This will take a LONG time as it
#is thousands of queries on StreamStats servers
newGeometries <- streamStats_Delineation(state = newSites$stateCd,
                                         latitude = newSites$dec_lat_va,
                                         longitude = newSites$dec_long_va,
                                         uid = newSites$site_no)





#From DOI Gages II project, we can find shapefile of gages that
#"The GAGES II dataset consists of gages which have had either 20+ complete years
#(not necessarily continuous) of discharge record since 1950, or are currently
#active, as of water year 2009, and whose watersheds lie within the United
#States, including Alaska, Hawaii, and Puerto Rico"
#https://www.sciencebase.gov/catalog/item/631405bbd34e36012efa304a
list.files("boundaries_shapefiles_by_aggeco/boundaries-shapefiles-by-aggeco/")
#Read in shapefile
boundsAll <- sf::st_read("boundaries_shapefiles_by_aggeco/boundaries-shapefiles-by-aggeco/",
            "bas_ref_all")
#Get the CB Extent geometry
cbExtent <- sqldf(connection = ds$connection,"
SELECT hydrocode, st_AsText(dh_geofield_geom) as wkt from dh_feature_fielded
WHERE hydrocode = 'cbp6_met_coverage'
AND bundle = 'landunit'
AND ftype = 'cbp_met_grid' 
")
#Create sf object for the CB Extent
cbExtent <- st_as_sf(cbExtent,wkt = 'wkt',crs = 4326)
#Reproject the GAGES II dataset to 4326
boundsAll <- st_transform(boundsAll,4326)
#Find the gages in GAGES II that intersect the CB Model Extent
CBGages <- sf::st_intersects(boundsAll$geometry,cbExtent$wkt)
#Create a spatial object with only those gages that intersect
CBGagesSF <- boundsAll[which(lengths(CBGages) > 0),]
#Plot gages of interest
plot(CBGagesSF$geometry,axes = TRUE)
plot(add = TRUE, cbExtent$wkt)

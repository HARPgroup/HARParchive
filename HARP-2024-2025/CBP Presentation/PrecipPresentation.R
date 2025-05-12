library(hydrotools)
basepath='/var/www/R'
source('/var/www/R/config.R')


library(sqldf)
library(raster)
library(sf)

#Get the watershed coverage from the server
watershedGeo <- sqldf(
  connection = ds$connection,
  "select hydrocode,ST_asText(dh_geofield_geom) as wkt
  FROM dh_feature_fielded
  WHERE bundle = 'watershed'
  AND ftype = 'usgs_full_drainage'"
)

#Get the gage numbers as their own field and store a copy of the data
gageWatershedSF <- watershedGeo[!is.na(watershedGeo$wkt),]
gageWatershedSF$gage <- gsub(".*_(\\d+)","\\1",gageWatershedSF$hydrocode)

#Create an SF object. Specify the coordinate system and the field name in the
#data frame that contains the well-known text. In this case, WKT is the name of
#the field with the polygon geometries
gageWatershedSF <- st_as_sf(gageWatershedSF,wkt = 'wkt',crs = 4326)
#Repair broken geometries
gageWatershedSF <- st_make_valid(gageWatershedSF)
#Add shape area in coordinate system units (likely meaningless in crs 4326)
gageWatershedSF$area <- st_area(gageWatershedSF)
gageWatershedSF <- gageWatershedSF[order(gageWatershedSF$area),]

#Read in daily rasters for the same day, April 16, 2024
nldas <- rast("http://deq1.bse.vt.edu:81/met/Presentation/plots/nldas2_414123816.tiff")
daymet <- rast("http://deq1.bse.vt.edu:81/met/Presentation/plots/daymet_110401307.tiff")
prism <- rast("http://deq1.bse.vt.edu:81/met/Presentation/plots/prism_1.tiff")
breaksRaster <- seq(length.out = 255, 0, max(c(terra::minmax(nldas,compute = TRUE)[2],
                                               terra::minmax(daymet,compute = TRUE)[2],
                                               terra::minmax(prism,compute = TRUE)[2])))
breaksRaster <- unique(round(breaksRaster,0))
#An optional color pallette. Not used below, but was used in some testing:
colPallete <- colorRampPalette(c('brown3','yellow','blue','green'))


#Create image of NLDAS raster
png('nldas.png',width = 6,height = 4,units = 'in',res = 300)
plot(nldas,axes = TRUE,
     ylim = c(36,40),
     xlim = c(-84,-76),
     main = 'NLDAS2 Precipitation (mm) On April 16, 2024',
     type = "continuous",
     breaks = breaksRaster, 
     col = rev(terrain.colors(length(breaksRaster))))
plot(gageWatershedSF$wkt,add = TRUE,lwd = 0.1)
dev.off()

#Create image of daymet raster
png('daymet.png',width = 6,height = 4,units = 'in',res = 300)
plot(daymet,axes = TRUE,
     ylim = c(36,40),
     xlim = c(-84,-76),
     main = 'daymet Precipitation (mm) On April 16, 2024',
     type = "continuous",
     breaks = breaksRaster, 
     col = rev(terrain.colors(length(breaksRaster))))
plot(gageWatershedSF$wkt,add = TRUE,lwd = 0.1)
dev.off()

#Create image of PRISM raster
png('PRISM.png',width = 6,height = 4,units = 'in',res = 300)
plot(prism,axes = TRUE,
     ylim = c(36,40),
     xlim = c(-84,-76),
     main = 'PRISM Precipitation (mm) On April 16, 2024',
     type = "continuous",
     breaks = breaksRaster, 
     col = rev(terrain.colors(length(breaksRaster))))
plot(gageWatershedSF$wkt,add = TRUE,lwd = 0.1)
dev.off()

#Repeat for a single hourly NLDAS raster and a single NLDAS hourly fraction
#raster
nldasHour <- raster("http://deq1.bse.vt.edu:81/met/Presentation/plots/nldas_1hour.tiff")
png('nldasHour.png',width = 6,height = 4,units = 'in',res = 300)
plot(nldasHour,axes = TRUE,
     ylim = c(36,40),
     xlim = c(-84,-76),
     main = 'NLDAS One Hour Precipitation (mm) Sometime On 2024-04-16')
plot(gageWatershedSF$wkt,add = TRUE,lwd = 0.1)
dev.off()
nldasHourFrac <- raster("http://deq1.bse.vt.edu:81/met/Presentation/plots/nldas_1hourfrac.tiff")
png('nldasHourFrac.png',width = 6,height = 4,units = 'in',res = 300)
plot(nldasHourFrac,axes = TRUE,
     ylim = c(36,40),
     xlim = c(-84,-76),
     main = 'NLDAS One Hour Precip Frac Sometime On 2024-04-16')
plot(gageWatershedSF$wkt,add = TRUE,lwd = 0.1)
dev.off()
#Get the borders to show resolution, but focus on a single watershed
#Watershed to focus on - Strausberg?
gageNo <- "01634000"
watershedMask <- gageWatershedSF[grepl(gageNo,gageWatershedSF$hydrocode),]
#Use rasterToPolygons to get polygons of each cell buffered around the watershed
#mask so that we can plot the resolution differences between each dataset
nldasClip <- rasterToPolygons(
  raster::intersect(nldas,
                    extent(st_buffer(watershedMask,100))
  )
)
prismClip <- rasterToPolygons(
  raster::intersect(prism,
                    extent(st_buffer(watershedMask,100))
  )
)
daymetClip <- rasterToPolygons(
  raster::intersect(daymet,
                    extent(st_buffer(watershedMask,100))
  )
)
#Use xlim and ylim to ensure plot is a little more readible
png('resolution.png',width = 6,height = 4,units = 'in',res = 300)
plot(daymetClip,border = "grey70",
     lwd = 0.5, 
     xlim = c(-78.70,-78.60),
     ylim = c(38.62,38.75),
     axes = FALSE,
     main  = 'Precip. Data Source Resolution'
)
axis(1)
axis(2)
box()
plot(prismClip,border = "steelblue4", add = TRUE, lwd = 2)
plot(nldasClip,border = "firebrick4",lwd = 3, add = TRUE)
legend('topright',legend = c('NLDAS2','PRISM','daymet'),
       col = c('firebrick4','steelblue4','grey70'),
       lty = 1,lwd = c(3,2,1))
dev.off()

#Create a plot that shows the baseflow, storm hydrograph, and precip hyetographs
#for a point
gageNo <- '01634000'
#Get precip for the gage
nldasPrecip <- read.csv(paste0('http://deq1.bse.vt.edu:81/met/nldas2/precip/usgs_ws_',gageNo,'_precip_daily.csv'))
daymetPrecip <- read.csv(paste0('http://deq1.bse.vt.edu:81/met/daymet/precip/usgs_ws_',gageNo,'_precip_daily.csv'))
prismPrecip <- read.csv(paste0('http://deq1.bse.vt.edu:81/met/PRISM/precip/usgs_ws_',gageNo,'_precip_daily.csv'))
nldasPrecip$obs_date <- as.Date(nldasPrecip$obs_date)
daymetPrecip$obs_date <- as.Date(daymetPrecip$obs_date)
prismPrecip$obs_date <- as.Date(prismPrecip$obs_date)

#Get the statistics and streamsflow associated with the storm
stormFlow <- read.csv(paste0('http://deq1.bse.vt.edu:81/met/stormVol_nldas2_tiled/flow/usgs_ws_',gageNo,'-stormevent-flow.csv'))
stormStats <- read.csv(paste0('http://deq1.bse.vt.edu:81/met/stormVol_nldas2_tiled/flow/usgs_ws_',gageNo,'-stormevent-stats.csv'))
#Find the largetst storm to plot (hoping that it's one of the best to plot)
stormToPlot <- stormStats[which.max(stormStats$volumeAboveBaseQMG),]
#Get the flow data associated with the maximum storm from above and include a
#buffer of x days
bufferDays <- 3
flowToPlot <- stormFlow[as.Date(stormFlow$timestamp) >= (as.Date(stormToPlot$startDate) - bufferDays) &
                          as.Date(stormFlow$timestamp) <= (as.Date(stormToPlot$endDate) + bufferDays),]
flowToPlot$timestamp <- as.Date(flowToPlot$timestamp)
#Union precip data together in one long style data frame and limit to dates in
#flowToPlot
combinePrecip <- sqldf("
SELECT np.*, 'nldas' as precipDataSource
FROM nldasPrecip as np
INNER JOIN flowToPlot as fp
ON (fp.timestamp = np.obs_date)
UNION ALL
SELECT dp.*, 'daymet' as precipDataSource
FROM daymetPrecip as dp
INNER JOIN flowToPlot as fp
ON fp.timestamp = dp.obs_date
UNION ALL
SELECT pp.*, 'prism' as precipDataSource
FROM prismPrecip as pp
INNER JOIN flowToPlot as fp
ON fp.timestamp = pp.obs_date
ORDER BY obs_date,precipDataSource
")

png('stormMethod.png',width = 6,height = 4,units = 'in',res = 300)
par(mar = c(2,4,1,4))
barplot(combinePrecip$precip_in,
        col = c('firebrick4','steelblue4','grey70'),
        axes = FALSE, ann = FALSE)
axis(4)
mtext('Precipitation (in)',4,line = 2)
par(new = TRUE)
plot(flowToPlot$timestamp,flowToPlot$flow,col = 'black',lwd = 3,
     type = "l",ann = FALSE, axes = FALSE,
     ylim = c(0,max(flowToPlot$flow*1.2)))
lines(flowToPlot$timestamp,flowToPlot$baseQ,col = 'black',lwd = 2,lty = 3)
axis(2)
mtext('Flow (CFS)',2,line = 2)
axis.Date(1,at = seq(as.Date(min(flowToPlot$timestamp)),
                     as.Date(max(flowToPlot$timestamp)), by = "3 days"))
legend('topright',legend = c("Streamflow","Baseflow","NLDAS","PRISM","daymet"),
       pch=c(NA,NA,15,15,15),col = c('black','black','firebrick4','steelblue4','grey70'),
       lty = c(1, 3, NA, NA, NA))
box()
dev.off()

#Show observed vs predicted flow using simple LM
library(R6)
source("https://github.com/HARPgroup/meta_model/blob/main/scripts/precip/lm_analysis_plots.R")
#source("../OWS/meta_model/scripts/precip/lm_analysis_plots.R")
#Read in ratings, precip, and flow
ratings <- read.csv("http://deq1.bse.vt.edu:81/met/simple_lm_nldas2_tiled/out/usgs_ws_",gageNo,"-ratings.csv")
nldasPrecip <- read.csv(paste0('http://deq1.bse.vt.edu:81/met/simple_lm_nldas2_tiled/precip/usgs_ws_',gageNo,'_precip_weekly.csv'))
nldasFlow <- read.csv(paste0('http://deq1.bse.vt.edu:81/met/simple_lm_nldas2_tiled/flow/usgs_ws_',gageNo,'-flow.csv'))
#Get model JSON using plotBin R6 object
jsonLM <- plotBin$new()
jsonLM$fromJSON(paste0("http://deq1.bse.vt.edu:81/met/simple_lm_nldas2_tiled/stats/usgs_ws_",gageNo,"-model.json"),TRUE)

#For each week, calculate the project flow using the monthly regession
nldasPrecip$projFlow <- numeric(nrow(nldasPrecip))
for (i in 1:nrow(nldasPrecip)){
  #Find the ith week for this loop iteration
  weekFlow <- nldasPrecip[i,]
  #Get the LM coefficients from the plotBin R6 object
  coeffs <- coef(jsonLM$atts$lms[[weekFlow$mo]])
  #Calculate the projected flow based on the coefficients
  projFlow <- coeffs[2] * weekFlow$precip_cfs + coeffs[1]
  #Store in nldasPrecip
  nldasPrecip$projFlow[i] <- projFlow
}
nldasPrecip$start_date <- as.Date(nldasPrecip$start_date)

png('simpleLMRegression.png',width = 6,height = 4,units = 'in',res = 300)
  plot(nldasPrecip$start_date, nldasPrecip$obs_flow,
      type = "l",col = 'firebrick4',
      xlim = c(as.Date("1984-01-01"),as.Date("2023-12-31")),
      axes = FALSE, ann = FALSE)
  axis.Date(1,at = seq(as.Date(min(nldasPrecip$start_date)),
                  as.Date(max(nldasPrecip$start_date)), by = "5 years"))
  axis(2)
  mtext("Flow (cfs)",side = 2, line = 2)
  box()
  lines(nldasPrecip$start_date, nldasPrecip$projFlow,col = 'steelblue4')
  legend('topright',legend = c('Obs. Flow','Proj. Flow'),
         col = c('firebrick4','steelblue4'),lty = 1)
dev.off()



#Get all the land segments
#Get the watershed coverage from the server
landunitGeo <- sqldf(
  connection = ds$connection,
  "select hydrocode,ST_asText(dh_geofield_geom) as wkt
  FROM dh_feature_fielded
  WHERE bundle = 'landunit'
  AND ftype = 'cbp6_landseg'"
)
#Get the gage numbers as their own field and store a copy of the data
landUnitSF <- landunitGeo[!is.na(landunitGeo$wkt),]

#Create an SF object. Specify the coordinate system and the field name in the
#data frame that contains the well-known text. In this case, WKT is the name of
#the field with the polygon geometries
landUnitSF <- st_as_sf(landUnitSF,wkt = 'wkt',crs = 4326)
#Repair broken geometries
landUnitSF <- st_make_valid(landUnitSF)
#Add shape area in coordinate system units (likely meaningless in crs 4326)
landUnitSF$area <- st_area(landUnitSF)


#Create image of amalgamation raster with landsegments
amalg <- raster("http://deq1.bse.vt.edu:81/met/Presentation/plots/amalgamate_443263957.tiff")
png('amalgamation_landsegment.png',width = 6,height = 4,units = 'in',res = 300)
plot(amalg,axes = TRUE,
     ylim = c(36,44),
     xlim = c(-84,-72),
     main = 'Best Fit Storm Volume Precipitation (mm) On April 16, 2024')
plot(landUnitSF$wkt,add = TRUE,lwd = 0.1)
dev.off()

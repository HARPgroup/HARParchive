#For learning and experimenting with model_geoprocessor(), fn_extract_basin() and more to generate maps 
library(dataRetrieval)
library(hydrotools)

basepath='/var/www/R'
source('/var/www/R/config.R')
ds <- RomDataSource$new("http://deq1.bse.vt.edu/d.dh", rest_uname)
ds$get_token(rest_pw)

#Get all vahydro watersheds
seglist <- ds$get('dh_feature', config=list(ftype='vahydro',bundle='watershed')) #line causes R to restart - Glenn
seglist$riverseg <- str_replace(seglist$hydrocode, 'vahydrosw_wshed_', '')

#Setting up function
model_geoprocessor <- function(seg_features) {
  for (i in 1:nrow(seg_features)) {
    spone <- sp::SpatialPolygonsDataFrame(
      readWKT(seg_features[i,]$dh_geofield), 
      data=as.data.frame(as.list(subset(seg_features[i,],select=-c(dh_geofield))))
    )
    if (i == 1) {
      # start with one 
      polygons_sp <- spone
    } else {
      # append
      polygons_sp <- rbind(polygons_sp, spone)
    }
  }
  return(polygons_sp)
}

#Extract basin
app_segs <- fn_extract_basin(seglist, 'JL4_6520_6710')
app_map <- model_geoprocessor(app_segs)

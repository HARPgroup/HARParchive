# Convert any data frame w/ geometry stored as Well Known Text to "SpatialXxxDataFrame" (Xx_sp)
library(sp)
library(rgeos)
library(sf)

## Specify the data frame and the character name of the geometry column
fn_process_geom <- function(data, geom_col, format="sf") {
  for (i in 1:nrow(data)) { 
    sp.i <- sp::addAttrToGeom(
      x=rgeos::readWKT(data[i,geom_col]), #read geometry from column identified in input
      y=as.data.frame(as.list(subset(data[i,],select=-c(names(data)==geom_col)))),      
      match.ID=TRUE
    )
    if (i == 1) {
      # start with one 
      data_sp <- sp.i
    } else {
      # append
      data_sp <- rbind(data_sp, sp.i)
    }
  }
  
  if(format=="sp"){
    final <- data_sp #leave as SpatialXxxDataFrame
  }
  if(format=="sf"){
    final <- sf::st_as_sf(data_sp) #convert to simple features (default)
  }
  
  return(final) #final data frame of specified format is created with geom
}
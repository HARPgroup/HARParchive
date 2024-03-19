# Defines all HARP-analyst-written functions for dealing with spatial data
library(sf)
library(sp)

#---- fn_geoCol ----
## Determines & returns the name of the spatial geometry column in the data
fn_geoCol <- function(data){
  colname <- grep("geo",colnames(data),value=TRUE) #Why "geo"? Spatial data that has already been converted from WKT will have some form of a "geometry" column rather than one named "wkt"
  if(length(colname) > 1){
    colname <- grep("geom",colname,value=TRUE) #for the case of "dh_geofield.geom" "dh_geofield.geo_type" "dh_geofield.lat" "dh_geofield.lon", etc.
  }
  if (length(colname) == 0) {
    # try WKT 
    colname <- grep("WKT",colnames(data),value=TRUE)
  }
  if (length(colname) == 0) {
    # try WKT 
    colname <- grep("wkt",colnames(data),value=TRUE)
  }
  return(colname)
}

#---- fn_sqldf_sf ----
## Allows the usage of SQLDF with simple features (sf) data frames:
## inputs:
# statement : any SQLDF string; within the string, reference the data frames per usual
# geomback : if you want the final output to retain class sf, geomback is the character name of the dataframe you are filtering down.
     #when applicable, the sf geometry data from this old dataframe will be tacked back onto the final/filtered dataframe
# crs : coord. reference syst. ; defaults to 4326, but can be customized if desired 

fn_sqldf_sf <- function(statemt, geomback="NA", crs=4326){
  dfs <- as.environment(as.list(.GlobalEnv, all.names=TRUE)) #make a copy of the global environment
  for(i in names(dfs)){
    if( "sf" %in% class(dfs[[i]]) ){ 
      #drop the geometry on any sf objects so they can go through SQLDF
      dfs[[i]] <- sf::st_drop_geometry( dfs[[i]] )
    }
  }
  dfs[["result"]] <- sqldf(statemt, envir=dfs) #SQLDF the non-sf dataframes in the new environment (so that the global envir. retains the sf objects)
  
  if(geomback!="NA"){
    output <- merge(x=dfs[["result"]], y=.GlobalEnv[[geomback]], all.x=TRUE, all.y=FALSE) #match the newly filtered obs. to their geometries
    output <- sf::st_as_sf(output, crs=crs) #convert back to sf
  } else {
    output <- dfs[["result"]]
  }
  return(output)
}

#---- fn_centroid_coords ----
## Adds centroid X & Y coordinate columns to a data frame
## inputs:
# data : dataframe for processing
# geom_col : the character name of the column containing spatial geometry data

fn_centroid_coords <- function(data, geom_col, crs=4326) {
  if( length(grep("sfc", lapply(data, class)))==0 ){ #EXPLAIN
    sf::st_as_sf(data, wkt=geom_col, crs=crs)
  } 
  for (r in 1:nrow(data)) { #get centroid coord. for labeling
    centroid <- sf::st_centroid(data[r, geom_col])
    centroid <- as.data.frame(sf::st_coordinates(centroid)) #EXPLAIN
    data$lng[r] <- centroid$X
    data$lat[r] <- centroid$Y
  }
  return(data)
}

#---- fn_wkt_sp ----
## Convert any data frame w/ spatial data stored as Well Known Text (WKT) to "SpatialXxxDataFrame" (Xx_sp)
## inputs:
# data : dataframe for processing
# geom_col : the character name of the column containing spatial geometry data

# #commenting out because rgeos is now archived on CRAN, fn_wkt_sp is no longer used in the code
# fn_wkt_sp <- function(data, geom_col) {
#   for (i in 1:nrow(data)) { 
#     sp.i <- sp::addAttrToGeom(
#       x=rgeos::readWKT(data[i,geom_col]), #read geometry from column identified in input
#       y=as.data.frame(as.list(subset(data[i,],select=-c(names(data)==geom_col)))),      
#       match.ID=TRUE
#     )
#     if (i == 1) { #start with one 
#       data_sp <- sp.i
#     } else { #append
#       data_sp <- rbind(data_sp, sp.i)
#     }
#   }
#   return(data_sp) #final data frame of specified format is created with geom
# }

# Add centroid coordinate columns to a data frame
library(sf)

fn_centroid_coords <- function(data, geom_col, crs=4326) {
  
  if( length(grep("sfc", lapply(data, class)))==0 ){ #EXPLAIN
    #data <- fn_process_geom(data, geom_col)
    st_as_sf(data, wkt=geom_col, crs=crs)
  } 
  
  for (r in 1:nrow(data)) { #get centroid coord. for labeling
    centroid <- sf::st_centroid(data[r, geom_col])
    centroid <- as.data.frame(sf::st_coordinates(centroid)) #EXPLAIN
    data$lng[r] <- centroid$X
    data$lat[r] <- centroid$Y
  }
  
  return(data)
}
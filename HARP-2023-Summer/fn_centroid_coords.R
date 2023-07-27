# Add centroid coordinate columns to a data frame

source(paste0(github_location,"/HARParchive/HARP-2023-Summer/fn_process_geom.R"),local = TRUE)
#library(sf) is included in fn_process_geom.R

fn_centroid_coords <- function(data, geom_col) {
  
  if( length(grep("sfc", lapply(data, class)))==0 ){ #EXPLAIN
    data <- fn_process_geom(data, geom_col)
  } 
  
  for (r in 1:nrow(data)) { #get centroid coord. for labeling
    centroid <- sf::st_centroid(data[r, geom_col])
    centroid <- as.data.frame(sf::st_coordinates(centroid)) #EXPLAIN
    data$lng[r] <- centroid$X
    data$lat[r] <- centroid$Y
  }
  
  return(data)
}
library(arcpullr)

# THIS ONE WORKS
# VGIN
map_server <- "https://gismaps.vdem.virginia.gov/arcgis/rest/services"
# VA LandCover - very sparse, 
map_layer = "Download/LandCover_Downloads/MapServer/0"

map_url <- paste(map_server,map_layer,sep ="/")
map_data <- get_spatial_layer(map_url)
ggplot2::ggplot() +
  ggplot2::geom_sf(data = map_data)

# these do not yet work - it may be that this module is simply unable to handle 
# the nuance required, or, there is something that we need to include in terms of 
# extra parameters
map_server <- "https://server.arcgisonline.com/arcgis/rest/services"
# Terrain
map_layer = "World_Terrain_Base/MapServer/0"
# Hill shade
map_layer = "Elevation/World_Hillshade/MapServer/0"

map_url <- paste(map_server,map_layer,sep ="/")
map_data <- get_spatial_layer(map_url)
ggplot2::ggplot() +
  ggplot2::geom_sf(data = map_data)

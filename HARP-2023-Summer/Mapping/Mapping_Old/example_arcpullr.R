library(arcpullr)

gg_map_layer <- function(map_server, map_layer) {
  
  map_url <- paste(map_server,map_layer,sep ="/")
  map_data <- get_spatial_layer(map_url)
  plotted_map <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = map_data)
  return(plotted_map)
}

# THIS ONE WORKS
# VGIN
map_server <- "https://gismaps.vdem.virginia.gov/arcgis/rest/services"
# VA LandCover - very sparse, 
map_layer = "Download/LandCover_Downloads/MapServer/0"
gg_map_layer(map_server, map_layer)

# these do not yet work - it may be that this module is simply unable to handle 
# the nuance required, or, there is something that we need to include in terms of 

# VGIN land cover - not yet working
map_server <- "https://gismaps.vdem.virginia.gov/arcgis/rest/services"
map_layer <- "VA_Base_Layers/VA_Land_Cover/MapServer/0"
gg_map_layer(map_server, map_layer)

# ESRI - not yet working
# Terrain
map_server <- "https://server.arcgisonline.com/arcgis/rest/services"
map_layer = "World_Terrain_Base/MapServer/0"
gg_map_layer(map_server, map_layer)


# Hill shade
map_server <- "https://server.arcgisonline.com/arcgis/rest/services"
map_layer = "Elevation/World_Hillshade/MapServer/0"
gg_map_layer(map_server, map_layer)

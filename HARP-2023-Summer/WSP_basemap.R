library(dataRetrieval)
library(nhdplusTools)
library(sf)
library(ggplot2)
library(ggmap)
library(ggsn)
library(ggspatial)
library(data.table)
library(sp)
library(hydrotools)
source("https://raw.githubusercontent.com/HARPgroup/hydro-tools/master/R/om_vahydro_metric_grid.R")

basepath='/var/www/R'
source('/var/www/R/config.R')
ds <- RomDataSource$new("http://deq1.bse.vt.edu/d.dh", rest_uname)
ds$get_token(rest_pw)

# function to generate map gg object (should replace hydro-tools/GIS_functions/mapgen.R)
mapgen <- function(start_point = data.frame(lat = 37.2863888889, lon = -80.0758333333, label = "Intake"),
                   points = data.frame(lat=double(),lon=double(),label=character()),
                   rsegs_sp) {

  ######################################################################
  # process points layer
  point_layer =  st_point(c(points$lon[1], points$lat[1]))
  if (length(points[,1]) > 1) {
    for (i in 2:length(points[,1])) {
      point <- points[i,]
      point = st_point(c(point$lon[1], point$lat[1]))
      point_layer = c(point_layer, point)
    }
  }
  point_layer = st_sfc(point_layer)
  st_crs(point_layer) <- 4326
  points_labels <- as.data.frame(sf::st_coordinates(point_layer))
  points_labels$NAME <- points$label
  
  ######################################################################
  # process intake point
  start_point_layer <- st_sf(id = 1, geom = st_sfc(st_point(c(start_point$lon, start_point$lat)), crs = 4326))
  start_point_labels <- as.data.frame(sf::st_coordinates(start_point_layer))
  start_point_labels$NAME <- start_point$label
  
  ######################################################################
  # process rseg layer
  rsegs_sf <- st_as_sf(rsegs_sp)
  st_crs(rsegs_sf) <- 4326 
  
  rsegs_centroids <- rgeos::gCentroid(rsegs_sp,byid=TRUE)
  rsegs_labels <- as.data.frame(sf::st_coordinates(st_as_sf(rsegs_centroids)))
  rsegs_labels$NAME <- rsegs_sf$riverseg
  rseg_domain <- st_bbox(rsegs_sp)
  #***************************
  #* Create domain
  #***************************
  sf_use_s2(FALSE) # switch off Spherical geometry (s2) 
  out_point = sf::st_sfc(sf::st_point(c(start_point$lon, start_point$lat)), crs = 4326)
  nhd_out <- get_nhdplus(out_point)
  nhd <- plot_nhdplus(list(nhd_out$comid), actually_plot = FALSE)
  domain <- st_buffer(st_as_sfc(st_bbox(rsegs_sf)), .2)
  domain  <- plot_nhdplus(bbox = st_bbox(domain), actually_plot = FALSE)
  
  sf_bbox <- st_bbox(domain$flowline)
  ggmap_bbox <- setNames(sf_bbox, c("left", "bottom", "right", "top"))
  basemap_toner <- get_map(source = "stamen", maptype = "toner", color=c("color"), location = ggmap_bbox, zoom = 12)
  toner_map <- ggmap(basemap_toner)
  
  ######################################################################
  # generate map gg object
  map_gg <- toner_map + 
    geom_sf(data = rsegs_sf, inherit.aes = FALSE, color = "black", fill = NA, lwd = 1.5) +
    geom_sf(data = nhd$flowline,inherit.aes = FALSE,color = "blue", fill = NA, size = 0.5) +
    geom_sf(data = domain$network_wtbd,inherit.aes = FALSE,color = "blue", fill = NA, size = 1) +
    geom_sf(data = domain$off_network_wtbd,inherit.aes = FALSE,color = "blue", fill = NA, size = 1) +
    #geom_sf(data = nhd$catchment,inherit.aes = FALSE,color = "blue", fill = NA, size = 1) +
    geom_sf(data = start_point_layer, inherit.aes = FALSE, color = "black", size = 10, pch =18) +
    geom_sf(data = point_layer, inherit.aes = FALSE, color = "white", fill = "black", size = 10, pch = 21) +
    theme(text = element_text(size = 30),axis.title.x=element_blank(),axis.title.y=element_blank()) +
    
    # plot labels
    geom_text(data = rsegs_labels, aes(X, Y, label = NAME), colour = "darkgoldenrod4", size = 8) +
    geom_label(data = start_point_labels, aes(X, Y, label = NAME), colour = "black", size = 10, nudge_x = -0.019, nudge_y = 0.006) +
    geom_label(data = points_labels, aes(X, Y, label = NAME), colour = "black", size = 10, nudge_x = -0.033, nudge_y = 0.005) +
    
    # scalebar
    ggsn::scalebar(nhd$flowline, location = 'bottomleft', dist = 2, dist_unit = 'mi',transform = TRUE, model = 'WGS84',st.bottom=FALSE, st.size=12) +
    
    # north arrow
    ggspatial::annotation_north_arrow(which_north = "grid", location = "tr",height = unit(4, "cm"),width = unit(3, "cm"), style = north_arrow_orienteering(text_size = 20))
  
  return(map_gg)
}

# function to retrieve & format model segment metric & geometry data

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

################################################################################
# Example Generating A Map Using mapgen():

# set up your dataframe of points you want displayed on the map (can be gages, intakes, facilities, anything!)
gage_02054530 <- dataRetrieval::readNWISsite("02054530")
gage_02055000 <- dataRetrieval::readNWISsite("02055000")
# find the watershed outlet. 
outlet_point = data.frame(
  lat=c(37.857835),
  lon=c(-78.266634),
  label=c("Rivanna River at Palmyra")
)

# execute mapgen() function by supplying a starting_point (i.e. intake location) and your points dataframe
# Get all vahydro watersheds
seglist <- ds$get('dh_feature', config=list(ftype='vahydro',bundle='watershed'))
seglist$riverseg <- str_replace(seglist$hydrocode, 'vahydrosw_wshed_', '')
# Then, extract the basin using fn_extract_basin()
app_segs <- fn_extract_basin(seglist, 'JL4_6520_6710')
app_map <- model_geoprocessor(app_segs)

# Now get watershed users
runid.list = c("runid_600", "runid_400")
df = data.frame(runid=runid.list)
df$model_version <- 'vahydro-1.0'
df$metric <- 'wd_mgd'
df$runlabel <- paste('WD MGD', df$runid)

fac_data <- om_vahydro_metric_grid( 
  metric=FALSE, runids=df, featureid='all', 
  entity_type='dh_feature', bundle='facility',
  ftype='all', model_version = 'vahydro-1.0',
  base_url = "http://deq1.bse.vt.edu/d.dh/entity-model-prop-level-export",
  ds = ds
)
fac_case <- sqldf(
  "select a.* from fac_data as a
   left outer join app_segs as b
   on (a.riverseg = b.riverseg)
  where b.riverseg is not null 
  "
)
# filter out WSP entries (optional)
fac_case <- sqldf("select * from fac_case where hydrocode not like 'wsp_%'")
mp_points = data.frame(lat=numeric(), lon=numeric(), label=character())
for (i in 1:nrow(fac_case)) {
  fc <- fac_case[i,]
  fac <- RomFeature$new(ds,list(hydroid=fc$featureid), TRUE)
  facg <- readWKT(fac$geom)
  mp_points <- rbind(
    mp_points,
    data.frame(lat=facg$y, lon=facg$x, label=fac$name)
  )
}
map_gg <- mapgen(
  start_point = outlet_point,
  points = mp_points, 
  app_map
)

# save the map image as png
fpath = "C:/Workspace/tmp/"
fname = paste(fpath,"fig.location_map_rivanna8.png",sep="")
ggsave(
  filename = fname,
  plot = map_gg,
  width = 20,
  height = 20
)

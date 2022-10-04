# using NHDPlus to get rough estimate of floodplain geometry

#----Setup-----
library("hydrotools") #needed to pull values from VAHydro 

# Link data source (ds) to VAHydro
basepath='/var/www/R';
source("/var/www/R/config.R") #will need file in same folder/directory
ds <- RomDataSource$new(site, rest_uname = rest_uname)
ds$get_token(rest_pw)

#arguments
riverseg <- "JL2_6850_6890" #Rockfish
channel<- "0. River Channel"

#----Pulling from VAHydro
rseg<- RomFeature$new(
  ds,
  list(
    hydrocode= paste("vahydrosw_wshed",riverseg,sep = "_"), 
    ftype='vahydro',
    bundle='watershed'
  ), 
  TRUE
)

#how do I get Geometry out of rseg???


# using Geometry in nhdplustools----

#remotes::install_github("mikejohnson51/AOI")
library(AOI)
aoi <-  aoi_get("Rockfish River, VA")

#install.packages("nhdplusTools")
library(nhdplusTools)
rockfish_data <- get_nhdplus(AOI=aoi, realization="flowline")
geom <- rockfish_data$geometry
points <- sf::st_as_sf(c(geom))
rocktable<- get_elev_along_path(points, 100)

if(!is.null(rocktable)) {
  bbox <- sf::st_bbox(rocktable) + c(-0.005, -0.005, 0.005, 0.005)
  
  nhdplusTools::plot_nhdplus(bbox = bbox, cache_data = FALSE)
  
  plot(sf::st_transform(sf::st_geometry(rocktable), 3857), pch = ".", add = TRUE, col = "red")
  plot(rocktable$distance_m, rocktable$elevation_m, main = "Distance vs. Elevation", xlab = "Distance (m)", ylab = "Elevation (m)")
  
}

# get_elevation experiment----
COMID <- c(8545673,8548041,8545773,8545771,8545765,8545713,8547533,8545537,8545493,8547479,8545397,8545399,8545389,8545381,8545369,8545377,8545351,8545321,8545293,8545181,8545165,8545111,8545109,8545055,8547471,8545069,8545079,8545089,8545091,8545093,8545083,8545009,8544987,8544985,8544943,8544925,8544859,8544833,8547459,8544805,8544751,8544683,8544663,8544639,8544631)
rockfish_data3 <- get_nhdplus(comid=COMID, realization="flowline")
geom <- rockfish_data3$geometry

point1 <- sf::st_sfc(sf::st_point(x = c(-78.8301, 37.90398)), crs = 4326)
point2<-sf::st_sfc(sf::st_point(x = c(-78.82982, 37.89385)), crs = 4326)
point3<-sf::st_sfc(sf::st_point(x = c(-78.8293, 37.89284)), crs = 4326)
point4<-sf::st_sfc(sf::st_point(x = c(-78.83077, 37.88659)), crs = 4326)
point5<- sf::st_sfc(sf::st_point(x = c(-78.8263, 37.88485)), crs = 4326)
point6<- sf::st_sfc(sf::st_point(x = c(-78.82282, 37.87879)), crs = 4326)
point7<- sf::st_sfc(sf::st_point(x = c(-78.82495, 37.86774)), crs = 4326)
point8<- sf::st_sfc(sf::st_point(x = c(-78.82565, 37.86634)), crs = 4326)
point9<- sf::st_sfc(sf::st_point(x = c(-78.81781, 37.86362)), crs = 4326)
point10<- sf::st_sfc(sf::st_point(x = c(-78.81746, 37.85439)), crs = 4326)


points <- sf::st_as_sf(c(point1, point2, point3, point4, point5, point6, point7, point8))

Table1 <- get_elev_along_path(points, 100)

if(!is.null(Table1)) {
  bbox <- sf::st_bbox(Table1) + c(-0.005, -0.005, 0.005, 0.005)
  
  nhdplusTools::plot_nhdplus(bbox = bbox, cache_data = FALSE)
  
  plot(sf::st_transform(sf::st_geometry(Table1), 3857), pch = ".", add = TRUE, col = "red")
  plot(sf::st_transform(sf::st_sfc(point1, crs = 4326), 3857), add = TRUE)
  plot(sf::st_transform(sf::st_sfc(point2, crs = 4326), 3857), add = TRUE)
  plot(sf::st_transform(sf::st_sfc(point3, crs = 4326), 3857), add = TRUE)
  plot(sf::st_transform(sf::st_sfc(point4, crs = 4326), 3857), add = TRUE)
  plot(sf::st_transform(sf::st_sfc(point5, crs = 4326), 3857), add = TRUE)
  plot(sf::st_transform(sf::st_sfc(point6, crs = 4326), 3857), add = TRUE)
  plot(sf::st_transform(sf::st_sfc(point7, crs = 4326), 3857), add = TRUE)
  plot(sf::st_transform(sf::st_sfc(point8, crs = 4326), 3857), add = TRUE)
  plot(sf::st_transform(sf::st_sfc(point9, crs = 4326), 3857), add = TRUE)
  plot(sf::st_transform(sf::st_sfc(point10, crs = 4326), 3857), add = TRUE)
  
  plot(Table1$distance_m, Table1$elevation_m, main = "Distance vs. Elevation", xlab = "Distance (m)", ylab = "Elevation (m)")
  
}

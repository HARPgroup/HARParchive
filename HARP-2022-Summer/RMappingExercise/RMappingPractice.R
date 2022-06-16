#R Mapping Excercise - Practice integrating mapping tools with R. 

library(sf)
library(ggplot2)
library(dplyr)
library(ggspatial)

river <- st_read("States/cb_2018_us_region_500k.shp") # adding in the river shapefile
boundry <- st_read("Watershed/Shape/WBDHU6.shp") # adding the watershed boundary

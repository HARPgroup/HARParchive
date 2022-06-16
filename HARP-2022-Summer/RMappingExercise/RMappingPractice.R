#R Mapping Excercise - Practice integrating mapping tools with R. 

install.packages("classInt")
library(classInt) # needed this for sf package to work for me -GC
library(sf)
library(dplyr)
library(sf)
library(ggplot2)
library(ggspatial)

river <- st_read("States/cb_2018_us_region_500k.shp") # adding in the river shapefile
boundry <- st_read("Watershed/Shape/WBDHU6.shp") # adding the watershed boundary

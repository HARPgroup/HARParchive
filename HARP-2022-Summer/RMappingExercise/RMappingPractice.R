
#R Mapping Excercise - Practice integrating mapping tools with R. 

#Everything needed for a good map:
  # - land (color 1)
  # - watershed (color 2)
  # - boundaries (color 3)
  # - water (color 4 / can depend on depth or type)
  # - points of interest (color 5, shape)
        # * all colors should blend well with each other and distinguish the wanted qualities
  # - appropriate frame
  # - scale bar
  # - north arrow
  # - labels
  # - legend (if needed)
  # - elevation/hillshade (if needed)

# if you don't have all packages:
#install.packages("classInt")
#install.packages("classint")
#install.packages("ggspatial")
#install.packages("sf")

# reading in the libraries
library(classInt) # needed this for sf package to work for me -Glenn
library(sf)
library(dplyr)
library(ggplot2)
library(ggspatial) # needed for making maps w/ ggplot

# reading in the data
# samps are just extra points added to the map manually through an excel file that we don't have!
# need to set working directory to location of source file to import data 
river <- st_read("Watershed/Shape/NHDArea.shp") # adding in the river shapefile
boundry <- st_read("Watershed/Shape/WBDHU6.shp") # adding the watershed boundary
#samps <- read.csv("Site_Metadata_Cleaned.csv") # adding data points
states <- st_read("States2/cb_2018_us_state_500k.shp")
virginia <- filter(states, NAME == "Virginia" | NAME == "Maryland") # only select state data on VA and MD
fred <- data.frame(-77.4605, 38.3032, "Fredrickburg") # add in the location of Fredricksburg

# creating the map
boundry<- data.frame(boundry)
fred<- data.frame(fred)
river<- data.frame(river)
states<- data.frame(states)
virginia<- data.frame(virginia)

ggplot() +
  geom_sf(data=virginia, fill= "green", color= "black", lwd=0.75) +
  geom_sf(data=boundry, fill=alpha("yellow", 0.2), color= "yellow", lwd=1.5) +
  geom_sf(data=river, color= "darkblue", fill= "blue") +
  geom_point(data=fred, x=fred[1], y=fred[2], pch=24, color="black", fill="red3", lwd=5) +
  xlim(-78.5,-76) +
  ylim(37.5,38.75) +
  annotation_scale() +
  annotation_north_arrow(height= ,) +
  annotate(geom=text, x= -77.2, y= 38.85, label= "Fredricksburg", fontface= "bold", color= "white") +
  theme(panel.background = element_rect(fill= 'lightblue'), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  

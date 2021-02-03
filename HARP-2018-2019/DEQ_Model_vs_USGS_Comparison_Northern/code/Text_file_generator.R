#Text file generator
# Kelsey Reitz
# 4/10/19

library(lubridate)
library(stringr)
library(readr)
rm(list = ls())
setwd("C:/Users/Kelsey/Desktop/HARP/R scripts/Task5")

# read in information
readdata <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRqnbZ5yjvVsizdYJ09yml5VOPXsRD3C9EFH5JXOtIqLeqjP000CJXYv3-MQQwK4AnYNhsPaqsOT1xE/pub?output=csv", stringsAsFactors = F)
data <- readdata
i <- 1
for (i in 1:nrow(data)){
#assign variable names
river <- data$river[i]
basin <- data$basin[i]
county <-data$County[i]
latcord <- data$lat[i]
longcord <- data$long[i]
latcord<- paste0(str_sub(latcord, start=1L, end=2L),str_sub(latcord, start=4L, end=11L))
longcord<-paste0(str_sub(longcord, start=0L, end=3L),str_sub(longcord, start=5L, end=12L))
dist <- data$Distance.to.nearest.City[i]
city <- data$Nearest.City[i]
da <- data$newda[i]
firstyear <- data$start[i]
lastyear <- data$end[i]
alteration <- data$impoundmens[i]

# set up the year statement
if (lastyear == 2019){
finalyear <- paste0("and is still taking data.")
}else finalyear <- paste0("but was decommissioned in ", lastyear, ".")

timeper <- paste0("This gage started taking data in ", firstyear, " ", finalyear)


text <- paste0("This river segment follows part of the flow of the ", 
               river, ", a tributary of the ", basin, ". The gage is located in ",
               county, ' County (Lat. ', latcord, '", Long. ', longcord, '"), approximately ', 
               dist, " of ", city,", VA. Drainage area is ", da, " sq. miles. ", 
               timeper," ", alteration)
             

write_file(text, path = paste0("0", data$gage[i], '.txt'), append = FALSE)
}

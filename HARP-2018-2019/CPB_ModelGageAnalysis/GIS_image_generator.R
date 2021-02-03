# 3 April 2019
# GIS image generator for CPB segments
# Kelsey Reitz

## load libraries
library(lubridate)
library(dplyr)
library(RCurl)
library(dataRetrieval)
library(stringr)
library(rapportools)

library(tidyr)
library(questionr)
library(rgdal)
library(raster)

# SETTING UP FOR LOOP TO ASSOCIATE RIVER SEGMENTS ----------
rm(list = ls())

# Pull gages from GIS file (updated GIS_seg from v3_dA_compare) ------------------------------
setwd('C:/Users/Kelsey/Desktop/GitHub/hydro-tools/HARP-2018/CPB_ModelGageAnalysis')
folder_location = 'C:/Users/Kelsey/Desktop/GitHub/hydro-tools/HARP-2018/CPB_ModelGageAnalysis'
file_path <-"C:/Users/Kelsey/Desktop/GitHub/hydro-tools/HARP-2018/CPB_ModelGageAnalysis\\GIS_Seg2"


#add lone wolves to groupings once it gets loaded
Lone_segs<- read.csv(paste0(file_path, "\\Lone_Segs.csv"), stringsAsFactors = F)
Lone_segs <- data.frame(Lone_segs$rivseg)
#remove all segments that end in _0000
Lone_segs[,1]<-as.character(Lone_segs[,1])
Lone_segs$outlet<- str_sub(Lone_segs[,1], start=9L, end=13L)
Lone_segs <- data.frame(Lone_segs[which(Lone_segs$outlet!="_0000"),1])


# Pull segment groupings from GIS file (updated GIS_seg from v3_dA_compare) 
Groupings<- read.csv(paste0(file_path,"\\All_Segs.csv"), stringsAsFactors = F)#Pull the (location of desired data table)
Groupings<- Groupings[,-1] #remove column 1 of row numbers
Groupings <- Groupings[,-123] #remove groupings column
addrow <- data.frame(matrix(nrow=ncol(Groupings), ncol=1))
addrow[1:nrow(Lone_segs),1]<-as.character(Lone_segs[,1])
Groupings[25,] <- t(addrow)


#continue with code: create upstream csv for use later 
modelgage <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRqnbZ5yjvVsizdYJ09yml5VOPXsRD3C9EFH5JXOtIqLeqjP000CJXYv3-MQQwK4AnYNhsPaqsOT1xE/pub?output=csv", stringsAsFactors = F)

i <- 1
for (i in 1:nrow(modelgage)){
  segofi <- modelgage$segment[i]
  logic_seglist <- (Groupings==segofi)
  upstream_row <- which(apply(logic_seglist,1,any))
  if (is.empty(upstream_row)==TRUE){
    modelgage$upsegs[i] <- segofi
  }else if (is.empty(upstream_row)==FALSE){
    upstreams <- Groupings[upstream_row,]
    upstreams <- upstreams[1,1:which(upstreams==segofi)]
    modelgage$upsegs[i] <- paste(upstreams, collapse = '+')
  }
  i <- i + 1
}
#modelgage_upstream segs is a csv with + version of all_segs
write.csv(modelgage, paste0(file_path,"\\ModelGage_UpstreamSegs.csv"))


LogicGrouping<-data.frame(is.na(Groupings[,2:(ncol(Groupings)-1)]))
LogicGrouping$AllTrue<-(rep(TRUE, nrow(LogicGrouping)))
h<-1
Coordinate<- data.frame(matrix(nrow=1, ncol=1))
for (h in 1:nrow(LogicGrouping)){
  z<-1
  while (LogicGrouping[h,z]!=TRUE){
    down<- z
    z<-z+1
  }
  Coordinate[h,1]<-h
  Coordinate[h,2]<-z
  h<-h+1
}
i<-1
p<-1
for (i in 1:nrow(Coordinate)){
  Groupings$LastSeg[i]<-as.character(Groupings[Coordinate[p,1],Coordinate[p,2]])
  i<-i+1
  p<-p+1
}

#create major and minor river seg group number 
i<-1
x<-1
y<-ncol(Groupings)-1
RivSegGroups<-data.frame(matrix(nrow=1,ncol=1))
for (i in 1:nrow(Groupings)){
  store<-data.frame(matrix(nrow=1,ncol=2))
  j<-1
  for (j in 1:ncol(Groupings)-1){
    store[j,1]<- as.character(Groupings[i,j])
    store[j,2]<-i
    j<-j+1
  }
  RivSegGroups[x:y,1]<-store[,1]
  RivSegGroups[x:y,2]<-store[,2]
  x<-x+nrow(store)
  y<-y+nrow(store)
  i<- i+1
}

RivSegGroups<-na.omit(RivSegGroups)
colnames(RivSegGroups)<- c('RiverSeg', 'Group')

RivSegGroups$MajBas <- str_sub(RivSegGroups$RiverSeg, start=1L, end=1L)
RivSegGroups$MinBas <- str_sub(RivSegGroups$RiverSeg, start=1L, end=2L)
RivSegGroups$MajBasNum <- as.numeric(as.factor(RivSegGroups$MajBas))
RivSegGroups$MinBasNum <- as.numeric(as.factor(RivSegGroups$MinBas))


# Start GIS Stuff ---------------

# Map projection 
Projection<- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs' 

# Importing desired focus area
RiverSeg<-readOGR ('C:/Users/Kelsey/Desktop/HARP/GIS/ApprovedSouthernRivers',"CPB_Rsegs")
RiverSeg<-spTransform(RiverSeg, CRS=Projection)   #Put shapefile in correct projection/coordinated system

States <- 'C:/Users/Kelsey/Desktop/GitHub/hydro-tools/HARP-2018/CPB_ModelGageAnalysis'
States<-readOGR(States, "cb_2017_us_state_20m")
States<- spTransform(States, CRS=Projection)  

RiverSeg@data$RiverSeg<-as.character(RiverSeg@data$RiverSeg)
#remove susquehanna
RiverSeg<- RiverSeg[as.character(RiverSeg@data$Watershed)!="Susquehanna River Basin",]



# LOOP TO DETERIME WHICH RIVER SEGMENTS HAVE DATA ----------
#The loop will run and add the desired metrics column to any segment that has a matching river segment ID with that metric
i <-1
RiverSeg@data$Group<-as.character(RiverSeg@data$Group)
for (i in 1:length(RiverSeg@data$RiverSeg)){
  if (RiverSeg@data$RiverSeg[i]%in%RivSegGroups$RiverSeg){ #if the river segment ID is in the metrics file make it true, if not make it false
    RiverSeg@data$Maj<- str_sub(RiverSeg@data$MajMin, start=1L, end=1L)
    RiverSeg@data$Min<- str_sub(RiverSeg@data$MajMin, start=1L, end=2L)
    RiverSeg@data$Maj <- as.numeric(as.factor(RiverSeg@data$Maj))
    RiverSeg@data$Min <- as.numeric(as.factor(RiverSeg@data$Min))
    RiverSeg@data$Group[i]<- RivSegGroups$Group[RivSegGroups$RiverSeg==RiverSeg@data$RiverSeg[i]]
    
  }
}


# SETTING UP FOR LEGEND ----------
Groupingsnum<-nrow(Groupings)-1
GroupingsnumFirst<-round(Groupingsnum/2, digits=0)
GroupingsnumSecond<-GroupingsnumFirst+1

# GRAPHING 
title<-('CPB Major-Minor Watershed Basins')
dir.create(paste0(file_path), showWarnings = FALSE);
png(filename=paste0(file_path, "\\All_Segs_MajMinRiverBasins.png"), 
    width=1400, height=950, units="px")
RiverSeg@data$color<- cut(RiverSeg@data$Min,c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5, 13.5, 14.5, 15.5, 16.5, 17.5, 18.5, 19.5, 20.5, 21.5), 
                          labels=c('blue', 'blueviolet', 'brown', 'brown1', 'burlywood1', 
                                   'cadetblue1', 'chartreuse', 'chocolate', 'maroon', 'orange', 
                                   'forestgreen', 'yellow', 'gold', 'darkorchid', 'darksalmon', 
                                   'deeppink2', 'lawngreen', 'navy', 'aquamarine', 'lightseagreen'
                                   ,'mediumspringgreen'))#, 'magenta'))
SouthernRivers<- RiverSeg[!is.na (RiverSeg@data$color),]
plot(SouthernRivers, col=paste0(SouthernRivers@data$color))
plot(States, add=TRUE, col='gray')
lines(States, col='white')
plot(SouthernRivers, col=paste0(SouthernRivers@data$color), add=T)
legend("bottomleft", legend=c(paste0(unique(gsub(",.*$", "", RiverSeg@data$MinBas)))), col=c('blue', 'blueviolet', 'brown', 'brown1', 'burlywood1', 'cadetblue1', 'chartreuse', 'chocolate', 'maroon', 'orange', 'forestgreen', 'yellow', 'gold', 'darkorchid', 'darksalmon', 'deeppink2', 'lawngreen', 'navy', 'aquamarine', 'lightseagreen', 'mediumseagreen'), lty=0, pch=15, pt.cex=2, bty='n', y.intersp=0.8, x.intersp=0.3, cex=2, lwd=2, ncol=1)
legend("topleft", legend=c(title), lty=0, pt.cex=3, bty='n', y.intersp=0.75, x.intersp=0.3, cex=5, lwd=2)
dev.off()


# for major basins--------------------------------------------------------------
# SETTING UP FOR LEGEND 
Groupingsnum<-nrow(Groupings)-1
GroupingsnumFirst<-round(Groupingsnum/2, digits=0)
GroupingsnumSecond<-GroupingsnumFirst+1

# GRAPHING ----------
title<-(' CPB Major Watershed Basins')
dir.create(paste0(file_path), showWarnings = FALSE);
png(filename=paste0(file_path, "\\All_Segs_MajRiverBasins.png"), 
    width=1400, height=950, units="px")
RiverSeg@data$color<- cut(RiverSeg@data$Maj,c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5), 
                          labels=c('blue', 'turquoise4', 'brown', 'brown1', 'burlywood1', 
                                   'cadetblue1', 'chartreuse'))
SouthernRivers<- RiverSeg[!is.na (RiverSeg@data$color),]
plot(SouthernRivers, col=paste0(SouthernRivers@data$color))
plot(States, add=TRUE, col='gray')
lines(States, col='white')
plot(SouthernRivers, col=paste0(SouthernRivers@data$color), add=T)
legend("bottomleft", legend=c(paste0(unique(gsub(",.*$", "", RiverSeg@data$MajBas)))), col=c('blue', 'turquoise4', 'brown', 'brown1', 'burlywood1', 'cadetblue1', 'chartreuse'), lty=0, pch=15, pt.cex=2, bty='n', y.intersp=0.8, x.intersp=0.3, cex=2, lwd=2, ncol=1)
legend("topleft", legend=c(title), lty=0, pt.cex=3, bty='n', y.intersp=0.75, x.intersp=0.3, cex=5, lwd=2)
dev.off()



#greyed out coastal segments  -----------------------------------------

# for minor basins
# SETTING UP FOR LEGEND ----------
Groupingsnum<-nrow(Groupings)-1
GroupingsnumFirst<-round(Groupingsnum/2, digits=0)
GroupingsnumSecond<-GroupingsnumFirst+1

# GRAPHING -----------------------------------------------------------------
title<-('Greyed Coast Maj-Min Watershed Basins')
dir.create(paste0(file_path), showWarnings = FALSE);
png(filename=paste0(file_path, "\\Greyed_Coast_MajMinRiverBasins.png"), 
    width=1400, height=950, units="px")
RiverSeg@data$color<- cut(RiverSeg@data$Min,c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5, 13.5, 14.5, 15.5, 16.5, 17.5, 18.5, 19.5, 20.5, 21.5), 
                          labels=c('blue', 'blueviolet', 'brown', 'brown1', 'burlywood1', 
                                   'cadetblue1', 'chartreuse', 'chocolate', 'maroon', 'orange', 
                                   'forestgreen', 'yellow', 'gold', 'darkorchid', 'darksalmon', 
                                   'deeppink2', 'lawngreen', 'navy', 'aquamarine', 'lightseagreen'
                                   ,'mediumspringgreen'))#, 'magenta'))
#grey out the coastal segments
i <- 1
RiverSeg@data$color <- as.character(RiverSeg@data$color)
for (i in 1:nrow(RiverSeg@data)){
  riversim <- as.character(RiverSeg@data$RiverSimu[i])
  if (riversim =="Direct Drainage, CB"){
    RiverSeg@data$color[i] <- "grey40"
  }else if (riversim !="Direct Drainage, CB"){
    RiverSeg@data$color[i] <- RiverSeg@data$color[i]
  }
}

SouthernRivers<- RiverSeg[!is.na (RiverSeg@data$color),]
plot(SouthernRivers, col=paste0(SouthernRivers@data$color))
plot(States, add=TRUE, col='gray')
lines(States, col='white')
plot(SouthernRivers, col=paste0(SouthernRivers@data$color), add=T)
legend("bottomleft", legend=c(paste0(unique(gsub(",.*$", "", RiverSeg@data$MinBas)))), col=c('blue', 'blueviolet', 'brown', 'brown1', 'burlywood1', 'cadetblue1', 'chartreuse', 'chocolate', 'maroon', 'orange', 'forestgreen', 'yellow', 'gold', 'darkorchid', 'darksalmon', 'deeppink2', 'lawngreen', 'navy', 'aquamarine', 'lightseagreen', 'mediumseagreen'), lty=0, pch=15, pt.cex=2, bty='n', y.intersp=0.8, x.intersp=0.3, cex=2, lwd=2, ncol=1)
legend("topleft", legend=c(title), lty=0, pt.cex=3, bty='n', y.intersp=0.75, x.intersp=0.3, cex=5, lwd=2)
dev.off()


#Greyed out major basins / coastals ---------------
title<-('Greyed Coast Major Watershed Basins')
dir.create(paste0(file_path), showWarnings = FALSE);
png(filename=paste0(file_path, "\\Greyed_Coast_MajRiverBasins.png"), 
    width=1400, height=950, units="px")
RiverSeg@data$color<- cut(RiverSeg@data$Maj,c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5), 
                          labels=c('blue', 'turquoise4', 'brown', 'brown1', 'burlywood1', 
                                   'cadetblue1', 'chartreuse'))
i <- 1
RiverSeg@data$color <- as.character(RiverSeg@data$color)
for (i in 1:nrow(RiverSeg@data)){
  riversim <- as.character(RiverSeg@data$RiverSimu[i])
  if (riversim =="Direct Drainage, CB"){
    RiverSeg@data$color[i] <- "grey40"
  }else if (riversim !="Direct Drainage, CB"){
    RiverSeg@data$color[i] <- RiverSeg@data$color[i]
  }
}

SouthernRivers<- RiverSeg[!is.na (RiverSeg@data$color),]
plot(SouthernRivers, col=paste0(SouthernRivers@data$color))
plot(States, add=TRUE, col='gray')
lines(States, col='white')
plot(SouthernRivers, col=paste0(SouthernRivers@data$color), add=T)
legend("bottomleft", legend=c(paste0(unique(gsub(",.*$", "", RiverSeg@data$MajBas)))), col=c('blue', 'turquoise4', 'brown', 'brown1', 'burlywood1', 'cadetblue1', 'chartreuse'), lty=0, pch=15, pt.cex=2, bty='n', y.intersp=0.8, x.intersp=0.3, cex=2, lwd=2, ncol=1)
legend("topleft", legend=c(title), lty=0, pt.cex=3, bty='n', y.intersp=0.75, x.intersp=0.3, cex=5, lwd=2)

dev.off()





#create a different outline for the lone wolf segments? or just plot by number: ----

#Gage Segment linked gages 
GageSegs <- RiverSeg

# get the segments pulled out of gagesegs and match for a plot
which(GageSegs@data$RiverSeg==modelgage$segment)


title<-('Gage-Segment Links')
dir.create(paste0(file_path), showWarnings = FALSE);
png(filename=paste0(file_path, "\\GageLinkedBasins.png"), 
    width=1400, height=950, units="px")

GageSegs@data$color<- cut(RiverSeg@data$Maj,c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5), 
                          labels=c('blue', 'turquoise4', 'brown', 'brown1', 'burlywood1', 
                                   'cadetblue1', 'chartreuse'))
i <- 1
RiverSeg@data$color <- as.character(RiverSeg@data$color)
for (i in 1:nrow(RiverSeg@data)){
  riversim <- as.character(RiverSeg@data$RiverSimu[i])
  if (riversim =="Direct Drainage, CB"){
    RiverSeg@data$color[i] <- "grey40"
  }else if (riversim !="Direct Drainage, CB"){
    RiverSeg@data$color[i] <- RiverSeg@data$color[i]
  }
}

SouthernRivers<- RiverSeg[!is.na (RiverSeg@data$color),]
plot(SouthernRivers, col=paste0(SouthernRivers@data$color))
plot(States, add=TRUE, col='gray')
lines(States, col='white')
plot(SouthernRivers, col=paste0(SouthernRivers@data$color), add=T)
legend("bottomleft", legend=c(paste0(unique(gsub(",.*$", "", RiverSeg@data$MajBas)))), col=c('blue', 'turquoise4', 'brown', 'brown1', 'burlywood1', 'cadetblue1', 'chartreuse'), lty=0, pch=15, pt.cex=2, bty='n', y.intersp=0.8, x.intersp=0.3, cex=2, lwd=2, ncol=1)
legend("topleft", legend=c(title), lty=0, pt.cex=3, bty='n', y.intersp=0.75, x.intersp=0.3, cex=5, lwd=2)

dev.off()


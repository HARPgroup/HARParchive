# Analysis of gages and river segments for Cheasapeake Bay Watershed
# Kelsey Reitz
# 2/15/2019

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

setwd("C:/Users/Kelsey/Desktop/GitHub/hydro-tools/HARP-2018/CPB_ModelGageAnalysis")

#Pull Hailey's model data from the drive
wshed <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQCrbbMS-XgzxOG0btMencBB_6F7kTqTodlb9FQU67nlGCSKKmXsXZNC-G-sUghdI4ya8heUIugBMjn/pub?output=csv", stringsAsFactors = FALSE)
wshed <- data.frame(wshed$Flow, wshed$RiverSeg, wshed$MajBas, wshed$RiverName, wshed$AreaSqMi)
wshed$end <- str_sub(wshed$wshed.RiverSeg, start=1L, end=2L)
colnames(wshed) <- c("flow", "rivseg", "basin", "river", "DAsqmi", "bascd")
wshed$rivseg <- as.character(wshed$rivseg)
wshed$basin <- as.character(wshed$basin)
wshed$river <- as.character(wshed$river)

wshed <- subset(wshed, wshed$basin!= "Susquehanna River Basin" & wshed$river!= "Drains to Tidal Water - No River Simulation") #get rid of susquehanna and not modeled stuff
rownames(wshed) <- 1:nrow(wshed)

# get rid of duplicate watersheds that are at the mouths of the rivers
dup <- wshed[which(duplicated(wshed$rivseg)==TRUE),]
nodups <- wshed[-c(which(duplicated(wshed$rivseg)==TRUE)),]

basins <- data.frame(unique(nodups$basin))
basins$rownum <- 1:nrow(basins)

eshore <- nodups[which(nodups$basin==basins[1,1]),]
potriv <- nodups[which(nodups$basin==basins[2,1]),]
jamriv <- nodups[which(nodups$basin==basins[3,1]),]
wshore <- nodups[which(nodups$basin==basins[4,1]),]
rapriv <- nodups[which(nodups$basin==basins[5,1]),]
paxriv <- nodups[which(nodups$basin==basins[6,1]),]
yokriv <- nodups[which(nodups$basin==basins[7,1]),]


# check to make sure all segs have been kept: 
nrow(nodups) == (nrow(eshore) + nrow(potriv) + nrow(jamriv) + nrow(wshore) + nrow(rapriv) + nrow(paxriv) + nrow(yokriv))
write.csv(nodups, "ModelSegs.csv")


# Pull gages from GIS file (p53 calibstats shapefile) ------------------------------
gages <- read.csv("Gage_stations.csv", stringsAsFactors = FALSE)
gages <- data.frame(as.numeric(as.character(gages$STAID)), gages$NAME, gages$CATCODE2)
gages$gages.NAME <- as.character(gages$gages.NAME)
gages$gages.CATCODE2 <- as.character(gages$gages.CATCODE2)
gages <- gages[(complete.cases(gages)),] #remove rows with na values
colnames(gages) <- c("staid", "name", "seg")

k <- 1
for (k in 1:nrow(gages)){
siteNo <- (str_pad(gages$staid[k], 8, pad = 0))
gagedata <- readNWISsite(siteNo)
gages$DAsqmi[k] <- gagedata$drain_area_va
}


common<- merge(nodups, gages, by.x="rivseg", by.y="seg")
colnames(common) <- c("segment", "flow", "modelbas", "river", "modelDA", "end_seg", "gageID", "gagename", "gageDA")

dacomp <- data.frame(common$segment, common$modelDA, common$gageID, common$gageDA)
colnames(dacomp) <- c("modelseg", "modelda", "gageID", "gageDA")


# Upstream/Downstream Analysis -----------------------------------
#pull upstream segments for segments
i <- 1
for (i in 1:nrow(nodups)){
  nodups[i,7]<- str_sub(nodups[i,2], start=5L, end=8L)
  nodups[i,8]<- str_sub(nodups[i,2], start=10L, end=-1L)
  i <- i + 1
}
colnames(nodups) <- c("flow", "rivseg", "basin", "river", "DAsqmi", "bascd", "Upstream", "Downstream")
# Determine Downstream Segment ----------
j <- 1
for (j in 1:nrow(nodups)){
  Downstream <- which(nodups$Upstream==nodups$Downstream[j])
  if (length(Downstream)==0){
    nodups[j,9]  <- 'NA'
  }else if (length(Downstream)!=0){
    nodups[j,9] <- as.character(nodups[Downstream,2])
  }
  j<-j+1
}
# Determine Upstream Segment ------
m<-1
for (m in 1:nrow(nodups)){
  Upstream <- which(nodups$V9==nodups$rivseg[m])
  NumUp <- nodups$rivseg[Upstream]
  nodups[m,10]<- paste(NumUp, collapse = '+')
  
  if (is.empty(nodups[m,10])==TRUE){
    nodups[m,10]<- 'NA'
  } 
  m<-m+1
}
colnames(nodups) <- c("flow", "rivseg", "basin", "river", "DAsqmi", "bascd", "Mid", "Last", "Downstream", "Upstream")



# Begin Exerpt of other code -------------------

folder_location = 'C:/Users/Kelsey/Desktop/GitHub/hydro-tools/HARP-2018/CPB_ModelGageAnalysis'
# Location of output destination
dir.create(paste0(folder_location, "\\GIS_Seg"), showWarnings = FALSE);
file_path <-"C:/Users/Kelsey/Desktop/GitHub/hydro-tools/HARP-2018/CPB_ModelGageAnalysis\\GIS_Seg"


#Find the highest upstream segments (where water starts) ----------
#Lone wolf segments are segments with no head or tail
UpStart <- nodups[which(nodups$Upstream=="NA"),]
rownames(UpStart)<- 1:nrow((UpStart))
LoneWolves <- UpStart[which(UpStart$Downstream=='NA'),]
UpStart <- UpStart[-c(which(UpStart$Downstream=='NA')),]

#write csv for lone wolves
write.csv(LoneWolves, paste0(file_path,"\\Lone_Segs.csv"))
#write csv for the upstream-most segments
write.csv(UpStart, paste0(file_path, "\\Headwater_Segs.csv"))






# Initalize variables for for loop that finds the next downstream segment ----------
i <- 1
alldata<- data.frame(matrix(ncol=2, nrow=nrow(UpStart)))


for (i in 1:nrow(UpStart)){
  
  #initialize variables for while loop  
  FirstSeg <- UpStart$rivseg[i]
  FirstSegRow<- which(nodups$rivseg==FirstSeg)
  NextSeg <- nodups$Downstream[FirstSegRow]
  x <- 2 #using x of 2 so that head segment is stored first, before entering while
  VarNextSeg <- data.frame(matrix(nrow=1, ncol=1))
  colnames(VarNextSeg) <- 'DownstreamSeg'
  VarNextSeg[1,1] <- FirstSeg
  
  #while loop adds the next downstream segment to the same row
  while (NextSeg != "NA"){
    NextSeg <- nodups$Downstream[FirstSegRow]
    VarNextSeg[x,1] <- NextSeg
    FirstSeg <- NextSeg
    FirstSegRow<- which(nodups$rivseg==FirstSeg)
    x <- x+1
  }
  VarNextSeg <-as.list(VarNextSeg[which(VarNextSeg!="NA"),]) #remove the last value from VarNextSeg
  #store VarNextSeg in alldata: has head and list of tails
  alldata[i,2] <- paste(VarNextSeg, collapse = '+')
  alldata[i,1] <- UpStart$rivseg[i] 
  i <- i + 1
}

# Initalize variables, then find the max number of river segments downstream of one head ----------
i <- 1
max<-1

for (i in 1:nrow(alldata)){
  DownSegs <- strsplit(alldata[i,2], "\\+")
  DownSegs <- t(as.data.frame(DownSegs[[1]]))
  maxnew <- length(DownSegs)
  if (maxnew>max){
    max<- maxnew
  }else 
    max <- max
  i <- i+1 
}

# Initialize variables for creating one large matrix of downstream segments -----------
j <- 1
newNA <- data.frame(matrix(nrow=1, ncol=1))
sepsegs <- data.frame(matrix(nrow=nrow(alldata), ncol=(max+2)))
colnames(sepsegs)<- 1:ncol(sepsegs)

for (j in 1:nrow(alldata)){
  DownSegs <- strsplit(alldata[j,2], "\\+")
  DownSegs <- data.frame(t(as.data.frame(DownSegs[[1]])))
  DownSegs[] <- lapply(DownSegs, as.character)
  
  numNA <- length(DownSegs)      #determine number of segments
  addNA <- as.numeric(max-numNA) #based on number of segments, may need to add NA values
  
  if (addNA !=0){                # add the required # of NAs and bind them to DownSegs
    newNA<- data.frame(t(rep(NA, addNA)))
    DownSegs<- cbind(DownSegs, newNA)
  }
  colnames(DownSegs)<- 1:ncol(DownSegs)
  
  #pull row j of all data, cbind it to DownSegs to make a complete row (stored in practice2)
  practice<- alldata[j,]
  practice2<- data.frame(cbind(practice, DownSegs)); 
  colnames(practice2)<- 1:ncol(practice2) 
  sepsegs[j,]<-practice2  #store results of practice2 in sepsegs for each j
  j <- j + 1
}


# Find furthest downstream segments, remove the lone wolves ----------
NoDownStream <- nodups[which(nodups$Downstream=='NA'),]
rownames(NoDownStream) <- 1:nrow(NoDownStream)
NoDownStream<- NoDownStream[-c((which(NoDownStream$Upstream=="NA"))),]


#pull only columns required for analysis (row 1=row 3, and row 2 is too condensed)
SegmentList <- sepsegs[,3:34]
colnames(SegmentList)<- 1:ncol(SegmentList)



# Create a csv for every tail with no outlet (that segment is the final outlet)----------
k <- 1
maxrow <- 1
for (k in 1:nrow(NoDownStream)){
  RivSeg <- NoDownStream$rivseg[k]
  logic_SegList <- (SegmentList==RivSeg)
  logic_Rows<- which(apply(logic_SegList, 1, any))
  All_Segs <- SegmentList[logic_Rows,]
  All_Segs_Unique <- data.frame(table(unlist(All_Segs)))
  ASU <- data.frame(All_Segs_Unique)
  colnames(ASU)<- "RIVSEG"
  newmaxrow <- nrow(ASU)
  if (newmaxrow > maxrow){
    maxrow <- newmaxrow
  }
  write.csv(ASU, paste0(file_path,"\\", RivSeg, "_Segs.csv"))
  k <- k + 1
  }

# create one master csv with all segments ---------------------------------

l <- 1
StoreASUS <- data.frame(matrix(nrow=1, ncol=maxrow))
for (l in 1:nrow(NoDownStream)){
  RivSeg <- NoDownStream$rivseg[l]
  logic_SegList <- (SegmentList==RivSeg)
  logic_Rows<- which(apply(logic_SegList, 1, any))
  All_Segs <- SegmentList[logic_Rows,]
  All_Segs_Unique <- data.frame(table(unlist(All_Segs)))
  ASUtrans <- data.frame(t(data.frame(All_Segs_Unique$Var1)))
  rownames(ASUtrans)<- "RIVSEG"
  ASUtrans[] <- lapply(ASUtrans, as.character)
  
  seg_length <- length(ASUtrans) #determine number of segments
  addNAmax <- as.numeric(maxrow-seg_length)
  
  
  if (addNAmax !=0){                # add the required # of NAs and bind them to DownSegs
    newNAmax<- data.frame(t(rep(NA, addNAmax)))
    ASUtrans<- cbind(ASUtrans, newNAmax)
  }
  colnames(ASUtrans)<- 1:ncol(ASUtrans)
  
  StoreASUS[l,]<- ASUtrans
  
  l <- l + 1
}

#add lone wolf data to StoreASUS

LoneSeg<- data.frame(LoneWolves$rivseg)
colnames(LoneSeg)<-'RIVSEG'
LoneSeg<- data.frame(t(LoneSeg))
LoneSeg[] <- lapply(LoneSeg, as.character)
lone_seg_length <- length(LoneSeg) #determine number of segments
addNAmax <- as.numeric(maxrow-lone_seg_length)

if (addNAmax !=0){                # add the required # of NAs and bind them to DownSegs
  newNAmax<- data.frame(t(rep(NA, addNAmax)))
  LoneSeg<- cbind(LoneSeg, newNAmax)
}
StoreASUS[l,]<- LoneSeg


row.names(StoreASUS)<-1:nrow(StoreASUS)
colnames(StoreASUS)<- 1:ncol(StoreASUS)

StoreASUS$GROUP <- 1:nrow(StoreASUS)

#StoreASUS<- t(StoreASUS)
write.csv(StoreASUS, paste0(file_path,"\\All_Segs.csv"))

# SETTING UP FOR LOOP TO ASSOCIATE RIVER SEGMENTS ----------
Groupings<- read.csv(paste0(file_path,"\\All_Segs.csv"))#Pull the (location of desired data table)
i<-1
x<-1
y<-ncol(Groupings)
RivSegGroups<-data.frame(matrix(nrow=1,ncol=1))
for (i in 1:nrow(Groupings)){
  store<-data.frame(matrix(nrow=1,ncol=2))
  j<-1
  for (j in 1:ncol(Groupings)){
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

# Start GIS Stuff ---------------

# Map projection 
Projection<- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs' 

# Importing desired focus area
RiverSeg<-readOGR ('C:/Users/Kelsey/Desktop/HARP/GIS/ApprovedSouthernRivers',"Study_CPB_Rsegs")
RiverSeg<-spTransform(RiverSeg, CRS=Projection)   #Put shapefile in correct projection/coordinated system

States<-readOGR('C:/Users/Kelsey/Downloads/cb_2017_us_state_20m', "cb_2017_us_state_20m")
States<- spTransform(States, CRS=Projection)  

RiverSeg@data$RiverSeg<-as.character(RiverSeg@data$RiverSeg)
RiverSeg@data$Group<-NA


# LOOP TO DETERIME WHICH RIVER SEGMENTS HAVE DATA ----------
#The loop will run and add the desired metrics column to any segment that has a matching river segment ID with that metric
for (i in 1:length(RiverSeg@data$RiverSeg)){
  if (RiverSeg@data$RiverSeg[i]%in%RivSegGroups$RiverSeg){ #if the river segment ID is in the metrics file make it true, if not make it false
    RiverSeg@data$Group[i]<- RivSegGroups$Group[RivSegGroups$RiverSeg==RiverSeg@data$RiverSeg[i]]
  }
}

# ADDING GROUPINGS COLUMN TO SHAPEFILE ----------
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

# SETTING UP FOR LEGEND ----------
Groupingsnum<-nrow(Groupings)-1
GroupingsnumFirst<-round(Groupingsnum/2, digits=0)
GroupingsnumSecond<-GroupingsnumFirst+1

# GRAPHING ----------
title<-(' CPB Watershed Basins')

dir.create(paste0(file_path), showWarnings = FALSE);
png(filename=paste0(file_path, "\\RiverBasins.png"), 
    width=1400, height=950, units="px")


RiverSeg@data$color<- cut(RiverSeg@data$Group,c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5, 13.5, 14.5, 15.5, 16.5, 17.5, 18.5, 19.5, 20.5, 21.5, 22.5, 23.5, 24.5, 25.5, 26.5), labels=c('blue', 'blueviolet', 'brown', 'brown1', 'burlywood1', 'cadetblue1', 'chartreuse', 'chocolate', 'maroon', 'orange', 'forestgreen', 'yellow', 'gold', 'darkorchid', 'darksalmon', 'deeppink2', 'lawngreen', 'navy', 'aquamarine', 'lightseagreen', 'mediumspringgreen', 'magenta', 'turquoise4', 'chocolate4', 'gray57', 'black'))
SouthernRivers<- RiverSeg[!is.na (RiverSeg@data$color),]
plot(SouthernRivers, col=paste0(SouthernRivers@data$color))
plot(States, add=TRUE, col='gray')
lines(States, col='white')
plot(SouthernRivers, col=paste0(SouthernRivers@data$color), add=T)
legend("bottomleft", legend=c('River Outlet', paste0(Groupings$LastSeg[1:25]), 'Incomplete Data'), col=c('blue', 'blueviolet', 'brown', 'brown1', 'burlywood1', 'cadetblue1', 'chartreuse', 'chocolate', 'maroon', 'orange', 'forestgreen', 'yellow', 'gold', 'darkorchid', 'darksalmon', 'deeppink2', 'lawngreen', 'navy', 'aquamarine', 'lightseagreen', 'mediumspringgreen', 'magenta', 'turquoise4', 'chocolate4', 'gray57', 'black'), lty=0, pch=15, pt.cex=2, bty='n', y.intersp=0.8, x.intersp=0.3, cex=2, lwd=2, ncol=1)
legend("topleft", legend=c(title), lty=0, pt.cex=3, bty='n', y.intersp=0.75, x.intersp=0.3, cex=5, lwd=2)

dev.off()

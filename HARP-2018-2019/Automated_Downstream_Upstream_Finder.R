# 7/13/18 
# Script will be transformed into a function that calculates the next
# upstream or downstream segment and maps them. 

# LOAD LIBRARIES ----------
library(stringr)
library(rapportools)
library(tidyr)
library(questionr)
library(rgdal)
library(raster)
library(randomcoloR)

# Clear workspace
rm(list = ls())

# USER INPUTS ----------
# Location of "Linking Segments" csv:
folder_location = "C:\\Users\\HaileyMae\\Documents\\GitHub\\hydro-tools\\HARP-2018";

# Location of output destination
dir.create(paste0(folder_location, "\\GIS_Seg"), showWarnings = FALSE);
file_path <-"C:\\Users\\HaileyMae\\Documents\\GitHub\\hydro-tools\\HARP-2018\\GIS_Seg" 

# Import Data of interest 
ImportTab <- read.csv(paste0(folder_location,"\\AllRiverSegments.csv"), 
                      header=TRUE, sep=',', stringsAsFactors=FALSE);
ImportTab <- data.frame(ImportTab)

# Map projection 
Projection<- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs' 

# Importing base map
States<- readOGR('E:\\SouthernRivers\\cb_2017_us_state_5m','cb_2017_us_state_5m' )  #Pull the (location of GIS shapefile, desired shapefile)
States<- spTransform(States, CRS=Projection)                                        #Put shapefile in correct projection/coordinated system

# Importing desired focus area
RiverSeg<-readOGR ('E:\\SouthernRivers\\BaseMap', 'AlteredRiverSegs')             #Pull the (location of GIS shapefile, desired shapefile)
RiverSeg<-spTransform(RiverSeg, CRS=Projection)                                     #Put shapefile in correct projection/coordinated system

# Importing HUC 6 data
HUC6<- readOGR('E:\\SouthernRivers\\BaseMap', 'HUC6')
HUC6<-spTransform(HUC6, CRS=Projection) 

# PREP DATA FOR CODE ----------
# Pull only Southern VA
ImportTab <- ImportTab[which(ImportTab$Watershed=="Southern  Virgina Watershed"),]
rownames(ImportTab)<- 1:nrow(ImportTab)

# Create dataframe for upstream and downstream segments based on code in string
ModelSegments <- data.frame(matrix(nrow = nrow(ImportTab), ncol = 5))
colnames(ModelSegments)<- c('RiverSeg', 'Middle', 'Last', 'Downstream', 'Upstream')
ModelSegments$RiverSeg <- ImportTab$RiverSeg

# Pull out 4 digit codes in middle and end for upstream/downstream segments
i <- 1
for (i in 1:nrow(ModelSegments)){
  
  ModelSegments[i,2]<- str_sub(ModelSegments[i,1], start=5L, end=8L)
  ModelSegments[i,3]<- str_sub(ModelSegments[i,1], start=10L, end=-1L)
  i <- i + 1
}

# Determine Upstream Segment ----------
j <- 1
for (j in 1:nrow(ModelSegments)){
  Upstream <- which(ModelSegments$Middle==ModelSegments$Last[j])
  
  if (length(Upstream)==0){
    ModelSegments[j,4]  <- 'NA'
    
  }else if (length(Upstream)!=0){
    ModelSegments[j,4] <- ModelSegments[Upstream,1]
  }
  
  j<-j+1
  
}

# Determine Downstream Segment ----------
k<-1
for (k in 1:nrow(ModelSegments)){
  Downstream <- which(ModelSegments$Downstream==ModelSegments$RiverSeg[k])
  NumUp <- ModelSegments$RiverSeg[Downstream]
  ModelSegments[k,5]<- paste(NumUp, collapse = '+')
  
  if (is.empty(ModelSegments[k,5])==TRUE){
    ModelSegments[k,5]<- 'NA'
  } 
  k<-k+1
}

#Find the highest upstream segments (where water starts) ----------
#Lone wolf segments are segments with no head or tail
UpStartRow <- which(ModelSegments$Upstream=="NA")
UpStart <- ModelSegments[UpStartRow,]
rownames(UpStart)<- 1:nrow((UpStart))
LoneWolfRow <- as.vector(which(UpStart$Downstream=="NA" & UpStart$Upstream=="NA"))
LoneWolves <- UpStart[LoneWolfRow,]
UpStart <- UpStart[-c(LoneWolfRow),]


# Initalize variables for for loop that finds the next downstream segment ----------
i <- 1
alldata<- data.frame(matrix(ncol=2, nrow=nrow(UpStart)))


for (i in 1:nrow(UpStart)){
  
  #initialize variables for while loop  
  FirstSeg <- UpStart$RiverSeg[i]
  FirstSegRow<- which(ModelSegments$RiverSeg==FirstSeg)
  NextSeg <- ModelSegments$Downstream[FirstSegRow]
  x <- 2 #using x of 2 so that head segment is stored first, before entering while
  VarNextSeg <- data.frame(matrix(nrow=1, ncol=1))
  colnames(VarNextSeg) <- 'DownstreamSeg'
  VarNextSeg[1,1] <- FirstSeg
  
  #while loop adds the next downstream segment to the same row
  while (NextSeg != "NA"){
    NextSeg <- ModelSegments$Downstream[FirstSegRow]
    VarNextSeg[x,1] <- NextSeg
    FirstSeg <- NextSeg
    FirstSegRow<- which(ModelSegments$RiverSeg==FirstSeg)
    x <- x+1
  }
  VarNextSeg <-as.list(VarNextSeg[which(VarNextSeg!="NA"),]) #remove the last value from VarNextSeg
  #store VarNextSeg in alldata: has head and list of tails
  alldata[i,2] <- paste(VarNextSeg, collapse = '+')
  alldata[i,1] <- UpStart$RiverSeg[i] 
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
NoDownStream <- which(ModelSegments$Downstream=='NA')
NoDownStream <- ModelSegments[NoDownStream,]
rownames(NoDownStream) <- 1:nrow(NoDownStream)
LoneWolfRow <- as.vector(which(NoDownStream$Downstream=="NA" & NoDownStream$Upstream=="NA"))
LoneWolves <- NoDownStream[LoneWolfRow,]
NoDownStream <- NoDownStream[-c(LoneWolfRow),]


#pull only columns required for analysis (row 1=row 3, and row 2 is too condensed)
SegmentList <- sepsegs[,3:16]
colnames(SegmentList)<- 1:ncol(SegmentList)

# Create a csv for every tail with no outlet (that segment is the final outlet)----------
k <- 1
for (k in 1:nrow(NoDownStream)){
  RivSeg <- NoDownStream$RiverSeg[k]
  logic_SegList <- (SegmentList==RivSeg)
  logic_Rows<- which(apply(logic_SegList, 1, any))
  All_Segs <- SegmentList[logic_Rows,]
  All_Segs_Unique <- data.frame(table(unlist(All_Segs)))
  ASU <- data.frame(All_Segs_Unique$Var1)
  colnames(ASU)<- "RIVSEG"
  write.csv(ASU, paste0(file_path,"\\", RivSeg, "_Segs.csv"))
  k <- k + 1
}

#write a csv for the lone wolves
LoneSeg <- data.frame(LoneWolves$RiverSeg)
colnames(LoneSeg)<- "RIVSEG"
write.csv(LoneSeg, paste0(file_path,"\\Lone_Segs.csv"))

# SETTING UP FOR LOOP TO ASSOCIATE RIVER SEGMENTS ----------
Groupings<- read.csv(paste0(file_path,"\\All_Segs.csv"))#Pull the (location of desired data table)
Groupings<-data.frame(t(Groupings))
Groupings$X100<- -1
Groupings<-data.frame(t(Groupings))
Groupings$GROUP<-as.numeric(Groupings$GROUP)
Groupings<- Groupings[order(as.numeric(as.character(Groupings$GROUP))), ]
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
RiverSeg@data$RiverSeg<-as.character(RiverSeg@data$RiverSeg)
RiverSeg@data$Group<-NA

i<-1
# LOOP TO DETERMINE WHICH RIVER SEGMENTS HAVE DATA ----------
#The loop will run and add the desired metrics column to any segment that has a matching river segment ID with that metric
for (i in 1:length(RiverSeg@data$RiverSeg)){
  if (RiverSeg@data$RiverSeg[i]%in%RivSegGroups$RiverSeg){ #if the river segment ID is in the metrics file make it true, if not make it false
    RiverSeg@data$Group[i]<- RivSegGroups$Group[RivSegGroups$RiverSeg==RiverSeg@data$RiverSeg[i]]
  }
}

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
Groupingsnum<-nrow(Groupings)-2
Groupoingsnumstart<-Groupings$GROUP[2]
GroupingsnumFirst<-round(Groupingsnum/2, digits=0)
GroupingsnumSecond<-GroupingsnumFirst+1
colors<-data.frame(distinctColorPalette(k=nrow(Groupings),altCol = FALSE, runTsne = FALSE))
colors<-na.omit(colors)
colorsall<-as.character(colors[1:12,1])
colorsall<-data.frame(colorsall)
colorsall[]<-lapply(colorsall, as.character)
color1<-as.character(colors[1:GroupingsnumFirst,1])
color1<-data.frame(color1)
color1[]<-lapply(color1, as.character)
color2<-as.character(colors[GroupingsnumSecond:nrow(Groupings),1])
color2<-data.frame(color2)
color2[]<-lapply(color2, as.character)

# GRAPHING -----
title<-(' Southern Virgina Watershed Basins')

dir.create(paste0(file_path), showWarnings = FALSE);
png(filename=paste0(file_path, "\\RiverBasins.png"), 
    width=1400, height=950, units="px")

RiverSeg@data$color<- cut(RiverSeg@data$Group, Groupings$GROUP , labels=colorsall$colorsall)
SouthernRivers<- RiverSeg[!is.na (RiverSeg@data$color),]
plot(SouthernRivers, col=paste0(SouthernRivers@data$color))
plot(States, add=TRUE, col='gray')
lines(States, col='black')
plot(SouthernRivers, col=paste0(SouthernRivers@data$color), add=T)
plot(HUC6, add=TRUE,  col=adjustcolor( "lightblue", alpha.f = 0.2))
lines(HUC6, col='white', lwd= 4)
legend("bottomleft", legend = c('Outlet Segment'), col='black', lty=0, pch=15, pt.cex=7, bty='n', y.intersp=0.8, x.intersp=0.3, cex=3.5, lwd=2)
legend("bottom", legend=c(paste0(Groupings$LastSeg[Groupoingsnumstart:GroupingsnumFirst])), col=color1$color1, lty=0, pch=15, pt.cex=7, bty='n', y.intersp=0.8, x.intersp=0.3, cex=3.5, lwd=2)
legend("bottomright", legend=c(paste0(Groupings$LastSeg[GroupingsnumSecond:Groupingsnum]), 'Incomplete Data'), col=color2$color2, lty=0, pch=15, pt.cex=7, bty='n', y.intersp=0.8, x.intersp=0.3, cex=3.5, lwd=2)
legend("topleft", legend=c(title), lty=0, pt.cex=3, bty='n', y.intersp=0.75, x.intersp=0.3, cex=5, lwd=2)

dev.off()
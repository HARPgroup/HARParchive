# Analysis of gages and river segments for Cheasapeake Bay Watershed
# Kelsey Reitz
# 3/12/2019

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

rownames(wshed) <- 1:nrow(wshed)

# get rid of duplicate watersheds that are at the mouths of the rivers
dup <- wshed[which(duplicated(wshed$rivseg)==TRUE),]
nodups <- wshed[-c(which(duplicated(wshed$rivseg)==TRUE)),]

basins <- data.frame(unique(nodups$basin))
basins$rownum <- 1:nrow(basins)

# eshore <- nodups[which(nodups$basin==basins[1,1]),]
# potriv <- nodups[which(nodups$basin==basins[2,1]),]
# jamriv <- nodups[which(nodups$basin==basins[3,1]),]
# wshore <- nodups[which(nodups$basin==basins[4,1]),]
# rapriv <- nodups[which(nodups$basin==basins[5,1]),]
# paxriv <- nodups[which(nodups$basin==basins[6,1]),]
# yokriv <- nodups[which(nodups$basin==basins[7,1]),]


# check to make sure all segs have been kept: 
# nrow(nodups) == (nrow(eshore) + nrow(potriv) + nrow(jamriv) + nrow(wshore) + nrow(rapriv) + nrow(paxriv) + nrow(yokriv))


# Upstream/Downstream Analysis -----------------------------------
#pull upstream segments for segments
i <- 1
for (i in 1:nrow(nodups)){
  nodups$Upstream[i]<- str_sub(nodups[i,2], start=5L, end=8L)
  nodups$Downstream[i]<- str_sub(nodups[i,2], start=10L, end=-1L)
  i <- i + 1
}


# Determine Downstream Segment ----------
j <- 1
for (j in 1:nrow(nodups)){
  Downstream <- which(nodups$Upstream==nodups$Downstream[j])
  if (length(Downstream)==0){
    nodups$downseg[j]  <- 'NA'
  }else if (length(Downstream)!=0){
    nodups$downseg[j] <- as.character(nodups[Downstream,2])
  }
  j<-j+1
}

# Determine Upstream Segment ------
m<-1
for (m in 1:nrow(nodups)){
  Upstream <- which(nodups$downseg==nodups$rivseg[m])
  NumUp <- nodups$rivseg[Upstream]
  nodups$upseg[m]<- paste(NumUp, collapse = '+')
  
  if (is.empty(nodups$upseg[m])==TRUE){
    nodups$upseg[m]<- 'NA'
  } 
  m<-m+1
}



# Begin Exerpt of other code -------------------

folder_location = 'C:/Users/Kelsey/Desktop/GitHub/hydro-tools/HARP-2018/CPB_ModelGageAnalysis'
# Location of output destination
dir.create(paste0(folder_location, "\\GIS_Seg2"), showWarnings = FALSE);
file_path <-"C:/Users/Kelsey/Desktop/GitHub/hydro-tools/HARP-2018/CPB_ModelGageAnalysis\\GIS_Seg2"


#Find the highest upstream segments (where water starts) ----------
#Lone wolf segments are segments with no head or tail
UpStart <- nodups[which(nodups$upseg=="NA"),]
rownames(UpStart)<- 1:nrow((UpStart))
LoneWolves <- UpStart[which(UpStart$downseg=='NA'),]
UpStart <- UpStart[-c(which(UpStart$downseg=='NA')),]

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
  NextSeg <- nodups$downseg[FirstSegRow]
  x <- 2 #using x of 2 so that head segment is stored first, before entering while
  VarNextSeg <- data.frame(matrix(nrow=1, ncol=1))
  colnames(VarNextSeg) <- 'DownstreamSeg'
  VarNextSeg[1,1] <- FirstSeg
  
  #while loop adds the next downstream segment to the same row
  while (NextSeg != "NA"){
    NextSeg <- nodups$downseg[FirstSegRow]
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
NoDownStream <- nodups[which(nodups$downseg=='NA'),]
rownames(NoDownStream) <- 1:nrow(NoDownStream)
NoDownStream<- NoDownStream[-((which(NoDownStream$upseg=="NA"))),]


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




# DO NOT ADD LONE WOLF DATA -- IT WILL THROW THE MAP OFF
# #add lone wolf data to StoreASUS
# 
# LoneSeg<- data.frame(LoneWolves$rivseg)
# colnames(LoneSeg)<-'RIVSEG'
# LoneSeg<- data.frame(t(LoneSeg))
# LoneSeg[] <- lapply(LoneSeg, as.character)
# lone_seg_length <- length(LoneSeg) #determine number of segments
# addNAmax <- as.numeric(maxrow-lone_seg_length)
# 
# if (addNAmax !=0){                # add the required # of NAs and bind them to DownSegs
#   newNAmax<- data.frame(t(rep(NA, addNAmax)))
#   LoneSeg<- cbind(LoneSeg, newNAmax)
# }
# StoreASUS[l,]<- LoneSeg
# 
# 
row.names(StoreASUS)<-1:nrow(StoreASUS)
colnames(StoreASUS)<- 1:ncol(StoreASUS)
 
StoreASUS$GROUP <- 1:nrow(StoreASUS)



#StoreASUS<- t(StoreASUS)
write.csv(StoreASUS, paste0(file_path,"\\All_Segs.csv"))
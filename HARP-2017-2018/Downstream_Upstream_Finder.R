# 7/13/18 
# Script will be transformed into a function that calculates the next
# upstream or downstream segment. 

#load libraries
library(stringr)
library(rapportools)
library(tidyr)
library(questionr)

# Clear workspace
rm(list = ls())


# Location of "Linking Segments" csv:
folder_location = "C:\\Users\\Kelsey\\Desktop\\HARP\\UpstreamSegs";
# Location of output destination
file_path <-"C:\\Users\\Kelsey\\Downloads\\GIS_Seg" 


#Import Data of interest 
ImportTab <- read.csv(paste0(folder_location,"\\AllRiverSegments.csv"), 
                          header=TRUE, sep=',', stringsAsFactors=FALSE);
ImportTab <- data.frame(ImportTab)

#Pull only Southern VA
ImportTab <- ImportTab[which(ImportTab$Watershed=="Southern  Virgina Watershed"),]
rownames(ImportTab)<- 1:nrow(ImportTab)

#Create dataframe for upstream and downstream segments based on code in string
ModelSegments <- data.frame(matrix(nrow = nrow(ImportTab), ncol = 5))
colnames(ModelSegments)<- c('RiverSeg', 'Middle', 'Last', 'Downstream', 'Upstream')
ModelSegments$RiverSeg <- ImportTab$RiverSeg

#pull out 4 digit codes in middle and end for upstream/downstream segments
i <- 1
for (i in 1:nrow(ModelSegments)){

ModelSegments[i,2]<- str_sub(ModelSegments[i,1], start=5L, end=8L)
ModelSegments[i,3]<- str_sub(ModelSegments[i,1], start=10L, end=-1L)
i <- i + 1
}

# Determine upstream segment
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

# Determine Downstream segment
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

#Find the highest upstream segments (where water starts)
#Lone wolf segments are segments with no head or tail
UpStartRow <- which(ModelSegments$Upstream=="NA")
UpStart <- ModelSegments[UpStartRow,]
rownames(UpStart)<- 1:nrow((UpStart))
LoneWolfRow <- as.vector(which(UpStart$Downstream=="NA" & UpStart$Upstream=="NA"))
LoneWolves <- UpStart[LoneWolfRow,]
UpStart <- UpStart[-c(LoneWolfRow),]


#initalize variables 
i <- 1
alldata<- data.frame(matrix(ncol=2, nrow=nrow(UpStart)))

#for loop finds the next downstream segment
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

# initalize variables, then find the max number of river segments downstream of one head
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

#initialize variables for creating one large matrix of downstream segments
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

# Find furthest downstream segments, remove the lone wolves
NoDownStream <- which(ModelSegments$Downstream=='NA')
NoDownStream <- ModelSegments[NoDownStream,]
rownames(NoDownStream) <- 1:nrow(NoDownStream)
LoneWolfRow <- as.vector(which(NoDownStream$Downstream=="NA" & NoDownStream$Upstream=="NA"))
LoneWolves <- NoDownStream[LoneWolfRow,]
NoDownStream <- NoDownStream[-c(LoneWolfRow),]


#pull only columns required for analysis (row 1=row 3, and row 2 is too condensed)
SegmentList <- sepsegs[,3:16]
colnames(SegmentList)<- 1:ncol(SegmentList)

# create a csv for every tail with no outlet
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
LoneSeg <- LoneWolves$RiverSeg
colnames(LoneSeg)<- "RIVSEG"
write.csv(LoneSeg, paste0(file_path,"\\Lone_Segs.csv"))

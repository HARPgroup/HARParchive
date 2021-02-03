#9/17/18 
# Script will be transformed into a function that calculates the next
# upstream or downstream segment and maps them. 

# LOAD LIBRARIES ----------
library(stringr)
library(dataRetrieval)
library(rapportools)
library(tidyr)
library(questionr)
library(rgdal)
library(raster)
library(randomcoloR)
library(dplyr)
library(rgeos) #used for geospatial processing 
library(ggmap) #required for foritfy()
library(ggsn) #used for adding scale bar and north arrow to map
library(sp) #required for SpatialPolygonsDataFrame()

# Clear workspace
rm(list = ls())

# USER INPUTS #####################################################################################################
###################################################################################################################
site <- "http://deq1.bse.vt.edu/d.bet"    #Specify the site of interest, either d.bet OR d.dh
#hydro_tools <- 'C:\\Users\\HaileyMae\\Documents\\GitHub\\hydro-tools\\' #location of hydro-tools repo
hydro_tools <- 'C:\\Users\\nrf46657\\Desktop\\VAHydro Development\\GitHub\\hydro-tools\\' #location of hydro-tools repo
#save_directory <- 'C:\\Users\\HaileyMae\\Documents\\GitHub\\plots\\' #Location to output images
save_directory <- 'C:\\Users\\nrf46657\\Desktop\\VAHydro Development\\GitHub\\plots\\' #Location to output images

###################################################################################################################
# Location of "Linking Segments" csv:
folder_location <- paste0(hydro_tools,"HARP-2018",sep="")

# Location of output destination
dir.create(paste0(folder_location, "\\GIS_Seg"), showWarnings = FALSE);
file_path <- paste0(folder_location,"\\GIS_Seg",sep="") 

# Import Data of interest 
ImportTab <- read.csv(paste0(folder_location,"\\AllRiverSegments.csv"), 
                      header=TRUE, sep=',', stringsAsFactors=FALSE);
ImportTab <- data.frame(ImportTab)

GAGEImport<-'https://docs.google.com/spreadsheets/d/18m1ua87Bd0Lc3kDlhvHuBkqdRmm8LhxUA3dBmPlAbLQ/pub?output=csv'
gagedata<- read.csv(GAGEImport, header=TRUE, sep=',')

###################################################################################################################



#Generate REST token              
rest_uname = FALSE
rest_pw = FALSE
source(paste(hydro_tools,"auth.private", sep = "\\")); #load rest username and password, contained in auth.private file
source(paste(hydro_tools,"VAHydro-2.0","rest_functions.R", sep = "\\")) #load REST functions
STATES <- read.table(file=paste(hydro_tools,"GIS_LAYERS","STATES.tsv",sep="\\"), header=TRUE, sep="\t") #Load state geometries
RIVDF <- read.table(file=paste(hydro_tools,"GIS_LAYERS","RIVDF.csv",sep="/"), header=TRUE, sep=",") #Load river geometries
WBDF <- read.table(file=paste(hydro_tools,"GIS_LAYERS","WBDF.csv",sep="/"), header=TRUE, sep=",") #Load waterbody geometries

token <- rest_token(site, token, rest_uname, rest_pw)

# Map projection 
Projection<- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs' 

#specify spatial extent for map  
extent <- data.frame(x = c(-84, -75), 
                     y = c(35, 41))  


bb=readWKT(paste0("POLYGON((",extent$x[1]," ",extent$y[1],",",extent$x[2]," ",extent$y[1],",",extent$x[2]," ",extent$y[2],",",extent$x[1]," ",extent$y[2],",",extent$x[1]," ",extent$y[1],"))",sep=""))
bbProjected <- SpatialPolygonsDataFrame(bb,data.frame("id"), match.ID = FALSE)
bbProjected@data$id <- rownames(bbProjected@data)
bbPoints <- fortify(bbProjected, region = "id")
bbDF <- merge(bbPoints, bbProjected@data, by = "id")

#--------------------------------------------------------------------------------------------
#Load State Geometry
#--------------------------------------------------------------------------------------------
VA <- STATES[which(STATES$state == "VA"),]
VA_geom <- readWKT(VA$geom)
VA_geom_clip <- gIntersection(bb, VA_geom)
VAProjected <- SpatialPolygonsDataFrame(VA_geom_clip,data.frame("id"), match.ID = TRUE)
VAProjected@data$id <- rownames(VAProjected@data)
VAPoints <- fortify( VAProjected, region = "id")
VADF <- merge(VAPoints,  VAProjected@data, by = "id")

TN <- STATES[which(STATES$state == "TN"),]
TN_geom <- readWKT(TN$geom)
TN_geom_clip <- gIntersection(bb, TN_geom)
TNProjected <- SpatialPolygonsDataFrame(TN_geom_clip,data.frame("id"), match.ID = TRUE)
TNProjected@data$id <- rownames(TNProjected@data)
TNPoints <- fortify( TNProjected, region = "id")
TNDF <- merge(TNPoints,  TNProjected@data, by = "id")

NC <- STATES[which(STATES$state == "NC"),]
NC_geom <- readWKT(NC$geom)
NC_geom_clip <- gIntersection(bb, NC_geom)
NCProjected <- SpatialPolygonsDataFrame(NC_geom_clip,data.frame("id"), match.ID = TRUE)
NCProjected@data$id <- rownames(NCProjected@data)
NCPoints <- fortify( NCProjected, region = "id")
NCDF <- merge(NCPoints,  NCProjected@data, by = "id")

KY <- STATES[which(STATES$state == "KY"),]
KY_geom <- readWKT(KY$geom)
KY_geom_clip <- gIntersection(bb, KY_geom)
KYProjected <- SpatialPolygonsDataFrame(KY_geom_clip,data.frame("id"), match.ID = TRUE)
KYProjected@data$id <- rownames(KYProjected@data)
KYPoints <- fortify( KYProjected, region = "id")
KYDF <- merge(KYPoints,  KYProjected@data, by = "id")

WV <- STATES[which(STATES$state == "WV"),]
WV_geom <- readWKT(WV$geom)
WV_geom_clip <- gIntersection(bb, WV_geom)
WVProjected <- SpatialPolygonsDataFrame(WV_geom_clip,data.frame("id"), match.ID = TRUE)
WVProjected@data$id <- rownames(WVProjected@data)
WVPoints <- fortify( WVProjected, region = "id")
WVDF <- merge(WVPoints,  WVProjected@data, by = "id")

MD <- STATES[which(STATES$state == "MD"),]
MD_geom <- readWKT(MD$geom)
MD_geom_clip <- gIntersection(bb, MD_geom)
MDProjected <- SpatialPolygonsDataFrame(MD_geom_clip,data.frame("id"), match.ID = TRUE)
MDProjected@data$id <- rownames(MDProjected@data)
MDPoints <- fortify( MDProjected, region = "id")
MDDF <- merge(MDPoints,  MDProjected@data, by = "id")

DE <- STATES[which(STATES$state == "DE"),]
DE_geom <- readWKT(DE$geom)
DE_geom_clip <- gIntersection(bb, DE_geom)
DEProjected <- SpatialPolygonsDataFrame(DE_geom_clip,data.frame("id"), match.ID = TRUE)
DEProjected@data$id <- rownames(DEProjected@data)
DEPoints <- fortify( DEProjected, region = "id")
DEDF <- merge(DEPoints,  DEProjected@data, by = "id")

PA <- STATES[which(STATES$state == "PA"),]
PA_geom <- readWKT(PA$geom)
PA_geom_clip <- gIntersection(bb, PA_geom)
PAProjected <- SpatialPolygonsDataFrame(PA_geom_clip,data.frame("id"), match.ID = TRUE)
PAProjected@data$id <- rownames(PAProjected@data)
PAPoints <- fortify( PAProjected, region = "id")
PADF <- merge(PAPoints,  PAProjected@data, by = "id")

NJ <- STATES[which(STATES$state == "NJ"),]
NJ_geom <- readWKT(NJ$geom)
NJ_geom_clip <- gIntersection(bb, NJ_geom)
NJProjected <- SpatialPolygonsDataFrame(NJ_geom_clip,data.frame("id"), match.ID = TRUE)
NJProjected@data$id <- rownames(NJProjected@data)
NJPoints <- fortify( NJProjected, region = "id")
NJDF <- merge(NJPoints,  NJProjected@data, by = "id")

OH <- STATES[which(STATES$state == "OH"),]
OH_geom <- readWKT(OH$geom)
OH_geom_clip <- gIntersection(bb, OH_geom)
OHProjected <- SpatialPolygonsDataFrame(OH_geom_clip,data.frame("id"), match.ID = TRUE)
OHProjected@data$id <- rownames(OHProjected@data)
OHPoints <- fortify( OHProjected, region = "id")
OHDF <- merge(OHPoints,  OHProjected@data, by = "id")

SC <- STATES[which(STATES$state == "SC"),]
SC_geom <- readWKT(SC$geom)
SC_geom_clip <- gIntersection(bb, SC_geom)
SCProjected <- SpatialPolygonsDataFrame(SC_geom_clip,data.frame("id"), match.ID = TRUE)
SCProjected@data$id <- rownames(SCProjected@data)
SCPoints <- fortify( SCProjected, region = "id")
SCDF <- merge(SCPoints,  SCProjected@data, by = "id")

DC <- STATES[which(STATES$state == "DC"),]
DC_geom <- readWKT(DC$geom)
DC_geom_clip <- gIntersection(bb, DC_geom)
DCProjected <- SpatialPolygonsDataFrame(DC_geom_clip,data.frame("id"), match.ID = TRUE)
DCProjected@data$id <- rownames(DCProjected@data)
DCPoints <- fortify( DCProjected, region = "id")
DCDF <- merge(DCPoints,  DCProjected@data, by = "id")
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
  print(paste("Extracting Code for ",i," of ",nrow(ModelSegments),sep=""))
  ModelSegments[i,2]<- str_sub(ModelSegments[i,1], start=5L, end=8L)
  ModelSegments[i,3]<- str_sub(ModelSegments[i,1], start=10L, end=-1L)
  i <- i + 1
}

# Determine Upstream Segment ----------
j <- 1
for (j in 1:nrow(ModelSegments)){
  print(paste("Determining Upstream Segment for ",j," of ",nrow(ModelSegments),sep=""))
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
  print(paste("Determining Downstream Segment for ",k," of ",nrow(ModelSegments),sep=""))
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
  print(paste("Finding Downstream Segment for ",i," of ",nrow(UpStart),sep=""))
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
  print(paste("Finding Max Number of River Segments Downstream for ",i," of ",nrow(alldata),sep=""))
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
  print(paste("Generating matrix of downstream segments: row ",j," of ",nrow(alldata),sep=""))
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
  print(paste("Generating csv for each tail with no outlet: row ",k," of ",nrow(NoDownStream),sep=""))
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
  print(paste("Groupings ",i," of ",nrow(Groupings),sep=""))
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

# Retrieve Riversegment Feature From VAHydro 
#-------------------------------------------------------------------------------------------- 
#----------Define function for watershedDF-----------------------
getWatershedDF <- function(geom){
  
  watershed_geom <- readWKT(geom)
  watershed_geom_clip <- gIntersection(bb, watershed_geom)
  if (is.null(watershed_geom_clip)) {
    watershed_geom_clip = watershed_geom
  }
  wsdataProjected <- SpatialPolygonsDataFrame(watershed_geom_clip,data.frame("id"), match.ID = FALSE)
  wsdataProjected@data$id <- rownames(wsdataProjected@data)
  watershedPoints <- fortify(wsdataProjected, region = "id")
  watershedDF <- merge(watershedPoints, wsdataProjected@data, by = "id")
  
  return(watershedDF)
}

i<-1
q<-1
RivSegMap<-data.frame(matrix(nrow=1, ncol=9))
nrowold<-0
for (i in 1:nrow(ImportTab)) {  
  print(paste("ImportTab ",i," of ",nrow(ImportTab),sep=""))
  RivSeg <- ImportTab$RiverSeg[i]
  namer <- paste0(RivSeg)
  
  # Retrieve Riversegment Feature From VAHydro  -----------------------------
  
  inputs <- list (
    bundle = 'watershed',
    ftype = 'vahydro',
    hydrocode = paste0('vahydrosw_wshed_',RivSeg)
  )
  hydrocode <- paste0('vahydrosw_wshed_',RivSeg)
  
  dataframe <- getFeature(inputs, token, site)
  #print(dataframe)
  if (dataframe!=FALSE){
    hydroid <- dataframe$hydroid
    inputs <- list(
      varkey = "wshed_drainage_area_sqmi",
      featureid = hydroid,
      entity_type = "dh_properties"
    )
    prop <- getProperty(inputs, site, prop)
    inputs <- list(
      varkey = "wshed_local_area_sqmi",
      featureid = hydroid,
      entity_type = "dh_feature"
    )
    local_da_prop <- getProperty(inputs, site, prop)
    local_da_prop$pid<-as.integer(as.character(local_da_prop$pid))
    #postProperty(inputs = local_da_prop)
    
    geom <- dataframe$geom
    watershedDF <- getWatershedDF(geom)
    watershedDF$Seg<-namer
    lastrow<-nrow(watershedDF)+nrowold
    RivSegMap[q:lastrow,]<-watershedDF 
    q<-q+nrow(watershedDF)
    nrowold<-nrowold+nrow(watershedDF)
  }
  
  i<-i+1
}

## PICK UP HERE NEED TO CHANGE LOOP BC IT ISNT ALL ONE BIG DATA FRAME ANYMORE
RivSegMap$X9<-as.character(RivSegMap$X9)
RivSegMap$Group<-NA

i<-1
# LOOP TO DETERMINE WHICH RIVER SEGMENTS HAVE DATA ----------
#The loop will run and add the desired metrics column to any segment that has a matching river segment ID with that metric
for (i in 1:nrow(RivSegMap)){
  print(paste("DETERMINE WHICH RIVER SEGMENTS HAVE DATA ",i," of ",nrow(RivSegMap),sep=""))
  if (RivSegMap$X9[i]%in%RivSegGroups$RiverSeg){ #if the river segment ID is in the metrics file make it true, if not make it false
    RivSegMap$Group[i]<- RivSegGroups$Group[RivSegGroups$RiverSeg==RivSegMap$X9[i]]
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
  print(paste("Coordinate ",i," of ",nrow(Coordinate),sep=""))
  Groupings$LastSeg[i]<-as.character(Groupings[Coordinate[p,1],Coordinate[p,2]])
  i<-i+1
  p<-p+1
}

# GRAPHING -----
title<-(' Southern Virgina Watershed Basins')

dir.create(paste0(file_path), showWarnings = FALSE);
png(filename=paste0(file_path, "\\RiverBasins.png"), 
    width=1400, height=950, units="px")

SouthernRivers<- RivSegMap[!is.na (RivSegMap$Group),]

# Pull out 2 digit codes at beginning of upstream/downstream segments
i <- 1
for (i in 1:nrow(SouthernRivers)){
  print(paste("Pull out 2 digit codes ",i," of ",nrow(SouthernRivers),sep=""))
  SouthernRivers[i,11]<-substr(SouthernRivers$X9[i], start=1L, stop=2L)
  print(paste("On SouthernRivers ",i," of ",nrow(SouthernRivers),sep=""))
  i <- i + 1
}
names(SouthernRivers)<-c(paste0(names(PADF)),'RivSeg','num', 'RivID')
SouthernRivers<-group_by(SouthernRivers,RivID)

finalmap <- ggplot(data = VADF, aes(x=long, y=lat, group = group))+
  geom_polygon(data = bbDF, color="black", fill = "powderblue",lwd=0.5)+
  geom_polygon(data = VADF, color="gray46", fill = "gray")+
  geom_polygon(data = TNDF, color="gray46", fill = "gray", lwd=0.5)+
  geom_polygon(data = NCDF, color="gray46", fill = "gray", lwd=0.5)+
  geom_polygon(data = SCDF, color="gray46", fill = "gray", lwd=0.5)+
  geom_polygon(data = KYDF, color="gray46", fill = "gray", lwd=0.5)+
  geom_polygon(data = WVDF, color="gray46", fill = "gray", lwd=0.5)+
  geom_polygon(data = MDDF, color="gray46", fill = "gray", lwd=0.5)+
  geom_polygon(data = DEDF, color="gray46", fill = "gray", lwd=0.5)+
  geom_polygon(data = PADF, color="gray46", fill = "gray", lwd=0.5)+
  geom_polygon(data = NJDF, color="gray46", fill = "gray", lwd=0.5)+
  geom_polygon(data = OHDF, color="gray46", fill = "gray", lwd=0.5)+
  geom_polygon(data = DCDF, color="gray46", fill = "gray", lwd=0.5)+
  
  
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          #axis.text =element_text(size=rel(2)),    #Uncomment to display lat/long on plot
          #axis.title = element_text(size=rel(2)),  #Uncomment to display lat/long on plot
          axis.ticks.y=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          legend.text = element_text(size=rel(2)),
          legend.title = element_text(size= rel(2))
    )
   # 

i<-1
Segs<-data.frame(matrix(nrow=159,ncol=2))
Segs<-(unique(SouthernRivers$RivSeg))
for (i in 1:length(Segs)){
  print(paste("Segs ",i," of ",length(Segs),sep=""))
  namer<-paste0(Segs[i])
  currentseg<-subset(SouthernRivers,SouthernRivers$RivSeg==Segs[i])
  currentseg$RivID<-as.character(currentseg$RivID)
  if (currentseg$RivID[1]=='OD'){
    segadd<-finalmap+geom_polygon(data = currentseg, color='gray46', aes(fill = 'c'),lwd=0.5)
    Dan<- c('Dan','lightblue')
  } else if (currentseg$RivID[1]=='OR'){
    segadd<-finalmap+geom_polygon(data = currentseg, color='gray46', aes(fill = 'i'),lwd=0.5)
    Roanoke<- c('Roanoke','blue')
  } else if (currentseg$num[1]==13){
    segadd<-finalmap+geom_polygon(data = currentseg, color='gray46', aes(fill = 'l'),lwd=0.5)
    IncompleteData<- c('Incomplete Data','gray46')
  } else if (currentseg$num[1]==12){
    segadd<-finalmap+geom_polygon(data = currentseg, color='gray46', aes(fill = 'k'),lwd=0.5)
    LackingGage<- c('Lacking Gage for Comparison','black')
  } else if (currentseg$num[1]==11){
    segadd<-finalmap+geom_polygon(data = currentseg, color='gray46', aes(fill = 'j'),lwd=0.5)
    Tennessee<- c('Tennessee','aquamarine')
  } else if (currentseg$num[1]==10){
    segadd<-finalmap+geom_polygon(data = currentseg, color='gray46', aes(fill = 'k'),lwd=0.5)
    LackingGage<- c('Lacking Gage for Comparison','black')
  } else if (currentseg$num[1]==9){
    segadd<-finalmap+geom_polygon(data = currentseg, color='gray46', aes(fill = 'd'),lwd=0.5)
    Holston<-c('Holston', 'darkorchid2')
  } else if (currentseg$num[1]==8){
    segadd<-finalmap+geom_polygon(data = currentseg, color='gray46', aes(fill = 'g'),lwd=0.5)
    NewRiver<- c('New', 'red')
  } else if (currentseg$num[1]==7){
    segadd<-finalmap+geom_polygon(data = currentseg, color='gray46', aes(fill = 'h'),lwd=0.5)
    Nottoway<-c('Nottoway', 'orange')
  } else if (currentseg$num[1]==6){
    segadd<-finalmap+geom_polygon(data = currentseg, color='gray46', aes(fill = 'b'),lwd=0.5)
    Blackwater<-c('Blackwater','pink')
  } else if (currentseg$num[1]==5){
    segadd<-finalmap+geom_polygon(data = currentseg, color='gray46', aes(fill = 'a'),lwd=0.5)
    BigSandy<-c('Big Sandy', 'deeppink2')
  } else if (currentseg$num[1]==4){
    segadd<-finalmap+geom_polygon(data = currentseg, color='gray46', aes(fill = 'e'),lwd=0.5)
    Meherrin<-c('Meherrin', 'lawngreen')
  } else if (currentseg$num[1]==3){
    segadd<-finalmap+geom_polygon(data = currentseg, color='gray46', aes(fill = 'a'),lwd=0.5)
    BigSandy<-c('Big Sandy', 'deeppink2')
  } 
  finalmap<-segadd
  i<-1+i
}

finalmap<-finalmap+geom_polygon(data = bbDF, color="black", fill = NA,lwd=0.5)
  
  #ADD NORTH ARROW AND SCALE BAR
finalmap<-finalmap+north(bbDF, location = 'topleft', symbol = 12, scale=0.1)+
  scalebar(bbDF, location = 'bottomleft', dist = 100, dd2km = TRUE, model = 'WGS84',st.bottom=FALSE, 
           anchor = c(
             x = (((extent$x[2] - extent$x[1])/2)+extent$x[1])-1.1,
             y = extent$y[1]+(extent$y[1])*0.001
           ))+
  scale_x_continuous(limits = c(extent$x))+
  scale_y_continuous(limits = c(extent$y))+
  
  scale_fill_manual(breaks=c('a', 'b', 'c', 'd', 'e', 'g', 'h', 'i', 'j', 'k', 'l'), 
                    limits=c('a', 'b', 'c', 'd', 'e', 'g', 'h', 'i', 'j', 'k', 'l'), 
                    labels = paste0(c(BigSandy[1],Blackwater[1],Dan[1],Holston[1], Meherrin[1], NewRiver[1], Nottoway[1], Roanoke[1], Tennessee[1], LackingGage[1], IncompleteData[1])),
                    values = paste0(c(BigSandy[2],Blackwater[2],Dan[2],Holston[2], Meherrin[2], NewRiver[2], Nottoway[2], Roanoke[2], Tennessee[2], LackingGage[2], IncompleteData[2])), name='Southern Virginia Watersheds')+
  

  # ADD RIVERS ####################################################################
  geom_point(data = RIVDF, aes(x = long, y = lat), color="steelblue1", size=0.09)+
  #################################################################################
  # ADD WATERBODIES ###############################################################
  geom_point(data = WBDF, aes(x = long, y = lat), color="steelblue1", size=0.09)
  #################################################################################


#Uncomment to plot without gage locations
#filename <- paste("Watersheds.png", sep="")
#ggsave(file=filename, path = save_directory, width=16, height=9.5)

i<-1
for(i in 1:nrow(gagedata)){
  siteinfo<-readNWISsite(paste0('0',gagedata$Gage_number[i]))
  gagedf<-data.frame(x=as.numeric(siteinfo$dec_long_va), y=as.numeric(siteinfo$dec_lat_va),X.id='id', id='1')
  segadd<-finalmap+geom_point(aes(x=x, y=y, group=id, shape = 'tri'), data=gagedf, color='black', fill = 'black', size=1.5)
  finalmap<-segadd
  i<-1+i
}

gagemap<- finalmap+scale_fill_manual(breaks=c('a', 'b', 'c', 'd', 'e', 'g', 'h', 'i', 'j', 'k', 'l'), limits=c('a', 'b', 'c', 'd', 'e', 'g', 'h', 'i', 'j', 'k', 'l'), labels = paste0(c(BigSandy[1],Blackwater[1],Dan[1],Holston[1], Meherrin[1], NewRiver[1], Nottoway[1], Roanoke[1], Tennessee[1], LackingGage[1], IncompleteData[1])),
                                     values = paste0(c(BigSandy[2],Blackwater[2],Dan[2],Holston[2], Meherrin[2], NewRiver[2], Nottoway[2], Roanoke[2], Tennessee[2], LackingGage[2], IncompleteData[2])), name='Southern Virginia Watersheds')
gagemap <- gagemap+scale_shape_manual(breaks=c('tri'), limits=c('tri'), labels=c('USGS Gage'), values = c(24), name = 'Points of Interest')


filename <- paste("WatershedswithGages.png", sep="")
ggsave(file=filename, path = save_directory, width=16, height=9.5)



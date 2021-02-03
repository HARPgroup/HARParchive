# 7/18/18 
# Creates a map of segment and associated GIS gage 

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

# USER INPUTS -----
# Specify which segment you would like graphed
DesiredSeg<- 'TU4_8680_8810'
DesiredState<- 'VA'

# Location of "Gage to Segment" csv:
folder_location <- "C:\\Users\\HaileyMae\\Documents\\GitHub\\hydro-tools\\HARP-2018\\DEQ_Model_vs_USGS_Comparison\\data";

# Location of "All_Segs" csv (csv that has all associated upstream and downstrem segments)
basin_folder <- "C:\\Users\\HaileyMae\\Documents\\GitHub\\hydro-tools\\HARP-2018\\GIS_Seg"

# Location of output destination
dir.create(paste0(folder_location, "\\GIS_Seg"), showWarnings = FALSE);
file_path <-"C:\\Users\\HaileyMae\\Documents\\GitHub\\hydro-tools\\HARP-2018\\DEQ_Model_vs_USGS_Comparison\\gage_locations" 

# Import Data of interest 
ImportData <- read.csv(paste0(folder_location,"\\Gage_To_Segment_GIS_Friendly.csv"), 
                       header=TRUE, sep=',', stringsAsFactors=FALSE);
ImportData <- data.frame(ImportData)

ImportHUC <- read.csv(paste0(folder_location,"\\HUC_To_Seg.csv"), 
                       header=TRUE, sep=',', stringsAsFactors=FALSE);
ImportHUC <- data.frame(ImportHUC)

# Import Basin Data 
ImportBasin <- read.csv(paste0(basin_folder,"\\All_Segs.csv"), 
                      header=TRUE, sep=',', stringsAsFactors=FALSE);
ImportBasin <- data.frame(ImportBasin)

# Map projection 
Projection<- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs' 

# Importing base map
States<- readOGR('E:\\SouthernRivers\\cb_2017_us_state_5m','cb_2017_us_state_5m' )  #Pull the (location of GIS shapefile, desired shapefile)
States<- spTransform(States, CRS=Projection)                                        #Put shapefile in correct projection/coordinated system

# Importing desired focus area
RiverSeg<-readOGR ('E:\\SouthernRivers\\BaseMap', 'AlteredRiverSegs')             
RiverSeg<-spTransform(RiverSeg, CRS=Projection)                                   

# Importing HUC 6 data
HUC6<- readOGR('E:\\SouthernRivers\\BaseMap', 'HUC6')
HUC6<-spTransform(HUC6, CRS=Projection) 

# Importing gage locations
Gage<- readOGR('E:\\SouthernRivers\\BaseMap', 'gis_GISOWNER_P5_CalibStats_P')
Gage<- spTransform(Gage, CRS=Projection)

# Importing river reaches 
Reaches<- readOGR('E:\\SouthernRivers\\BaseMap', 'gis_GISOWNER_P5_Reaches')
Reaches<- spTransform(Reaches, CRS=Projection)

# -------------------------------------------------------------------------------------------------
# DETERMINING IF DESIRED SEG SHOULD BE GROUPED WITH OTHER SEGMENTS-----
DesiredSeg<-data.frame(DesiredSeg)
DesiredSeg$Amount<-1
colnames(DesiredSeg)<- c("RIVSEG", "Amount")
DesiredSeg$RIVSEG<-data.frame(lapply(DesiredSeg$RIVSEG,as.character), stringsAsFactors=FALSE)
colnames(DesiredSeg)<- c("RIVSEG", "Amount")

a<-1
b<-1
c<-0
MultiSegs<-data.frame(matrix(nrow=1, ncol=1))
GroupedSegs<-data.frame(matrix(nrow=1,ncol=1))

for (a in 1:nrow(ImportData)){
  
  if (length(which(ImportData$gage_number[a]==ImportData$gage_number))>1){
    d<-1
    e<-1
    for (d in 1:nrow(ImportData)){
      if (ImportData$gage_number[a]==ImportData$gage_number[d]){
        MultiSegs[e,1]<-ImportData$river_segment[d]
        MultiSegs[e,2]<-ImportData$gage_number[d]
        e<-e+1
      }
      d<-d+1
      
    }
  
  g<-nrow(MultiSegs)
  c<-c+g
  GroupedSegs[b:c,1]<-MultiSegs[,1]
  GroupedSegs[b:c,2]<-MultiSegs[,2]
  b<-b+g
  
  }
  
  a<-a+1
}
colnames(GroupedSegs)<- c('Segment', "Gage")
GroupedSegs<-data.frame(unique(GroupedSegs))
Specified<-DesiredSeg$RIVSEG[1,1]

f<- 1
SingleSeg<-data.frame(matrix(nrow=1, ncol=1))
MappingSeg<-data.frame(matrix(nrow=1, ncol=1))
for (f in 1:nrow(GroupedSegs)){
  if (Specified==GroupedSegs$Segment[f]){
    g<-1
    h<-1
    for (g in 1:nrow(GroupedSegs)){
      if(GroupedSegs$Gage[f]==GroupedSegs$Gage[g]){
        DesiredSeg[h,1]<- GroupedSegs$Segment[g]
        DesiredSeg[h,2]<-h
        h<-h+1
        k<-1
      }
      g<-g+1
    }
  }
  if (nrow(DesiredSeg)>0){
    for (k in 1:nrow(DesiredSeg)){
      SingleSeg<-(str_sub(DesiredSeg[k,1], start=1L, end=13L))
      MappingSeg[k,1]<-as.character(SingleSeg[k])
      k<-k+1
    }
  }
  f<-f+1
}

MappingSeg<-na.omit(MappingSeg)
colnames(MappingSeg)<-('RivSeg')

# SETTING UP FOR LOOPS -----
#Create an empty column for the desired data you would like to be applied to your river segments
#The metrics will be added to the shapefile for the river segments as seprate columns and the desired metric will be given its own column
RiverSeg@data$RiverSeg<-as.character(RiverSeg@data$RiverSeg)
RiverSeg@data$Gage<-NA
States@data$State<-NA

  j<-1
  for (j in 1:length(RiverSeg@data$RiverSeg)){
    if (RiverSeg@data$RiverSeg[j]%in%MappingSeg$RivSeg){ #if the river segment ID is in the metrics file make it true, if not make it false
      RiverSeg@data$Gage[j]<- paste0('0', ImportData$gage_number[ImportData$river_segment==RiverSeg@data$RiverSeg[j]])
    }
    j<-j+1
  }  
  i<-1
  for (i in 1:length(States@data$STUSPS)){
    if (States@data$STUSPS[i]%in%DesiredState){ #if the river segment ID is in the metrics file make it true, if not make it false
      States@data$State[i]<- DesiredState[DesiredState==States@data$STUSPS[i]]
    }
    i<-i+1
  }  
  
SouthernRivers<- RiverSeg[!is.na (RiverSeg@data$Gage),]
StateID<-States[!is.na (States@data$State),]

# GRAPHING CLOSE UP-----
  title<-paste0((MappingSeg$RivSeg), collapse= ' \ ')
  Gageinfo<-paste0('Gage ID: ','\n' ,SouthernRivers@data$Gage[1])
  png(filename=paste0(file_path, '\\', title ," and Gage.png"), 
      width=1400, height=950, units="px")
  
  plot(SouthernRivers)
  plot(States, col='lightgreen', add=T)
  lines(States, col='white',add=T)
  plot(Reaches, col='blue', lwd=2, add=T)
  lines(SouthernRivers, col='black', lwd=3, add=T)
  plot(Gage, col='red', type = 'p', pch=19, lwd=9, add=T)
  
  legend("bottomleft", legend=c(Gageinfo, 'Segment', 'Reaches'), col=c('red', 'black' , 'blue'), lty=0, pch=15, pt.cex=7, bty='y', y.intersp=0.8, x.intersp=0.3, cex=3.5, lwd=2)
  legend("topleft", legend=c(title), lty=0, pt.cex=3, bty='n', y.intersp=0.75, x.intersp=0.3, cex=3.9, lwd=2)
  
  dev.off()
  
# GRAPHING BASIN LOCATION -----
  ImportBasin<-data.frame(t(ImportBasin))
  ImportBasin$X100<- -1
  ImportBasin<-data.frame(t(ImportBasin))
  ImportBasin$GROUP<-as.numeric(ImportBasin$GROUP)
  ImportBasin<- ImportBasin[order(as.numeric(as.character(ImportBasin$GROUP))), ]
  l<-1
  m<-1
  n<-ncol(ImportBasin)
  RivSegGroups<-data.frame(matrix(nrow=1,ncol=1))
  for (l in 1:nrow(ImportBasin)){
    store<-data.frame(matrix(nrow=1,ncol=2))
    o<-1
    for (o in 1:ncol(ImportBasin)){
      store[o,1]<- as.character(ImportBasin[l,o])
      store[o,2]<-l
      o<-o+1
    }
    RivSegGroups[m:n,1]<-store[,1]
    RivSegGroups[m:n,2]<-store[,2]
    m<-m+nrow(store)
    n<-n+nrow(store)
    l<- l+1
  }  
  RivSegGroups<-na.omit(RivSegGroups)
  colnames(RivSegGroups)<- c('RiverSeg', 'Group')
  RiverSeg@data$RiverSeg<-as.character(RiverSeg@data$RiverSeg)
  RiverSeg@data$Group<-NA
  
# LOOP TO DETERMINE WHICH RIVER SEGMENTS HAVE DATA ----------
#The loop will run and add the desired metrics column to any segment that has a matching river segment ID with that metric
  p<-1
  for (p in 1:length(RiverSeg@data$RiverSeg)){
    if (RiverSeg@data$RiverSeg[p]%in%RivSegGroups$RiverSeg){ #if the river segment ID is in the metrics file make it true, if not make it false
      RiverSeg@data$Group[p]<- RivSegGroups$Group[RivSegGroups$RiverSeg==RiverSeg@data$RiverSeg[p]]
    }
    p<-p+1
  }
  
  LogicGrouping<-data.frame(is.na(ImportBasin[,2:(ncol(ImportBasin)-1)]))
  LogicGrouping$AllTrue<-(rep(TRUE, nrow(LogicGrouping)))
  q<-1
  Coordinate<- data.frame(matrix(nrow=1, ncol=1))
  for (q in 1:nrow(LogicGrouping)){
    r<-1
    while (LogicGrouping[q,r]!=TRUE){
      down<- r
      r<-r+1
    }
    Coordinate[q,1]<-q
    Coordinate[q,2]<-r
    q<-q+1
  }
  
  s<-1
  t<-1
  for (s in 1:nrow(Coordinate)){
    ImportBasin$LastSeg[s]<-as.character(ImportBasin[Coordinate[t,1],Coordinate[t,2]])
    s<-s+1
    t<-t+1
  }
  
  Basins<- RiverSeg[!is.na (RiverSeg@data$Group),]
  rownames(Basins@data)<- 1:nrow(Basins)
  Basins@data$RiverSeg<-as.character(Basins@data$RiverSeg)
  Basins@data$Basin<-NA
  u<-1
  for (u in 1:length(Basins@data$Group)){
    if (Basins@data$RiverSeg[u]%in%MappingSeg$RivSeg){ #if the river segment ID is in the metrics file make it true, if not make it false
      Basins@data$Basin[u]<- Basins@data$Group[u][MappingSeg$RivSeg==Basins@data$RiverSeg[u]]
    }
    u<-u+1
  }
  BasinNum<-Basins[!is.na(Basins@data$Basin),]
  BasinGroup<-subset(Basins@data, Basins@data$Group==as.numeric(BasinNum@data$Basin))
  
  v<-1
  for (v in 1:length(Basins@data$Group)){
    if (Basins@data$Group[v]%in%BasinGroup$Group){ #if the river segment ID is in the metrics file make it true, if not make it false
      Basins@data$Basin[v]<- BasinGroup$Group[BasinGroup$Group==Basins@data$Group[v]]
    }
    v<-v+1
  }
  
  GraphedBasin<-Basins[!is.na(Basins@data$Basin),]
  HUC6@data$Name<-as.character(HUC6@data$Name)
  HUC6@data$Graph<-NA
  
  w<-1
  for (w in 1:length(HUC6@data$Name)){
    if (ImportHUC$RivSeg[w]%in%BasinGroup$RiverSeg){
      x<-1
      for (x in 1:length(HUC6@data$Name)){
        if (HUC6@data$Name[x]%in%ImportHUC$HUC[w]){ #if the river segment ID is in the metrics file make it true, if not make it false
      HUC6@data$Graph[x]<- 1
        }
    x<-x+1
      }
    }
    w<-w+1
  }
  GraphedHUC<-HUC6[!is.na(HUC6@data$Graph),]
  title<-paste0((MappingSeg$RivSeg), collapse= ' \ ')
  titlet<-paste0(title,'\n' ,' with drainage basin and HUC6')
  png(filename=paste0(file_path, '\\', title ," location.png"), 
      width=1400, height=950, units="px")
  
  #specify spatial extent for map  
  extent <- data.frame(x = c(-84, -75), 
                       y = c(35, 41))  
  bb=readWKT(paste0("POLYGON((",extent$x[1]," ",extent$y[1],",",extent$x[2]," ",extent$y[1],",",extent$x[2]," ",extent$y[2],",",extent$x[1]," ",extent$y[2],",",extent$x[1]," ",extent$y[1],"))",sep=""))
  plot(StateID)
  plot(States, col='lightgreen', add=T)
  lines(States, col='white',lwd=2, add=T)
  GraphedHUC6<-gIntersection(bb, GraphedHUC)
  plot(GraphedHUC6,col=adjustcolor( "blue", alpha.f = 0.3), add=T)
  plot(SouthernRivers, col='red', add=T)
  lines(GraphedBasin, col='black', lwd=1, add=T)

  legend("topleft", legend=c(titlet), lty=0, pt.cex=3, bty='n', y.intersp=0.75, x.intersp=0.3, cex=4, lwd=2)
  
  dev.off()

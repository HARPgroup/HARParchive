# DESCRIPTION -----
# This code is used to create quick maps for spatial analysis of the percent error of metrics associated with river segments

# LIBRARIES -----
library(rgdal)
library(raster)
library(rgeos)
library(ggmap)
library(ggsn)
library(sp)

# USER INPUTS -----

# Address of "DEQ_Model_vs_USGS_Comparison" folder
# Include "DEQ_Model_vs_USGS_Comparison" in address!

container <-'C:\\Users\\Daniel\\Documents\\HARP\\GitHub\\hydro-tools\\HARP-2018\\DEQ_Model_vs_USGS_Comparison';

# Should new or original data be used?
new.or.original <- "new"

# CARRYOVER IF MASTER IS BEING RUN ----------------------------------------
if (exists("container.master") == TRUE) {
  container <- container.master
  siteNo <- siteNo.master
  new.or.original <- new.or.original.master
}

# NEW OR ORIGINAL DATA SWITCH ---------------------------------------------

if (new.or.original == "new") {
  container.cont <- "\\spatial_analysis\\user's_results\\"
} else if (new.or.original == "original") {
  container.cont <- "\\spatial_analysis\\harp_analysts'_results\\"
} else {
  print("ERROR: neither new or original data specified")
}

site <- "http://deq2.bse.vt.edu/d.bet"    #Specify the site of interest, either d.bet OR d.dh

basepath='C:\\Users\\Daniel\\Documents\\HARP\\GitHub\\hydro-tools';

# SETUP

source(paste(basepath,'config.local.private',sep='/'));
source(paste(hydro_tools,"VAHydro-2.0/rest_functions.R", sep = "/")); 
source(paste(hydro_tools,"VAHydro-1.0/fn_vahydro-1.0.R", sep = "/"));  
source(paste(hydro_tools,"LowFlow/fn_iha.R", sep = "/"));
#retrieve rest token
source(paste(hydro_tools,"auth.private", sep = "/"));#load rest username and password, contained in auth.private file
token <- rest_token(site, token, rest_uname, rest_pw);
options(timeout=120); # set timeout to twice default level to avoid abort due to high traffic

# IMPORT DATA -----
#The projection that you want the map to be output in requires a Proj4 code. 
#Search the projection you want (in this case WGS 1984 was used becuase it is compatible with VA Hydro) and choose the link for 'spatialreference.org'
#Once on the website click on the Proj4 link and copy and paste the provided line in quotes after its associated variable name 

#Importing desired data
#The desired data for this code is the metrics that have been run for the river segments located outside of the Cheasapeake Bay Watershed
#Metrics<- read.csv(paste0(container,'\\results\\all.segments.pct.error.csv'))    #Pull the (location of desired data table)
Metrics <- read.csv(file=paste(container,"\\spatial_analysis\\user's_results\\all.segments.pct.error.csv",sep=""), header=TRUE, sep=",")    #Pull the (location of desired data table)
GageToSeg <- read.csv(file=paste0(container,'\\data\\SPLITSEG_Gage_To_Segment.csv'))

#Determining number of metrics
num.metrics <- length(Metrics) - 1
num.segs <- length(GageToSeg$river_segment)

#--------------------------------------------------------------------------------------------
#LOAD STATE GEOMETRY
#--------------------------------------------------------------------------------------------
STATES <- read.table(file=paste(hydro_tools,"GIS_LAYERS","STATES.tsv",sep="\\"), header=TRUE, sep="\t") #Load state geometries

#specify spatial extent for map  
extent <- data.frame(x = c(-84, -75), 
                     y = c(35, 41))  


bb=readWKT(paste0("POLYGON((",extent$x[1]," ",extent$y[1],",",extent$x[2]," ",extent$y[1],",",extent$x[2]," ",extent$y[2],",",extent$x[1]," ",extent$y[2],",",extent$x[1]," ",extent$y[1],"))",sep=""))
bbProjected <- SpatialPolygonsDataFrame(bb,data.frame("id"), match.ID = FALSE)
bbProjected@data$id <- rownames(bbProjected@data)
bbPoints <- fortify(bbProjected, region = "id")
bbDF <- merge(bbPoints, bbProjected@data, by = "id")

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

# This eliminates the gage number, river segment ID, or X value from contributing to the count
for (i in 1:num.segs) {
  RivSeg <- as.character(GageToSeg$river_segment[i])
  RivSegGage <- as.character(paste0(0,GageToSeg$gage_number[i]))
  
  namer <- paste0("watershedDF", i)
  c.namer <- paste0("c.watershedDF", i)
  
  # GETTING MODEL DATA FROM VA HYDRO
  hydrocode = paste("vahydrosw_wshed_",RivSeg,sep="");
  ftype = 'vahydro'; # nhd_huc8, nhd_huc10, vahydro
  inputs <- list (
    hydrocode = hydrocode,
    bundle = 'watershed',
    ftype = 'vahydro'
  )
  odata <- getFeature(inputs, token, site, feature);
  geom <- odata$geom
  
  # CLIP WATERSHED GEOMETRY TO BOUNDING BOX
  watershed_geom <- readWKT(geom)
  watershed_geom_clip <- gIntersection(bb, watershed_geom)
  if (is.null(watershed_geom_clip)) {
    watershed_geom_clip = watershed_geom
  }
  wsdataProjected <- SpatialPolygonsDataFrame(watershed_geom_clip,data.frame("id"), match.ID = FALSE)
  wsdataProjected@data$id <- rownames(wsdataProjected@data)
  watershedPoints <- fortify(wsdataProjected, region = "id")
  watershedDF <- merge(watershedPoints, wsdataProjected@data, by = "id")
  assign(namer, watershedDF)
  
  # GETTING FULL DRAINAGE AREA GEOGRAPHIC DATA FROM VA HYDRO
  hydrocode = paste("usgs_ws_",RivSegGage,sep="");
  ftype = 'vahydro'; # nhd_huc8, nhd_huc10, vahydro
  inputs <- list (
    hydrocode = hydrocode,
    bundle = 'watershed',
    ftype = 'usgs_full_drainage'
  )
  #property dataframe returned
  feature = FALSE;
  odata <- getFeature(inputs, token, site, feature);
  
  if (odata !=FALSE) {
    geomcontrib <- odata$geom;
    
    # CLIP WATERSHED GEOMETRY TO BOUNDING BOX
    watershed_geom <- readWKT(geomcontrib)
    watershed_geom_clip <- gIntersection(bb, watershed_geom)
    if (is.null(watershed_geom_clip)) {
      watershed_geom_clip = watershed_geom
    }
    wsdataProjected <- SpatialPolygonsDataFrame(watershed_geom_clip,data.frame("id"), match.ID = FALSE)
    wsdataProjected@data$id <- rownames(wsdataProjected@data)
    watershedPoints <- fortify(wsdataProjected, region = "id")
    watershedDF <- merge(watershedPoints, wsdataProjected@data, by = "id")
    assign(c.namer, watershedDF)
  }
}

# Initiating counter
ctr <- 1

# Determining metric column names
metric.names <- as.character(colnames(Metrics))
dir.create(paste0(container,"\\spatial_analysis\\user's_results\\error_maps"), showWarnings = FALSE);
cols <- c('firebrick4', 'firebrick3', 'firebrick1', 'white', 'chartreuse1', 'chartreuse3', 'chartreuse4')

for (ctr in 1:num.metrics) {
DesiredMetric<- metric.names[ctr+1]

Metrics[,paste0("DesiredMetric")] <- Metrics[DesiredMetric]

# GRAPHING -----

map <- ggplot(data = VADF, aes(x=long, y=lat, group = group))+
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
  geom_polygon(data = DCDF, color="gray46", fill = "gray", lwd=0.5)
for (i in 1:num.segs) {
  c.namer <- paste0('c.watershedDF', i)
  full.exist <- exists(c.namer)
  if (Metrics$DesiredMetric[i] < -50) {
    if (full.exist == TRUE) {
      map <- map + geom_polygon(data = eval(parse(text = c.namer)), color='gray75', aes(fill='a'), lwd = 0.1, alpha = 0.2)
    }
  } else if (Metrics$DesiredMetric[i] < -25) {
    if (full.exist == TRUE) {
      map <- map + geom_polygon(data = eval(parse(text = c.namer)), color='gray75', aes(fill='b'), lwd = 0.1, alpha = 0.2)
    }    
  } else if (Metrics$DesiredMetric[i] < -5) {
    if (full.exist == TRUE) {
      map <- map + geom_polygon(data = eval(parse(text = c.namer)), color='gray75', aes(fill='c'), lwd = 0.1, alpha = 0.2)
    }    
  } else if (Metrics$DesiredMetric[i] < 5) {
    if (full.exist == TRUE) {
      map <- map + geom_polygon(data = eval(parse(text = c.namer)), color='gray75', aes(fill='d'), lwd = 0.1, alpha = 0.2)
    }    
  } else if (Metrics$DesiredMetric[i] < 25) {
    if (full.exist == TRUE) {
      map <- map + geom_polygon(data = eval(parse(text = c.namer)), color='gray75', aes(fill='e'), lwd = 0.1, alpha = 0.2)
    }    
  } else if (Metrics$DesiredMetric[i] < 50) {
    if (full.exist == TRUE) {
      map <- map + geom_polygon(data = eval(parse(text = c.namer)), color='gray75', aes(fill='f'), lwd = 0.1, alpha = 0.2)
    }    
  } else {
    if (full.exist == TRUE) {
      map <- map + geom_polygon(data = eval(parse(text = c.namer)), color='gray75', aes(fill='g'), lwd = 0.1, alpha = 0.2)
    }    
  }
}
for (i in 1:num.segs) {
  namer <- paste0('watershedDF', i)
  if (Metrics$DesiredMetric[i] < -50) {
    map <- map + geom_polygon(data = eval(parse(text = namer)), color='black', aes(fill='a'), lwd = 0.1)
  } else if (Metrics$DesiredMetric[i] < -25) {
    map <- map + geom_polygon(data = eval(parse(text = namer)), color='black', aes(fill='b'), lwd = 0.1)
  } else if (Metrics$DesiredMetric[i] < -5) {
    map <- map + geom_polygon(data = eval(parse(text = namer)), color='black', aes(fill='c'), lwd = 0.1)
  } else if (Metrics$DesiredMetric[i] < 5) {
    map <- map + geom_polygon(data = eval(parse(text = namer)), color='black', aes(fill='d'), lwd = 0.1)
  } else if (Metrics$DesiredMetric[i] < 25) {
    map <- map + geom_polygon(data = eval(parse(text = namer)), color='black', aes(fill='e'), lwd = 0.1)
  } else if (Metrics$DesiredMetric[i] < 50) {
    map <- map + geom_polygon(data = eval(parse(text = namer)), color='black', aes(fill='f'), lwd = 0.1)
  } else {
    map <- map + geom_polygon(data = eval(parse(text = namer)), color='black', aes(fill='g'), lwd = 0.1)
  }
}

#Add legend, add title, ggsave
map <- map + ggtitle(DesiredMetric)
map <- map + scale_fill_manual(breaks = c('a', 'b', 'c', 'd', 'e', 'f', 'g'), limits=c('a', 'b', 'c', 'd', 'e', 'f', 'g'), labels=c("Less than -50%", '-50% to -25%', '-25% to -5%', '-5% to 5%', "5% to 25%", "25% to 50%", "Greater than 50%"), values=cols, name = "Percent Error")
#ADD NORTH ARROW AND SCALE BAR
map<-map+north(bbDF, location = 'topleft', symbol = 12, scale=0.1)+
  scalebar(bbDF, location = 'bottomleft', dist = 100, dd2km = TRUE, model = 'WGS84',st.bottom=FALSE, st.size = 3.5,
           anchor = c(
             x = (((extent$x[2] - extent$x[1])/2)+extent$x[1])-1.1,
             y = extent$y[1]+(extent$y[1])*0.001
           ))+
  scale_x_continuous(limits = c(extent$x))+
  scale_y_continuous(limits = c(extent$y))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        #axis.text =element_text(size=rel(2)),    #Uncomment to display lat/long on plot
        #axis.title = element_text(size=rel(2)),  #Uncomment to display lat/long on plot
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size=rel(1)),
        legend.title = element_text(size= rel(1.2)))

# Incrementing counter
ctr <- ctr + 1
ggsave(filename = paste0(DesiredMetric, ".jpeg"), device = "jpeg", path = paste0(container,"\\spatial_analysis\\user's_results\\error_maps"), width = 7, height = 4.5)
}

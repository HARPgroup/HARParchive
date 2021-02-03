rm(list = ls())  #clear variables
library(rgeos) #used for geospatial processing 
library(ggmap) #required for foritfy()
library(ggsn) #used for adding scale bar and north arrow to map
library(sp) #required for SpatialPolygonsDataFrame()
  
#----------------------------------------------
site <- "http://deq1.bse.vt.edu/d.bet"    #Specify the site of interest, either d.bet OR d.dh
hydro_tools <- 'C:\\Users\\HaileyMae\\Documents\\GitHub\\hydro-tools\\' #location of hydro-tools repo
save_directory <- 'C:\\Users\\HaileyMae\\Documents\\GitHub\\plots\\' #Location to output images

#----------------------------------------------

#Generate REST token              
rest_uname = FALSE
rest_pw = FALSE
source(paste(hydro_tools,"auth.private", sep = "\\")); #load rest username and password, contained in auth.private file
source(paste(hydro_tools,"VAHydro-2.0","rest_functions.R", sep = "\\")) #load REST functions
STATES <- read.table(file=paste(hydro_tools,"GIS_LAYERS","STATES.tsv",sep="\\"), header=TRUE, sep="\t") #Load state geometries
token <- rest_token(site, token, rest_uname, rest_pw)

#--------------------------------------------------------------------------------------------
# Retrieve Riversegment Feature From VAHydro 
#-------------------------------------------------------------------------------------------- 
  inputs <- list (
    bundle = 'watershed',
    ftype = 'vahydro',
    hydrocode = 'vahydrosw_wshed_NR3_8690_8500'
  )

  dataframe <- getFeature(inputs, token, site)
  #print(dataframe)
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
  postProperty(inputs = local_da_prop, base_url = site, prop = prop)
  
  geom <- dataframe$geom
#--------------------------------------------------------------------------------------------
# Retrieve USGS Gage Feature From VAHydro 
#-------------------------------------------------------------------------------------------- 
  gage.inputs <- list (
    bundle = 'usgsgage',
    hydrocode = 'usgs_03168000'
  )
  
gage.df <- getFeature(gage.inputs, token, site)
#-------------------------------------------------------------------------------------------- 
#--------------------------------------------------------------------------------------------
  
#specify spatial extent for map  
extent <- data.frame(x = c(-84, -75), 
                     y = c(35, 41))  
  

bb=readWKT(paste0("POLYGON((",extent$x[1]," ",extent$y[1],",",extent$x[2]," ",extent$y[1],",",extent$x[2]," ",extent$y[2],",",extent$x[1]," ",extent$y[2],",",extent$x[1]," ",extent$y[1],"))",sep=""))
bbProjected <- SpatialPolygonsDataFrame(bb,data.frame("id"), match.ID = FALSE)
bbProjected@data$id <- rownames(bbProjected@data)
bbPoints <- fortify(bbProjected, region = "id")
bbDF <- merge(bbPoints, bbProjected@data, by = "id")

#--------------------------------------------------------------------------------------------

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

#--------------------------------------------------------------------------------------------
# Geoprocess gage geometry
#--------------------------------------------------------------------------------------------
split_1 <- read.table(text = as.character(gage.df$geom), sep = "(", colClasses = "character")
split_2 <- read.table(text = split_1$V2, sep = ")", colClasses = "character")
split_3 <- read.table(text = split_2$V1, sep = " ", colClasses = "character")
GAGEDF <- data.frame(x=as.numeric(split_3$V1),y=as.numeric(split_3$V2),X.id.="id",id="1")

#--------------------------------------------------------------------------------------------
#LOAD STATE GEOMETRY
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
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------

map <- ggplot(data = VADF, aes(x=long, y=lat, group = group))+
  geom_polygon(data = VADF, color="gray46", fill = "gray")+
  geom_polygon(data = TNDF, color="gray46", fill = NA, lwd=0.5)+
  geom_polygon(data = NCDF, color="gray46", fill = NA, lwd=0.5)+
  geom_polygon(data = KYDF, color="gray46", fill = NA, lwd=0.5)+
  geom_polygon(data = WVDF, color="gray46", fill = NA, lwd=0.5)+
  geom_polygon(data = MDDF, color="gray46", fill = NA, lwd=0.5)+
  geom_polygon(data = DEDF, color="gray46", fill = NA, lwd=0.5)+
  geom_polygon(data = PADF, color="gray46", fill = NA, lwd=0.5)+
  geom_polygon(data = NJDF, color="gray46", fill = NA, lwd=0.5)+
  geom_polygon(data = OHDF, color="gray46", fill = NA, lwd=0.5)+

  geom_polygon(data = watershedDF, color="khaki4", fill = "green",alpha = 0.25,lwd=0.5)+
  geom_polygon(data = bbDF, color="black", fill = NA,lwd=0.5)+
  
  geom_point(aes(x = x, y = y, group = id), data = GAGEDF, fill="red", color="black", size = 3, shape=24)+
  
  #ADD NORTH ARROW AND SCALE BAR
  north(bbDF, location = 'topleft', symbol = 12, scale=0.1)+
  scalebar(bbDF, dist = 100, dd2km = TRUE, model = 'WGS84',st.bottom=FALSE)+
  
  scale_x_continuous(limits = c(extent$x))+
  scale_y_continuous(limits = c(extent$y))+
  
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

filename <- paste("Test.png", sep="")
ggsave(file=filename, path = save_directory, width=12, height=9.5)

#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------

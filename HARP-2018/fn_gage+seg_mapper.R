fn_gage_and_seg_mapper <- function(RivSeg, site, hydro_tools, token) {

#Libraries---------
library(rgeos) #used for geospatial processing 
library(ggmap) #required for foritfy()
library(ggsn) #used for adding scale bar and north arrow to map
library(sp) #required for SpatialPolygonsDataFrame()
library(rlist) #use for grouping map images from ggplot
  
source(paste(hydro_tools, "HARP-2018/DEQ_Model_ONLY_v1.0/code/fn_ALL.upstream.R", sep = "/"));
source(paste(hydro_tools, "HARP-2018/DEQ_Model_ONLY_v1.0/code/fn_upstream.R", sep = "/"));
  
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
  
AllSegList <- c('OR5_7980_8200', 'OR2_8020_8130', 'OR2_8070_8120', 'OR4_8120_7890',
                  'OR2_8130_7900', 'OR5_8200_8370', 'OR4_8271_8120', 'TU3_8480_8680',
                  'TU1_8570_8680', 'TU3_8650_8800', 'TU4_8680_8810', 'TU2_8790_9070',
                  'TU4_8800_9290', 'TU4_8810_9000', 'BS4_8540_8441', 'BS3_8580_8440',
                  'BS2_8590_8440', 'BS1_8730_8540', 'MN2_8250_8190', 'MN4_8260_8400',
                  'MN0_8300_0001', 'MN4_8400_8380', 'MN4_8510_8380', 'MN2_8530_8510',
                  'NR6_7820_7960', 'NR1_7880_8050', 'BS3_8350_8330', 'BS4_8440_8441',
                  'MN3_7540_7680', 'MN1_7590_7860', 'MN1_7620_7710', 'MN3_7680_7860',
                  'MN4_7710_8161', 'MN2_7720_7830', 'MN1_7730_8160', 'MN3_7770_7930',
                  'MN4_7810_8080', 'MN2_7830_7950', 'MN3_7860_8080', 'MN3_7930_8010',
                  'MN4_7950_7710', 'MN1_7990_8100', 'MN3_8010_7950', 'MN4_8080_8110',
                  'MN2_8100_8190', 'MN5_8161_8160', 'MN3_8190_8260', 'MN5_8230_8161',
                  'NR6_7960_8050', 'NR1_8030_8051', 'NR6_8050_8051', 'NR6_8051_8000',
                  'NR6_8170_7960', 'NR6_8180_8051', 'NR2_8210_8180', 'NR3_8290_8170',
                  'NR3_8420_8430', 'NR3_8430_7820', 'NR6_8640_8500', 'NR3_8690_8500',
                  'NR5_8700_8640', 'NR3_8740_8500', 'NR5_8760_8640', 'NR1_8820_8760',
                  'NR5_8870_8760', 'NR1_8960_8870', 'NR1_9030_9080', 'NR5_9050_8870',
                  'NR5_9080_9050', 'NR4_9130_9080', 'NR1_9150_9050', 'NR3_9170_9130',
                  'NR3_9190_9170', 'NR3_9240_9130', 'NR2_9250_9170', 'NR3_9310_9240',
                  'OD3_8340_8520', 'OD3_8520_8621', 'OD2_8560_8630', 'OD6_8621_8470',
                  'OD3_8630_8720', 'OD6_8660_8621', 'OD2_8670_8890', 'OD3_8710_8470',
                  'OD3_8720_8900', 'OD5_8770_8780', 'OD5_8780_8660', 'OD2_8830_8710',
                  'OD2_8840_9020', 'OD3_8850_8931', 'OD5_8890_8770', 'OD5_8900_8770',
                  'OD1_8910_8930', 'OD2_8920_8830', 'OD3_8930_8931', 'OD3_8931_9140',
                  'OD5_8940_8780', 'OD4_8990_8900', 'OD3_9020_9110', 'OD4_9110_9140',
                  'OD4_9140_8990', 'OD1_9270_9110', 'OR2_7610_7780', 'OR2_7650_8070',
                  'OR2_7670_7840', 'OR1_7700_7980', 'OR3_7740_8271', 'OR2_7780_7890',
                  'OR2_7840_7970', 'OR5_7890_7970', 'OR2_7900_7740', 'OR5_7910_8410',
                  'OR5_7970_8200', 'OR1_8280_8020', 'OR1_8320_8271', 'OR5_8370_8410',
                  'OR5_8410_8470', 'OR2_8450_8490', 'OR2_8460_8271', 'OR7_8470_8490',
                  'TU2_8860_9000', 'TU3_8880_9230', 'TU2_8950_9040', 'TU2_8970_9280',
                  'TU5_9000_9280', 'TU1_9010_9290', 'TU3_9040_9180', 'TU3_9060_9230',
                  'TU2_9070_9090', 'TU2_9100_9200', 'TU3_9180_9090', 'TU2_9200_9180',
                  'TU1_9220_9200', 'TU3_9230_9260', 'NR2_8600_8700', 'NR6_8500_7820')

# Splitting the River Segment string into each segment name
RivSegStr <- strsplit(RivSeg, "\\+")
RivSegStr <- RivSegStr[[1]]
num.segs <- length(RivSegStr)

# Getting all upstream segments for each of the linked segs, combining
# to form a vector of all upstream segments.
AllUpstreamSegs <- vector()
for (i in 1:num.segs) {
  RivSeg <- RivSegStr[i]
  UpstreamSegs <- fn_ALL.upstream(RivSeg, AllSegList)
  AllUpstreamSegs <- c(AllUpstreamSegs, UpstreamSegs)
}
eliminate <- which(AllUpstreamSegs=="NA")
if (is.empty(eliminate) == FALSE) {
  AllUpstreamSegs <- AllUpstreamSegs[-eliminate]
}
AllUpstreamSegs <- unique(AllUpstreamSegs)
num.upstream <- length(AllUpstreamSegs)

STATES <- read.table(file=paste(hydro_tools,"GIS_LAYERS","STATES.tsv",sep="\\"), header=TRUE, sep="\t") #Load state geometries

# first specify bounding box / extent of map: -----------------------------
extent <- data.frame(x = c(-84, -75), 
                     y = c(35, 41))  

bb=readWKT(paste0("POLYGON((",extent$x[1]," ",extent$y[1],",",extent$x[2]," ",extent$y[1],",",extent$x[2]," ",extent$y[2],",",extent$x[1]," ",extent$y[2],",",extent$x[1]," ",extent$y[1],"))",sep=""))
bbProjected <- SpatialPolygonsDataFrame(bb,data.frame("id"), match.ID = FALSE)
bbProjected@data$id <- rownames(bbProjected@data)
bbPoints <- fortify(bbProjected, region = "id")
bbDF <- merge(bbPoints, bbProjected@data, by = "id")


# specify states info: LOAD STATE GEOMETRY --------------------------------
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

# set up ggplot for states ---------------
statemap <- ggplot(data = VADF, aes(x=long, y=lat, group = group))+
  geom_polygon(data = VADF, color="gray46", fill = "gray")+
  geom_polygon(data = TNDF, color="gray46", fill = NA, lwd=0.5)+
  geom_polygon(data = NCDF, color="gray46", fill = NA, lwd=0.5)+
  geom_polygon(data = KYDF, color="gray46", fill = NA, lwd=0.5)+
  geom_polygon(data = WVDF, color="gray46", fill = NA, lwd=0.5)+
  geom_polygon(data = MDDF, color="gray46", fill = NA, lwd=0.5)+
  geom_polygon(data = DEDF, color="gray46", fill = NA, lwd=0.5)+
  geom_polygon(data = PADF, color="gray46", fill = NA, lwd=0.5)+
  geom_polygon(data = NJDF, color="gray46", fill = NA, lwd=0.5)+
  geom_polygon(data = OHDF, color="gray46", fill = NA, lwd=0.5)
if (num.upstream > 0) {
for (i in 1:num.upstream) {  
  RivSeg <- AllUpstreamSegs[i]
  namer <- paste0("upstream.watershedDF", i)
  
  # Retrieve Riversegment Feature From VAHydro  -----------------------------
  
  inputs <- list (
    bundle = 'watershed',
    ftype = 'vahydro',
    hydrocode = paste0('vahydrosw_wshed_', RivSeg)
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
  #postProperty(inputs = local_da_prop, base_url = site, prop = prop)
  
  geom <- dataframe$geom
  watershedDF <- getWatershedDF(geom)
  assign(namer, watershedDF)
}
}

for (i in 1:num.segs) {  
  RivSeg <- RivSegStr[i]
  namer <- paste0("seg.watershedDF", i)
  
  # Retrieve Riversegment Feature From VAHydro  -----------------------------
  
  inputs <- list (
    bundle = 'watershed',
    ftype = 'vahydro',
    hydrocode = paste0('vahydrosw_wshed_', RivSeg)
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
  #postProperty(inputs = local_da_prop, base_url = site, prop = prop)
  
  geom <- dataframe$geom
  watershedDF <- getWatershedDF(geom)
  assign(namer, watershedDF)
}

# Determine if Gage Exists for Segment (gage_linked?) ---------------------

modelinput <- list(
  varkey = "om_model_element",
  featureid = local_da_prop$featureid,
  entity_type = "dh_feature"
)
findgage <- getProperty(modelinput, site, findgage)

gagetrue <- list(
  varkey = "gage_weighted", 
  featureid = findgage$pid,
  entity_type = "dh_properties"
)
gagetrue <- getProperty(gagetrue, site, gagetrue)

# if gagetrue is not logical, analyze with gages, check for linked segments. 
if (is.logical(gagetrue)==FALSE){
  
  # Retrieve USGS Gage Feature From VAHydro  --------------------------------
  Gage <- as.character(gagetrue$propcode)
  gage.inputs <- list (
    bundle = 'usgsgage',
    hydrocode = paste0('usgs_', Gage)
  )
  
  gage.df <- getFeature(gage.inputs, token, site)
  
  
  # Set up gage geometry: 
  # Geoprocess gage geometry----------------------
  split_1 <- read.table(text = as.character(gage.df$geom), sep = "(", colClasses = "character")
  split_2 <- read.table(text = split_1$V2, sep = ")", colClasses = "character")
  split_3 <- read.table(text = split_2$V1, sep = " ", colClasses = "character")
  GAGEDF <- data.frame(x=as.numeric(split_3$V1),y=as.numeric(split_3$V2),X.id.="id",id="1")
}
    
    #--------------------------------------------------------------------------------------------
    #--------------------------------------------------------------------------------------------
    
    

map <- statemap
if (num.upstream > 0) {
for (i in 1:num.upstream) {
  namer <- paste0("upstream.watershedDF", i)
  map <- map +
    geom_polygon(data = eval(parse(text = namer)), color="gray35", fill = "lightgreen",alpha = 0.25,lwd=0.5)
}      
}
for (i in 1:num.segs) {
      namer <- paste0("seg.watershedDF", i)
      map <- map +
      geom_polygon(data = eval(parse(text = namer)), color="black", fill = "green3",alpha = 0.25,lwd=0.5)
}
      map <- map + geom_polygon(data = bbDF, color="black", fill = NA,lwd=0.5)+
      geom_point(aes(x = x, y = y, group = id), data = GAGEDF, fill="red", color="black", size = 1.8, shape=24)


#additions to map -------------
map <- map + 
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

inputs <- list (
  bundle = 'watershed',
  ftype = 'vahydro',
  hydrocode = paste0('vahydrosw_wshed_', RivSeg)
)
print(map)
return(map)
}
fn_gage_and_seg_mapper <- function(RivSeg, siteNo, site, hydro_tools, token) {

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
  
AllSegList <- c('EL0_4560_4562','EL0_4561_4562','EL0_4562_0001','EL2_4400_4590',
                'EL2_4590_0001','EL2_5110_5270','EL2_5270_0001','PM0_4640_4820',
                'PM1_3120_3400','PM1_3450_3400','PM1_3510_4000','PM1_3710_4040',
                'PM1_4000_4290','PM1_4250_4500','PM1_4430_4200','PM1_4500_4580',
                'PM2_2860_3040','PM2_3400_3340','PM2_4860_4670','PM3_3040_3340',
                'PM3_4660_4620','PM3_4670_4660','PM4_3340_3341','PM4_3341_4040',
                'PM4_4040_4410','PM7_4150_4290','PM7_4200_4410','PM7_4290_4200',
                'PM7_4410_4620','PM7_4580_4820','PM7_4620_4580','PM7_4820_0001',
                'PS0_6150_6160','PS0_6160_6161','PS1_4790_4830','PS1_4830_5080',
                'PS2_5550_5560','PS2_5560_5100','PS2_6420_6360','PS2_6490_6420',
                'PS2_6660_6490','PS2_6730_6660','PS3_5100_5080','PS3_5990_6161',
                'PS3_6161_6280','PS3_6280_6230','PS3_6460_6230','PS4_5080_4380',
                'PS4_5840_5240','PS4_6230_6360','PS4_6360_5840','PS5_4370_4150',
                'PS5_4380_4370','PS5_5200_4380','PS5_5240_5200','PU0_3000_3090',
                'PU0_3601_3602','PU0_3611_3530','PU0_3751_3752','PU0_3871_3690',
                'PU0_5620_5380','PU0_6080_5620','PU1_3030_3440','PU1_3100_3690',
                'PU1_3170_3580','PU1_3580_3780','PU1_3850_4190','PU1_3940_3970',
                'PU1_4190_4300','PU1_4300_4440','PU1_4760_4450','PU1_4840_4760',
                'PU1_5380_5050','PU1_5520_5210','PU1_5820_5380','PU2_2790_3290',
                'PU2_2840_3080','PU2_3080_3640','PU2_3090_4050','PU2_3140_3680',
                'PU2_3180_3370','PU2_3370_4020','PU2_3630_3590','PU2_3770_3600',
                'PU2_3900_3750','PU2_4050_4180','PU2_4160_3930','PU2_4220_3900',
                'PU2_4340_3860','PU2_4360_4160','PU2_4720_4750','PU2_4730_4220',
                'PU2_4750_4450','PU2_5190_4310','PU2_5700_5210','PU2_6050_5190',
                'PU3_2510_3290','PU3_3290_3390','PU3_3390_3730','PU3_3680_3890',
                'PU3_3860_3610','PU3_4280_3860','PU3_4450_4440','PU3_5210_5050',
                'PU4_3780_3930','PU4_3890_3990','PU4_3970_3890','PU4_3990_3780',
                'PU4_4210_4170','PU4_4310_4210','PU4_4440_3970','PU4_5050_4310',
                'PU5_3930_4170','PU5_4170_4020','PU6_3440_3590','PU6_3530_3440',
                'PU6_3590_3640','PU6_3600_3602','PU6_3602_3730','PU6_3610_3530',
                'PU6_3640_3600','PU6_3690_3610','PU6_3730_3750','PU6_3750_3752',
                'PU6_3752_4080','PU6_3870_3690','PU6_4020_3870','PU6_4080_4180',
                'PU6_4180_4150','JA0_7291_7290','JA2_7290_0001','JA1_7600_7570',
                'JA1_7640_7280','JA2_7410_7470','JA2_7550_7280','JA2_7570_7480',
                'JA4_7280_7340','JA4_7340_7470','JA4_7470_7480','JA5_7480_0001',
                'JB3_6820_7053','JB3_7053_0001','PL1_4460_4780','PL1_4780_0001',
                'JL1_6560_6440','JL1_6760_6910','JL1_6770_6850','JL1_6910_6960',
                'JL1_6940_7200','JL1_7080_7190','JL1_7170_6800','JL1_7190_7250',
                'JL1_7200_7250','JL1_7530_7430','JL2_6240_6520','JL2_6440_6441',
                'JL2_6441_6520','JL2_6850_6890','JL2_7110_7120','JL2_7120_6970',
                'JL2_7240_7350','JL2_7250_7090','JL2_7350_7090','JL3_7020_7100',
                'JL3_7090_7150','JL4_6520_6710','JL4_6710_6740','JL6_6740_7100',
                'JL6_6890_6990','JL6_6960_6970','JL6_6970_6740','JL6_6990_6960',
                'JL6_7150_6890','JL6_7160_7440','JL6_7320_7150','JL6_7430_7320',
                'JL6_7440_7430','JL7_6800_7070','JL7_7030_6800','JL7_7070_0001',
                'JL7_7100_7030','JU1_6290_6590','JU1_6300_6650','JU1_6340_6650',
                'JU1_6590_6600','JU1_6880_7260','JU1_7560_7500','JU1_7630_7490',
                'JU1_7690_7490','JU1_7750_7560','JU2_6410_6640','JU2_6600_6810',
                'JU2_6810_6900','JU2_7140_7330','JU2_7180_7380','JU2_7360_7000',
                'JU2_7450_7360','JU3_6380_6900','JU3_6640_6790','JU3_6650_7300',
                'JU3_6790_7260','JU3_6900_6950','JU3_6950_7330','JU3_7400_7510',
                'JU3_7490_7400','JU4_7000_7300','JU4_7260_7380','JU4_7330_7000',
                'JU4_7380_7160','JU5_7300_7510','JU5_7420_7160','JU5_7500_7420',
                'JU5_7510_7500','PL0_5141_5140','PL1_5370_5470','PL2_4970_5250',
                'PL2_5140_5360','PL2_5470_5360','PL3_5250_0001','PL3_5360_5250',
                'PL0_5010_5130','PL1_5130_0001','PL0_5490_0001','PL0_5540_5490',
                'PL2_5300_5630','PL2_5630_0001','PL0_5730_5690','PL1_5690_0001',
                'PL0_5530_5710','PL0_5710_0001','RU2_5220_5640','RU2_5500_5610',
                'RU2_5810_5610','RU2_5940_6200','RU2_6090_6220','RU2_6200_6170',
                'RU2_6220_6170','RU3_5610_5640','RU3_6170_6040','RU4_5640_6030',
                'RU4_6040_6030','RU5_6030_0001','WM1_3660_3910','WM1_3910_0001',
                'WM0_3881_3880','WM1_3882_3880','WM3_3880_4060','WM3_4060_0001',
                'WU1_3240_3331','WU1_3330_0001','WU1_3331_3330','WU0_3021_3020',
                'WU1_3350_3490','WU1_3490_3480','WU2_3020_3320','WU2_3320_3480',
                'WU3_3480_3481','WU3_3481_0001','XU0_4090_4270','XU0_4091_4270',
                'XU0_4130_4070','XU2_4070_4330','XU2_4270_4650','XU2_4330_4480',
                'XU2_4480_4650','XU3_4650_0001','YM1_6370_6620','YM2_6120_6430',
                'YM3_6430_6620','YM4_6620_0001','YP1_6570_6680','YP1_6680_6670',
                'YP2_6390_6330','YP3_6330_6700','YP3_6470_6690','YP3_6670_6720',
                'YP3_6690_6720','YP3_6700_6670','YP4_6720_6750','YP4_6750_0001',
                'YP0_6840_0001','YP0_6860_6840')

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

# set up ggplot for states ---------------
statemap <- ggplot(data = VADF, aes(x=long, y=lat, group = group))+
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

# Create gage dataframe (gage_linked?) ---------------------
library(dataRetrieval)

gage <- readNWISsite(siteNo)
GAGEDF <- data.frame(x=as.numeric(gage$dec_long_va),y=as.numeric(gage$dec_lat_va),X.id.="id",id="1")
    
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
      geom_point(aes(x = x, y = y, group = id), data = GAGEDF, fill="red", color="black", size = 2.75, shape=24)


#additions to map -------------
map <- map + 
  #ADD NORTH ARROW AND SCALE BAR
  north(bbDF, location = 'topleft', symbol = 12, scale=0.1)+
        scalebar(data = bbDF, transform = TRUE, dist_unit = "km", location = 'bottomleft', dist = 100, model = 'WGS84',st.bottom=FALSE, st.size = 3.5,
                 anchor = c(
                   x = (((extent$x[2] - extent$x[1])/2)+extent$x[1])-1.1,
                   y = extent$y[1]+(extent$y[1])*0.001
                 ))+
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


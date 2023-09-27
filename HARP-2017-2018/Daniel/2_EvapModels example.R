#install.packages("__") for rgeos, rgdal, raster

library(rgeos)
library(rgdal)
library(raster)

my.filepath <- 'C:\\Users\\danielh7\\Desktop\\'     #REPLACE WITH FILEPATH OF EvapInputs.gdb

m<-c('01','02','03','04','05','06','07','08','09','10','11','12')
VA<-readOGR(paste(my.filepath,'EvapInputs.gdb',sep=""),layer="VA")
#readOGR: reads an OGR (OpenGIS Simple Features Reference Implementation) data source and layer into a suitable
  #spatial vector object -- only handles layers with conformable geometry features, not a mixture of pt/line/poly
#readOGR arguments: data source name, layer name
BB<-readOGR(paste(my.filepath,'EvapInputs.gdb',sep=""),layer="BoundingBox")
BB<-spTransform(BB,CRS="+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")
#spTransform: provides transformation between datum and confersion between projections from one unambiguously
  #specified coordinate reference system to another
#crs(VA)

#--------------------------------------------------------------------------------------------------
# Alternativley The bounding box can also be creted within R
# change the code below to coordinates of interest
#library(sp)
#x_min<--74
#x_max<--85
#y_min<-35
#y_max<-42
#coords = matrix(c(x_min,y_min,x_max,y_min,x_max,y_max,x_min,y_max,x_min,y_min),ncol = 2, byrow = TRUE)
#BB = Polygon(coords)
#BB = SpatialPolygons(list(Polygons(list(BB), ID = "a")), proj4string=CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"))
#--------------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------
# View Bounding Box and Virginia as plot image 
plot(BB,axes=T)
plot(VA,add=T)
#--------------------------------------------------------------------------------------------------


########Mean monthly raster creation using PRISM data (pre-downloaded)########
#You MUST delete output TIFF files before writing new ones!
#Can download ECHO as to get all monthly data for 1970 max temperature (can also call tmin, ppt, nad tmean). Works for 1895-1980
# yr<-as.character(seq(1955,1970))
# pathTMean<- paste(my.filepath,'PRISM\\TMean',sep="")
# pathTMax <- paste(my.filepath,'PRISM\\TMax',sep="")
# pathTMin <- paste(my.filepath,'PRISM\\TMin',sep="")
# for(i in 1:length(yr)){
#   temp<-tempfile()
#   download.file(paste0("http://services.nacse.org/prism/data/public/4km/tmean/",yr[i]),temp,mode='wb')
#   unzip(zipfile=temp,exdir=pathTMean)
# 
#   temp<-tempfile()
#   download.file(paste0("http://services.nacse.org/prism/data/public/4km/tmax/",yr[i]),temp,mode='wb')
#   unzip(temp,exdir=pathTMax)  
#   
#   temp<-tempfile()
#   download.file(paste0("http://services.nacse.org/prism/data/public/4km/tmin/",yr[i]),temp,mode='wb')
#   unzip(temp,exdir=pathTMin)  
# }

yr<-as.character(seq(1955,1970))
#as.character: returns a string of 1's and 0's or a character of features
pathTMean<- paste(my.filepath,'PRISM\\TMean',sep="")
pathTMax <- paste(my.filepath,'PRISM\\TMax',sep="")
pathTMin <- paste(my.filepath,'PRISM\\TMin',sep="")
for(i in 1:length(yr)){
  temp<-tempfile()
  download.file(paste0("http://services.nacse.org/prism/data/public/4km/tmean/",yr[i]),temp,mode='wb')
  unzip(zipfile=temp,exdir=pathTMean)
  
  temp<-tempfile()
  download.file(paste0("http://services.nacse.org/prism/data/public/4km/tmax/",yr[i]),temp,mode='wb')
  unzip(temp,exdir=pathTMax)  
  
  temp<-tempfile()
  download.file(paste0("http://services.nacse.org/prism/data/public/4km/tmin/",yr[i]),temp,mode='wb')
  unzip(temp,exdir=pathTMin)  
}

#For average temperature
setwd(paste(my.filepath,"PRISM\\TMean",sep=""))
print(paste("Mean Temperature:",sep=""))
dir.create('Monthly Averages')
files<-list.files()
files<-files[grep('bil.bil$',files)]
yr<-as.character(seq(1955,1970))
r<-character()
for (i in 1:length(yr)){
  case<-files[grep(yr[i],files)]
  r<-c(case,r)
}
for (j in 1:length(m)){
  print(paste("Month ",j," of ",length(m),sep=""))
  case<-grep(paste0(m[j],'_bil.bil$'),r)
  rstack<-stack()
  for (i in 1:length(case)){
    print(paste("case ",i," of ",length(case),sep=""))
    test<-raster(r[case[i]])
    test<-projectRaster(test,crs=proj4string(BB))
    testclip<-mask(test,BB)
    rstack<-addLayer(rstack,testclip)
  }
  assign(paste0('MeanTemp_',m[j]),mean(rstack))
  writeRaster(mean(rstack), filename=paste0('Monthly Averages/MeanTemp',m[j],'.tiff'))
}


#For maximum temperature
setwd(paste(my.filepath,"PRISM\\TMax",sep=""))
print(paste("Maximum Temperature:",sep=""))
dir.create('Monthly Averages')
files<-list.files()
files<-files[grep('bil.bil$',files)]
yr<-as.character(seq(1955,1970))
r<-character()
for (i in 1:length(yr)){
  case<-files[grep(yr[i],files)]
  r<-c(case,r)
}
for (j in 1:length(m)){
  print(paste("Month ",j," of ",length(m),sep=""))
  case<-grep(paste0(m[j],'_bil.bil$'),r)
  rstack<-stack()
  for (i in 1:length(case)){
    print(paste("case ",i," of ",length(case),sep=""))
    test<-raster(r[case[i]])
    test<-projectRaster(test,crs=proj4string(BB))
    testclip<-mask(test,BB)
    rstack<-addLayer(rstack,testclip)
  }
  assign(paste0('MaxTemp_',m[j]),mean(rstack))
  writeRaster(mean(rstack), filename=paste0('Monthly Averages/MaxTemp',m[j],'.tiff'))
}


#For minimum temperature
setwd(paste(my.filepath,"PRISM\\TMin",sep=""))
print(paste("Minimum Temperature:",sep=""))
dir.create('Monthly Averages')
files<-list.files()
files<-files[grep('bil.bil$',files)]
yr<-as.character(seq(1955,1970))
r<-character()
for (i in 1:length(yr)){
  case<-files[grep(yr[i],files)]
  r<-c(case,r)
}
for (j in 1:length(m)){
  print(paste("Month ",j," of ",length(m),sep=""))
  case<-grep(paste0(m[j],'_bil.bil$'),r)
  rstack<-stack()
  for (i in 1:length(case)){
    print(paste("case ",i," of ",length(case),sep=""))
    test<-raster(r[case[i]])
    test<-projectRaster(test,crs=proj4string(BB))
    testclip<-mask(test,BB)
    rstack<-addLayer(rstack,testclip)
  }
  assign(paste0('MinTemp_',m[j]),mean(rstack))
  writeRaster(mean(rstack), filename=paste0('Monthly Averages/MinTemp',m[j],'.tiff'))
}


########Solar radiation calculation########
WB<-readOGR(paste(my.filepath,'EvapInputs.gdb',sep=""),layer="Waterbodies")

#Project coordinates into NAD 1983 UTM zone 17N to caclulate area. Then, reproject back for consistency with other files
WB<-spTransform(WB,'+proj=utm +zone=17 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
WB@data$FID<-1:length(WB@data$COMID)
#Not sure about these yet
WB@data$AreaRcalc<-gArea(WB,byid=T)
WB<-spTransform(WB,CRS="+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")
WBCenter<-gCentroid(WB,byid=T,id=WB@data$FID)
#See http://www.fao.org/docrep/x0490e/x0490e07.htm#TopOfPage
diy<-seq(1,365)
distFsun<-1+0.033*cos(2*pi*diy/365)
sigma<-0.409*sin(2*pi*diy/365-1.39)
for (i in 1:length(WB@data$FID)){
  print(paste("Mean extraterrestrial radiation calc for WB ",i," of ",length(WB@data$FID),sep=""))
  lat<-as.numeric(WBCenter@coords[i,2])*pi/180
  sunsethour<-acos(-tan(sigma)*tan(lat))
  Ra<-(25*60*0.0820/pi)*distFsun*(sunsethour*sin(lat)*sin(sigma)+cos(lat)*cos(sigma)*sin(sunsethour))
  WB@data$JanRa[i]<-mean(Ra[1:31])
  WB@data$FebRa[i]<-mean(Ra[32:59])
  WB@data$MarRa[i]<-mean(Ra[60:90])
  WB@data$AprRa[i]<-mean(Ra[91:120])
  WB@data$MayRa[i]<-mean(Ra[121:151])
  WB@data$JunRa[i]<-mean(Ra[152:181])
  WB@data$JulRa[i]<-mean(Ra[182:212])
  WB@data$AugRa[i]<-mean(Ra[213:243])
  WB@data$SepRa[i]<-mean(Ra[244:273])
  WB@data$OctRa[i]<-mean(Ra[274:304])
  WB@data$NovRa[i]<-mean(Ra[305:334])
  WB@data$DecRa[i]<-mean(Ra[335:365])
}
########Evaporation Estimations########
print(paste("WB Evap Estimations",sep=""))
#Want to make sure the extracted values line up with correct waterbodies; test in GIS
WB@data$JanDiffT<-extract(MaxTemp_01,WBCenter)-extract(MinTemp_01,WBCenter)
WB@data$JanMeanT<-extract(MeanTemp_01,WBCenter)
WB@data$FebDiffT<-extract(MaxTemp_02,WBCenter)-extract(MinTemp_02,WBCenter)
WB@data$FebMeanT<-extract(MeanTemp_02,WBCenter)
WB@data$MarDiffT<-extract(MaxTemp_03,WBCenter)-extract(MinTemp_03,WBCenter)
WB@data$MarMeanT<-extract(MeanTemp_03,WBCenter)
WB@data$AprDiffT<-extract(MaxTemp_04,WBCenter)-extract(MinTemp_04,WBCenter)
WB@data$AprMeanT<-extract(MeanTemp_04,WBCenter)
WB@data$MayDiffT<-extract(MaxTemp_05,WBCenter)-extract(MinTemp_05,WBCenter)
WB@data$MayMeanT<-extract(MeanTemp_05,WBCenter)
WB@data$JunDiffT<-extract(MaxTemp_06,WBCenter)-extract(MinTemp_06,WBCenter)
WB@data$JunMeanT<-extract(MeanTemp_06,WBCenter)
WB@data$JulDiffT<-extract(MaxTemp_07,WBCenter)-extract(MinTemp_07,WBCenter)
WB@data$JulMeanT<-extract(MeanTemp_07,WBCenter)
WB@data$AugDiffT<-extract(MaxTemp_08,WBCenter)-extract(MinTemp_08,WBCenter)
WB@data$AugMeanT<-extract(MeanTemp_08,WBCenter)
WB@data$SepDiffT<-extract(MaxTemp_09,WBCenter)-extract(MinTemp_09,WBCenter)
WB@data$SepMeanT<-extract(MeanTemp_09,WBCenter)
WB@data$OctDiffT<-extract(MaxTemp_10,WBCenter)-extract(MinTemp_10,WBCenter)
WB@data$OctMeanT<-extract(MeanTemp_10,WBCenter)
WB@data$NovDiffT<-extract(MaxTemp_11,WBCenter)-extract(MinTemp_11,WBCenter)
WB@data$NovMeanT<-extract(MeanTemp_11,WBCenter)
WB@data$DecDiffT<-extract(MaxTemp_12,WBCenter)-extract(MinTemp_12,WBCenter)
WB@data$DecMeanT<-extract(MeanTemp_12,WBCenter)
#The below line may summarize by polygon, but it takes a very long time to run (~10 min for each extraction)
#extract(MinTemp_12,WB,fun=mean,border=F)

#Hargreaves equation may take the form 0.0023Ra*TD^0.5*(T+17.8). First coefficient may also be ~0.0135
#If Ra is in Mj/m^2/day, this can be converted to mm/day by dividing by latent heat and assuming density of 1000kg/m^3
#0.0023Ra*TD^0.5*(T+17.8)/lambda
#Output is in mm/day
print("Hargreaves equation used to estimate monthly evaporation at each waterbody:")
c1<-0.0023
c2<-17.8
c1*WB@data$JanRa[1]*sqrt(WB@data$JanDiffT[1])*(WB@data$JanMeanT[1]+c2)/(2.501-.002361*WB@data$JanMeanT[1])
WB@data$JanHarg<-c1*WB@data$JanRa*sqrt(WB@data$JanDiffT)*(WB@data$JanMeanT+c2)/(2.501-.002361*WB@data$JanMeanT)
WB@data$FebHarg<-c1*WB@data$FebRa*sqrt(WB@data$FebDiffT)*(WB@data$FebMeanT+c2)/(2.501-.002361*WB@data$FebMeanT)
WB@data$MarHarg<-c1*WB@data$MarRa*sqrt(WB@data$MarDiffT)*(WB@data$MarMeanT+c2)/(2.501-.002361*WB@data$MarMeanT)
WB@data$AprHarg<-c1*WB@data$AprRa*sqrt(WB@data$AprDiffT)*(WB@data$AprMeanT+c2)/(2.501-.002361*WB@data$AprMeanT)
WB@data$MayHarg<-c1*WB@data$MayRa*sqrt(WB@data$MayDiffT)*(WB@data$MayMeanT+c2)/(2.501-.002361*WB@data$MayMeanT)
WB@data$JunHarg<-c1*WB@data$JunRa*sqrt(WB@data$JunDiffT)*(WB@data$JunMeanT+c2)/(2.501-.002361*WB@data$JunMeanT)
WB@data$JulHarg<-c1*WB@data$JulRa*sqrt(WB@data$JulDiffT)*(WB@data$JulMeanT+c2)/(2.501-.002361*WB@data$JulMeanT)
WB@data$AugHarg<-c1*WB@data$AugRa*sqrt(WB@data$AugDiffT)*(WB@data$AugMeanT+c2)/(2.501-.002361*WB@data$AugMeanT)
WB@data$SepHarg<-c1*WB@data$SepRa*sqrt(WB@data$SepDiffT)*(WB@data$SepMeanT+c2)/(2.501-.002361*WB@data$SepMeanT)
WB@data$OctHarg<-c1*WB@data$OctRa*sqrt(WB@data$OctDiffT)*(WB@data$OctMeanT+c2)/(2.501-.002361*WB@data$OctMeanT)
WB@data$NovHarg<-c1*WB@data$NovRa*sqrt(WB@data$NovDiffT)*(WB@data$NovMeanT+c2)/(2.501-.002361*WB@data$NovMeanT)
WB@data$DecHarg<-c1*WB@data$DecRa*sqrt(WB@data$DecDiffT)*(WB@data$DecMeanT+c2)/(2.501-.002361*WB@data$DecMeanT)

#Calculate average monthly daylight hours for use in the Thornthwaite Method
print("Calculate average monthly daylight hours for use in the Thornthwaite Method")
for (i in 1:length(WB@data$FID)){
  print(paste("Avg mo daylight hrs calc for WB ",i," of ",length(WB@data$FID),sep=""))
  lat<-as.numeric(WBCenter@coords[i,2])*pi/180
  sunsethour<-acos(-tan(sigma)*tan(lat))
  Ld<-24*sunsethour/pi
  Ra<-(25*60*0.0820/pi)*distFsun*(sunsethour*sin(lat)*sin(sigma)+cos(lat)*cos(sigma)*sin(sunsethour))
  WB@data$JanLd[i]<-mean(Ld[1:31])
  WB@data$FebLd[i]<-mean(Ld[32:59])
  WB@data$MarLd[i]<-mean(Ld[60:90])
  WB@data$AprLd[i]<-mean(Ld[91:120])
  WB@data$MayLd[i]<-mean(Ld[121:151])
  WB@data$JunLd[i]<-mean(Ld[152:181])
  WB@data$JulLd[i]<-mean(Ld[182:212])
  WB@data$AugLd[i]<-mean(Ld[213:243])
  WB@data$SepLd[i]<-mean(Ld[244:273])
  WB@data$OctLd[i]<-mean(Ld[274:304])
  WB@data$NovLd[i]<-mean(Ld[305:334])
  WB@data$DecLd[i]<-mean(Ld[335:365])
}

ThornI<-function(meanT){
  if(meanT<0){
    out<-0
  }else{
    out<-(meanT/5)^1.514
  }
  return(out)
}

#Calculate Thornthwaite temperature index and coefficient
for (i in 1:length(WB@data$FID)){
  print(paste("Thornthwaite temperature index calc for WB ",i," of ",length(WB@data$FID),sep=""))
  Jan<-ThornI(WB@data$JanMeanT[i])
  Feb<-ThornI(WB@data$FebMeanT[i])
  Mar<-ThornI(WB@data$MarMeanT[i])
  Apr<-ThornI(WB@data$AprMeanT[i])
  May<-ThornI(WB@data$MayMeanT[i])
  Jun<-ThornI(WB@data$JunMeanT[i])
  Jul<-ThornI(WB@data$JulMeanT[i])
  Aug<-ThornI(WB@data$AugMeanT[i])
  Sep<-ThornI(WB@data$SepMeanT[i])
  Oct<-ThornI(WB@data$OctMeanT[i])
  Nov<-ThornI(WB@data$NovMeanT[i])
  Dec<-ThornI(WB@data$DecMeanT[i])
  WB@data$I[i]<-Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec
}
WB@data$a<-(0.000000675*WB@data$I^3)-(0.0000771*WB@data$I^2)+(0.01791*WB@data$I)+0.49239

#Calculate and set NAs to zero, as temperatures below 0 have 0 ET
c<-16
WB@data$JanThorn<-(c*WB@data$JanLd*(10*WB@data$JanMeanT/WB@data$I)^WB@data$a)/31/12
WB@data$JanThorn[is.na(WB@data$JanThorn)]<-0
WB@data$FebThorn<-(c*WB@data$FebLd*(10*WB@data$FebMeanT/WB@data$I)^WB@data$a)/28/12
WB@data$FebThorn[is.na(WB@data$FebThorn)]<-0
WB@data$MarThorn<-(c*WB@data$MarLd*(10*WB@data$MarMeanT/WB@data$I)^WB@data$a)/31/12
WB@data$MarThorn[is.na(WB@data$MarThorn)]<-0
WB@data$AprThorn<-(c*WB@data$AprLd*(10*WB@data$AprMeanT/WB@data$I)^WB@data$a)/30/12
WB@data$AprThorn[is.na(WB@data$AprThorn)]<-0
WB@data$MayThorn<-(c*WB@data$MayLd*(10*WB@data$MayMeanT/WB@data$I)^WB@data$a)/31/12
WB@data$MayThorn[is.na(WB@data$MayThorn)]<-0
WB@data$JunThorn<-(c*WB@data$JunLd*(10*WB@data$JunMeanT/WB@data$I)^WB@data$a)/30/12
WB@data$JunThorn[is.na(WB@data$JunThorn)]<-0
WB@data$JulThorn<-(c*WB@data$JulLd*(10*WB@data$JulMeanT/WB@data$I)^WB@data$a)/31/12
WB@data$JulThorn[is.na(WB@data$JulThorn)]<-0
WB@data$AugThorn<-(c*WB@data$AugLd*(10*WB@data$AugMeanT/WB@data$I)^WB@data$a)/31/12
WB@data$AugThorn[is.na(WB@data$AugThorn)]<-0
WB@data$SepThorn<-(c*WB@data$SepLd*(10*WB@data$SepMeanT/WB@data$I)^WB@data$a)/30/12
WB@data$SepThorn[is.na(WB@data$SepThorn)]<-0
WB@data$OctThorn<-(c*WB@data$OctLd*(10*WB@data$OctMeanT/WB@data$I)^WB@data$a)/31/12
WB@data$OctThorn[is.na(WB@data$OctThorn)]<-0
WB@data$NovThorn<-(c*WB@data$NovLd*(10*WB@data$NovMeanT/WB@data$I)^WB@data$a)/30/12
WB@data$NovThorn[is.na(WB@data$NovThorn)]<-0
WB@data$DecThorn<-(c*WB@data$DecLd*(10*WB@data$DecMeanT/WB@data$I)^WB@data$a)/31/12
WB@data$DecThorn[is.na(WB@data$DecThorn)]<-0

#Multiply by area, convert to m^3/day then convert to million gallons per year
evapH<-numeric()
evapT<-numeric()
for (i in 1:length(WB@data$FID)){
  print(paste("Evap BGY estimation for WB ",i," of ",length(WB@data$FID),sep=""))
  evapH[i]<-sum(WB@data[i,grep('Harg',colnames(WB@data))]*c(31,28,31,30,31,30,31,31,30,31,30,31))
  evapT[i]<-sum(WB@data[i,grep('Thorn',colnames(WB@data))]*c(31,28,31,30,31,30,31,31,30,31,30,31))
}
WB@data$AnnualHrg_mmpyr<-evapH
WB@data$AnnualHrg_BGY<-WB@data$AreaRcalc*(evapH/1000)*264.1721/1000000000
WB@data$AnnualThrn_mmpyr<-evapT
WB@data$AnnualThrn_BGY<-WB@data$AreaRcalc*(evapT/1000)*264.1721/1000000000

#===================================================================================
# PLOTS
#===================================================================================
# MEAN TEMP
JanMat <- matrix(MeanTemp_01)
FebMat <- matrix(MeanTemp_02)
MarMat <- matrix(MeanTemp_03)
AprMat <- matrix(MeanTemp_04)
MayMat <- matrix(MeanTemp_05)
JunMat <- matrix(MeanTemp_06)
JulMat <- matrix(MeanTemp_07)
AugMat <- matrix(MeanTemp_08)
SepMat <- matrix(MeanTemp_09)
OctMat <- matrix(MeanTemp_10)
NovMat <- matrix(MeanTemp_11)
DecMat <- matrix(MeanTemp_12)
MeanMin <- min(minValue(MeanTemp_01), minValue(MeanTemp_02), minValue(MeanTemp_03), minValue(MeanTemp_04), minValue(MeanTemp_05), minValue(MeanTemp_06), minValue(MeanTemp_07), minValue(MeanTemp_08), minValue(MeanTemp_09), minValue(MeanTemp_10), minValue(MeanTemp_11), minValue(MeanTemp_12))
MeanMax <- max(maxValue(MeanTemp_01), maxValue(MeanTemp_02), maxValue(MeanTemp_03), maxValue(MeanTemp_04), maxValue(MeanTemp_05), maxValue(MeanTemp_06), maxValue(MeanTemp_07), maxValue(MeanTemp_08), maxValue(MeanTemp_09), maxValue(MeanTemp_10), maxValue(MeanTemp_11), maxValue(MeanTemp_12))
r.range <- c(MeanMin, MeanMax)
# par(mfrow = c(1, 2))
colfunc <- colorRampPalette(c("blue", "red"))
colfunc(15)
plot(crop(MeanTemp_12,BB), col=(colfunc(15)), main= expression('December Mean Temperature (1955-1970) ('*degree*'C)'),legend=TRUE,axes=TRUE, zlim=r.range, xlab='Longitude',ylab='Latitude',cex.lab=1.5,cex.main=1.5,cex.axis=1.5)
plot(VA,add=T,lwd=2)
# plot(crop(MeanTemp_08,BB), col=(colfunc(15)), main= expression('August'),axes=TRUE,legend=F, zlim = r.range)
# plot(VA,add=T,lwd=2)
# plot(crop(MeanTemp_08,BB), col=(colfunc(15)), legend.only=TRUE, axis.args = list(at=seq(r.range[1], r.range[2])), zlim=r.range, legend.scale(c(0,1)))
# 

# MAX TEMP
JanMat <- matrix(MaxTemp_01)
FebMat <- matrix(MaxTemp_02)
MarMat <- matrix(MaxTemp_03)
AprMat <- matrix(MaxTemp_04)
MayMat <- matrix(MaxTemp_05)
JunMat <- matrix(MaxTemp_06)
JulMat <- matrix(MaxTemp_07)
AugMat <- matrix(MaxTemp_08)
SepMat <- matrix(MaxTemp_09)
OctMat <- matrix(MaxTemp_10)
NovMat <- matrix(MaxTemp_11)
DecMat <- matrix(MaxTemp_12)
MaxMin <- min(minValue(MaxTemp_01), minValue(MaxTemp_02), minValue(MaxTemp_03), minValue(MaxTemp_04), minValue(MaxTemp_05), minValue(MaxTemp_06), minValue(MaxTemp_07), minValue(MaxTemp_08), minValue(MaxTemp_09), minValue(MaxTemp_10), minValue(MaxTemp_11), minValue(MaxTemp_12))
MaxMax <- max(maxValue(MaxTemp_01), maxValue(MaxTemp_02), maxValue(MaxTemp_03), maxValue(MaxTemp_04), maxValue(MaxTemp_05), maxValue(MaxTemp_06), maxValue(MaxTemp_07), maxValue(MaxTemp_08), maxValue(MaxTemp_09), maxValue(MaxTemp_10), maxValue(MaxTemp_11), maxValue(MaxTemp_12))
r.range <- c(MaxMin, MaxMax)
plot(crop(MaxTemp_12,BB), col=(colfunc(15)), main = expression('December Max Temperature (1955-1970) ('*degree*'C)'), legend = TRUE, axes = TRUE, zlim = r.range, xlab = 'Longitude', ylab = 'Latitude', cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)
plot(VA, add = T, lwd = 2)

#MIN TEMP
JanMat <- matrix(MinTemp_01)
FebMat <- matrix(MinTemp_02)
MarMat <- matrix(MinTemp_03)
AprMat <- matrix(MinTemp_04)
MayMat <- matrix(MinTemp_05)
JunMat <- matrix(MinTemp_06)
JulMat <- matrix(MinTemp_07)
AugMat <- matrix(MinTemp_08)
SepMat <- matrix(MinTemp_09)
OctMat <- matrix(MinTemp_10)
NovMat <- matrix(MinTemp_11)
DecMat <- matrix(MinTemp_12)
MinMin <- min(minValue(MinTemp_01), minValue(MinTemp_02), minValue(MinTemp_03), minValue(MinTemp_04), minValue(MinTemp_05), minValue(MinTemp_06), minValue(MinTemp_07), minValue(MinTemp_08), minValue(MinTemp_09), minValue(MinTemp_10), minValue(MinTemp_11), minValue(MinTemp_12))
MinMax <- max(maxValue(MinTemp_01), maxValue(MinTemp_02), maxValue(MinTemp_03), maxValue(MinTemp_04), maxValue(MinTemp_05), maxValue(MinTemp_06), maxValue(MinTemp_07), maxValue(MinTemp_08), maxValue(MinTemp_09), maxValue(MinTemp_10), maxValue(MinTemp_11), maxValue(MinTemp_12))
r.range <- c(MinMin, MinMax)
plot(crop(MinTemp_12,BB), col=(colfunc(15)), main = expression('December Min Temperature (1955-1970) ('*degree*'C)'), legend = TRUE, axes = TRUE, zlim = r.range, xlab = 'Longtidue', ylab = 'Latitude', cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)
plot(VA, add = TRUE, lwd = 2)

#O VERALL GRAPHS
par(mfrow = c(2, 2))
TotalMin <- min(minValue(MeanTemp_01), minValue(MeanTemp_02), minValue(MeanTemp_03), minValue(MeanTemp_04), minValue(MeanTemp_05), minValue(MeanTemp_06), minValue(MeanTemp_07), minValue(MeanTemp_08), minValue(MeanTemp_09), minValue(MeanTemp_10), minValue(MeanTemp_11), minValue(MeanTemp_12), minValue(MaxTemp_01), minValue(MaxTemp_02), minValue(MaxTemp_03), minValue(MaxTemp_04), minValue(MaxTemp_05), minValue(MaxTemp_06), minValue(MaxTemp_07), minValue(MaxTemp_08), minValue(MaxTemp_09), minValue(MaxTemp_10), minValue(MaxTemp_11), minValue(MaxTemp_12), minValue(MinTemp_01), minValue(MinTemp_02), minValue(MinTemp_03), minValue(MinTemp_04), minValue(MinTemp_05), minValue(MinTemp_06), minValue(MinTemp_07), minValue(MinTemp_08), minValue(MinTemp_09), minValue(MinTemp_10), minValue(MinTemp_11), minValue(MinTemp_12))
TotalMax <- max(maxValue(MeanTemp_01), maxValue(MeanTemp_02), maxValue(MeanTemp_03), maxValue(MeanTemp_04), maxValue(MeanTemp_05), maxValue(MeanTemp_06), maxValue(MeanTemp_07), maxValue(MeanTemp_08), maxValue(MeanTemp_09), maxValue(MeanTemp_10), maxValue(MeanTemp_11), maxValue(MeanTemp_12), maxValue(MaxTemp_01), maxValue(MaxTemp_02), maxValue(MaxTemp_03), maxValue(MaxTemp_04), maxValue(MaxTemp_05), maxValue(MaxTemp_06), maxValue(MaxTemp_07), maxValue(MaxTemp_08), maxValue(MaxTemp_09), maxValue(MaxTemp_10), maxValue(MaxTemp_11), maxValue(MaxTemp_12), maxValue(MinTemp_01), maxValue(MinTemp_02), maxValue(MinTemp_03), maxValue(MinTemp_04), maxValue(MinTemp_05), maxValue(MinTemp_06), maxValue(MinTemp_07), maxValue(MinTemp_08), maxValue(MinTemp_09), maxValue(MinTemp_10), maxValue(MinTemp_11), maxValue(MinTemp_12))
r.range <- c(TotalMin, TotalMax)
plot(crop(MeanTemp_12,BB), col=(colfunc(15)), main= expression('December Mean'),legend=!TRUE,axes=TRUE, zlim=r.range, xlab='Longitude',ylab='Latitude')
plot(VA,add=T,lwd=2)
plot(crop(MaxTemp_12,BB), col=(colfunc(15)), main = expression('December Max'), legend = !TRUE, axes = TRUE, zlim = r.range, xlab = 'Longitude', ylab = 'Latitude')
plot(VA, add = T, lwd = 2)
plot(crop(MinTemp_12,BB), col=(colfunc(15)), main = expression('December Min'), legend = !TRUE, axes = TRUE, zlim = r.range, xlab = 'Longitude', ylab = 'Latitude')
plot(VA, add = TRUE, lwd = 2)
plot(crop(MeanTemp_08,BB), col=(colfunc(15)), legend.only=TRUE, horizontal = !TRUE, zlim=r.range, cex = 2, ncol = 2)


plot(crop(MeanTemp_08,BB))


# # SOLAR RADIATION 
par(mar=c(5,6,2,4))
plot(Ra,type='l',lwd=2,ylab='Mean Radiation (MJ/m^2/day)',xlab='Month',cex.lab=2,cex.axis=2)
# 
# # DAYLIGHT HOURS
par(mar=c(5,6,2,4))
plot(Ld,type='l',lwd=2,ylab='Mean Daylight Hours (hours)',xlab='Month',cex.lab=2,cex.axis=2)
# 
# # THORNTHWAITE PET
ET<-c(mean(WB@data$JanThorn),
      mean(WB@data$FebThorn),
      mean(WB@data$MarThorn),
      mean(WB@data$AprThorn),
      mean(WB@data$MayThorn),
      mean(WB@data$JunThorn),
      mean(WB@data$JulThorn),
      mean(WB@data$AugThorn),
      mean(WB@data$SepThorn),
      mean(WB@data$OctThorn),
      mean(WB@data$NovThorn),
      mean(WB@data$DecThorn))
par(mar=c(5,6,2,4))
plot(ET,type='l',lwd=2,ylab='Mean Thornthwaite PET (mm/day)',xlab='Month',cex.lab=2,cex.axis=2)
# 
# # HARGREAVES AND THORNTHWAITE
ET<-c(mean(WB@data$JanHarg),mean(WB@data$FebHarg),mean(WB@data$MarHarg),mean(WB@data$AprHarg),mean(WB@data$MayHarg),mean(WB@data$JunHarg),mean(WB@data$JulHarg),mean(WB@data$AugHarg),mean(WB@data$SepHarg),mean(WB@data$OctHarg),mean(WB@data$NovHarg),mean(WB@data$DecHarg))
ET2<-c(mean(WB@data$JanThorn),mean(WB@data$FebThorn),mean(WB@data$MarThorn),mean(WB@data$AprThorn),mean(WB@data$MayThorn),mean(WB@data$JunThorn),mean(WB@data$JulThorn),mean(WB@data$AugThorn),mean(WB@data$SepThorn),mean(WB@data$OctThorn),mean(WB@data$NovThorn),mean(WB@data$DecThorn))
par(mar=c(5,6,2,4))
plot(ET,type='l',lwd=2,ylab='Mean PET (mm/day)',xlab='Month',cex.lab=2,cex.axis=2,col='red')
lines(ET2,col='blue',lwd=2)
legend(x=1,y=6,bty='n',col=c('red','blue'),legend=c("Hargreaves","Thornthwaite"),lwd=2,cex=2,y.intersp = 0.75)

#===================================================================================
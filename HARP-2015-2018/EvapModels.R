library(lubridate)
library(rgeos)
library(rgdal)
library(raster)
library(dplyr)

my.filepath <- 'C:\\Users\\conno\\Desktop\\GitHub\\cbp_wsm\\evap\\'

m<-c('01','02','03','04','05','06','07','08','09','10','11','12')
VA<-readOGR(paste(my.filepath,'EvapInputs.gdb',sep=""),layer="VA")
VA<-spTransform(VA,CRS='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
BB<-readOGR(paste(my.filepath,'EvapInputs.gdb',sep=""),layer="BoundingBox")
BB<-spTransform(BB,CRS="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
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
#BB = SpatialPolygons(list(Polygons(list(BB), ID = "a")), proj4string=CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
#--------------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------
# View Bounding Box and Virginia as plot image 
#plot(BB,axes=T)
#plot(VA,add=T)
#--------------------------------------------------------------------------------------------------


########Mean monthly raster creation using PRISM data (pre-downloaded)########
#You MUST delete output TIFF files before writing new ones!
#Can download ECHO as to get all monthly data for 1970 max temperature (can also call tmin, ppt, tdmean, and tmean). Works for 1895-1980
yr<-as.character(seq(1955,1970))
psmC<-rep(T,length(yr))
for(i in 1:length(yr)){
  if(yr[i]>1980){
    psmC[i]<-F 
  }
}
setwd(my.filepath)
pathTMean<- paste(my.filepath,'PRISM\\TMean',sep="")
pathTMax <- paste(my.filepath,'PRISM\\TMax',sep="")
pathTMin <- paste(my.filepath,'PRISM\\TMin',sep="")
pathTDew <- paste(my.filepath,'PRISM\\TDew',sep="")
for(i in 1:length(yr)){
  if(psmC[i]){
    temp<-tempfile()
    download.file(paste0("http://services.nacse.org/prism/data/public/4km/tmean/",yr[i]),temp,mode='wb')
    unzip(zipfile=temp,exdir=pathTMean)
  
    temp<-tempfile()
    download.file(paste0("http://services.nacse.org/prism/data/public/4km/tmax/",yr[i]),temp,mode='wb')
    unzip(temp,exdir=pathTMax)  
    
    temp<-tempfile()
    download.file(paste0("http://services.nacse.org/prism/data/public/4km/tmin/",yr[i]),temp,mode='wb')
    unzip(temp,exdir=pathTMin)  
    
    temp<-tempfile()
    download.file(paste0("http://services.nacse.org/prism/data/public/4km/tdmean/",yr[i]),temp,mode='wb')
    unzip(temp,exdir=pathTDew)  
  }else{
    for(j in 1:length(m)){
      temp<-tempfile()
      download.file(paste0("http://services.nacse.org/prism/data/public/4km/tmean/",yr[i],m[j]),temp,mode='wb')
      unzip(zipfile=temp,exdir=pathTMean)
      
      temp<-tempfile()
      download.file(paste0("http://services.nacse.org/prism/data/public/4km/tmax/",yr[i],m[j]),temp,mode='wb')
      unzip(temp,exdir=pathTMax)  
      
      temp<-tempfile()
      download.file(paste0("http://services.nacse.org/prism/data/public/4km/tmin/",yr[i],m[j]),temp,mode='wb')
      unzip(temp,exdir=pathTMin)  
      
      temp<-tempfile()
      download.file(paste0("http://services.nacse.org/prism/data/public/4km/tdmean/",yr[i],m[j]),temp,mode='wb')
      unzip(temp,exdir=pathTDew)  
    }
  }
}


#For average temperature
setwd(paste(my.filepath,"PRISM\\TMean",sep=""))
print(paste("Mean Temperature:",sep=""))
dir.create('Monthly Averages')
files<-list.files()
files<-files[grep('bil.bil$',files)]
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

#For dew point temperature
setwd(paste(my.filepath,"PRISM\\TDew",sep=""))
print(paste("Dew Point Temperature:",sep=""))
dir.create('Monthly Averages')
files<-list.files()
files<-files[grep('bil.bil$',files)]
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
  assign(paste0('DewTemp_',m[j]),mean(rstack))
  writeRaster(mean(rstack), filename=paste0('Monthly Averages/DewTemp',m[j],'.tiff'))
}

########Pan Evaporation Data########
setwd(paste0(my.filepath,'ReferenceET/'))
dyinmo<-c(31,28,31,30,31,30,31,31,30,31,30,31)
for (i in 1:length(m)){
  print(paste0("Checking month ",i," of ",length(m)))
  files<-list.files()
  files<-files[grep(month.abb[as.numeric(m[i])],files)]
  if(length(files)>0){
    lab<-paste0(month.abb[as.numeric(m[i])],"ReferenceET.tif")
    test<-raster(lab)
    test<-test*25.4/dyinmo[i]
    test<-projectRaster(test,crs=proj4string(BB))
    testclip<-mask(test,BB) 
    assign(paste0('RefET_',m[i]),testclip)
  }
}
PanCoeff<-raster('PanCoefficient.tif')
PanCoeff<-projectRaster(PanCoeff,crs=proj4string(BB))
PanCoeff<-mask(PanCoeff,BB) 

########Solar radiation calculation########
WB<-readOGR(paste(my.filepath,'EvapInputs.gdb',sep=""),layer="Waterbodies")

#Project coordinates into NAD 1983 UTM zone 17N to caclulate area. Then, reproject back for consistency with other files
WB<-spTransform(WB,CRS='+proj=utm +zone=17 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
WB@data$FID<-1:length(WB@data$COMID)
#Not sure about these yet
WB@data$AreaRcalc<-gArea(WB,byid=T)
WB<-spTransform(WB,CRS='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
WBCenter<-gCentroid(WB,byid=T,id=WB@data$FID)

bins<-c(0,0.25,5,10,15,30,50,100,max(WB@data$AreaRcalc))
lab<-c('< 0.25 acres','0.25 - 5 acres','5 - 10 acres','10 - 15 acres','15 - 30 acres','30 - 50 acres','50 - 100 acres','> 100 acres')
WB@data$Bin<-as.character(cut(WB@data$AreaRcalc*10000/2.54/2.54/12/12/43560,bins,labels=1:8))
WB@data$NewArea_Class<-as.character(cut(WB@data$AreaRcalc*10000/2.54/2.54/12/12/43560,bins,labels=lab))

#See http://www.fao.org/docrep/x0490e/x0490e07.htm#TopOfPage
diy<-seq(1,365)
distFsun<-1+0.033*cos(2*pi*diy/365)
sigma<-0.409*sin(2*pi*diy/365-1.39)
for (i in 1:length(WB@data$FID)){
  #print(paste("Mean extraterrestrial radiation calc for WB ",i," of ",length(WB@data$FID),sep=""))
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
WB@data$JanMaxT<-extract(MaxTemp_01,WBCenter)
WB@data$JanMinT<-extract(MinTemp_01,WBCenter)
WB@data$JanDiffT<-WB@data$JanMaxT-WB@data$JanMinT
WB@data$JanMeanT<-extract(MeanTemp_01,WBCenter)
WB@data$JanDewT<-extract(DewTemp_01,WBCenter)
WB@data$FebMaxT<-extract(MaxTemp_02,WBCenter)
WB@data$FebMinT<-extract(MinTemp_02,WBCenter)
WB@data$FebDiffT<-WB@data$FebMaxT-WB@data$FebMinT
WB@data$FebMeanT<-extract(MeanTemp_02,WBCenter)
WB@data$FebDewT<-extract(DewTemp_02,WBCenter)
WB@data$MarMaxT<-extract(MaxTemp_03,WBCenter)
WB@data$MarMinT<-extract(MinTemp_03,WBCenter)
WB@data$MarDiffT<-WB@data$MarMaxT-WB@data$MarMinT
WB@data$MarMeanT<-extract(MeanTemp_03,WBCenter)
WB@data$MarDewT<-extract(DewTemp_03,WBCenter)
WB@data$AprMaxT<-extract(MaxTemp_04,WBCenter)
WB@data$AprMinT<-extract(MinTemp_04,WBCenter)
WB@data$AprDiffT<-WB@data$AprMaxT-WB@data$AprMinT
WB@data$AprMeanT<-extract(MeanTemp_04,WBCenter)
WB@data$AprDewT<-extract(DewTemp_04,WBCenter)
WB@data$MayMaxT<-extract(MaxTemp_05,WBCenter)
WB@data$MayMinT<-extract(MinTemp_05,WBCenter)
WB@data$MayDiffT<-WB@data$MayMaxT-WB@data$MayMinT
WB@data$MayMeanT<-extract(MeanTemp_05,WBCenter)
WB@data$MayDewT<-extract(DewTemp_05,WBCenter)
WB@data$JunMaxT<-extract(MaxTemp_06,WBCenter)
WB@data$JunMinT<-extract(MinTemp_06,WBCenter)
WB@data$JunDiffT<-WB@data$JunMaxT-WB@data$JunMinT
WB@data$JunMeanT<-extract(MeanTemp_06,WBCenter)
WB@data$JunDewT<-extract(DewTemp_06,WBCenter)
WB@data$JulMaxT<-extract(MaxTemp_07,WBCenter)
WB@data$JulMinT<-extract(MinTemp_07,WBCenter)
WB@data$JulDiffT<-WB@data$JulMaxT-WB@data$JulMinT
WB@data$JulMeanT<-extract(MeanTemp_07,WBCenter)
WB@data$JulDewT<-extract(DewTemp_07,WBCenter)
WB@data$AugMaxT<-extract(MaxTemp_08,WBCenter)
WB@data$AugMinT<-extract(MinTemp_08,WBCenter)
WB@data$AugDiffT<-WB@data$AugMaxT-WB@data$AugMinT
WB@data$AugMeanT<-extract(MeanTemp_08,WBCenter)
WB@data$AugDewT<-extract(DewTemp_08,WBCenter)
WB@data$SepMaxT<-extract(MaxTemp_09,WBCenter)
WB@data$SepMinT<-extract(MinTemp_09,WBCenter)
WB@data$SepDiffT<-WB@data$SepMaxT-WB@data$SepMinT
WB@data$SepMeanT<-extract(MeanTemp_09,WBCenter)
WB@data$SepDewT<-extract(DewTemp_09,WBCenter)
WB@data$OctMaxT<-extract(MaxTemp_10,WBCenter)
WB@data$OctMinT<-extract(MinTemp_10,WBCenter)
WB@data$OctDiffT<-WB@data$OctMaxT-WB@data$OctMinT
WB@data$OctMeanT<-extract(MeanTemp_10,WBCenter)
WB@data$OctDewT<-extract(DewTemp_10,WBCenter)
WB@data$NovMaxT<-extract(MaxTemp_11,WBCenter)
WB@data$NovMinT<-extract(MinTemp_11,WBCenter)
WB@data$NovDiffT<-WB@data$NovMaxT-WB@data$NovMinT
WB@data$NovMeanT<-extract(MeanTemp_11,WBCenter)
WB@data$NovDewT<-extract(DewTemp_11,WBCenter)
WB@data$DecMaxT<-extract(MaxTemp_12,WBCenter)
WB@data$DecMinT<-extract(MinTemp_12,WBCenter)
WB@data$DecDiffT<-WB@data$DecMaxT-WB@data$DecMinT
WB@data$DecMeanT<-extract(MeanTemp_12,WBCenter)
WB@data$DecDewT<-extract(DewTemp_12,WBCenter)
WB@data$PanCoeff<-extract(PanCoeff,WBCenter)
WB@data$AprRefET<-extract(RefET_04,WBCenter)
WB@data$MayRefET<-extract(RefET_05,WBCenter)
WB@data$JunRefET<-extract(RefET_06,WBCenter)
WB@data$JulRefET<-extract(RefET_07,WBCenter)
WB@data$AugRefET<-extract(RefET_08,WBCenter)
WB@data$SepRefET<-extract(RefET_09,WBCenter)
WB@data$OctRefET<-extract(RefET_10,WBCenter)

#Pan Coefficient raster is too small. Two waterbody centroids are outside its bounds. Below are the manual inputs:
WB@data$PanCoeff[WB@data$FID==39447]<-0.78
WB@data$PanCoeff[WB@data$FID==47772]<-0.78

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
#Also calculate the Thornthwaite temperature index with the following function:
ThornI<-function(meanT){
  if(meanT<0){
    out<-0
  }else{
    out<-(meanT/5)^1.514
  }
  return(out)
}

print("Calculate average monthly daylight hours for use in the Thornthwaite Method")
for (i in 1:length(WB@data$FID)){
  #print(paste("Avg mo daylight hrs calc for WB ",i," of ",length(WB@data$FID),sep=""))
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

#Calculate Thornthwaite coefficient
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

#Hamon equation may take the form 0.55*25.4*Ld^2*(SVD/100). Need to find saturated vapor density SVD
#Can use the metholody suggested by the Corps of Engineers in Harwell, 2012 and the FAO report
#Output is in mm/day
print("Hamon equation used to estimate monthly evaporation at each waterbody:")

#Function to calculate saturated vapor pressure in kPa from the FAO report
es<-function(temperature){
  SVP<-0.6108*exp((17.27*temperature)/(temperature+237.3))
  return(SVP)
}
#Need to find average saturated vapor pressure in kPa for each month at each waterbody
WB@data$JanEs<-0.5*(es(WB@data$JanMinT)+es(WB@data$JanMaxT))
WB@data$FebEs<-0.5*(es(WB@data$FebMinT)+es(WB@data$FebMaxT))
WB@data$MarEs<-0.5*(es(WB@data$MarMinT)+es(WB@data$MarMaxT))
WB@data$AprEs<-0.5*(es(WB@data$AprMinT)+es(WB@data$AprMaxT))
WB@data$MayEs<-0.5*(es(WB@data$MayMinT)+es(WB@data$MayMaxT))
WB@data$JunEs<-0.5*(es(WB@data$JunMinT)+es(WB@data$JunMaxT))
WB@data$JulEs<-0.5*(es(WB@data$JulMinT)+es(WB@data$JulMaxT))
WB@data$AugEs<-0.5*(es(WB@data$AugMinT)+es(WB@data$AugMaxT))
WB@data$SepEs<-0.5*(es(WB@data$SepMinT)+es(WB@data$SepMaxT))
WB@data$OctEs<-0.5*(es(WB@data$OctMinT)+es(WB@data$OctMaxT))
WB@data$NovEs<-0.5*(es(WB@data$NovMinT)+es(WB@data$NovMaxT))
WB@data$DecEs<-0.5*(es(WB@data$DecMinT)+es(WB@data$DecMaxT))

#By assuming the ideal gas law applies as in Harwell, 2012 we can find SVD in g/m^3
WB@data$JanSVD<-2166.74*(WB@data$JanEs/(273.15+WB@data$JanMeanT))
WB@data$FebSVD<-2166.74*(WB@data$FebEs/(273.15+WB@data$FebMeanT))
WB@data$MarSVD<-2166.74*(WB@data$MarEs/(273.15+WB@data$MarMeanT))
WB@data$AprSVD<-2166.74*(WB@data$AprEs/(273.15+WB@data$AprMeanT))
WB@data$MaySVD<-2166.74*(WB@data$MayEs/(273.15+WB@data$MayMeanT))
WB@data$JunSVD<-2166.74*(WB@data$JunEs/(273.15+WB@data$JunMeanT))
WB@data$JulSVD<-2166.74*(WB@data$JulEs/(273.15+WB@data$JulMeanT))
WB@data$AugSVD<-2166.74*(WB@data$AugEs/(273.15+WB@data$AugMeanT))
WB@data$SepSVD<-2166.74*(WB@data$SepEs/(273.15+WB@data$SepMeanT))
WB@data$OctSVD<-2166.74*(WB@data$OctEs/(273.15+WB@data$OctMeanT))
WB@data$NovSVD<-2166.74*(WB@data$NovEs/(273.15+WB@data$NovMeanT))
WB@data$DecSVD<-2166.74*(WB@data$DecEs/(273.15+WB@data$DecMeanT))

#Then, we can solve for the Hamon Evapotranspiration
25.4*0.55*(WB@data$JanLd[1]/12)^2*(WB@data$JanSVD[1]/100)
WB@data$JanHam<-25.4*0.55*(WB@data$JanLd/12)^2*(WB@data$JanSVD/100)
WB@data$FebHam<-25.4*0.55*(WB@data$FebLd/12)^2*(WB@data$FebSVD/100)
WB@data$MarHam<-25.4*0.55*(WB@data$MarLd/12)^2*(WB@data$MarSVD/100)
WB@data$AprHam<-25.4*0.55*(WB@data$AprLd/12)^2*(WB@data$AprSVD/100)
WB@data$MayHam<-25.4*0.55*(WB@data$MayLd/12)^2*(WB@data$MaySVD/100)
WB@data$JunHam<-25.4*0.55*(WB@data$JunLd/12)^2*(WB@data$JunSVD/100)
WB@data$JulHam<-25.4*0.55*(WB@data$JulLd/12)^2*(WB@data$JulSVD/100)
WB@data$AugHam<-25.4*0.55*(WB@data$AugLd/12)^2*(WB@data$AugSVD/100)
WB@data$SepHam<-25.4*0.55*(WB@data$SepLd/12)^2*(WB@data$SepSVD/100)
WB@data$OctHam<-25.4*0.55*(WB@data$OctLd/12)^2*(WB@data$OctSVD/100)
WB@data$NovHam<-25.4*0.55*(WB@data$NovLd/12)^2*(WB@data$NovSVD/100)
WB@data$DecHam<-25.4*0.55*(WB@data$DecLd/12)^2*(WB@data$DecSVD/100)

#Turc equation may take the form 0.013*(Rs+50)*T/(T+15). If below 50% RH, 0.013*(Rs+50)*T/(T+15)*(1+(50-RH)/70)
#Need to find relative humidity (%) and daily solar radiation (in cal/cm^2/day)
#Can use the metholody suggested by the FAO report
#Output is in mm/day
print("Turc equation used to estimate monthly evaporation at each waterbody:")
#Find average relative humidity at each waterbody
WB@data$JanRH<-100*(es(WB@data$JanDewT)/WB@data$JanEs)
WB@data$FebRH<-100*(es(WB@data$FebDewT)/WB@data$FebEs)
WB@data$MarRH<-100*(es(WB@data$MarDewT)/WB@data$MarEs)
WB@data$AprRH<-100*(es(WB@data$AprDewT)/WB@data$AprEs)
WB@data$MayRH<-100*(es(WB@data$MayDewT)/WB@data$MayEs)
WB@data$JunRH<-100*(es(WB@data$JunDewT)/WB@data$JunEs)
WB@data$JulRH<-100*(es(WB@data$JulDewT)/WB@data$JulEs)
WB@data$AugRH<-100*(es(WB@data$AugDewT)/WB@data$AugEs)
WB@data$SepRH<-100*(es(WB@data$SepDewT)/WB@data$SepEs)
WB@data$OctRH<-100*(es(WB@data$OctDewT)/WB@data$OctEs)
WB@data$NovRH<-100*(es(WB@data$NovDewT)/WB@data$NovEs)
WB@data$DecRH<-100*(es(WB@data$DecDewT)/WB@data$DecEs)


#Multiply by area, convert to m^3/day then convert to billion gallons per year
evapH<-numeric()
evapT<-numeric()
evapHa<-numeric()
for (i in 1:length(WB@data$FID)){
  #print(paste("Evap BGY estimation for WB ",i," of ",length(WB@data$FID),sep=""))
  evapH[i]<-sum(WB@data[i,grep('Harg',colnames(WB@data))]*c(31,28,31,30,31,30,31,31,30,31,30,31))
  evapT[i]<-sum(WB@data[i,grep('Thorn',colnames(WB@data))]*c(31,28,31,30,31,30,31,31,30,31,30,31))
  evapHa[i]<-sum(WB@data[i,grep('Ham',colnames(WB@data))]*c(31,28,31,30,31,30,31,31,30,31,30,31))
}
WB@data$AnnualHrg_mmpyr<-evapH
WB@data$AnnualHrg_BGY<-WB@data$AreaRcalc*(evapH/1000)*264.1721/1000000000
WB@data$AnnualThrn_mmpyr<-evapT
WB@data$AnnualThrn_BGY<-WB@data$AreaRcalc*(evapT/1000)*264.1721/1000000000
WB@data$AnnualHamn_mmpyr<-evapHa
WB@data$AnnualHamn_BGY<-WB@data$AreaRcalc*(evapHa/1000)*264.1721/1000000000
WB@data$AnnualAvg_mmpyr<-(evapH+evapT+evapHa)/3
WB@data$AnnualAvg_BGY<-(WB@data$AnnualHrg_BGY+WB@data$AnnualThrn_BGY+WB@data$AnnualHamn_BGY)/3

#ALL WATERBODIES SHOULD HAVE VALUE, NO SUM SHOULD RETURN NA. This is important for the below plotting code, which is written under this assumption
sum(WB@data$AnnualHrg_BGY);sum(WB@data$AnnualThrn_BGY);sum(WB@data$AnnualHamn_BGY);sum(WB@data$AnnualAvg_BGY)
#===================================================================================
# PLOTS
#===================================================================================
# MEAN TEMP
# plot(crop(MeanTemp_08,BB),main= expression('August Mean Temperature (2012-2016) ('*degree*'C)'),axes=T,xlab='Longitude',ylab='Latitude',cex.lab=2,cex.main=2,cex.axis=2)
# plot(VA,add=T,lwd=2)
# 
# # SOLAR RADIATION 
# par(mar=c(5,6,2,4))
# plot(Ra,type='l',lwd=2,ylab='Mean Radiation (MJ/m^2/day)',xlab='Month',cex.lab=2,cex.axis=2)
# 
# # DAYLIGHT HOURS
# par(mar=c(5,6,2,4))
# plot(Ld,type='l',lwd=2,ylab='Mean Daylight Hours (hours)',xlab='Month',cex.lab=2,cex.axis=2)
# 
# # THORNTHWAITE PET
# ET<-c(mean(WB@data$JanThorn),
#       mean(WB@data$FebThorn),
#       mean(WB@data$MarThorn),
#       mean(WB@data$AprThorn),
#       mean(WB@data$MayThorn),
#       mean(WB@data$JunThorn),
#       mean(WB@data$JulThorn),
#       mean(WB@data$AugThorn),
#       mean(WB@data$SepThorn),
#       mean(WB@data$OctThorn),
#       mean(WB@data$NovThorn),
#       mean(WB@data$DecThorn))
# par(mar=c(5,6,2,4))
# plot(ET,type='l',lwd=2,ylab='Mean Thornthwaite PET (mm/day)',xlab='Month',cex.lab=2,cex.axis=2)
# 
# # HARGREAVES AND THORNTHWAITE
# ET<-c(mean(WB@data$JanHarg),mean(WB@data$FebHarg),mean(WB@data$MarHarg),mean(WB@data$AprHarg),mean(WB@data$MayHarg),mean(WB@data$JunHarg),mean(WB@data$JulHarg),mean(WB@data$AugHarg),mean(WB@data$SepHarg),mean(WB@data$OctHarg),mean(WB@data$NovHarg),mean(WB@data$DecHarg))
# ET2<-c(mean(WB@data$JanThorn),mean(WB@data$FebThorn),mean(WB@data$MarThorn),mean(WB@data$AprThorn),mean(WB@data$MayThorn),mean(WB@data$JunThorn),mean(WB@data$JulThorn),mean(WB@data$AugThorn),mean(WB@data$SepThorn),mean(WB@data$OctThorn),mean(WB@data$NovThorn),mean(WB@data$DecThorn))
# par(mar=c(5,6,2,4))
# plot(ET,type='l',lwd=2,ylab='Mean PET (mm/day)',xlab='Month',cex.lab=2,cex.axis=2,col='red')
# lines(ET2,col='blue',lwd=2)
# legend(x=1,y=6,bty='n',col=c('red','blue'),legend=c("Hargreaves","Thornthwaite"),lwd=2,cex=2,y.intersp = 0.75)

#WATERBODY SUMMARY
#Summarize surface area, number of impoundments, and ET by waterbody class
WBSummary<-summarize(group_by(WB@data,Bin),Class=first(NewArea_Class),ET_BGY=sum(AnnualAvg_BGY),SA_ac=sum(AreaRcalc*10000/2.54/2.54/12/12/43560),Number=n())

#EVAPORATION COMPARISONS TO PAN ET DATA
#Show how Pan data compares with each of the three models. Also, show the average
summer<-data.frame(Month=month.abb[4:10],Hargreaves=NA,Thornthwaite=NA,Hamon=NA,Pan=NA,Average=NA)
summer$Month<-as.character(summer$Month)
for (i in 1:length(summer$Month)){
  subst<-grep(paste0(summer$Month[i],'Harg'),colnames(WB@data))
  subst<-WB@data[,subst]
  summer$Hargreaves[i]<-sum(WB@data$AreaRcalc*(subst/1000)*264.1721/1000000000)
  
  subst<-grep(paste0(summer$Month[i],'Thorn'),colnames(WB@data))
  subst<-WB@data[,subst]
  summer$Thornthwaite[i]<-sum(WB@data$AreaRcalc*(subst/1000)*264.1721/1000000000)
  
  subst<-grep(paste0(summer$Month[i],'Ham'),colnames(WB@data))
  subst<-WB@data[,subst]
  summer$Hamon[i]<-sum(WB@data$AreaRcalc*(subst/1000)*264.1721/1000000000)
  
  subst<-grep(paste0(summer$Month[i],'RefET'),colnames(WB@data))
  subst<-WB@data[,subst]
  summer$Pan[i]<-sum(WB@data$AreaRcalc*(subst*WB@data$PanCoeff/1000)*264.1721/1000000000)
  
  summer$Average[i]<-mean(as.numeric(summer[i,2:4]))
}
par(mar=c(5,6,2,4),lwd=2,cex.axis=2,cex.lab=2)
plot(4:10,summer$Hargreaves,type='o',lwd=4,pch=19,cex=2,xlab='Month',ylab='Evaporation (BGY)',ylim=c(0.25,1.5))
lines(4:10,summer$Thornthwaite,type='o',cex=2,lwd=4,pch=19,col='dark red')
lines(4:10,summer$Hamon,type='o',cex=2,lwd=4,pch=19,col='dark blue')
lines(4:10,summer$Pan,type='o',cex=2,lwd=4,pch=19,col='dark orange')
lines(4:10,summer$Average,type='o',cex=2,lwd=4,col='dark green',lty=3)
legend(x=6.3,y=1.0,legend=c("Hargreaves","Thornthwaite","Hamon","Pan","Model Average"),
       col=c('black','dark red','dark blue','dark orange','dark green'),lty=c(1,1,1,1,3),
       pch=c(19,19,19,19,1),lwd=4,cex=2,x.intersp = 0.4,y.intersp = 0.5,seg.len=0.8,bty='n')

#DEQ Withdraw
Withdraw<-read.csv(paste0(my.filepath,'DEQwithdraw.csv'),stringsAsFactors = F)
Withdraw$Date<-as.Date(Withdraw$Date,format='%m/%d/%Y')
Withdraw$Month<-month(Withdraw$Date)
WithdrawSummary<-as.data.frame(summarize(group_by(Withdraw,Month),AvgMGM=mean(Withdraw)))
WithdrawSummary$AvgMGD<-WithdrawSummary$AvgMGM/c(31,28,31,30,31,30,31,31,30,31,30,31)


#WATERBODY DENSITY
#To plot waterbody density across the state. Need to call PNG function here because raster and spatial data frames don't always agree when axes change
#For instance, zooming can make mistakes by rescaling rasters differently from the spatial data frames
WBDens<-raster(paste0(my.filepath,"ReferenceET/WBDensity_projected.tif"))
WBDens<-projectRaster(WBDens,crs=proj4string(BB))
colorfunc<-colorRampPalette(c('dark blue','cyan','yellow','red'))
color<-colorfunc(100)
png(paste0(my.filepath,'Output/Density',".png"),width=1780,height=860)
plot(nonVA,xlim=c(-83.5,-73),ylim=c(36,40),col='light grey')
plot(WBDens,add=T,col=color,legend=F)
lines(nonVA,lwd=2)
lines(VAoutline,lwd=2)
r.range <- c(round(minValue(WBDens),1), round(maxValue(WBDens),1))
plot(WBDens, legend.only=TRUE, col=color,
     legend.width=2, legend.shrink=0.75,
     axis.args=list(at=seq(r.range[1], r.range[2],0.5),
                    labels=seq(r.range[1], r.range[2], 0.5), 
                    cex.axis=2.5),
     smallplot=c(0.80,0.85,0.3,0.65),
     legend.args=list(text=expression(Imp.~Density~'(#/'*km^{2}*')'), side=4, font=2, line=7, cex=3))
dev.off()


#LANDCOVER
NLCD<-raster(paste0(my.filepath,"ReferenceET/NLCD.tif"))
NLCDval<-unique(values(NLCD))
NLCDval<-NLCDval[order(NLCDval)]
newclass<-c(1,2,2,2,2,2,3,3,3,3,3,4,4,1,1,NA)
recls<-data.frame(is=NLCDval,becomes=newclass)
recls<-as.matrix(recls)
NLCD<-reclassify(NLCD,recls)

png(paste0(my.filepath,'Output/NLCD',".png"),width=1780,height=860)
plot(nonVA,xlim=c(-83.5,-73),ylim=c(36,40),col='light grey')
plot(NLCD,add=T,col=c('blue','red','dark green','darkgoldenrod1'),legend=F)
lines(nonVA,lwd=2)
lines(VAoutline,lwd=2)
legend(-74.38, 39, c('Agriculture','Forest','Developed','Water'),
       col =c('darkgoldenrod1','dark green','red','blue'),lty=0,
       pch=15,pt.cex=6,bty='n',y.intersp = 1,x.intersp = 1,cex=3,lwd=2,seg.len=0.25)
dev.off()


#NORMALIZED EVAP BY 7q10
#To plot evap across the state normalized by 7q10
#Initialize 7q10 data
HUCdata<-read.csv(paste0(my.filepath,"HUC8_7q10.csv"))
HUCdata$HUC8<-paste0('0',HUCdata$HUC8)#To fix previous excel errors
HUCdata$Area_sqmi<-as.numeric(as.character(HUCdata$Area_sqmi))
HUCdata$AltArea_sqmi<-as.numeric(as.character(HUCdata$AltArea_sqmi))
HUCdata$X7q10_cfs<-as.numeric(as.character(HUCdata$X7q10_cfs))
HUCdata$Normalized7q10<-NA
#Normalize by draiange area of the gauge used
for(i in 1:length(HUCdata$Name)){
  if(is.na(HUCdata$AltArea_sqmi[i])){
    HUCdata$Normalized7q10[i]<-HUCdata$X7q10_cfs[i]/HUCdata$Area_sqmi[i]
  }else{
    HUCdata$Normalized7q10[i]<-HUCdata$X7q10_cfs[i]/HUCdata$AltArea_sqmi[i]
  }
}
#load up some HUCs
HUC8<-readOGR(paste(my.filepath,'HUC.gdb',sep=""),layer="WBDHU8")
HUC8<-spTransform(HUC8,CRS='+proj=utm +zone=17 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
HUC8@data$FID<-1:length(HUC8@data$TNMID)
#Calculate area in UTM zone 17N
HUC8@data$AreaRcalc<-gArea(HUC8,byid=T)
HUC8<-spTransform(HUC8,CRS=proj4string(BB))
#Create an extra copy for modification
HUC8Overlay<-HUC8
HUC8Overlay@data<-HUC8Overlay@data[,c(11,12)]
names(HUC8Overlay@data)<-c("HUC8","HUC8Name")
#Find the HUC each waterbody resides in through a spatial join
WBHUC<-over(WBCenter,HUC8Overlay)
WB@data$HUC8<-WBHUC$HUC8
WB@data$HUC8Name<-WBHUC$HUC8Name
#Summarize ET by HUC and then store in the HUC8 file
HUCSum<-as.data.frame(summarize(group_by(WB@data,HUC8Name),HUC=first(HUC8),Hamn_BGY=sum(AnnualHamn_BGY),Thrn_BGY=sum(AnnualThrn_BGY),Hrg_BGY=sum(AnnualHrg_BGY),Average_BGY=sum(AnnualAvg_BGY),Average_mmpyr=sum(AnnualAvg_mmpyr),WBArea=sum(AreaRcalc)))
HUCSum$HUC8Name<-as.character(HUCSum$HUC8Name)
HUC8@data$Normalized7q10<-NA
for(i in 1:length(HUC8@data$TNMID)){
  if(length(HUCSum$HUC8Name[HUCSum$HUC8Name==HUC8@data$Name[i]])>0){
    HUC8@data$Hamn_BGY[i]<-HUCSum$Hamn_BGY[HUCSum$HUC8Name==HUC8@data$Name[i]]
    HUC8@data$Thrn_BGY[i]<-HUCSum$Thrn_BGY[HUCSum$HUC8Name==HUC8@data$Name[i]]
    HUC8@data$Hrg_BGY[i]<-HUCSum$Hrg_BGY[HUCSum$HUC8Name==HUC8@data$Name[i]]
    HUC8@data$AvgET_BGY[i]<-HUCSum$Average_BGY[HUCSum$HUC8Name==HUC8@data$Name[i]]
    HUC8@data$AvgET_mmpyr[i]<-HUCSum$Average_mmpyr[HUCSum$HUC8Name==HUC8@data$Name[i]]
    HUC8@data$WBArea_m2[i]<-HUCSum$WBArea[HUCSum$HUC8Name==HUC8@data$Name[i]]
    if(as.character(HUC8@data$HUC8[i])%in%HUCdata$HUC8){
      HUC8@data$Normalized7q10[i]<-HUCdata$Normalized7q10[HUCdata$HUC8==HUC8@data$HUC8[i]]
    }
  }
}
#Some data for pretty plotting
nonVA<-readOGR(paste(my.filepath,'EvapInputs.gdb',sep=""),layer="NonVAStates")
nonVA<-spTransform(nonVA,CRS=proj4string(BB))
VAoutline<-readOGR(paste(my.filepath,'EvapInputs.gdb',sep=""),layer="Virginia_ESRI")
VAoutline<-spTransform(VAoutline,CRS=proj4string(BB))
#Calculate HUC 7q10 and store all HUC data into the clipped file for plotting
HUC8@data$HUC7q10_cfs<-HUC8@data$Normalized7q10*HUC8@data$AreaRcalc*100*100/2.54/2.54/12/12/5280/5280
HUC8@data$RatioET_7q10<-(HUC8@data$AvgET_BGY*1000000000*231/12/12/12/365/24/3600)/HUC8@data$HUC7q10_cfs
#Clip to the state boundary
HUC8Cl<-gIntersection(HUC8,VAoutline,id=as.character(HUC8@data$HUC8),byid=TRUE,drop_lower_td=TRUE)
HUC8Cl<-SpatialPolygonsDataFrame(HUC8Cl,HUC8@data[as.character(HUC8@data$HUC8)%in%names(HUC8Cl),],match.ID = "HUC8")
HUC8Cl@data$Color<-cut(HUC8Cl@data$RatioET_7q10,c(0,0.05,0.1,0.25,0.5,0.75,1,2,max(HUC8Cl@data$RatioET_7q10,na.rm=T)),labels=c('dark blue','blue','cyan','light green','lime green','yellow','orange','red'))
HUC8Cl@data$Color<-as.character(HUC8Cl@data$Color)
HUC8Cl@data$Color[is.na(HUC8Cl@data$Color)]<-'wheat4'

png(paste0(my.filepath,'Output/E_7q10',".png"),width=1780,height=860)
plot(nonVA,xlim=c(-83.5,-73),ylim=c(36,40),col='light grey')
plot(HUC8Cl,add=T,col=HUC8Cl@data$Color)
lines(nonVA,lwd=2)
lines(VAoutline,lwd=2)
legend(-74.38, 39, c('0 - 5%','5 - 10%','10 - 25%','25 - 50%','50 - 75%','75 - 100%','100 - 200%','> 200%','n.d.'),
       col =c('dark blue','blue','cyan','light green','lime green','yellow','orange','red','wheat4'),lty=0,
       pch=15,pt.cex=6,bty='n',y.intersp = 1,x.intersp = 1,cex=3,lwd=2,seg.len=0.25)
mtext(at=-73.85,line=-14,'E/7q10',cex=4)
dev.off()

#NORMALIZED BY DEMAND
#Read in demand measuring points. Note that some geometries are bad and need correction
Hydro<-read.csv(paste0(my.filepath,"DEQwithdrawMP.csv"),stringsAsFactors = F)
#Create new columns for unformatted lat/long
Hydro$MPLat<-Hydro$Latitude
Hydro$MPLong<-Hydro$Longitude
#A simple search function to check for NAs and replace them for use in boolean code
NAReplace<-function(x){
  if(is.na(x)){
    return(-99)
  }else{
    return(x)
  }
}
plus<-function(x){
  if(all(is.na(x))){
    c(NA)
  }else{
    sum(x,na.rm = TRUE)}
}
#Correct lat/long values for possible typos i.e. missing negative signs, switched lat/long, or both
#Is set to check if data falls in rough state boundaries set by (35.-70) and (45,-90)
Hydro$Latitude<-numeric(length(Hydro$MPLat))
Hydro$Longitude<-numeric(length(Hydro$MPLong))
for (i in 1:length(Hydro$Facility)){
  Hydro$Latitude[i]<-Hydro$MPLat[i]
  Hydro$Longitude[i]<-Hydro$MPLong[i]
  Hydro$Latitude[i]<-NAReplace(Hydro$Latitude[i])
  Hydro$Longitude[i]<-NAReplace(Hydro$Longitude[i])
  if((Hydro$Latitude[i]<35|Hydro$Latitude[i]>45)|(Hydro$Longitude[i]<(-90)|Hydro$Longitude[i]>(-70))){
    Hydro$Latitude[i]<-Hydro$MPLong[i]
    Hydro$Longitude[i]<-Hydro$MPLat[i]
    Hydro$Latitude[i]<-NAReplace(Hydro$Latitude[i])
    Hydro$Longitude[i]<-NAReplace(Hydro$Longitude[i]) 
    if((Hydro$Latitude[i]<35|Hydro$Latitude[i]>45)|(Hydro$Longitude[i]<(-90)|Hydro$Longitude[i]>(-70))){
      Hydro$Latitude[i]<-Hydro$MPLat[i]
      Hydro$Longitude[i]<-(-1)*Hydro$MPLong[i]
      Hydro$Latitude[i]<-NAReplace(Hydro$Latitude[i])
      Hydro$Longitude[i]<-NAReplace(Hydro$Longitude[i]) 
      if((Hydro$Latitude[i]<35|Hydro$Latitude[i]>45)|(Hydro$Longitude[i]<(-90)|Hydro$Longitude[i]>(-70))){
        Hydro$Latitude[i]<-Hydro$MPLong[i]
        Hydro$Longitude[i]<-(-1)*Hydro$MPLat[i]
        Hydro$Latitude[i]<-NAReplace(Hydro$Latitude[i])
        Hydro$Longitude[i]<-NAReplace(Hydro$Longitude[i]) 
        if((Hydro$Latitude[i]<35|Hydro$Latitude[i]>45)|(Hydro$Longitude[i]<(-90)|Hydro$Longitude[i]>(-70))){
          Hydro$Latitude[i]<-(-1)*Hydro$MPLat[i]
          Hydro$Longitude[i]<-Hydro$MPLong[i]
          Hydro$Latitude[i]<-NAReplace(Hydro$Latitude[i])
          Hydro$Longitude[i]<-NAReplace(Hydro$Longitude[i])
          if((Hydro$Latitude[i]<35|Hydro$Latitude[i]>45)|(Hydro$Longitude[i]<(-90)|Hydro$Longitude[i]>(-70))){
            Hydro$Latitude[i]<-Hydro$MPLat[i]
            Hydro$Longitude[i]<-Hydro$MPLong[i]
          }
        }
      }
    }
  }
}

#Remove MPs with missing lat/long
Hydro<-Hydro[!(is.na(Hydro$Latitude)&is.na(Hydro$Longitude)),]
#Convert to spatial points
Hydro<-SpatialPointsDataFrame(data.frame(lon=Hydro$Longitude,lat=Hydro$Latitude),Hydro,proj4string = crs(BB))
Hydro<-spTransform(Hydro,proj4string(BB))
#Spatially join with the HUC layer
HydroHUC<-over(Hydro,HUC8Overlay)
Hydro@data$HUC8<-HydroHUC$HUC8
Hydro@data$HUC8Name<-HydroHUC$HUC8Name
Hydro@data$Current.Use.Five.Year.Average..MGY.<-as.numeric(Hydro$Current.Use.Five.Year.Average..MGY.)
#Add values to the spatial HUC polygons data frame layer
HUCSum<-as.data.frame(summarize(group_by(Hydro@data,HUC8Name),Withdraw=plus(Current.Use.Five.Year.Average..MGY.)))
HUCSum$HUC8Name<-as.character(HUCSum$HUC8Name)
HUCSum$HUC8Name[is.na(HUCSum$HUC8Name)]<-'None'
for(i in 1:length(HUC8@data$TNMID)){
  if(length(HUCSum$HUC8Name[HUCSum$HUC8Name==HUC8@data$Name[i]])>0){
    HUC8@data$Withdraw_BGY[i]<-HUCSum$Withdraw[HUCSum$HUC8Name==HUC8@data$Name[i]]/1000
  }
}

HUC8@data$RatioET_ETpD<-HUC8@data$AvgET_BGY/(HUC8@data$AvgET_BGY+HUC8@data$Withdraw_BGY)
HUC8Cl<-gIntersection(HUC8,VAoutline,id=as.character(HUC8@data$HUC8),byid=TRUE,drop_lower_td=TRUE)
HUC8Cl<-SpatialPolygonsDataFrame(HUC8Cl,HUC8@data[as.character(HUC8@data$HUC8)%in%names(HUC8Cl),],match.ID = "HUC8")
HUC8Cl@data$Color<-cut(HUC8Cl@data$RatioET_ETpD,c(0,0.2,0.4,0.6,0.8,1),labels=c('dark blue','cyan','light green','orange','red'))
HUC8Cl@data$Color<-as.character(HUC8Cl@data$Color)
HUC8Cl@data$Color[is.na(HUC8Cl@data$Color)]<-'wheat4'

png(paste0(my.filepath,'Output/E_EpD',".png"),width=1780,height=860)
plot(nonVA,xlim=c(-83.5,-73),ylim=c(36,40),col='light grey')
plot(HUC8Cl,add=T,col=HUC8Cl@data$Color)
lines(nonVA,lwd=2)
lines(VAoutline,lwd=2)
legend(-74.45, 39, c('0 - 20%','20 - 40%','40 - 60%','60 - 80%','80 - 100%','n.d.'),
       col =c('dark blue','cyan','light green','orange','red','wheat4'),lty=0,
       pch=15,pt.cex=6,bty='n',y.intersp = 1,x.intersp = 1,cex=3,lwd=2,seg.len=0.25)
mtext(at=-73.85,line=-14,'E/(E+D)',cex=4)
dev.off()

setwd(paste0(my.filepath,"Output/"))
#Just need the comid, area, size class, size class name, average ET in mm/py, HUC8, and HUC8name
colnames(WB@data)
WBexp<-WB
WBexp@data<-WBexp@data[,c(1,16,17,18,191,193,194)]
WBexp@data$AnnualAvg_inpyr<-WBexp@data$AnnualAvg_mmpyr/10/2.54
WBexp<-SpatialPointsDataFrame(data.frame(lon=WBCenter@coords[1:length(WBCenter@coords[,2])],lat=WBCenter@coords[,2]),WBexp@data,proj4string = crs(BB))
writeOGR(WBexp,'.','WBexp',driver="ESRI Shapefile")
#Need to run kriging in ArcGIS and then export raster for plotting here
AnnualE<-raster(paste0(my.filepath,"Output/Annual_evap.tif"))
AnnualE<-projectRaster(AnnualE,crs=proj4string(BB))
AnnualE<-mask(AnnualE,VAoutline)
colorfunc<-colorRampPalette(c('dark blue','cyan','yellow','red'))
color<-colorfunc(100)
png(paste0(my.filepath,'Output/AnnualE_inpdy',".png"),width=1780,height=860)
plot(nonVA,xlim=c(-83.5,-73),ylim=c(36,40),col='light grey')
plot(AnnualE,add=T,col=color,legend=F)
lines(nonVA,lwd=2)
lines(VAoutline,lwd=2)
r.range <- c(round(minValue(AnnualE),1), round(maxValue(AnnualE),1))
plot(AnnualE, legend.only=TRUE, col=color,
     legend.width=2, legend.shrink=0.75,
     axis.args=list(at=seq(r.range[1], r.range[2],3),
                    labels=seq(r.range[1], r.range[2],3), 
                    cex.axis=2.5),
     smallplot=c(0.80,0.85,0.3,0.65),
     legend.args=list(text=expression(Annual~Evaporation~'(in/day)'), side=4, font=2, line=7, cex=3))
dev.off()

#To analyze monthly evap from a single HUC
WB@data$HUC8<-as.character(WB@data$HUC8)
WBsbst<-WB[WB@data$HUC8=='02070006',]
monthlyHUC<-data.frame(Month=month.abb[1:12],Hargreaves=NA,Thornthwaite=NA,Hamon=NA,Average=NA)
monthlyHUC$Month<-as.character(monthlyHUC$Month)
for (i in 1:length(monthlyHUC$Month)){
  subst<-grep(paste0(monthlyHUC$Month[i],'Harg'),colnames(WBsbst@data))
  subst<-WBsbst@data[,subst]
  monthlyHUC$Hargreaves[i]<-sum(WBsbst@data$AreaRcalc*(subst/1000)*264.1721/1000000)
  
  subst<-grep(paste0(monthlyHUC$Month[i],'Thorn'),colnames(WBsbst@data))
  subst<-WBsbst@data[,subst]
  monthlyHUC$Thornthwaite[i]<-sum(WBsbst@data$AreaRcalc*(subst/1000)*264.1721/1000000)
  
  subst<-grep(paste0(monthlyHUC$Month[i],'Ham'),colnames(WBsbst@data))
  subst<-WBsbst@data[,subst]
  monthlyHUC$Hamon[i]<-sum(WBsbst@data$AreaRcalc*(subst/1000)*264.1721/1000000)
  
  monthlyHUC$Average[i]<-mean(as.numeric(monthlyHUC[i,2:4]))
}

HUCs<-HUC8@data
cols<-c("HUC8","Name","AreaRcalc","Withdraw_BGY","Normalized7q10","HUC7q10_cfs","Hamn_BGY","Thrn_BGY","Hrg_BGY","AvgET_BGY","AvgET_mmpyr","RatioET_7q10","RatioET_ETpD","WBArea_m2")
HUCs<-HUCs[,colnames(HUCs)%in%cols]
names(HUCs)[3]<-'Area_m^2'
names(HUCs)[4]<-'Normalized7q10_cfs/mi^2'
names(HUCs)[14]<-'WBArea_m^2'
HUCs<-HUCs[,c(2,1,3,14,11,4,9,5,6,7,8,13,10,12)]
write.csv(HUCs,paste0(my.filepath,"Output/HUCsummary.csv"))
#===================================================================================
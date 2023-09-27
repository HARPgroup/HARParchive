#The point of this code is to show the powers of R. Evapotranspiration will be calculated using different methods after importing GIS and other data

#----------------------------------------------------------------------------------------------------------------------------------------------------
# Installing Packages#
#----------------------------------------------------------------------------------------------------------------------------------------------------
install.packages("rgoes") #useful for spatial analysis and allows for quick manipulation of polygons to produce center-of-mass centroid points
install.packages("regdal") #useful for spatial analysis that lets R interface with ESRI geodatabases and file types
install.packages("raster") #contains functions that make it easier to handle and manipulate rasters

#----------------------------------------------------------------------------------------------------------------------------------------------------
#Loading Packages#
#----------------------------------------------------------------------------------------------------------------------------------------------------
library (rgeos)
library (rgdal)
library(raster)

#----------------------------------------------------------------------------------------------------------------------------------------------------
#Create Vectors needed for PRISM (geodatabase)#
#----------------------------------------------------------------------------------------------------------------------------------------------------
my.filepath <- 'C:\\Users\\hailey96\\Documents\\' #creates path to get to data, \ had to be changed to \\
m<-c('01','02','03','04','05','06','07','08','09','10','11','12')
VA<-readOGR(paste(my.filepath,'EvapInputs.gdb',sep=""),layer="VA")
BB<-readOGR(paste(my.filepath,'EvapInputs.gdb',sep=""),layer="BoundingBox")
BB<-spTransform(BB,CRS="+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")

#----------------------------------------------------------------------------------------------------------------------------------------------------
#View Bounding Box and Virginia as plot image# 
#----------------------------------------------------------------------------------------------------------------------------------------------------
plot(BB,axes=T)
plot(VA,add=T)
#----------------------------------------------------------------------------------------------------------------------------------------------------
#Mean Monthly Raster Creation#
#----------------------------------------------------------------------------------------------------------------------------------------------------
my.filepath <- 'C:\\Users\\hailey96\\Documents\\' #this is where you want the data saved because we are pulling it from somewhere else and it is doing calculations 

yr<-as.character(seq(1955,1970))
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

#----------------------------------------------------------------------------------------------------------------------------------------------------
#Changing the working directory of the PRISM data#
#----------------------------------------------------------------------------------------------------------------------------------------------------
setwd(paste(my.filepath,"PRISM\\TMean",sep=""))
dir.create('Monthly Averages')

#----------------------------------------------------------------------------------------------------------------------------------------------------
#Compile PRISM rasters for each individual month of the year for every year of interest#
#----------------------------------------------------------------------------------------------------------------------------------------------------
#For mean temp
setwd(paste(my.filepath,"PRISM\\TMean",sep=""))
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

#For max temp
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


#For min temp
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

#----------------------------------------------------------------------------------------------------------------------------------------------------
#Solar Radiation Calculations#
#----------------------------------------------------------------------------------------------------------------------------------------------------
WB<-readOGR(paste(my.filepath,'EvapInputs.gdb',sep=""),layer="Waterbodies")
WB<-spTransform(WB,'+proj=utm +zone=17 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
WB@data$FID<-1:length(WB@data$COMID)
WB@data$AreaRcalc<-gArea(WB,byid=T)
WB<-spTransform(WB,CRS="+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")
WBCenter<-gCentroid(WB,byid=T,id=WB@data$FID)

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
 plot(crop(MeanTemp_12,BB),main= expression('December Mean Temperature (1955-1970) ('*degree*'C)'),axes=T,xlab='Longitude',ylab='Latitude',cex.lab=2,cex.main=2,cex.axis=2)
 plot(VA,add=T,lwd=2)
 
 # # SOLAR RADIATION 
 par(mar=c(5,6,2,4))
 plot(Ra,type='l',lwd=2,ylab='Mean Radiation (MJ/m^2/day)',xlab='Month',cex.lab=2,cex.axis=2)
 
# # DAYLIGHT HOURS
 par(mar=c(5,6,2,4))
 plot(Ld,type='l',lwd=2,ylab='Mean Daylight Hours (hours)',xlab='Month',cex.lab=2,cex.axis=2)
 
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
 
# # HARGREAVES AND THORNTHWAITE
 ET<-c(mean(WB@data$JanHarg),mean(WB@data$FebHarg),mean(WB@data$MarHarg),mean(WB@data$AprHarg),mean(WB@data$MayHarg),mean(WB@data$JunHarg),mean(WB@data$JulHarg),mean(WB@data$AugHarg),mean(WB@data$SepHarg),mean(WB@data$OctHarg),mean(WB@data$NovHarg),mean(WB@data$DecHarg))
 ET2<-c(mean(WB@data$JanThorn),mean(WB@data$FebThorn),mean(WB@data$MarThorn),mean(WB@data$AprThorn),mean(WB@data$MayThorn),mean(WB@data$JunThorn),mean(WB@data$JulThorn),mean(WB@data$AugThorn),mean(WB@data$SepThorn),mean(WB@data$OctThorn),mean(WB@data$NovThorn),mean(WB@data$DecThorn))
 par(mar=c(5,6,2,4))
 plot(ET,type='l',lwd=2,ylab='Mean PET (mm/day)',xlab='Month',cex.lab=2,cex.axis=2,col='red')
 lines(ET2,col='blue',lwd=2)
 legend(x=1,y=6,bty='n',col=c('red','blue'),legend=c("Hargreaves","Thornthwaite"),lwd=2,cex=2,y.intersp = 0.75)
#===================================================================================
 #Me Experimenting with the data#
 #==================================================================================
 
 ## Min Temp Plot
 plot(crop(MinTemp_12,BB),main=expression('December Min Temperature (1955-1970) ('*degree*'C)'), axes=T, xlab='Longitude',ylab='Latitude',cex.lab=2,cex.main=2,cex.axis=2)
 plot(VA,add=T,lwd=2)
 
 ## Max Temp Plot
 plot(crop(MaxTemp_12,BB),main=expression('December Max Temperature (1955-1970) ('*degree*'C)'), axes=T, xlab='Longitude',ylab='Latitude',cex.lab=2,cex.main=2,cex.axis=2)
 plot(VA,add=T,lwd=2)
 #==================================================================================
 #Difference between high and low to get general temp range per Month
 Diff<-MaxTemp_01 - MinTemp_01

  ## Difference in Temp per Month Plot
 plot(crop(Diff,BB),main=expression('December Max Temperature (1955-1970) ('*degree*'C)'), axes=T, xlab='Longitude',ylab='Latitude',cex.lab=2,cex.main=2,cex.axis=2)
 plot(VA,add=T,lwd=2)
 #==================================================================================
 #Creating Max Temp vector
 MaxTempVec<-c(MaxTemp_01, MaxTemp_02, MaxTemp_03,MaxTemp_04,MaxTemp_05,MaxTemp_06,MaxTemp_07,MaxTemp_08,MaxTemp_09,MaxTemp_10,MaxTemp_11,MaxTemp_12) #A vector of all of the raster data
 MaxTempVecValues<-c(maxValue(MaxTemp_01),maxValue(MaxTemp_02),maxValue(MaxTemp_03),maxValue(MaxTemp_04),maxValue(MaxTemp_05), maxValue(MaxTemp_06), maxValue(MaxTemp_07), maxValue(MaxTemp_08),maxValue(MaxTemp_09), maxValue(MaxTemp_10), maxValue(MaxTemp_11),maxValue(MaxTemp_12)) #A vector of the highest value in each raster
 ## I'm sure there is an easier way to do this 

 #Finding Highest Temp in all Months & which month it is (probably August)
 OverallMax<-max(MaxTempVecValues)#Making a variable that is the highest value out of all months
 for (i in 1:length(MaxTempVecValues)){ #Loop going as long as the vector of maxtemp is
   if (OverallMax==MaxTempVecValues[[i]]){ #Looking to see if the highest value for all months is equal to itself in the max value of each raster vector
     k<-i
     OverallMaxRaster<-MaxTempVec[[k]]#If the highest value is equal to itself I want to know what month that is and store it as another variable so there is no need for human interaction
     #To do that I made the variable k which is equal to i when i is the highest value and since this is months and months are correlated to numbers I can make my highest raster variable equal to the kth value of the raster vector meaning I am pulling the raster with the highest value overall
   }
 }
   
 #Creating Min Temp Vector
 MinxTempVec<-c(MinTemp_01, MinTemp_02, MinTemp_03,MinTemp_04,MinTemp_05,MinTemp_06,MinTemp_07,MinTemp_08,MinTemp_09,MinTemp_10,MinTemp_11,MinTemp_12)
 MinTempVecValues<- c(minValue(MinTemp_01),minValue(MinTemp_02),minValue(MinTemp_03),minValue(MinTemp_04),minValue(MinTemp_05), minValue(MinTemp_06), minValue(MinTemp_07), minValue(MinTemp_08),minValue(MinTemp_09), minValue(MinTemp_10), minValue(MinTemp_11),minValue(MinTemp_12))
 ## I'm sure there is an easier way to do this 
 
 #Finding Lowest Temp in all Months
 OverallMin<-min(MinTempVecValues)#Making a variable that is the lowest value out of all months
 for (i in 1:length(MinTempVecValues)){ #Loop going as long as the vector of mintemp is
   if (OverallMin==MinTempVecValues[[i]]){ #Looking to see if the lowest value of all the months is equal to itsefl
     h<-i
     OverallMinRaster<-MinTempVec[[h]]#If the lowerue is equal to itself I want to know what month that is and store it as another variable so there is no need for human interaction
     #To do that I made the variable k which is equal to i when i is the highest value and since this is months and months are correlated to numbers I can make my highest raster variable equal to the kth value of the raster vector meaning I am pulling the raster with the highest value overall
   }
 }
 
 #Finding biggest temp change over entire year to get range of temps over the year
   DiffYear<-OverallMaxRaster-OverallMinRaster
   
   ## Difference over the Whole Year Plot
   plot(crop(DiffYear,BB),main=expression('Overall Temperature Range (1955-1970) ('*degree*'C)'), axes=T, xlab='Longitude',ylab='Latitude',cex.lab=2,cex.main=2,cex.axis=2)
   plot(VA,add=T,lwd=2)
   
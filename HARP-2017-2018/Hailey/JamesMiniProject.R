##Used HUC6 for basin to show which points were acceptable 

#02042500 Chickahominy River near Providence Forge (Coastal)
#02016000 Cow Pasture River near Clifton Forge (Mountain)
#02035000 James River at Cartersville (Piedmont)
#===============================================================================================================
#Install Packages
install.packages("tidyverse")
install.packages("dataRetrieval")
install.packages("PearsonDS")
install.packages("dplyr")
install.packages("lubridate")
install.packages("zoo")
install.packages("caTools")
install.packages("IHA", repos='http://R-Forge.R-project.org')
install.packages("GSODR")

#===============================================================================================================
#Turn on necessary packages. 
library(dataRetrieval)
library(PearsonDS)
library(dplyr)
library(lubridate)
library(zoo)
library(caTools)
library(IHA) 
library(GSODR)

#================================================================================================================
#Get Site Info 
siteNoC<-"02042500"     #Gauge number for Coastal location 
siteNoP<-"02035000"     #Gauge number for Piedmont location
siteNoM<-"02016000"     #Gauge number for Mountain location
pCode<-'00060'                                      
stat<-'00003'                                       
start.date<-'2007-01-01'                            
end.date<-'2017-12-31'
Coastal<-readNWISdv(siteNoC,pCode,start.date,end.date) #Coastal Data
Piedmont<-readNWISdv(siteNoP,pCode,start.date,end.date) #Piedmont Data
Mountains<-readNWISdv(siteNoM,pCode,start.date,end.date) #Mountain Data

#Organizing Data Coastal
names(Coastal)<-c('Agency', 'Site', 'Date', 'Flow', 'QC') #Renaming columns so they make more sense 
Coastal<-Coastal[order(Coastal$Date),] #Ordering data based on date 

#Organizing Data Piedmont
names(Piedmont)<-c('Agency', 'Site', 'Date', 'Flow', 'QC') #Renaming columns so they make more sense 
Piedmont<-Piedmont[order(Piedmont$Date),] #Ordering data based on date 

#Organizing Data Mountains
names(Mountains)<-c('Agency', 'Site', 'Date', 'Flow', 'QC') #Renaming columns so they make more sense 
Mountains<-Mountains[order(Mountains$Date),] #Ordering data based on date 

#================================================================================================================
#Plotting Flows of Each Individual Area
par(mfrow=c(1,2)) #Lets you plot both graphs in the same window 
plot(Coastal$Date, Coastal$Flow, xlab='Date', ylab='Flow (cfs)', type='l', col='blue', main="Flow of James River In Coastal Area (2007-2017)")
plot(Piedmont$Date, Piedmont$Flow, xlab='Date', ylab='Flow (cfs)', type='l', col='red', main="Flow of James River In Piedmont Area (2007-2017)")
plot(Mountains$Date, Mountains$Flow, xlab='Date', ylab='Flow (cfs)', type='l', col='green', main="Flow of James River in Mountainous Area (2007-2017)")

#=================================================================================================================
#Adjusting for area by doing flow/upstream area
CoastalDetails<-readNWISsite("02042500") #Pulls data on the size of the watershed and locations
CoastalArea<-(Coastal$Flow*3600*12*24)/(CoastalDetails$drain_area_va*27878400) #Dividing flow by area

PiedmontDetials<-readNWISsite("02035000") #Pulls data on the size of the watershed and locations
PiedmontArea<-(Piedmont$Flow*3600*12*24)/(PiedmontDetials$drain_area_va*27878400) #Dividing flow by area

MountainsDetials<-readNWISsite("02016000") #Pulls data on the size of the watershed and locations
MountainsArea<-(Mountains$Flow*3600*12*24)/(MountainsDetials$drain_area_va*27878400) #Dividing flow by area

#Plotting Each Area on Separate Graph with Adjusted Area
par(mfrow=c(1,3))
boxplot(CoastalArea, ylab='Flow (in/day)', main="Coastal Plain", ylim=c(0,0.12))#Creates plot of date on x and flow on y
boxplot(PiedmontArea, ylab='Flow (in/day)', main="Piedmont", ylim=c(0,0.12))#Creates plot of date on x and flow on y
boxplot(MountainsArea, ylab='Flow (in/day)', main="Mountains", ylim=c(0,0.12))#Creates plot of date on x and flow on y


#All on one graph with adjusted area
par(mfrow=c(1,1))
plot(Mountains$Date, MountainsArea, xlab='Year', ylab='Flow (in/day)', type='l', col='green', main="Area-Adjusted James River Flow Across Physiographic Regions (2007-2017)")#Creates plot of date on x and flow on y
lines(Coastal$Date, CoastalArea, col=c('blue'))
lines(Piedmont$Date, PiedmontArea, col='red')
legend("topright", legend=c("Mountains", "Piedmont", "Coastal"), col=c('green','red','blue'), lty=1, bty='n', lwd=1, cex=1)

#==============================================================================================================
#Precip Data
JamesCoastal<-nearest_stations(CoastalDetails$dec_lat_va,CoastalDetails$dec_long_va,25) #Find stations in a 30km radius
JamesCoastalGSODdata<-get_GSOD(years=c(2007:2017),station=JamesCoastal)
(mean(JamesCoastalGSODdata$PRCP,na.rm=T)/25.4)*365

JamesPiedmont<-nearest_stations(PiedmontDetials$dec_lat_va,PiedmontDetials$dec_long_va,50) #Find stations in a 30km radius
JamesPiedmontGSODdata<-get_GSOD(years=c(2007:2017),station=JamesPiedmont)
(mean(JamesPiedmontGSODdata$PRCP,na.rm=T)/25.4)*365

JamesMountain<-nearest_stations(MountainsDetials$dec_lat_va,MountainsDetials$dec_long_va,80) #Find stations in a 30km radius
JamesMountainGSODdata<-get_GSOD(years=c(2007:2017),station=JamesMountain)
(mean(JamesMountainGSODdata$PRCP,na.rm=T)/25.4)*365
#==============================================================================================================
#Get Site Info For Coastal Plains Region
siteNoC1<-"01669520"     #Gauge number for DRAGON SWAMP AT MASCOT, VA
siteNoC2<-"01669000"     #Gauge number for PISCATAWAY CREEK NEAR TAPPAHANNOCK, VA
siteNoC3<-"01673550"     #Gauge number for TOTOPOTOMOY CREEK NEAR STUDLEY, VA
pCode<-'00060'                                      
stat<-'00003'                                       
start.date<-'2007-01-01'                            
end.date<-'2017-12-31'
Coastal1<-readNWISdv(siteNoC1,pCode,start.date,end.date) #Dragon Swamp Data
Coastal2<-readNWISdv(siteNoC2,pCode,start.date,end.date) #Piscataway Data
Coastal3<-readNWISdv(siteNoC3,pCode,start.date,end.date) #Totopotomoy Data

#Organizing Data 
names(Coastal1)<-c('Agency', 'Site', 'Date', 'Flow', 'QC') #Renaming columns so they make more sense 
Coastal1<-Coastal1[order(Coastal1$Date),] #Ordering data based on date 
names(Coastal2)<-c('Agency', 'Site', 'Date', 'Flow', 'QC') #Renaming columns so they make more sense 
Coastal2<-Coastal2[order(Coastal2$Date),] #Ordering data based on date 
names(Coastal3)<-c('Agency', 'Site', 'Date', 'Flow', 'QC') #Renaming columns so they make more sense 
Coastal3<-Coastal3[order(Coastal3$Date),] #Ordering data based on date 

#Plotting Flows of Each Individual Site
par(mfrow=c(2,2)) #Lets you plot both graphs in the same window 
plot(Coastal1$Date, Coastal1$Flow, xlab='Date', ylab='Flow (cfs)', type='l', col='blue', main="Flow at Dragon Swamp (2007-2017)")
plot(Coastal2$Date, Coastal2$Flow, xlab='Date', ylab='Flow (cfs)', type='l', col='red', main="Flow at Piscataway Creek (2007-2017)")
plot(Coastal3$Date, Coastal3$Flow, xlab='Date', ylab='Flow (cfs)', type='l', col='green', main="Flow at Totopotomoy Creek (2007-2017)")

#=================================================================================================================
#Adjusting for area by doing flow/upstream area
CoastalDetails1<-readNWISsite("01669520") #Pulls data on the size of the watershed and locations
CoastalArea1<-(Coastal1$Flow*3600*12*24)/(CoastalDetails1$drain_area_va*27878400) #Dividing flow by area

CoastalDetails2<-readNWISsite("01669000") #Pulls data on the size of the watershed and locations
CoastalArea2<-(Coastal2$Flow*3600*24*12)/(CoastalDetails2$drain_area_va*27878400) #Dividing flow by area

CoastalDetails3<-readNWISsite("01673550") #Pulls data on the size of the watershed and locations
CoastalArea3<-(Coastal3$Flow*3600*24*12)/(CoastalDetails3$drain_area_va*27878400) #Dividing flow by area

#Plotting Each Area on Separate Graph with Adjusted Area
par(mfrow=c(1,3))
boxplot(CoastalArea1, ylab='Flow (in/day)', type='l', main="Dragon Swamp ", ylim=c(0,0.15))#Creates plot of date on x and flow on y
boxplot(CoastalArea2, ylab='Flow (in/day)', type='l', main="Piscataway Creek", ylim=c(0,0.15))#Creates plot of date on x and flow on y
boxplot(CoastalArea3, ylab='Flow (in/day)', type='l', main="Totopotomoy Creek", ylim=c(0,0.15))#Creates plot of date on x and flow on y


#All on one graph with adjusted area
par(mfrow=c(1,1))
plot(Mountains$Date, MountainsArea, xlab='Year', ylab='Flow (in/day)', type='l', col='green', main="Area-Adjusted  Flow of Coastal Plains:Chesapeake Basin (2007-2017)")#Creates plot of date on x and flow on y
lines(Coastal$Date, CoastalArea, col=c('blue'))
lines(Piedmont$Date, PiedmontArea, col='red')
legend("topright", legend=c("Dragon Swamp(Gauge 1)", "Piscataway Creek(Gauge 2)", "Totopotomoy Creek (Gauge 3)"), col=c('green','red','blue'), lty=1, bty='n', lwd=1, cex=1)

#=================================================================================================================
Coastal1Station<-nearest_stations(CoastalDetails1$dec_lat_va,CoastalDetails1$dec_long_va,80) #Find stations in a 30km radius
Coastal1GSODdata<-get_GSOD(years=c(2007:2017),station=Coastal1Station)
(mean(Coastal1GSODdata$PRCP,na.rm=T)/25.4)*365

Coastal2Station<-nearest_stations(CoastalDetails2$dec_lat_va,CoastalDetails2$dec_long_va,80) #Find stations in a 30km radius
Coastal2GSODdata<-get_GSOD(years=c(2007:2017),station=Coastal2Station)
(mean(Coastal2GSODdata$PRCP,na.rm=T)/25.4)*365

Coastal3Stations<-nearest_stations(CoastalDetails3$dec_lat_va,CoastalDetails3$dec_long_va,25) #Find stations in a 30km radius
Coastal3GSODdata<-get_GSOD(years=c(2007:2017),station=Coastal3Stations)
(mean(Coastal3GSODdata$PRCP,na.rm=T)/25.4)*365

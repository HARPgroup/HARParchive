# 24 May 2018 Kelsey Reitz
# Analysis of 3 USGS streamgauges in Roanoke River Watershed, within the Valley and Ridge (western physiographic regions) of VA.
# gauge numbers outlined below. 


#install packages
install.packages("tidyverse")
install.packages("dataRetrieval")
install.packages("zoo")
install.packages("caTools")
install.packages("IHA", repos='http://R-Forge.R-project.org')
install.packages("PearsonDS")
install.packages("dplyr")
install.packages("lubridate")

#initialize packages
library(tidyverse)
library(zoo)
library(caTools)
library(IHA)
library(dataRetrieval)
library(PearsonDS)
library(dplyr)
library(lubridate)



# Load gauge data for counties --------------------------------------------

# Montgomery County: gauge number 02054500
# Botetourt County:  gauge number 02055100
# Roanoke County:   gauge number 02056000
# Get gauge information from USGS
MsiteNo<-"02054500"
BsiteNo<-"02055100"
RsiteNo<-"02056000"
pCode<-'00060'
stat<-'00003' #average daily data. 
start.date<-'2007-01-01'  #Start date is 20 years before 1982
end.date<-'2017-12-31'    #End date is 20 years after 1982

Montgomery<-readNWISdv(MsiteNo,pCode,start.date,end.date)
Botetourt<- readNWISdv(BsiteNo,pCode,start.date,end.date)
Roanoke  <- readNWISdv(RsiteNo,pCode,start.date,end.date)

MonArea <- readNWISsite(MsiteNo)
BotArea <- readNWISsite(BsiteNo)
RoaArea <- readNWISsite(RsiteNo)


#Reorder the column names of USGS into understandable terms. Order the data in order of time.
names(Montgomery) <- c('Agency', 'Site', 'Date', 'Flow', 'QC')
names(Botetourt) <- c('Agency', 'Site', 'Date', 'Flow', 'QC')
names(Roanoke) <- c('Agency', 'Site', 'Date', 'Flow', 'QC')
Montgomery<- Montgomery[order(Montgomery$Date),] 
Botetourt<- Botetourt[order(Botetourt$Date),] 
Roanoke<- Roanoke[order(Roanoke$Date),] 

# Unit Flow for gauges ----------------------------------------------------
convert <- (12*3600*24/27878400)
Montgomery$UnitMon <- (Montgomery$Flow/MonArea$drain_area_va)*convert
Botetourt$UnitBot <- (Botetourt$Flow/BotArea$drain_area_va)*convert
Roanoke$UnitRoa <- (Roanoke$Flow/RoaArea$drain_area_va)*convert


#plot hydrographs for locations
par(mfrow=c(1,3))
plot(Botetourt$Date, Botetourt$Flow, type = 'l', ylim = c(0,20000), ylab='Flow at gauge 1 (cfs)', xlab = 'year', main='Botetourt Station #02055100')
plot(Montgomery$Date, Montgomery$Flow, type = 'l', ylim = c(0,20000), ylab='Flow at gauge 2 (cfs)', xlab = 'year', main='Montgomery Station #0205450')
plot(Roanoke$Date, Roanoke$Flow, type = 'l', ylim = c(0,20000), ylab='Flow at gauge 3 (cfs)', xlab = 'year', main='Roanoke Station #02056000')

#plot unit flow for locations
par(mfrow=c(1,3))
plot(Botetourt$Date, Botetourt$UnitBot, type = 'l', ylim = c(0,2.5), ylab='Flow at gauge 1 (cfs)', xlab = 'year', col='blue', main='Botetourt Station #02055100')
plot(Montgomery$Date, Montgomery$UnitMon, type = 'l', ylim = c(0,2.5), ylab='Flow at gauge 2 (cfs)', xlab = 'year', col='green',main='Montgomery Station #0205450')
plot(Roanoke$Date, Roanoke$UnitRoa, type = 'l', ylim = c(0,2.5), ylab='Flow at gauge 3 (cfs)', xlab = 'year', col='red', main='Roanoke Station #02056000')

#plot all three together
par(mfrow=c(1,1))
plot(Botetourt$Date, Botetourt$UnitBot, type = 'l', ylim = c(0,2.5), ylab='Flow per unit area (in/day)', xlab = 'year', col='blue',  main='Area-Adjusted Roanoke River (and Contributing Rivers) Flow across Valley and Ridge USGS gauges')
lines(Roanoke$Date, Roanoke$UnitRoa, type = 'l', col ='red')
lines(Montgomery$Date, Montgomery$UnitMon, type = 'l', col = 'green')
legend("topright", legend= c("Botetourt (gauge 1)","Montgomery (gauge 2)", "Roanoke (gauge 3)"),col=c('blue','green', 'red'), cex=1.25, bty='n', lty=1, x.intersp = 0.5, y.intersp = 0.75)


# Plot Peak Data ---------------------------------------------------------------
#plot peak vs USGS Botetourt and Montgomery and Roanoke

#first need to create a year column in peak_Botetourt and peak_Montgomery
peak_Botetourt$Year <- year(peak_Botetourt$peak_dt)
peak_Montgomery$Year <- year(peak_Montgomery$peak_dt)
peak_Roanoke$Year <- year(peak_Roanoke$peak_dt)

par(mfrow=c(1,3))
plot(peak_Botetourt$Year, peak_Botetourt$peak_va, xlab='Date', ylab='Flow(cfs)', type='l', ylim=c(0,6500))
lines(Botetourt_peaks$Year, Botetourt_peaks$MaxFlow, col = 'blue')
legend(2008, 5000, legend=c("USGS Peak Flow","Predicted Peak Flow"),col=c('black','blue'), cex=1, bty='n', lty=1, x.intersp = 0.5, y.intersp = 0.75)

plot(peak_Roanoke$Year, peak_Roanoke$peak_va, xlab='Date', ylab='Flow (cfs)', type = 'l')
lines(Roanoke_peaks$Year, Roanoke_peaks$MaxFlow, col='blue', type='l', xlab='Date', ylab='Flow (cfs)', ylim=c(0,6500))


plot(peak_Montgomery$Year, peak_Montgomery$peak_va, xlab='Date', ylab='Flow (cfs)', type = 'l', ylim=c(0,6500))
lines(Montgomery_peaks$Year, Montgomery_peaks$MaxFlow, col = 'blue', type='l', xlab='Date', ylab='Flow (cfs)')




# Load gauge data for counties outside of Valley and Ridge --------------------------------------------

# Roanoke2 County (Calling D or Dundee): gauge number 02056650
# Mecklenburg County (calling S or SoHo):  gauge number 02079640
# Get gauge information from USGS
DsiteNo<-"02056650"
SsiteNo<-"02079640"
pCode<-'00060'
stat<-'00003' #average daily data. 
start.date<-'2007-01-01'  #Start date is 20 years before 1982
end.date<-'2017-12-31'    #End date is 20 years after 1982

Dundee<-readNWISdv(DsiteNo,pCode,start.date,end.date)
SoHo<- readNWISdv(SsiteNo,pCode,start.date,end.date)

DunArea <- readNWISsite(MsiteNo)
SoHoArea <- readNWISsite(SsiteNo)

#Reorder the column names of USGS into understandable terms. Order the data in order of time.
names(Dundee) <- c('Agency', 'Site', 'Date', 'Flow', 'QC')
names(SoHo) <- c('Agency', 'Site', 'Date', 'Flow', 'QC')
Dundee<- Dundee[order(Dundee$Date),] 
SoHo<- SoHo[order(SoHo$Date),] 

# Unit Flow for gauges ----------------------------------------------------

Dundee$UnitDun <- Dundee$Flow*convert/DunArea$drain_area_va
SoHo$UnitSoHo <- SoHo$Flow*convert/SoHoArea$drain_area_va


#plot hydrographs for locations
par(mfrow=c(1,3))
plot(Dundee$Date, Dundee$Flow, type = 'l', ylim = c(0,20000), ylab='Flow at gauge 1 (cfs)', xlab = 'year', main='Dundee Station #02056650')
plot(SoHo$Date, SoHo$Flow, type = 'l', ylim = c(0,20000), ylab='Flow at gauge 2 (cfs)', xlab = 'year', main='Mecklenburg Station #02079640')
plot(Roanoke$Date, Roanoke$Flow, type = 'l', ylim = c(0,20000), ylab='Flow at gauge 3 (cfs)', xlab = 'year', main='Roanoke Station #02056000')

#plot unit hydrographs for locations
par(mfrow=c(1,3))
plot(Dundee$Date, Dundee$UnitDun, type = 'l', ylim = c(0,2.5), ylab='Flow at gauge 1 (cfs)', xlab = 'year', col='blue', main='Dundee Station #02056650')
plot(SoHo$Date, SoHo$UnitSoHo, type = 'l', ylim = c(0,2.5), ylab='Flow at gauge 2 (cfs)', xlab = 'year', col='green',main='Mecklenburg Station #02079640')
plot(Roanoke$Date, Roanoke$UnitRoa, type = 'l', ylim = c(0,2.5), ylab='Flow at gauge 3 (cfs)', xlab = 'year', col='red', main='Roanoke Station #02056000')

#plot all three together
par(mfrow=c(1,1))
plot(SoHo$Date, SoHo$UnitSoHo, type = 'l', col = 'green', ylim = c(0,1.65), ylab='Flow per unit area (in/day)', xlab = 'year', main='Area-Adjusted Roanoke River (and Contributing Rivers) Flow across Physiographic Regions')
lines(Roanoke$Date, Roanoke$UnitRoa, type = 'l', col ='red')
lines(Dundee$Date, Dundee$UnitDun, type = 'l', col='blue')
legend("topright", legend= c("Mecklenburg (gauge 1)", "Roanoke (gauge 2)", "Dundee (gauge 3)"),col=c('green', 'red', 'blue'), cex=1.25, bty='n', lty=1, x.intersp = 0.5, y.intersp = 0.75)



# Boxplot Analysis --------------------------------------------------------
par(mfrow=c(1,3))
boxplot(Botetourt$UnitBot, type='l', main='Botetourt', ylim=c(0,.1))
boxplot( Montgomery$UnitMon, type='l', main='Montgomery', ylim=c(0,.1))
boxplot(Roanoke$UnitRoa,  type='l', main='Roanoke', ylim=c(0,.1))
par(mfrow=c(1,3))
boxplot(Dundee$UnitDun,  type='l', main='Dundee', ylim=c(0,.1))
boxplot(SoHo$UnitSoHo,  type='l', main='Mecklenburg', ylim=c(0,.1))
boxplot(Roanoke$UnitRoa,  type='l', main='Roanoke', ylim=c(0,.1))







# Lubridate package: create new columns -----------------------------------

#create new columns using the lubridate package for Botetourt
Botetourt$Month <- month(Botetourt$Date)
Botetourt$Year <- year(Botetourt$Date)
Botetourt_aligroup <- group_by(Botetourt, Year)
Botetourt_peaks <- as.data.frame(summarize(Botetourt_aligroup, MaxFlow=max(Flow))) #finds max flow for each year and stores in data frame
peak_Botetourt <- readNWISpeak(BsiteNo, start.date, end.date) #USGS does same thing from NWIS

#create new columns using the lubridate package for Montgomery
Montgomery$Month <- month(Montgomery$Date)
Montgomery$Year <- year(Montgomery$Date)
Montgomery_aligroup <- group_by(Montgomery, Year)
Montgomery_peaks <- as.data.frame(summarize(Montgomery_aligroup, MaxFlow=max(Flow))) #finds max flow for each year and stores in data frame
peak_Montgomery <- readNWISpeak(BsiteNo, start.date, end.date) #USGS does same thing from NWIS

#create new columns using the lubridate package for Roanoke
Roanoke$Month <- month(Roanoke$Date)
Roanoke$Year <- year(Roanoke$Date)
Roanoke_aligroup <- group_by(Roanoke, Year)
Roanoke_peaks <- as.data.frame(summarize(Roanoke_aligroup, MaxFlow=max(Flow))) #finds max flow for each year and stores in data frame
peak_Roanoke <- readNWISpeak(RsiteNo, start.date, end.date) #USGS does same thing from NWIS

#plot()





# GIS Impervious Area Analysis --------------------------------------------

# display area data in m^2. 
Water<- 602171 * 30^2 / (1000^2)
Impervious<- 700489 * 30^2 / (1000^2)
Pervious<- 26804798 * 30^2 / (1000^2)
RHuc6 <- Water + Impervious + Pervious

WPer <- Water/RHuc6 *100
IPer <- Impervious/RHuc6 *100
PPer <- Pervious/RHuc6 * 100

Water; Impervious; Pervious; RHuc6

WPer; IPer; PPer; 

WPer + IPer + PPer

# calculate from HUC 6 area of 25297.03
25297.03*2.14/100
25297.03*2.49/100
25297.03*95.37/100


541.35 + 629.90 + 24125.78


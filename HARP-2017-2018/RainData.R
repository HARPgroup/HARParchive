# 25 May 2018 Kelsey Reitz
# program should coordinate flow and precipitation for Roanoke, VA

#initialize packages
library(zoo)
library(IHA)
library(dataRetrieval)
library(PearsonDS)
library(dplyr)
library(lubridate)




# Import rain gauge information -------------------------------------------
library(GSODR)

Roanoke<-readNWISsite("02056000")
Montgomery <- readNWISsite("02054500")
Botetourt<- readNWISsite("02055100")
Dundee <- readNWISsite("02056650")
SoHo <- readNWISsite("02079640")

wxstns<-nearest_stations(SoHo$dec_lat_va,SoHo$dec_long_va,60) #search 60 km radius SoHo, 30 for others


#Load weather stations within a 30km radius
SoHo_GSODdata<-get_GSOD(years=c(2007:2017),station=wxstns)

#Check the data to make sure correct download: https://www.wunderground.com/history/airport/KPSK/2017/1/1/CustomHistory.html?dayend=1&monthend=1&yearend=2018&req_city=&req_state=&req_statename=&reqdb.zip=&reqdb.magic=&reqdb.wmo=

# sum(GSODdata$PRCP,na.rm=T)/25.4
# mean(GSODdata$PRCP,na.rm=T)/25.4*365 # get average yearly precipitation over 10 years

mean(Roanoke_GSODdata$PRCP,na.rm=T)/25.4*365
mean(Montgomery_GSODdata$PRCP,na.rm=T)/25.4*365
mean(Botetourt_GSODdata$PRCP,na.rm=T)/25.4*365
mean(Dundee_GSODdata$PRCP,na.rm=T)/25.4*365
mean(SoHo_GSODdata$PRCP,na.rm=T)/25.4*365

#units in inches for mean
sum(Roanoke_GSODdata$PRCP,na.rm=T)/25.4
mean(Roanoke_GSODdata$PRCP,na.rm=T)/25.4*365/12

sum(Montgomery_GSODdata$PRCP,na.rm=T)/25.4
mean(Montgomery_GSODdata$PRCP,na.rm=T)/25.4*365/12

sum(Botetourt_GSODdata$PRCP,na.rm=T)/25.4
mean(Botetourt_GSODdata$PRCP,na.rm=T)/25.4*365/12

sum(Dundee_GSODdata$PRCP,na.rm=T)/25.4
mean(Dundee_GSODdata$PRCP,na.rm=T)/25.4*365/12

sum(SoHo_GSODdata$PRCP,na.rm=T)/25.4/12
mean(SoHo_GSODdata$PRCP,na.rm=T)/25.4*365/12



#get drainage area for station from mi^2 to ft^2
Rodrain_area <- Roanoke$drain_area_va*27878400
Montdrain_area <- Montgomery$drain_area_va*27878400
Botdrain_area <- Botetourt$drain_area_va*27878400
Dunddrain_area <- Dundee$drain_area_va*27878400
SoHodrain_area <- SoHo$drain_area_va*27878400

#get volume:in cubic feet
Rodrain_area*mean(Roanoke_GSODdata$PRCP,na.rm=T)/25.4*365/12
Montdrain_area*mean(Montgomery_GSODdata$PRCP, na.rm=T)/25.4*365/12
Botdrain_area*mean(Botetourt_GSODdata$PRCP,na.rm=T)/25.4*365/12
Dunddrain_area*mean(Dundee_GSODdata$PRCP,na.rm=T)/25.4*365/12
SoHodrain_area*mean(SoHo_GSODdata$PRCP,na.rm=T)/25.4*365/12






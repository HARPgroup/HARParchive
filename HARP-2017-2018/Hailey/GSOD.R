library(dataRetrieval)
library(GSODR)

Allisonia<-readNWISsite("03171000")

wxstns<-nearest_stations(Allisonia$dec_lat_va,Allisonia$dec_long_va,30)
#Load weather stations within a 30km raidus
GSODdata<-get_GSOD(years=c(2017,2018),station=wxstns)

#Check the data to make sure correct download: https://www.wunderground.com/history/airport/KPSK/2017/1/1/CustomHistory.html?dayend=1&monthend=1&yearend=2018&req_city=&req_state=&req_statename=&reqdb.zip=&reqdb.magic=&reqdb.wmo=
sum(GSODdata$PRCP,na.rm=T)/25.4#Too much, need closer to 25 inches per year


wxstns<-nearest_stations(Allisonia$dec_lat_va,Allisonia$dec_long_va,15)
GSODdata<-get_GSOD(years=c(2017,2018),station=wxstns)
sum(GSODdata$PRCP,na.rm=T)/25.4#Better, but still not perfect
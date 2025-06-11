install.packages("dataRetrieval")
library(dataRetrieval)
library(lubridate)
library(sqldf)
library(ggplot2)
#01634000 Shen
#02055000 Roa
#03524000 Clinch
#10132000 Weber

# 30 years
Shenandoah_cfs <-readNWISdata(sites = "01634000",
             parameterCD=c("00060"),
             startDate = "1994-01-01",
            endDate = "2024-12-31")


Roanoke_cfs <-readNWISdata(sites = "02055000",
                             parameterCD=c("00060"),
                             startDate = "1994-01-01",
                             endDate = "2024-12-31")


Clinch_cfs <-readNWISdata(sites = "03524000",
                             parameterCD=c("00060"),
                             startDate = "1994-01-01",
                             endDate = "2024-12-31")

Weber_cfs <-readNWISdata(sites = "10132000",
                             parameterCD=c("00060"),
                             startDate = "1994-01-01",
                             endDate = "2024-12-31")

Shenandoah_cfs <- sqldf(
  "Select dateTime, X_00060_00003 as Discharge_cfs
  from Shenandoah_cfs
  "
)

Roanoke_cfs <- sqldf(
  "Select dateTime, X_00060_00003 as Discharge_cfs
  from Roanoke_cfs
  "
)

Clinch_cfs <- sqldf(
  "Select dateTime, X_00060_00003 as Discharge_cfs
  from Clinch_cfs
  "
)

Weber_cfs <- sqldf(
  "Select dateTime, X_00060_00003 as Discharge_cfs
  from Weber_cfs
  "
)

ggplot(data=Shenandoah_cfs, mapping = aes(x=dateTime, y= Discharge_cfs))+
  geom_line(color="red")+labs(title = "Shenandoah (Strasburg, VA) cfs/day 1994-2024")+xlab("Date")+ylab("Discharge cfs")

ggplot(data=Roanoke_cfs, mapping = aes(x=dateTime, y= Discharge_cfs))+
  geom_line(color="blue")+labs(title = "Roanoke River(Roanoke,VA) cfs/day 1994-2024")+xlab("Date")+ylab("Discharge cfs")

ggplot(data=Clinch_cfs, mapping = aes(x=dateTime, y= Discharge_cfs))+
  geom_line(color="purple")+labs(title = "Clinch (Cleveland,VA) cfs/day 1994-2024")+xlab("Date")+ylab("Discharge cfs")

ggplot(data=Weber_cfs, mapping = aes(x=dateTime, y= Discharge_cfs))+
  geom_line(color="orange")+labs(title = "Weber (Echo, UT) cfs/day 1994-2024")+xlab("Date")+ylab("Discharge cfs")

# 5 Years


lims5year <- as.POSIXct(strptime(c("2019-01-01 19:00:00" , "2024-12-30 19:00:00"),
                                 format = "%Y-%m-%d %H:%M"))

ggplot(data=Shenandoah_cfs, mapping = aes(x=dateTime, y= Discharge_cfs))+
  geom_line(color="red")+
  labs(title = "Shenandoah (Strasburg, VA) cfs/day 2019-2024")+xlab("Date")+
  xlim(lims5year)+ylab("Discharge cfs")
  

ggplot(data=Roanoke_cfs, mapping = aes(x=dateTime, y= Discharge_cfs))+
  geom_line(color="blue")+labs(title = "Roanoke River(Roanoke,VA) cfs/day 2019-2024")+xlab("Date")+
  xlim(lims5year)+ylab("Discharge cfs")

ggplot(data=Clinch_cfs, mapping= aes(x=dateTime, y= Discharge_cfs))+
  geom_line(color="purple")+labs(title = "Clinch (Cleveland,VA) cfs/day 2019-2024")+xlab("Date")+
  xlim(lims5year)+ylab("Discharge cfs")

ggplot(data=Weber_cfs, mapping = aes(x=dateTime, y= Discharge_cfs))+
  geom_line(color="orange")+labs(title = "Weber (Echo, UT) cfs/day 2019-2024")+xlab("Date")+
  xlim(lims5year)+ylab("Discharge cfs")

#2014
lim2014<- as.POSIXct(strptime(c("2014-01-01 19:00:00" , "2014-12-30 19:00:00"),
                                 format = "%Y-%m-%d %H:%M"))

ggplot(data=Shenandoah_cfs, mapping = aes(x=dateTime, y= Discharge_cfs))+
  geom_line(color="red")+labs(title = "Shenandoah (Strasburg, VA) cfs/day 2014")+xlab("Date")+
  xlim(lim2014)+ylab("Discharge cfs")

ggplot(data=Roanoke_cfs, mapping = aes(x=dateTime, y= Discharge_cfs))+
  geom_line(color="blue")+labs(title = "Roanoke River (Roanoke, VA) cfs/day 2014")+xlab("Date")+
  xlim(lim2014)+ylab("Discharge cfs")

ggplot(data=Clinch_cfs, mapping = aes(x=dateTime, y= Discharge_cfs))+
  geom_line(color="purple")+labs(title = "Clinch (Cleveland,VA) cfs/day 2014")+xlab("Date")+
  xlim(lim2014)+ylab("Discharge cfs")

ggplot(data=Weber_cfs, mapping = aes(x=dateTime, y= Discharge_cfs))+
  geom_line(color="orange")+labs(title = "Weber (Echo, UT) cfs/day 2014")+xlab("Date")+
xlim(lim2014)+ylab("Discharge cfs")

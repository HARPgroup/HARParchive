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
  geom_line(color="red")+labs(title = "Shenandoah (Strasburg, VA) cfs/day 1994-2024")+xlab("Date")

ggplot(data=Roanoke_cfs, mapping = aes(x=dateTime, y= Discharge_cfs))+
  geom_line(color="blue")+labs(title = "Roanoke River(Roanoke,VA) cfs/day 1994-2024")+xlab("Date")

ggplot(data=Clinch_cfs, mapping = aes(x=dateTime, y= Discharge_cfs))+
  geom_line(color="green")+labs(title = "Clinch (Cleveland,VA) cfs/day 1994-2024")+xlab("Date")

ggplot(data=Weber_cfs, mapping = aes(x=dateTime, y= Discharge_cfs))+
  geom_line(color="orange")+labs(title = "Weber (Echo, UT) cfs/day 1994-2024")+xlab("Date")

# 5 Years

Shenandoah_5Year_cfs <-readNWISdata(sites = "01634000",
                              parameterCD=c("00060"),
                              startDate = "2019-01-01",
                              endDate = "2024-12-31")


Roanoke_5Year_cfs <-readNWISdata(sites = "02055000",
                           parameterCD=c("00060"),
                           startDate = "2019-01-01",
                           endDate = "2024-12-31")


Clinch_5Year_cfs <-readNWISdata(sites = "03524000",
                          parameterCD=c("00060"),
                          startDate = "2019-01-01",
                          endDate = "2024-12-31")

Weber_5Year_cfs <-readNWISdata(sites = "10132000",
                         parameterCD=c("00060"),
                         startDate = "2019-01-01",
                         endDate = "2024-12-31")

Shenandoah_5Year_cfs <- sqldf(
  "Select dateTime, X_00060_00003 as Discharge_cfs
  from Shenandoah_5Year_cfs
  "
)

Roanoke_5Year_cfs <- sqldf(
  "Select dateTime, X_00060_00003 as Discharge_cfs
  from Roanoke_5Year_cfs
  "
)

Clinch_5Year_cfs <- sqldf(
  "Select dateTime, X_00060_00003 as Discharge_cfs
  from Clinch_5Year_cfs
  "
)

Weber_5Year_cfs <- sqldf(
  "Select dateTime, X_00060_00003 as Discharge_cfs
  from Weber_5Year_cfs
  "
)

ggplot(data=Shenandoah_5Year_cfs, mapping = aes(x=dateTime, y= Discharge_cfs))+
  geom_line(color="red")+labs(title = "Shenandoah (Strasburg, VA) cfs/day 2019-2024")+xlab("Date")

ggplot(data=Roanoke_5Year_cfs, mapping = aes(x=dateTime, y= Discharge_cfs))+
  geom_line(color="blue")+labs(title = "Roanoke River(Roanoke,VA) cfs/day 2019-2024")+xlab("Date")

ggplot(data=Clinch_5Year_cfs, mapping = aes(x=dateTime, y= Discharge_cfs))+
  geom_line(color="green")+labs(title = "Clinch (Cleveland,VA) cfs/day 2019-2024")+xlab("Date")

ggplot(data=Weber_5Year_cfs, mapping = aes(x=dateTime, y= Discharge_cfs))+
  geom_line(color="orange")+labs(title = "Weber (Echo, UT) cfs/day 2019-2024")+xlab("Date")

#2014

Shenandoah_2014_cfs <-readNWISdata(sites = "01634000",
                              parameterCD=c("00060"),
                              startDate = "2014-01-01",
                              endDate = "2014-12-31")


Roanoke_2014_cfs <-readNWISdata(sites = "02055000",
                           parameterCD=c("00060"),
                           startDate = "2014-01-01",
                           endDate = "2014-12-31")


Clinch_2014_cfs <-readNWISdata(sites = "03524000",
                          parameterCD=c("00060"),
                          startDate = "2014-01-01",
                          endDate = "2014-12-31")

Weber_2014_cfs <-readNWISdata(sites = "10132000",
                         parameterCD=c("00060"),
                         startDate = "2014-01-01",
                         endDate = "2014-12-31")

Shenandoah_2014_cfs <- sqldf(
  "Select dateTime, X_00060_00003 as Discharge_cfs
  from Shenandoah_2014_cfs
  "
)

Roanoke_2014_cfs <- sqldf(
  "Select dateTime, X_00060_00003 as Discharge_cfs
  from Roanoke_2014_cfs
  "
)

Clinch_2014_cfs <- sqldf(
  "Select dateTime, X_00060_00003 as Discharge_cfs
  from Clinch_2014_cfs
  "
)

Weber_2014_cfs <- sqldf(
  "Select dateTime, X_00060_00003 as Discharge_cfs
  from Weber_2014_cfs
  "
)

ggplot(data=Shenandoah_2014_cfs, mapping = aes(x=dateTime, y= Discharge_cfs))+
  geom_line(color="red")+labs(title = "Shenandoah (Strasburg, VA) cfs/day 2014")+xlab("Date")

ggplot(data=Roanoke_2014_cfs, mapping = aes(x=dateTime, y= Discharge_cfs))+
  geom_line(color="blue")+labs(title = "Roanoke River (Roanoke, VA) cfs/day 2014")+xlab("Date")

ggplot(data=Clinch_2014_cfs, mapping = aes(x=dateTime, y= Discharge_cfs))+
  geom_line(color="green")+labs(title = "Clinch (Cleveland,VA) cfs/day 2014")+xlab("Date")

ggplot(data=Weber_2014_cfs, mapping = aes(x=dateTime, y= Discharge_cfs))+
  geom_line(color="orange")+labs(title = "Weber (Echo, UT) cfs/day 2014")+xlab("Date")

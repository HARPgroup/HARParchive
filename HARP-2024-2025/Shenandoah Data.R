library(lubridate)
library(sqldf)
library(ggplot2)
Cootes_Store <- dataRetrieval::readNWISdv("01632000",parameterCd = "00060")

Mount_Jackson <- dataRetrieval::readNWISdv("01633000",parameterCd = "00060")

Strasburg <- dataRetrieval::readNWISdv("01634000",parameterCd = "00060")

Cootes_Store_cfs <- sqldf(
  "Select date, X_00060_00003 as Discharge_cfs
  from Cootes_Store
  "
)
  Mount_Jackson_cfs <- sqldf(
    "Select date, X_00060_00003 as Discharge_cfs
  from Mount_Jackson
  "
  )
    Strasburg_cfs <- sqldf(
      "Select date, X_00060_00003 as Discharge_cfs
  from Strasburg
  "
    )
    
ggplot(data=Cootes_Store_cfs, mapping = aes(x=Date, y= Discharge_cfs))+
      geom_line(color="red")+
  labs(title = "Shenandoah (Cootes Store, VA) cfs/day")+
  xlab("Date")+ylab("Discharge cfs")

ggplot(data=Mount_Jackson_cfs, mapping = aes(x=Date, y= Discharge_cfs))+
  geom_line(color="blue")+
  labs(title = "Shenandoah (Mount Jackson, VA) cfs/day")+
  xlab("Date")+ylab("Discharge cfs")

ggplot(data=Strasburg_cfs, mapping = aes(x=Date, y= Discharge_cfs))+
  geom_line(color="purple")+
  labs(title = "Shenandoah (Strasburg, VA) cfs/day")+
  xlab("Date")+ylab("Discharge cfs")

min_cootes <- min(Cootes_Store_cfs$Discharge_cfs, na.rm=T)
Cootes_min_date <- Cootes_Store_cfs[Cootes_Store_cfs$Discharge_cfs == min_cootes, "Date"]

min_Jackson <- min(Mount_Jackson_cfs$Discharge_cfs, na.rm=T)
Jackson_min_date <- Mount_Jackson_cfs[Mount_Jackson_cfs$Discharge_cfs == min_Jackson, "Date"]

min_Strasburg <- min(Strasburg_cfs$Discharge_cfs, na.rm=T)
Strasburg_min_date <- Strasburg_cfs[Cootes_Store_cfs$Discharge_cfs == min_Strasburg, "Date"]



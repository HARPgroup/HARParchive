library(hydrotools)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(sqldf)
library(grwat)

#load in stream data from USGS
flows_MJ <- dataRetrieval::readNWISdv("01633000",parameterCd = "00060")
flows_MJ <- dataRetrieval::renameNWISColumns(flows_MJ)

flows_CS <- dataRetrieval::readNWISdv("01632000",parameterCd = "00060")
flows_CS <- dataRetrieval::renameNWISColumns(flows_CS)

flows_S <- dataRetrieval::readNWISdv("01634000",parameterCd = "00060")
flows_S <- dataRetrieval::renameNWISColumns(flows_S)

base_MJ <- flows_MJ %>% 
  mutate(Qbase = gr_baseflow(Flow, method = 'lynehollick'))

base_CS <- flows_CS %>% 
  mutate(Qbase = gr_baseflow(Flow, method = 'lynehollick'))

base_S <- flows_S %>% 
  mutate(Qbase = gr_baseflow(Flow, method = 'lynehollick'))

ggplot()+
  geom_line(data=base_MJ, aes(x=Date, y=Qbase), color="red")+
  xlab('Year')+
  ylab('Baseflow in CFS')+
  ggtitle('Lynehollick Baseflow Estimation for Mount Jackson')+
  theme_minimal()

ggplot()+
  geom_line(data=base_CS, aes(x=Date, y=Qbase), color="blue")+
  xlab('Year')+
  ylab('Baseflow in CFS')+
  ggtitle('Lynehollick Baseflow Estimation for Cootes Store')+
  theme_minimal()

ggplot()+
  geom_line(data=base_S, aes(x=Date, y=Qbase), color="purple")+
  xlab('Year')+
  ylab('Baseflow in CFS')+
  ggtitle('Lynehollick Baseflow Estimation for Strasburg')+
  theme_minimal()

flows_MJ <- flows_MJ %>%
  left_join(base_MJ %>% select(Date, Qbase), by = "Date")

flows_CS <- flows_CS %>%
  left_join(base_CS %>% select(Date, Qbase), by = "Date")

flows_S <- flows_S %>%
  left_join(base_S %>% select(Date, Qbase), by = "Date")


ggplot(data=flows_MJ, aes(x=Date))+
  geom_line(aes(y= Flow), color = "blue")+
  geom_line(aes(y = Qbase), color ="red")+
  scale_x_date(limits = as.Date(c("1948-08-04", "1948-08-19"))) +
  ggtitle("Baseflow vs. Flow During Recession Event 24 and 25 Mount Jackson (1948") +
  ylim(0, 3000) +
  ylab("Discharge (CFS)") +
  theme_minimal()

ggplot(data=flows_CS, aes(x=Date))+
  geom_line(aes(y= Flow), color = "blue")+
  geom_line(aes(y = Qbase), color ="red")+
  scale_x_date(limits = as.Date(c("1929-10-02", "1929-10-10"))) +
  ggtitle("Baseflow vs. Flow During Recession Event 17 Cootes Store (1929)") +
  ylim(0, 3000) +
  ylab("Discharge (CFS)") +
  theme_minimal()

ggplot(data=flows_S, aes(x=Date))+
  geom_line(aes(y= Flow), color = "blue")+
  geom_line(aes(y = Qbase), color ="red")+
  scale_x_date(limits = as.Date(c("1928-04-13", "1928-04-20"))) +
  ggtitle("Baseflow vs. Flow During Recession Event 6 Strasburg (1948)") +
  ylim(0, 3000) +
  ylab("Discharge (CFS)") +
  theme_minimal()

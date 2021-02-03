#4/3/2019

#code will compare spring hollow and wayside flows and withdrawals


rm(list = ls())
library(lubridate)
library(dataRetrieval)
library(ggplot2)
library(scales)
#set workspace
setwd("C:/Users/Kelsey/Desktop/GitHub/hydro-tools/HARP-2018/PointSource_RoanokeAnalysis/VAHydro_Model")

#--------------------------------
# pull in flow data from the vahydro code (data stored as csv)
#--------------------------------

wayside_Q <- read.csv("vahydro_wayside_flow.csv", stringsAsFactors = F)
upstream_Q <- read.csv("vahydro_upstream_flow.csv")

#format data
wayside_Q$timestamp <- as.Date(as.character(wayside_Q$timestamp))
wayside_Q$Qout <- as.numeric(as.character(wayside_Q$Qout))
upstream_Q$timestamp <- as.Date(as.character(upstream_Q$timestamp))
upstream_Q$Qout <- as.numeric(as.character(upstream_Q$Qout))
#--------------------------------
#pull in withdrawal data (data stored as csv)
#--------------------------------
# pull VAHydro data for wayside and spring hollow (downloaded from VAHydro in stored csv files) ------

#read the csvs
wayside_W <- read.csv("C:/Users/Kelsey/Desktop/GitHub/hydro-tools/HARP-2018/PointSource_RoanokeAnalysis/wayside_updated.csv", stringsAsFactors = F)
spholla_W <- read.csv("C:/Users/Kelsey/Desktop/GitHub/hydro-tools/HARP-2018/PointSource_RoanokeAnalysis/spholla_updated.csv", stringsAsFactors = FALSE)

#fix format of data
wayside_W$Date <- as.Date(wayside_W$Date) 
spholla_W$Date <- as.Date(spholla_W$Date)
wayside_W$Value <- as.numeric(gsub(",", "",wayside_W$Value)) 
spholla_W$Value <- as.numeric(gsub(",", "",spholla_W$Value))

#pull the data of interest out (mgm monthly withdrawal)
wayside_W <- subset(wayside_W, varkey=="wd_mgm")
spholla_W <- subset(spholla_W, varkey=="wd_mgm")

# units are in mgd
#convert mgd to cfs for vahydro 
#    MGD  <-  gal/Mgal *  ft3/gal    *   day/hr *  hr/sec
unit_conv <-  (10^6)   * (1/7.480519) *  (1/24)  * (1/3600)
wayside_W$Value <- wayside_W$Value * unit_conv
spholla_W$Value <- spholla_W$Value * unit_conv

#--------------------------------
# pull in data from gages (NWIS)
#--------------------------------
upstream_gage <- readNWISdv('02054500', parameterCd='00060', startDate = "1984-01-01", 
                   endDate = "2005-12-31", statCd = "00003")
wayside_gage <- readNWISdv('02054530', parameterCd='00060', startDate = "1984-01-01", 
                            endDate = "2005-12-31", statCd = "00003")


#--------------------------------
# plot upstream model vs gage for vahydro
#--------------------------------
upstream_view <- ggplot(upstream_gage, aes(Date)) + 
  geom_line(aes(y=X_00060_00003, colour="gage"), size=0.5) + 
  geom_line(data=upstream_Q, aes(x=timestamp, y=Qout, colour="vahydromodel"), size=0.7) + 
  scale_colour_manual(values=c("blue", "red")) + 
  labs(x="Date", y="Flow [cfs]", colour="Legend") + 
  coord_cartesian( xlim=c(as.Date("1984-01-01"), as.Date("2005-12-31"))) + 
  ggtitle("Upstream gage v. vahydro model") + 
  scale_y_continuous(trans = log_trans(), 
                    breaks = c(10, 100, 1000, 10000, 10000), 
                    limits=c(1,10000))
ggsave(file="upstream_gage_v_vahydro.png", width=9, height=5, units="in")


#zoom the same thing down to 2002
upstream_view <- ggplot(upstream_gage, aes(Date)) + 
  geom_line(aes(y=X_00060_00003, colour="gage"), size=0.5) + 
  geom_line(data=upstream_Q, aes(x=timestamp, y=Qout, colour="vahydromodel"), size=0.7) + 
  scale_colour_manual(values=c("blue", "red")) + 
  labs(x="Date", y="Flow [cfs]", colour="Legend") + 
  coord_cartesian(xlim=c(as.Date("2002-01-01"), as.Date("2002-12-31"))) + 
  ggtitle("Upstream gage v. vahydro model 2002") + 
  scale_y_continuous(trans = log_trans(), 
                     breaks = c(10, 100, 1000, 10000, 10000), 
                     limits=c(1,10000))
ggsave(file="upstream_gage_v_vahydro_2002.png", width=9, height=5, units="in")



#--------------------------------
# plot wayside model vs gage for vahydro
#--------------------------------
upstream_view <- ggplot(wayside_gage, aes(Date)) + 
  geom_line(aes(y=X_00060_00003, colour="gage"), size=0.5) + 
  geom_line(data=wayside_Q, aes(x=timestamp, y=Qout, colour="vahydromodel"), size=0.7) + 
  scale_colour_manual(values=c("blue", "red")) + 
  labs(x="Date", y="Flow [cfs]", colour="Legend") + 
  coord_cartesian( xlim=c(as.Date("1984-01-01"), as.Date("2005-12-31"))) + 
  ggtitle("Wayside gage v. vahydro model") + 
  scale_y_continuous(trans = log_trans(), 
                     breaks = c(10, 100, 1000, 10000, 10000), 
                     limits=c(1,10000))
ggsave(file="wayside_gage_v_vahydro.png", width=9, height=5, units="in")


#zoom the same thing down to 2002
upstream_view <- ggplot(wayside_gage, aes(Date)) + 
  geom_line(aes(y=X_00060_00003, colour="gage"), size=0.5) + 
  geom_line(data=wayside_Q, aes(x=timestamp, y=Qout, colour="vahydromodel"), size=0.7) + 
  scale_colour_manual(values=c("blue", "red")) + 
  labs(x="Date", y="Flow [cfs]", colour="Legend") + 
  coord_cartesian(xlim=c(as.Date("2002-01-01"), as.Date("2002-12-31"))) + 
  ggtitle("Wayside gage v. vahydro model 2002") + 
  scale_y_continuous(trans = log_trans(), 
                     breaks = c(10, 100, 1000, 10000, 10000), 
                     limits=c(1,10000))
ggsave(file="wayside_gage_v_vahydro_2002.png", width=9, height=5, units="in")


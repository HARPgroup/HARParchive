###This Script Runs Alongside USGSplots
#Dendron, VA
site_idva <- '02047500'
pCode <- '00060'
startDate2 <- '2000-10-01'
endDate2 <- '2019-9-30'
#Santa Ynez River, CA
site_idsb <- '11123000'

library(dataRetrieval)
library(ggplot2)
library(dplyr)
library(scales)

siteinfova <- readNWISsite(site_idva)
rawDailyva <- readNWISdv(site_idva,pCode,startDate2,endDate2)
siteinfosb <- readNWISsite(site_idsb)
rawDailysb <- readNWISdv(site_idsb,pCode,startDate2,endDate2)

va <- siteinfova$drain_area_va
sb <- siteinfosb$drain_area_va

avgVa <- mean(rawDailyva$X_00060_00003)
sRunoffva2 <- (rawDailyva$X_00060_00003/(va))*(1/5280^2)*(12)*(86400)*(365) #in/yr
sRunoffva3 <- (rawDailyva$X_00060_00003/(va)) #cfs/square mile

avgSb <- mean(rawDailysb$X_00060_00003)
sRunoffsb2 <- (rawDailysb$X_00060_00003/(sb))*(1/5280^2)*(12)*(86400)*(365) #in/yr
sRunoffsb3 <- (rawDailysb$X_00060_00003/(sb)) #cfs/square mile


##Adding Water Year column to data.frame
#VA
year <- 1:365
yearLeap <- 1:366
rawDailyva$WaterYear <- c(rep("2001", length(year)), rep("2002", length(year)), rep("2003", length(year)), rep("2004", length(yearLeap)), rep("2005", length(year)), rep("2006", length(year)), rep("2007", length(year)), rep("2008", length(yearLeap)), rep("2009", length(year)), rep("2010", length(year)), rep("2011", length(year)), rep("2012", length(yearLeap)), rep("2013", length(year)), rep("2014", length(year)), rep("2015", length(year)), rep("2016", length(yearLeap)), rep("2017", length(year)), rep("2018", length(year)), rep("2019", length(year)))
#CA
rawDailysb$WaterYear <- c(rep("2001", length(year)), rep("2002", length(year)), rep("2003", length(year)), rep("2004", length(yearLeap)), rep("2005", length(year)), rep("2006", length(year)), rep("2007", length(year)), rep("2008", length(yearLeap)), rep("2009", length(year)), rep("2010", length(year)), rep("2011", length(year)), rep("2012", length(yearLeap)), rep("2013", length(year)), rep("2014", length(year)), rep("2015", length(year)), rep("2016", length(yearLeap)), rep("2017", length(year)), rep("2018", length(year)), rep("2019", length(year)))

##Adding Year and Month column to data.frame
#VA
Date = rawDailyva$Date
rawDailyva$Year = format(as.Date(Date, format="%d%m%Y"), "%Y")
rawDailyva$Month = format(as.Date(Date, format="%d%m%Y"), "%m")
#CA
rawDailysb$Year = format(as.Date(Date, format="%d%m%Y"), "%Y")
rawDailysb$Month = format(as.Date(Date, format="%d%m%Y"), "%m")

##19 year Box Plots for VA
#discharge (cfs)
ggplot(rawDailyva, aes(x=rawDailyva$WaterYear))+
  geom_boxplot(aes(y=rawDailyva$X_00060_00003))+
  geom_point(aes(y=rawDailyva$X_00060_00003), color='red',stat='summary',fun='mean')+
  theme_bw()+
  labs( title = 'Discharge Boxplots in Dendron, VA from water year 2001-2019', x = 'Year', y = 'Discharge (cfs)') +
  theme(plot.title = element_text(face = 'bold',hjust = 0.5))+
  coord_cartesian(ylim=c(0,2000))

#specific discharge (in/yr)
ggplot(rawDailyva, aes(x=rawDailyva$WaterYear))+
  geom_boxplot(aes(y=sRunoffva2))+
  theme_bw()+
  labs( title = 'Specific Discharge Boxplots in Dendron, VA from water year 2001-2019', x = 'Year', y = 'Specific Discharge (in/yr)') +
  theme(plot.title = element_text(face = 'bold',hjust = 0.5))+
  coord_cartesian(ylim=c(0,100))

#specific discharge (cfs/square mile)
ggplot(rawDailyva, aes(x=rawDailyva$WaterYear))+
  geom_boxplot(aes(y=sRunoffva3))+
  geom_point(aes(y=sRunoffva3), color='red',stat='summary',fun='mean')+
  theme_bw()+
  labs( title = 'Specific Discharge Boxplots in Dendron, VA from water year 2001-2019', x = 'Year', y = 'Specific Discharge (cfs/mi^2)') +
  theme(plot.title = element_text(face = 'bold',hjust = 0.5))+
  coord_cartesian(ylim=c(0,7.5))

##19 year Box Plots for CA
#discahrge (cfs)
ggplot(rawDailysb, aes(x=rawDailysb$WaterYear))+
  geom_boxplot(aes(y=rawDailysb$X_00060_00003))+
  geom_point(aes(y=rawDailysb$X_00060_00003), color='red',stat='summary',fun='mean')+
  theme_bw()+
  labs( title = 'Discharge Boxplots on the Santa Ynez River from water year 2001-2019', x = 'Year', y = 'Discharge (cfs)') +
  theme(plot.title = element_text(face = 'bold',hjust = 0.5))+
  coord_cartesian(ylim=c(0,100))

#specific discharge (in/yr)
ggplot(rawDailysb, aes(x=rawDailysb$WaterYear))+
  geom_boxplot(aes(y=sRunoffsb2))+
  theme_bw()+
  labs( title = 'Specific Discharge Boxplots on the Santa Ynez River from water year 2001-2019', x = 'Year', y = 'Specific Discharge (in/yr)') +
  theme(plot.title = element_text(face = 'bold',hjust = 0.5))

#specific discharge (cfs/square mile)
ggplot(rawDailysb, aes(x=rawDailysb$WaterYear))+
  geom_boxplot(aes(y=sRunoffsb3))+
  theme_bw()+
  labs( title = 'Specific Discharge Boxplots on the Santa Ynez River from water year 2001-2019', x = 'Year', y = 'Specific Discharge (cfs/mi^2)') +
  theme(plot.title = element_text(face = 'bold',hjust = 0.5))

##Line graph over 20 years for Dendron VA
ggplot(rawDailyva, aes(x=rawDailyva$Date))+
  geom_line(aes(y=rawDailyva$X_00060_00003))+
  ylim(0,5000)+
  theme_bw() 

##Monthly Box Plots for VA
#Box Plot for specific discharge each month (in/yr)
ggplot(rawDailyva, aes(x=rawDailyva$Month))+
  geom_boxplot(aes(y=sRunoffva2))+
  geom_point(aes(y=sRunoffva2), color='red',stat='summary',fun='mean')+
  theme_bw()+
  labs( title = 'Specific Discharge in Dendron, VA by Month from 2001-2019 Water Year', x = 'Month', y = 'Specific Discharge (in/yr)') +
  theme(plot.title = element_text(face = 'bold',hjust = 0.5))+
  coord_cartesian(ylim=c(0,60))

#2001 vs 2019 Comparison Box Plot for specific discharge each month (in/yr)
Dateva = rawDailyQva$Date
Dateoldva = rawDailyQoldva$Date
rawDailyQva$Year = format(as.Date(Dateva, format="%d%m%Y"), "%Y")
rawDailyQva$Month = format(as.Date(Dateva, format="%d%m%Y"), "%m")
rawDailyQoldva$Year = format(as.Date(Dateoldva, format="%d%m%Y"), "%Y")
rawDailyQoldva$Month = format(as.Date(Dateoldva, format="%d%m%Y"), "%m")
ggplot(NULL)+
  geom_boxplot(aes(x=rawDailyQva$Month, y=sRunoffoldva, color = '2001'))+
  geom_boxplot(aes(x=rawDailyQva$Month, y=sRunoffva, color = '2019'))+
  theme_bw()+
  labs( title = '2001 vs 2019 Dendron, VA Specific Discharge by Month', x = 'Month', y = 'Specific Discharge (in/yr)') +
  theme(plot.title = element_text(face = 'bold',hjust = 0.5))+
  coord_cartesian(ylim=c(0,50))

##Monthly Box Plots for CA
#Box Plot for specific discharge each month (in/yr)
ggplot(rawDailysb, aes(x=rawDailysb$Month))+
  geom_boxplot(aes(y=sRunoffsb2))+
  geom_point(aes(y=sRunoffsb2), color='red',stat='summary',fun='mean')+
  theme_bw()+
  labs( title = 'Specific Discharge on the Santa Ynez River by Month from 2001-2019', x = 'Month', y = 'Specific Discharge (in/yr)') +
  theme(plot.title = element_text(face = 'bold',hjust = 0.5))+
  coord_cartesian(ylim=c(0,20))

##Monthly Comparison Box Plots
#Box Plot for specific discharge each month (in/yr)
ggplot(rawDailyva, aes(x=rawDailyva$Month))+
  geom_boxplot(aes(y=sRunoffva2, color = 'Dendron, VA'))+
  geom_boxplot(aes(y=sRunoffsb2, color = 'Santa Ynez River, CA'))+
  theme_bw()+
  labs( title = 'Dendron vs Santa Ynez River Specific Discharge by Month from 2001-2019 Water Year', x = 'Month', y = 'Specific Discharge (in/yr)') +
  theme(plot.title = element_text(face = 'bold',hjust = 0.5))+
  coord_cartesian(ylim=c(0,25))

##Seasonal Box Plots
#Adding Season Columns to data.frame
monthVa <- as.numeric(rawDailyva$Month)
rawDailyva$Season <- ifelse(monthVa>11,'Winter',
                            ifelse(monthVa>8,'Fall',
                                   ifelse(monthVa>5,'Summer',
                                          ifelse(monthVa>2,'Spring','Winter')))) #Dendron, VA
rawDailyva$Season <- factor(rawDailyva$Season, levels=c('Fall','Winter','Spring','Summer')) #Reorder for boxplot
rawDailysb$Season <- ifelse(monthVa>11,'Winter',
                            ifelse(monthVa>8,'Fall',
                                   ifelse(monthVa>5,'Summer',
                                          ifelse(monthVa>2,'Spring','Winter')))) #Santa Ynez River, CA
rawDailysb$Season <- factor(rawDailysb$Season, levels=c('Fall','Winter','Spring','Summer')) #Reorder for boxplot

#Seasonal Box Plot Dendron, VA
ggplot(NULL, aes(x=rawDailyva$Season))+
  geom_boxplot(aes(y=rawDailyva$X_00060_00003))+
  geom_point(aes(y=rawDailyva$X_00060_00003), color='red',stat='summary',fun='mean')+
  theme_bw()+
  labs( title = 'Seasonal Discharge in Dendron, VA from Water Year 2001-2019', x = 'Season', y = 'Specific Discharge (cfs)') +
  theme(plot.title = element_text(face = 'bold',hjust = 0.5))+
  coord_cartesian(ylim=c(0,1000))

#Seasonal Box Plot Santa Ynez, CA
ggplot(NULL, aes(x=rawDailysb$Season))+
  geom_boxplot(aes(y=rawDailysb$X_00060_00003))+
  geom_point(aes(y=rawDailysb$X_00060_00003), color='red',stat='summary',fun='mean')+
  theme_bw()+
  labs( title = 'Seasonal Discharge on the Santa Ynez River from Water Year 2001-2019', x = 'Season', y = 'Discharge (cfs)') +
  theme(plot.title = element_text(face = 'bold',hjust = 0.5))+
  coord_cartesian(ylim=c(0,100))

#Comparing Spring across years for Santa Ynez, CA
rawDailysbSpring <- filter(rawDailysb, Season %in% 'Spring')
avgSbSpring <- mean(rawDailysbSpring$X_00060_00003)
ggplot(NULL, aes(x=rawDailysbSpring$WaterYear))+
  geom_boxplot(aes(y=rawDailysbSpring$X_00060_00003))+
  geom_point(aes(y=rawDailysbSpring$X_00060_00003), color = 'red', stat = 'summary', fun = 'mean')+
  theme_bw()+
  labs( title = 'Spring Discharge on the Santa Ynez River from Water Year 2001-2019', x = 'Year', y = 'Discharge (cfs)') +
  theme(plot.title = element_text(face = 'bold',hjust = 0.5))+
  coord_cartesian(ylim=c(0,300))

#Comparing Winter across years for Dendron, VA
rawDailyvaWinter <- filter(rawDailyva, Season %in% 'Winter')
avgVaWinter <- mean(rawDailyvaWinter$X_00060_00003)
ggplot(NULL, aes(x=rawDailyvaWinter$WaterYear))+
  geom_boxplot(aes(y=rawDailyvaWinter$X_00060_00003))+
  geom_point(aes(y=rawDailyvaWinter$X_00060_00003), color = 'red', stat = 'summary', fun = 'mean')+
  theme_bw()+
  labs( title = 'Winter Discharge in Dendron, VA from Water Year 2001-2019', x = 'Year', y = 'Discharge (cfs)') +
  theme(plot.title = element_text(face = 'bold',hjust = 0.5))+
  coord_cartesian(ylim=c(0,1500))
                  
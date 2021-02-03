library(ggplot2)
library(dplyr)
library(tidyr)
library(IHA)
library(stringr)
library(zoo)
library(dataRetrieval)
library(PearsonDS)
library(plotly)

site_id.1 <- '01646502' #Potomac River, Washington D.C. - Natty
site_id.2 <- '02047500' #Blackwater River, Dendron - Alex
site_id.3 <- '01626000' #South River, Waynesboro - Sarah
site_id.4 <- '02040000' #Appomattox River, Mattoax - Miriam
site_id.5 <- '03473000' #Holston River, Damascus - Kyle
site_id.6 <- '02041650'
site_id.7 <- '01595800'
site_id.8<- '01673000'
site_id.9 <- '01668000'
site_id.10 <- '01646500'




startDate <- '2008-10-01'
endDate <- '2019-09-30'
pCode <-'00060'


site.id <- c(site_id.1, site_id.2, site_id.3, site_id.4, site_id.5,
             site_id.6,site_id.7, site_id.8,site_id.9,site_id.10)

 meanflow <- c()
 minflow <- c()
 maxflow <- c()
 medianflow <- c()

#  for(i in 1:5) {
#   siteinfo <- readNWISsite(site.id[i])
#   rawDailyQ <- readNWISdv(site.id[i], pCode, startDate, endDate)
#   meanflow[i] <- mean(rawDailyQ$X_00060_00003)
#   minflow[i] <- min(rawDailyQ$X_00060_00003)
#   maxflow[i] <- max(rawDailyQ$X_00060_00003)
#   medianflow[i] <- median(rawDailyQ$X_00060_00003)
#   plot(rawDailyQ$Date,rawDailyQ$X_00060_00003 )
# 
#  }



#7Q10 loop practice

x7Q10 <- c()
drainage_area <- c()
altitude<-c()

for (i in 1:length(site.id)) {
  
  siteinfo <- readNWISsite(site.id[i])
  
  drainage_area[i]<- siteinfo$drain_area_va
  altitude[i]<-siteinfo$alt_va
  rawDailyQ <- readNWISdv(site.id[i], pCode, startDate, endDate)
  
  flow <- zoo(rawDailyQ$X_00060_00003,  order.by = rawDailyQ$Date)
  p <- 1/10
  data2 <- data.frame(group2(flow, year=c("water"), mimic.tnc = T))
  x7daymin <- data2$X7.Day.Min
  
  x7daymin[x7daymin< 0] <- NA
  log7daymin <- log(x7daymin)
  log7daymin <- na.omit(log7daymin)
  pars <- PearsonDS:::pearsonIIIfitML(log7daymin)
  x7Q10[i] <- exp(qpearsonIII(p, params = pars$par))
  
}

print(x7Q10)

q<-data.frame(drainage_area, x7Q10, altitude)


#Plotly visualization integration

g<-ggplot(q, aes(x=drainage_area, 
           y=x7Q10, 
           col=altitude))+
  labs(x='Drainage Area', 
       y = '7Q10',
       title = 'USGS Gage Drainage Area vs. 7Q10')+
  geom_point()+
  geom_smooth(method = 'lm')

plot(g)
ggplotly(g)


#Test Edit

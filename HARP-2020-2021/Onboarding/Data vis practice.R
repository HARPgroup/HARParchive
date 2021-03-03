library(dataRetrieval)
library(ggplot2)
library(gganimate)
library(gifski)
library(png)

site_id <- '13010065'
startDate <- '2018-10-01'
endDate <- '2019-09-30'
pCode <-'00060'
siteinfo <- readNWISsite(site_id)
rawDailyQ <- readNWISdv(site_id, pCode, startDate, endDate)




p <- ggplot(rawDailyQ, 
       aes(x=rawDailyQ$Date, y=rawDailyQ$X_00060_00003)) +
  geom_line(color='darkblue')+
  labs(title='USGS 13010065 SNAKE RIVER AB JACKSON LAKE AT FLAGG RANCH WY',
       x='Date (2019 Water Year)',
       y='Discharge (cfs/sq. mile)' )+
  theme(
    plot.title = element_text(size = 12, face = "bold",  hjust = 0.5))+
  scale_y_log10()+
  
  transition_reveal(rawDailyQ$Date)


animate(p, renderer = gifski_renderer())
anim_save('.')











#San Diego GroundWater

library(dataRetrieval)
library(tidyverse)

# startDate <- '2009-10-01'
# endDate <- '2019-09-30'

pCode <-'00060'
siteNumbers <- '323808117060402'


gw<-readNWISgwl(
  siteNumbers,
  startDate = "",
  endDate = "",
  convertType = TRUE,
  tz = "UTC")



plot(gw$lev_dt, gw$lev_va, 
     ylim = rev(range(gw$lev_va)),
     main='Loess Curve - USGS 323808117060402, San Diego County, CA', 
     xlab= 'Date',
     ylab = 'Groundwater Level Below Land Surface (ft)',  
     cex.axis = 0.8,
     cex.main = 1,
     cex.lab= 0.9)

y.loess <- loess(lev_va ~ as.numeric(lev_dt),span=0.8, gw)

y.predict <- predict(y.loess, gw)

lines(gw$lev_dt,y.predict, col='darkblue')




ggplot(gw, aes(x=lev_dt, y= lev_va)) + 
  geom_point() +
  geom_smooth(method="loess")+
  scale_y_reverse()+
  labs(title='Loess Curve - USGS 323808117060402, San Diego County, CA', 
 x='Date',
 y='Groundwater Level Below Land Surface (ft)')+
theme(
  plot.title = element_text(size = 12, face = "bold",  hjust = 0.5))
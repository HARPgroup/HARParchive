# Dry Run California case study using the IHA flow metrcis 

#1. One Dam Conundrum: The Story of Dry Run, California
#In 1982, the Warm Springs Dam was closed on Dry Run to create Lake Sonoma. 
#A great tourist attraction and useful hydropower site, the Lake has generally been regarded as a useful engineering project on the West Coast. 
#However, what did the dam do to the Creek? 

#Date started: 6/15/2022
#By: HARP team summer 2022

library(dataRetrieval)
library(zoo)
library(IHA)
library(dplyr)
library(lubridate)
library(ggplot2)

#importing data for gauge 11465200

siteNo <- '11465200'
startDate_pre <- '1970-01-01'
endDate_pre <- '1980-12-31'
startDate_post <- '1985-01-01'
endDate_post <- '1995-12-31'
pCode <- '00060' #code for streamflow
sCode <- '00003' #average daily data
DRflow_pre <-readNWISdv(siteNo, pCode, startDate_pre, endDate_pre)
DRflow_post <-readNWISdv(siteNo, pCode, startDate_post, endDate_post)
head(DRflow_pre)

#renaming columns
colnames(DRflow_pre) <- c('Agency', 'Site', 'Date', 'Flow', 'Code')
colnames(DRflow_post) <- c('Agency', 'Site', 'Date', 'Flow', 'Code')

#plotting flow pre and post dam
par(mfrow=c(1,2)) #creates 2 windows side-by-side for the plots to be shown
plot(DRflow_pre$Date, DRflow_pre$Flow, type = 'l', col = 'red', ylim = c(0,20000))
plot(DRflow_post$Date, DRflow_post$Flow, type = 'l', col = 'blue', ylim = c(0,20000))


#----IHA parameter #1: Monthly Magnitude----------------------------------------------

#making tables into zoo objects for IHA package 

DRpre_z <- zoo(DRflow_pre$Flow, order.by=DRflow_pre$Date)
DRpost_z <- zoo(DRflow_post$Flow, order.by=DRflow_post$Date)

Monthly_pre_mean <- as.data.frame(group1(DRpre_z, year = 'calendar', FUN = mean))  
Monthly_post_mean <- as.data.frame(group1(DRpost_z, year = 'calendar', FUN = mean))
Monthly_pre_med <- as.data.frame(group1(DRpre_z, year = 'calendar', FUN = median))
Monthly_post_med <- as.data.frame(group1(DRpost_z, year = 'calendar', FUN = median))
head(Monthly_pre_mean) #previewing one of the resulting tables for monthly magnitude 

#plotting monthly magnitude as mean
par(mfrow=c(1,2)) 
boxplot(Monthly_pre_mean, ylab = 'Monthly magnitude (cfs)', ylim = c(0,4000), main = 'Monthly mean pre-dam')
boxplot(Monthly_post_mean, ylab = 'Monthly magnitude (cfs)', ylim = c(0,4000), main = 'Monthly mean post-dam')

#plotting monthly magnitude as median
par(mfrow=c(1,2)) 
boxplot(Monthly_pre_med, ylab = 'Monthly magnitude (cfs)', ylim = c(0,3000), main = 'Monthly median pre-dam')
boxplot(Monthly_post_med, ylab = 'Monthly magnitude (cfs)', ylim = c(0,3000), main = 'Monthly median post-dam')

#looking closer at a month of low flows: September

#plotting change in monthly magnitude (mean) for September before and after dam construction 
par(mfrow=c(1,2))
plot(Monthly_pre_mean$September, xlab = 'Year', ylab = 'Monthly magnitude (cfs)', ylim = c(0,200), main = 'Sept mean flows pre-dam', type = 'l', col = 'red')
plot(Monthly_post_mean$September, xlab = 'Year', ylab = 'Monthly magnitude (cfs)', ylim = c(0,200), main = 'Sept mean flows post-dam', type = 'l', col = 'blue')

#looking closer at a month of high flows: January

#plotting change in monthly magnitude (mean) for January before and after dam construction 
par(mfrow=c(1,2))
plot(Monthly_pre_mean$January, xlab = 'Year', ylab = 'Monthly magnitude (cfs)', ylim = c(0,5000), main = 'Sept mean flows pre-dam', type = 'l', col = 'red')
plot(Monthly_post_mean$January, xlab = 'Year', ylab = 'Monthly magnitude (cfs)', ylim = c(0,5000), main = 'Sept mean flows post-dam', type = 'l', col = 'blue')



#----IHA parameter #2:              ---------------------------------------------------------------------------

pre2 <- group2(DRpre_z)
post2 <- group2(DRpost_z)

pre2_max <- select(pre2, 3,5,7,9,11) 
post2_max <- select(post2, 3,5,7,9,11)

boxplot(pre2_max, pre2_max, ylab = 'Flow (cfs)', col = 'blue')
title(main = "Max Flow Pre Dam")
boxplot(post_g2_max, post_g2_max, ylab = 'Flow (cfs)', col = 'red')
title(main = "Max Flow Post Dam")

#----IHA parameter #3: Timing of Extremes ----------------------------------------------------------
#use IHA group3 function to find the extreme dates
pre.zoo <- zoo(DRflow_pre$Flow, order.by=DRflow_pre$Date) #group by dates

#get Julian dates in *water* year for more understandable graph
pre.J <- group3(pre.zoo, 'water', mimic.tnc = F)

# repeat for post
post.zoo <- zoo(DRflow_post$Flow, order.by=DRflow_post$Date)
post.J <- group3(post.zoo,'water', mimic.tnc = F)

pre.J #preview tables
post.J
#create data frame for plotting
pre.J<- data.frame(pre.J)
post.J<- data.frame(post.J)
#plot pre vs post dam extremes
#max plot
par(mfrow=c(1,2)) #plots the 2 plots next to each other
plot(1:12, pre.J$Max, type="l", ylab = 'Julian Day', col = 'blue', xlab = 'Water Year', ylim=c(1,365))
lines(post.J$Max, type='l', col='red')
title(main= 'Date of Maximum Flow')
legend(x= 4 ,y= 300 ,legend=c("Pre Dam","Post Dam"), col=c('blue','red'), bty='n',lty=1)
#min plot
plot(1:12, pre.J$Min, type="l", ylab = 'Julian Day', col = 'blue', xlab = 'Water Year', ylim=c(1,365))
lines(post.J$Min, type='l', col='red')
title(main= 'Date of Minimum Flow')
legend(x= 2 ,y= 360 ,legend=c("Pre Dam","Post Dam"), col=c('blue','red'), bty='n',lty=1)

#note: Water J Day 1 = October 1st ; Day 300 = July 28


#----IHA Parameter 4:       -----------------------------------------------------------------

#table of pre-dam pulses
library(IHA)
pre<-zoo(USGS_pre$Flow,order.by=USGS_pre$Date)
x<-group4(pre,year="water")
x

#table of post-dam pulses
post<-zoo(USGS_post$Flow,order.by=USGS_post$Date)
y<-group4(post,year="water")
y

#percentiles for pre dam
quantile(DRflow_pre$Flow, probs=0.25)
quantile(DRflow_pre$Flow, probs=0.75)

#percentiles for post dam
quantile(DRflow_post$Flow, probs=0.25)
quantile(DRflow_post$Flow, probs=0.75)

#plotting pre dam
plot(DRflow_pre$Date, DRflow_pre$Flow, ylim=c(0,12500), main='Flow vs. Time Pre-dam',type="l", col="blue", xlab="Date", 
     ylab="Flow [cfs]")
legend("topright", legend= c("Flow [cfs]","25th percentile", "75th percentile"),col=c('blue','red','green'), cex=1.0, bty='n', lty=1, x.intersp = 0.5, y.intersp = 0.75)

abline(a=NULL,b=NULL,h=1.5,v=NULL,col="red")
abline(a=NULL,b=NULL,h=163,v=NULL,col="green")

#plotting post dam
plot(DRflow_post$Date, DRflow_post$Flow, ylim=c(0,12500), main='Flow vs. Time Post-dam',type="l", col="blue", xlab="Date", 
     ylab="Flow [cfs]")
legend("topright", legend= c("Flow [cfs]","25th percentile", "75th percentile"),col=c('blue','red','green'), cex=1.0, bty='n', lty=1, x.intersp = 0.5, y.intersp = 0.75)

abline(a=NULL,b=NULL,h=88,v=NULL,col="red")
abline(a=NULL,b=NULL,h=132,v=NULL,col="green")


#----IHA Parameter 5: Rise/Fall Rates & Hydrologic Reversals----------------------------------------
#use IHA group5 function to find Rates
pre.zoo <- zoo(DRflow_pre$Flow, order.by=DRflow_pre$Date) #group by dates
pre.rt <- group5(pre.zoo)

post.zoo <- zoo(DRflow_post$Flow, order.by=DRflowpost$Date)
post.rt <- group5(post.zoo)

#rename col's for plot purposes (remove spaces)
colnames(pre.rt)<- c('RiseRate','FallRate','Reversals')
colnames(post.rt)<- c('RiseRate','FallRate','Reversals')

pre.rt
post.rt

pre.rt<- data.frame(pre.rt) #needs to be data frame to plot
post.rt<- data.frame(post.rt)

#plot rise rates
par(mfrow=c(1,2))
plot(1:12, pre.rt$RiseRate, type='l', col='blue', ylab='Rise Rate (cfs/day)', xlab='Year')
lines(post.rt$RiseRate, type='l', col='red')
title(main='Daily Consecutive Rise Rates')
legend(x=3.75, y=145, legend=c("Pre Dam","Post Dam"), col=c('blue', 'red'), bty='n', lty=1)
#plot fall rates
plot(1:12, pre.rt$FallRate, type='l', col='blue', ylab='Fall Rate (cfs/day)', xlab='Year', ylim=c(0,-150))
lines(post.rt$FallRate, type='l', col='red')
title(main='Daily Consecutive Fall Rates')
legend(x=3, y=-145, legend=c("Pre Dam","Post Dam"), col=c('blue', 'red'), bty='n', lty=1)
#  rise & fall rates post dam extremely similar --> streamflow more regulated/consistent speed?

#zoom in on fall rates alone
plot(1:12, pre.rt$FallRate, type='l', col='blue', ylab='Fall Rate (cfs/day)', xlab='Year', ylim=c(0,-14)) #same code but change ylim
lines(post.rt$FallRate, type='l', col='red')
title(main='Fall Rates (up close)')
legend(x=3, y=-145, legend=c("Pre Dam","Post Dam"), col=c('blue', 'red'), bty='n', lty=1)

#plot hydrologic reversals
plot(1:12, pre.rt$Reversals, type='l', col='blue', ylab='# of Reversals', xlab='Year', ylim=c(0,150))
lines(post.rt$Reversals, type='l', col='red')
title(main='Number of Hydrologic Reversals')
legend(x=3, y=30 , legend=c("Pre Dam","Post Dam"), col=c('blue','red'), bty='n', lty=1)

#--------------------------------------------------------------------------------------------------------
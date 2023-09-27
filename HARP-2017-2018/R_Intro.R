# 22 May 2018 Kelsey Reitz
# Sample Tutorials on R and Gauge Data

#install packages
install.packages('dataRetrieval')
install.packages('zoo')
install.packages('caTools')
install.packages('IHA', repos='http://R-Forge.R-project.org')
install.packages('PearsonDS')
install.packages('dplyr')
install.packages('lubridate')

#initialize packages
library(zoo)
library(IHA)
library(dataRetrieval)
library(PearsonDS)
library(dplyr)
library(lubridate)

??readNWISdv #calls the help center w/ sheets for info about command


# New River at Allisonia --------------------------------------------------


siteNo<-"03168000"
pCode<-'00060'
stat<-'00003'
start.date<-'2000-01-01'
end.date<-'2010-12-31'
USGS<-readNWISdv(siteNo,pCode,start.date,end.date)

names(USGS)
names(USGS) <- c('Agency', 'Site', 'Date', 'Flow', 'QC') #rename the columns of USGS

USGS <- USGS[order(USGS$Date),] #order the data in order of time, do so by the order and , (meaning all)

plot(USGS$Date, USGS$Flow) # plot data and make it look pretty
par(mar=c(5,6,2,4),cex.axis=2, lwd=2, cex.lab=2)  
plot(USGS$Date, USGS$Flow, xlab='Date', ylab='Flow (cfs)') #create labels for axes
plot(USGS$Date,USGS$Flow, xlab='Date', ylab='Flow (cfs)',type = 'l') #this is an l not a 1! 
lines(USGS$Date, USGS$Flow, col = 'red')


USGS$WY<-numeric(length(USGS$Date)) #create a new column in USGS that will be used to store water year

#loop based on water year iteration:
USGS$WY<-numeric(length(USGS$Date))
b<-year(USGS$Date[1])              #initialize b variable to be first year of USGS date column
for (i in 1:length(USGS$Date)){    #create for loop to end of Date data, starting at 1, ending at last entry
  USGS$WY[i] <- b-1                #wy[i] = date year -1
  if(USGS$Date[i]>=as.Date(paste0(b, "-10-01"))){ #if the date is > or = to Oct 1, add one to b (year = year)
    b <- b+1
    USGS$WY[i]<-b-1
  }
}

#create new columns using the lubridate package
USGS$Month <- month(USGS$Date)
USGS$Year <- year(USGS$Date)
aligroup <- group_by(USGS, Year)
peaks <- as.data.frame(summarize(aligroup, MaxFlow=max(Flow))) #finds max flow for each year and stores in data frame
peakUSGS <- readNWISpeak(siteNo, start.date, end.date) #USGS does same thing from NWIS

plot(peakUSGS$peak_va, xlab='Date', ylab='Flow (cfs)', type = 'l')
lines(peaks$MaxFlow, col = 'blue')

# the difference in the two lines: the blue line is the summarized line from USGS Data, black line is daily
# summarized from dplyr



# function to calculate xqy statistics: 7q10, 30q2:
xQy_dor<- function(dat,x,y){
  #create variables -- drought of record 90 days, time in a year 365 days, and 3 empty variables
  dor_time<-90
  timecheck<-365
  xQy<-numeric()
  dor<-numeric()
  ndays<-numeric()
  
  #create a for loop to loop for low flows in each water year
  for(i in 1:length(unique(dat$WY))){
    subst<-dat[dat$WY==unique(dat$WY)[i],] #create a subset of data by filtering data points in specified WY i
    xQy[i]<-min(rollapply(subst$Flow,x,mean))
    dor[i]<-min(rollapply(subst$Flow,dor_time,mean))
    ndays[i]<-length(subst$Date)
  }
  xQy<-xQy[which(ndays>=timecheck)]#Get rid of incompete water years
  xQy<-xQy[xQy>0]
  xQy<-log(xQy)
  n<-length(xQy)
  N<-length(ndays[ndays>=timecheck])
  Padj<-((N/y)/n)-((N-n)/n)
  if(Padj<=0|n<=1){
    imp_XQY<-0
  }else{
    pars<-PearsonDS:::pearsonIIIfitML(xQy)
    imp_XQY<-exp(qpearsonIII(Padj, params = pars$par))  
  }
  dor_imp<-min(dor)
  if(dor_imp<0){
    dor_imp<-0
  }
  return(c(imp_XQY,dor_imp))
}



#using rollapply:
xQy<-numeric()
dor<-numeric()
ndays<-numeric()
x<-7
y<-10
dor_time<-90

i=1
subst<-USGS[USGS$WY==unique(USGS$WY)[i],]
USGS[USGS$WY==unique(USGS$WY)[i]&USGS$Flow>5000,]
min(rollapply(subst$Flow,x,mean))
min(rollapply(subst$Flow,dor_time,mean))
#rollapply is a running mean of data points. For example, this finds a rolling mean for x amount of time


#now find the minimums for all 12 water years using rollapply:

for(i in 1:length(unique(USGS$WY))){
  subst<-USGS[USGS$WY==unique(USGS$WY)[i],]
  xQy[i]<-min(rollapply(subst$Flow,x,mean))
  dor[i]<-min(rollapply(subst$Flow,dor_time,mean))
  ndays[i]<-length(subst$Date)
}


plot(unique(USGS$WY),dor,type='l',xlab='Water Year',ylab='Flow (cfs)')
lines(unique(USGS$WY),xQy,col='blue')

# now plot the data in a way that can be seen -- add more to these two lines:
plot(unique(USGS$WY),dor,type='l',xlab='Water Year',ylab='Flow (cfs)',ylim=c(100,3000))
lines(unique(USGS$WY),xQy,col='blue')
legend(x=2004,y=3000,legend=c("90 day min flow","7 day min flow"),col=c('black','blue'),cex=2,bty='n',lty=1,x.intersp = 0.5,y.intersp = 0.75)

boxplot(xQy, dor, names = c('Min 7 day', 'Min 90 day'), ylab = 'Flow (cfs)') #create a boxplot
hist(xQy) # create a histogram


#alternative to finding flows using the IHA package:
f3 <- zoo(USGS$Flow, order.by=USGS$Date)
g2 <- group2(f3)

timecheck<-365
xQy <- xQy[which(ndays >= timecheck)]
xQy <- xQy[xQy>0]
xQy <- log(xQy) #xQy calculations take place in the log space: explains why 0 flows were removed above

n<- length(xQy) 
N<- length(ndays[ndays>=timecheck])
Padj<- ((N/y)/n) - ((N-n)/n)   #statistical analysis of xQy

#following loop checks that if adjusted probability is less than 0, 7q10 is zero
#otherwise, data is a pearson III distribution
if(Padj<=0|n<=1){
  imp_XQY<-0
}else{
  pars<-PearsonDS:::pearsonIIIfitML(xQy)
  USGS_7q10<-exp(qpearsonIII(Padj, params = pars$par))  
}


#Drought of record: find minimum average annual 90 day flow
dor_metric <- min(dor)
USGS_7q10; dor_metric #display the 7q10 and drought of record

USGS_7q10<-xQy_dor(USGS,x=7,y=10)[1]
USGS_30q2<-xQy_dor(USGS,x=30,y=2)[1]
USGS_dor<-xQy_dor(USGS,x=7,y=10)[2]



#Looking at the lowest 10% of flows in September: this is important because it is a dry and drought heavy month
# To look at this, we can look at only the month of september, and data within the 10th quantile. How to do this?
# Index flow in USGS to only give those in september. Then use quantile function to find 10th percentile

SeptFlows <- USGS$Flow[USGS$Month==9] #Find the flows in the flow column only for september
Sept10 <- as.vector(quantile(SeptFlows, 0.1)); rm(SeptFlows)

Aug <- USGS[USGS$Month==8 ,]
AugFlows <- group_by(Aug, Year)
LFs <- summarize(AugFlows, minFlow=min(Flow)) #provides a summary  of AugFlows and minimum flows
ALF <- median(LFs$minFlow) # find median minimum flow for August low flow

q10<-as.numeric(quantile(USGS$Flow,0.1))
q25<-as.numeric(quantile(USGS$Flow,0.25))
q50<-as.numeric(quantile(USGS$Flow,0.5))
q75<-as.numeric(quantile(USGS$Flow,0.75))
q90<-as.numeric(quantile(USGS$Flow,0.9))
avg<-mean(USGS$Flow)
zero<-sum(USGS$Flow==0)

#produce a dataframe that gives the output of all these values: 
a<-data.frame(ALF=ALF,DoR=USGS_dor,Sept10=Sept10,'7q10'=USGS_7q10,'xQy'=USGS_30q2,
              "10%"=q10,"25%"=q25,"50%"=q50,"75%"=q75,"90%"=q90,"Zero"=zero,Mean=avg)
a



# New River at Radford -----------------------------------------------------------------
siteNo<-"03171000"
pCode<-'00060'
stat<-'00003'
start.date<-'2000-01-01'
end.date<-'2010-12-31'
USGS_imp<-readNWISdv(siteNo,pCode,start.date,end.date)
USGS_imp<-USGS_imp[order(USGS_imp$Date),]
names(USGS_imp)<-c('Agency','Site','Date','Flow','QC')

plot(USGS_imp$Date,USGS_imp$Flow,xlab='Date',ylab='Flow (cfs)',type='l',col='red')
lines(USGS$Date,USGS$Flow,col='blue')

USGS_imp$Month<-month(USGS_imp$Date)
USGS_imp$Year<-year(USGS_imp$Date)
USGS_imp$WY<-numeric(length(USGS_imp$Date))
b<-year(USGS_imp$Date[1])
for (i in 1:length(USGS_imp$Date)){
  USGS_imp$WY[i]<-b-1
  if(USGS_imp$Date[i]>=as.Date(paste0(b,"-10-01"))){
    b<-b+1
    USGS_imp$WY[i]<-b-1
  }
}

# Repeating calculations for the Radford gauge
USGS_imp$Month<-month(USGS_imp$Date)  # month from date
USGS_imp$Year<-year(USGS_imp$Date)    # year from date column
USGS_imp$WY<-numeric(length(USGS_imp$Date)) #create a numeric water year variable
b<-year(USGS_imp$Date[1])
for (i in 1:length(USGS_imp$Date)){
  USGS_imp$WY[i]<-b-1
  if(USGS_imp$Date[i]>=as.Date(paste0(b,"-10-01"))){
    b<-b+1
    USGS_imp$WY[i]<-b-1
  }
}
rm(b,i)

#7q10, 30q2, dor, SeptFlows, Sept10, and ALF
USGS_imp_7q10<-xQy_dor(USGS_imp,x=7,y=10)[1]
USGS_imp_30q2<-xQy_dor(USGS_imp,x=30,y=2)[1]
USGS_imp_dor<-xQy_dor(USGS_imp,x=7,y=10)[2]
SeptFlows<-as.vector((select(filter(USGS_imp,Month==9),Flow))$Flow)
Sept10<-as.vector(quantile(SeptFlows,0.1));rm(SeptFlows)
ALF<-median(as.vector(summarize(group_by(filter(USGS_imp,Month==8),Year),minFlow=min(Flow)))$minFlow)
#quantiles
q10<-as.numeric(quantile(USGS_imp$Flow,0.1))
q25<-as.numeric(quantile(USGS_imp$Flow,0.25))
q50<-as.numeric(quantile(USGS_imp$Flow,0.5))
q75<-as.numeric(quantile(USGS_imp$Flow,0.75))
q90<-as.numeric(quantile(USGS_imp$Flow,0.9))
avg<-mean(USGS_imp$Flow)
zero<-sum(USGS_imp$Flow==0)
b<-data.frame(ALF=ALF,DoR=USGS_imp_dor,Sept10=Sept10,'7q10'=USGS_imp_7q10,'xQy'=USGS_imp_30q2,
              "10%"=q10,"25%"=q25,"50%"=q50,"75%"=q75,"90%"=q90,"Zero"=zero,Mean=avg)

#plot flow vs probability to see the difference in distributions
prob <- seq(0,1,0.01)
plot(prob, quantile(USGS$Flow, prob), type='l', xlab = 'Probability' , ylab ='Flow (cfs)')
# to actually see this change, plot it on a log plot
USGS$LogFlow <- log10(USGS$Flow)         #create a log flow column of USGS
USGS_imp$LogFlow <- log10(USGS_imp$Flow) #create a log flow column of USGS_imp
plot(prob, quantile(USGS$LogFlow, prob), type='l', xlab='Probability', ylab=expression('Log'[10]*'(Flow)'), col='red') #plot log in red
lines(prob, quantile(USGS_imp$LogFlow, prob), col='blue') #create another line for USGS_imp
legend(x=0, y=4.5, legend = c("@ Allisonia", "@ Radford"), col=c('red', 'blue'), cex=2, bty='n', lty=1, x.intersp = 0.5,y.intersp = 0.75)

#package to aid in plotting log graphs:
install.packages('sfsmisc')
library(sfsmisc)

par(mar=c(5,12,2,4))
plot(prob, quantile(USGS$Flow,prob),type='l', col='red', ann=F,xaxt='n',yaxt='n',log="y")
lines(prob,quantile(USGS_imp$Flow,prob),col='blue')
axis(1,cex.axis=2, cex=2)
eaxis(2,log=T, cex.axis=2, cex=2)
mtext("Flow(cfs)",  side=2,  line=8, cex=2)
mtext("Probability", side=1, line=3, cex=2)
legend(x=0,y=50000,legend=c("@ Allisonia","@ Radford"),col=c('red','blue'),cex=2,bty='n',lty=1,x.intersp = 0.5,y.intersp = 0.75)

Allisonia <- readNWISsite("03168000")
da_Alli<-Allisonia$drain_area_va

Radford <- readNWISsite("03171000")
names(Radford)
da_Rad<-Radford$drain_area_va
pred<-USGS_imp$Flow * da_Alli/ da_Rad #predicted upstream flow = downstream*(upstreamarea/downstreamarea)

par(mar=c(5,6,2,4), cex.axis=2,lwd=2,cex.lab=2)
plot(USGS$Date, pred, type='l', xlab='Date', ylab='Flow (cfs)') #plot date against predicted flow and USGSFlow
lines(USGS$Date, USGS$Flow, col='red')

#quantify the prediction: this will give us a clue to use regression 
plot(USGS$Flow, pred, xlab="Observed", ylab="Predicted")
reg <-lm(pred~USGS$Flow)
summary(reg)
#now visualize the fit:
plot(USGS$Flow, pred, xlab='Date', ylab='Predicted')
lines(USGS$Flow, fitted(reg), lwd=2, col= 'red')
mtext(paste0("Predicted = ",round(coef(reg)[1],2),"+",round(coeff(reg[2],2),"*Observed"),3,line=-6,at 12000,cex=1)
mtext(bquote(italic(R)^2 == .(format(as.numeric(summary(reg)[8]),digits = 2))),3,line=-8,cex=1,at=12000)

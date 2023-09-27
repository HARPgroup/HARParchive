#Installing packages may have to be done in a certain order if other programs have dependencies 
install.packages('dataRetrieval')
install.packages('bitops')
install.packages('PearsonDS')
install.packages('dplyr')
install.packages('lubridate')
install.packages('zoo')
install.packages('caTools')
install.packages('IHA', repos='http://R-Forge.R-project.org')    #This one MUST go last becuase it has ALOT of dependencies 

#The data packages must be downloaded into the code to be able to use them. It is like turning them on. 
library(dataRetrieval)
library(PearsonDS)
library(dplyr)
library(lubridate)
library(zoo)
library(IHA)        #Must be 'turned on' last due to its dependencies 

#Getting the Site info that we need
siteNo<-"03168000"                                  #This is the gauge number that we are looking at 
pCode<-'00060'                                      # The parameter you are looking at 00060=Discharge in cfs
stat<-'00003'                                       # The stat code which specifies if we want the mean, max, min or median 00003 is Mean
start.date<-'2000-01-01'                            #Date in year-month-day
end.date<-'2010-12-31'
USGS<-readNWISdv(siteNo,pCode,start.date,end.date) #Putting together the data we need to get a gauge reading. Used readNWISdv because we wanted daily data

#Organizing Data
names(USGS)<-c('Agency', 'Site', 'Date', 'Flow', 'QC') #Renaming columns so they make more sense 
USGS<-USGS[order(USGS$Date),] #Ordering data based on date 

#Plotting Flow
plot(USGS$Date, USGS$Flow) #Creates plot of date on x and flow on y
par(mar=c(5,6,2,4),cex.axis=2,lwd=2,cex.lab=2) #Adjusts sizing of plot
plot(USGS$Date, USGS$Flow, xlab='Date', ylab='Flow (cfs)') #Changes axes labels

#Making a line graph
plot(USGS$Date, USGS$Flow, xlab='Date', ylab='Flow (cfs)', type='l') #The type=lowercase L is what makes it a line graph

#Evaluating data by water year 
USGS$WY<-numeric(length(USGS$Date)) #Adds a new column 'WY' or water year to the USGS data and makes it as long as the Date column
b<-year(USGS$Date[1]) #b is the water year counter starting at the 1st date value
#i is the current value that we are on, it increases every time we go through the loop
for (i in 1:length(USGS$Date)){USGS$WY[i]<-b-1 #We do b-1 becuase even though the beginning of the year is in 2000 the water year is still in 1999 b/c it started in Oct 1999
                                if (USGS$Date[i]>=as.Date(paste0(b,"-10-01")))
                                {b<-b+1 #If Oct 1st is passed add a year to the water year
                                  USGS$WY[i]<-b-1}}  

#Grouping Data by year
USGS$Month<-month(USGS$Date)
USGS$Year<-year(USGS$Date)
aligroup<-group_by(USGS,Year)#New grouping where data is grouped by year 

#Finding max flow for each year from our data
peaks<-as.data.frame(summarize(aligroup,MaxFlow=max(Flow)))

#Finding max flow for each year from USGS online data
peakUSGS<-readNWISpeak(siteNo,start.date,end.date)

#Comparing our peak data and USGS peak data
plot(peakUSGS$peak_va,xlab='Date',ylab='Flow (cfs)',type='l')
lines(peaks$MaxFlow,col='blue')
#They are not the same, same general trend but not same numbers our data is slightly off

#Finding xQy statistics (7q10,30q2)
xQy_dor<-function(dat,x,y){ #Creating function to find xQy that can be reused
  dor_time<-90 #Time frame for drought of record
  timecheck<-365
  xQy<-numeric() #Creating empty vectors that will be filled
  dor<-numeric()
  ndays<-numeric()
  for(i in 1:length(unique(dat$WY))){
    subst<-dat[dat$WY==unique(dat$WY)[i],]
    xQy[i]<-min(rollapply(subst$Flow,x,mean)) #Applies some function to every possible set of x datapoints, in this case the function is mean
    dor[i]<-min(rollapply(subst$Flow,dor_time,mean))
    ndays[i]<-length(subst$Date)
  }
  xQy<-xQy[which(ndays>=timecheck)]#Get rid of incompete water years
  xQy<-xQy[xQy>0]#Finding xQy metrics for complete water years on average flows about 0
  xQy<-log(xQy)
  n<-length(xQy) #Adjusting probability based on removed zero flows
  N<-length(ndays[ndays>=timecheck])
  Padj<-((N/y)/n)-((N-n)/n)
  if(Padj<=0|n<=1){
    imp_XQY<-0
  }else{
    pars<-PearsonDS:::pearsonIIIfitML(xQy)
    imp_XQY<-exp(qpearsonIII(Padj, params = pars$par))  
  }
  dor_imp<-min(dor)
  if(dor_imp<0){ #Estimate flow adjusted probability location
    dor_imp<-0
  }
  return(c(imp_XQY,dor_imp))
}

#Plot of all 12 water years in our study period
plot(unique(USGS$WY),dor,type='l',xlab='Water Year',ylab='Flow (cfs)')
lines(unique(USGS$WY),xQy,col='blue')
#Graph is too hard to read so we adjust it 
plot(unique(USGS$WY),dor,type='l',xlab='Water Year',ylab='Flow (cfs)',ylim=c(100,3000))
lines(unique(USGS$WY),xQy,col='blue')
legend(x=2004,y=3000,legend=c("90 day min flow","7 day min flow"),col=c('black','blue'),cex=2,bty='n',lty=1,x.intersp = 0.5,y.intersp = 0.75)

#Evaluating distribution of water years flow using boxplot
boxplot(xQy,dor,names=c('Min 7 day','Min 90 day'),ylab='Flow (cfs)')

#Evaluating distribution of xQy using histogram
hist(xQy)

#Finding annual low flow using IHA
f3<-zoo(USGS$Flow,order.by=USGS$Date)
g2<-group2(f3)

#Get 7q10 for 10 year period at our gauge
USGS_7q10<-xQy_dor(USGS,x=7,y=10)[1]
USGS_30q2<-xQy_dor(USGS,x=30,y=2)[1]
USGS_dor<-xQy_dor(USGS,x=7,y=10)[2]

#Finding the lowest 10% of flows in September
SeptFlows<-USGS$Flow[USGS$Month==9]
Sept10<-as.vector(quantile(SeptFlows,0.1));rm(SeptFlows)

#Finding August Low Flow
Aug<-USGS[USGS$Month==8,]
AugFlows<-group_by(Aug,Year)
LFs<-summarize(AugFlows,minFlow=min(Flow))
ALF<-median((LFs$minFlow))

#Calculating 10% flow
q10<-as.numeric(quantile(USGS$Flow,0.1))

#Calculatnig 25% flow
q25<-as.numeric(quantile(USGS$Flow,0.25))

#Calculating 50% flow (median)
q50<-as.numeric(quantile(USGS$Flow,0.5))

#Calculating 75% flow
q75<-as.numeric(quantile(USGS$Flow,0.75))

#Calculating 90% flow
q90<-as.numeric(quantile(USGS$Flow,0.9))

#Caluclating mean flow and flow equal to zero
avg<-mean(USGS$Flow)
zero<-sum(USGS$Flow==0)

#Organize above calculations into useful output
a<-data.frame(ALF=ALF,DoR=USGS_dor,Sept10=Sept10,'7q10'=USGS_7q10,'xQy'=USGS_30q2,
              "10%"=q10,"25%"=q25,"50%"=q50,"75%"=q75,"90%"=q90,"Zero"=zero,Mean=avg)

#Getting Radford Gauge Data and organizing it 
siteNo<-"03171000"
pCode<-'00060'#discharge, table 2
stat<-'00003'#mean, table 3
start.date<-'2000-01-01'
end.date<-'2010-12-31'
USGS_imp<-readNWISdv(siteNo,pCode,start.date,end.date)
USGS_imp<-USGS_imp[order(USGS_imp$Date),]
names(USGS_imp)<-c('Agency','Site','Date','Flow','QC')

#Comparing Radford and Allisonia flow
plot(USGS_imp$Date,USGS_imp$Flow, xlab='Date',ylab = 'Flow (cfs)', type='l', col='red')
lines(USGS$Date, USGS$Flow, col='blue')

#Finding ALF,DOR,September 10%, 7q10, 30q2, average and quantile calculations for Radford
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
rm(b,i)
USGS_imp_7q10<-xQy_dor(USGS_imp,x=7,y=10)[1]
USGS_imp_30q2<-xQy_dor(USGS_imp,x=30,y=2)[1]
USGS_imp_dor<-xQy_dor(USGS_imp,x=7,y=10)[2]
SeptFlows<-as.vector((select(filter(USGS_imp,Month==9),Flow))$Flow)
Sept10<-as.vector(quantile(SeptFlows,0.1));rm(SeptFlows)
ALF<-median(as.vector(summarize(group_by(filter(USGS_imp,Month==8),Year),minFlow=min(Flow)))$minFlow)
q10<-as.numeric(quantile(USGS_imp$Flow,0.1))
q25<-as.numeric(quantile(USGS_imp$Flow,0.25))
q50<-as.numeric(quantile(USGS_imp$Flow,0.5))
q75<-as.numeric(quantile(USGS_imp$Flow,0.75))
q90<-as.numeric(quantile(USGS_imp$Flow,0.9))
avg<-mean(USGS_imp$Flow)
zero<-sum(USGS_imp$Flow==0)
b<-data.frame(ALF=ALF,DoR=USGS_imp_dor,Sept10=Sept10,'7q10'=USGS_imp_7q10,'xQy'=USGS_imp_30q2,
              "10%"=q10,"25%"=q25,"50%"=q50,"75%"=q75,"90%"=q90,"Zero"=zero,Mean=avg)

#Output Allisonia and Radford calculations to compare
a;b
#Shows that all stats increase when looking at Radford (downstream)

#Evaluate distribution of flow by creating flow duration curves (Show the probability of seeing a flow at or less than a given magnitude)
#For simplicity we plot quantile flow against probability to see difference in distributions
prob<-seq(0,1,0.01)
plot(prob,quantile(USGS$Flow,prob),type='l',xlab='Probability',ylab='Flow (cfs)')

#Change plot to logspace to see changes better
USGS$LogFlow<-log10(USGS$Flow)
USGS_imp$LogFlow<-log10(USGS_imp$Flow)
plot(prob,quantile(USGS$LogFlow,prob),type='l',xlab='Probability',ylab=expression('Log'[10]*'(Flow)'),col='red')
lines(prob,quantile(USGS_imp$LogFlow,prob),col='blue')
legend(x=0,y=4.5,legend=c("@ Allisonia","@ Radford"),col=c('red','blue'),cex=2,bty='n',lty=1,x.intersp = 0.5,y.intersp = 0.75)

#Install and use package that makes log cfs
install.packages('sfsmisc')
library(sfsmisc)
par(mar=c(5,12,2,4))
plot(prob,quantile(USGS$Flow,prob),type='l',col='red',ann=F,xaxt='n',yaxt='n',log="y")
lines(prob,quantile(USGS_imp$Flow,prob),col='blue')
axis(1,cex.axis=2,cex=2)
eaxis(2,log=T,cex.axis=2,cex=2)
mtext("Flow (cfs)",side=2,line=8,cex=2)
mtext("Probability",side=1,line=3,cex=2)
legend(x=0,y=50000,legend=c("@ Allisonia","@ Radford"),col=c('red','blue'),cex=2,bty='n',lty=1,x.intersp = 0.5,y.intersp = 0.75)

#Find drainage area of each site
Allisonia<-readNWISsite("03168000")
da_Alli<-Allisonia$drain_area_va

Radford<-readNWISsite("03171000")
da_Rad<-Radford$drain_area_va

#Predict flow by taking the downstream gauge, dividing by its area, and multiplying by the drainage area of the upstream gauge
pred<-USGS_imp$Flow*da_Alli/da_Rad

#Plot predicted data
par(mar=c(5,6,2,4),cex.axis=2,lwd=2,cex.lab=2)
plot(USGS$Date,pred,type='l',xlab='Date',ylab='Flow (cfs)')
lines(USGS$Date,USGS$Flow,col='red')

#Compare predicted flow to ovserved flow
plot(USGS$Flow,pred,xlab="Observed",ylab='Predicted')

#Create linear regression to see how good predicted vs observed
reg<-lm(pred~USGS$Flow)

#Plot regression
plot(USGS$Flow,pred,xlab="Observed",ylab='Predicted')
lines(USGS$Flow,fitted(reg),lwd=2)
mtext(paste0("Predicted = ",round(coef(reg)[1],2),"+",round(coef(reg)[2],2),"*Observed"),3,line=-6,at=12000,cex=1)
mtext(bquote(italic(R)^2 == .(format(as.numeric(summary(reg)[8]),digits = 2))),3,line=-8,cex=1,at=12000)

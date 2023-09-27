# This code will evaluate the Warm Springs Dam effects on the Dryn Run river in California when it was installed in 1982. The goal is to look at the flow metrics from 20 years before the dam was installed and 20 years after

#===============================================================================================================
#Turn on necessary packages. 
library(dataRetrieval)
library(PearsonDS)
library(dplyr)
library(lubridate)
library(zoo)
library(IHA) 

#================================================================================================================
#Get Site Info for 20 years Before 
siteNo<-"11465200"                                  #This is the gauge number that we are looking at 
pCode<-'00060'                                      #The parameter you are looking at 00060=Discharge in cfs
stat<-'00003'                                       #The stat code which specifies that we want mean
start.dateB<-'1962-01-01'                            #Date in year-month-day
end.dateB<-'1982-12-31'
USGSB<-readNWISdv(siteNo,pCode,start.dateB,end.dateB) #Putting together the data we need to get a gauge reading. Used readNWISdv because we wanted daily data

#================================================================================================================
#Get Site Info for 20 years After 
siteNo<-"11465200"                                  #This is the gauge number that we are looking at 
pCode<-'00060'                                      #The parameter you are looking at 00060=Discharge in cfs
stat<-'00003'                                       #The stat code which specifies that we want mean
start.dateA<-'1982-01-01'                            #Date in year-month-day
end.dateA<-'2002-12-31'
USGSA<-readNWISdv(siteNo,pCode,start.dateA,end.dateA) #Putting together the data we need to get a gauge reading. Used readNWISdv because we wanted daily data

#================================================================================================================
#Organizing Data
names(USGSB)<-c('Agency', 'Site', 'Date', 'Flow', 'QC') #Renaming columns so they make more sense 
USGSB<-USGSB[order(USGSB$Date),] #Ordering data based on date 
names(USGSA)<-c('Agency', 'Site', 'Date', 'Flow', 'QC') #Renaming columns so they make more sense 
USGSA<-USGSA[order(USGSA$Date),] #Ordering data based on date 

#================================================================================================================
#Plotting Flows
par(mfrow=c(1,2)) #Lets you plot both graphs in the same window 
plot(USGSB$Date, USGSB$Flow, xlab='Date', ylab='Flow (cfs)', type='l', col='blue', main="Flow of Dry Run California (1960-1980)", ylim=c(0,20000))#Creates plot of date on x and flow on y
plot(USGSA$Date, USGSA$Flow, xlab='Date', ylab='Flow (cfs)', type='l', col='red', main="Flow of Dry Run California (1981-2001)", ylim=c(0,20000))#Creates plot of date on x and flow on y

#================================================================================================================
#Evaluating Before data by water year 
USGSB$WY<-numeric(length(USGSB$Date)) #Adds a new column 'WY' or water year to the USGS data and makes it as long as the Date column
b<-year(USGSB$Date[1]) #b is the water year counter starting at the 1st date value
#i is the current value that we are on, it increases every time we go through the loop
for (i in 1:length(USGSB$Date)){USGSB$WY[i]<-b-1 #We do b-1 becuase even though the beginning of the year is in 2000 the water year is still in 1999 b/c it started in Oct 1999
if (USGSB$Date[i]>=as.Date(paste0(b,"-10-01")))
{b<-b+1 #If Oct 1st is passed add a year to the water year
USGSB$WY[i]<-b-1}}  

#Grouping Data by year
USGSB$Month<-month(USGSB$Date)
USGSB$Year<-year(USGSB$Date)
aligroupB<-group_by(USGSB,Year)#New grouping where data is grouped by year 

#==================================================================================================================
#Evaluating After data by water year 
USGSA$WY<-numeric(length(USGSA$Date)) #Adds a new column 'WY' or water year to the USGS data and makes it as long as the Date column
a<-year(USGSA$Date[1]) #b is the water year counter starting at the 1st date value
#i is the current value that we are on, it increases every time we go through the loop
for (k in 1:length(USGSA$Date)){USGSA$WY[k]<-a-1 #We do b-1 becuase even though the beginning of the year is in 2000 the water year is still in 1999 b/c it started in Oct 1999
if (USGSA$Date[k]>=as.Date(paste0(a,"-10-01")))
{a<-a+1 #If Oct 1st is passed add a year to the water year
USGSA$WY[k]<-a-1}}  

#Grouping Data by year
USGSA$Month<-month(USGSA$Date)
USGSA$Year<-year(USGSA$Date)
aligroupA<-group_by(USGSA,Year)#New grouping where data is grouped by year 

#================================================================================================================
#Finding max flow for each year from our before data
peaksB<-as.data.frame(summarize(aligroupB,MaxFlow=max(Flow)))

#Finding max flow for each year from USGS online before data
peakUSGSB<-readNWISpeak(siteNo,start.dateB,end.dateB)

#Adding Year column to peaksUSGSB
peakUSGSB$Year<-year(peakUSGSB$peak_dt)

#Comparing our peak data and USGS peak data from before
plot(peakUSGSB$Year,peakUSGSB$peak_va,xlab='Date',ylab='Flow (cfs)',type='l', main='Peak Flow Comparisons Before Dam Addition')
lines(peaksB$Year,peaksB$MaxFlow,col='blue')
legend(x=1961, y=7000, legend=c("Calculations", "USGS Data"),col=c('black','blue'), bty='n', lty=1, cex=1.5)

#================================================================================================================
#Finding max flow for each year from our before data
peaksA<-as.data.frame(summarize(aligroupA,MaxFlow=max(Flow)))

#Finding max flow for each year from USGS online before data
peakUSGSA<-readNWISpeak(siteNo,start.dateA,end.dateA)

#Adding Year column to peaksUSGSA
peakUSGSA$Year<-year(peakUSGSA$peak_dt)

#Comparing our peak data and USGS peak data from before
plot(peakUSGSA$Year,peakUSGSA$peak_va,xlab='Date',ylab='Flow (cfs)',type='l', main='Peak Flow Comparisons After Dam Addition')
lines(peaksA$Year,peaksA$MaxFlow,col='blue')
legend(x=1996, y=12000, legend=c("Calculations", "USGS Data"),col=c('black','blue'), bty='n', lty=1, cex=1.5)

#=================================================================================================================
#Comparing Peak Flows before and after dam addition
#USGS Compare
par(mfrow=c(1,2)) #Lets you plot both graphs in the same window 
plot(peakUSGSB$peak_va, col='blue', xlab='Year Increment',ylab='Flow (cfs)',type='l', main='                                                                                        Peak Flow Comparisions') 
lines(peakUSGSA$peak_va, col='orange')
legend(x=14, y=32000, legend=c("USGS Before", "USGS After"),col=c('blue','orange'), bty='n', lty=1)

#Calculations Compare
plot(peaksB$MaxFlow, col='green', xlab='Year Increment',ylab='Flow (cfs)',type='l', main='Before and After Dam Addition                                                                                     ')
lines(peaksA$MaxFlow, col='red')
legend(x=14, y=18000, legend=c("Before Calc", "After Calc"),col=c('green','red'), bty='n', lty=1)

#All Compare
par(mfrow=c(1,1))
plot(peakUSGSB$peak_va, col='blue', xlab='Year Increment',ylab='Flow (cfs)',type='l', main='Peak Flow Comparisons Before and After Dam Addition')
lines(peaksB$MaxFlow, col='green')
lines(peaksA$MaxFlow, col='red')
lines(peakUSGSA$peak_va, col='orange')
legend(x=15, y=32000, legend=c("Before Calc", "After Calc","USGS Before", "USGS After"),col=c('green','red','blue','orange'), bty='n', lty=1, cex=1.5)

#=================================================================================================================
#Finding xQy statistics (flow metrics)
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

#=======================================================================================================================
#Get 7q10, 30q2 and DOR for Before
USGSB_7q10<-xQy_dor(USGSB,x=7,y=10)[1]
USGSB_30q2<-xQy_dor(USGSB,x=30,y=2)[1]
USGSB_dor<-xQy_dor(USGSB,x=7,y=10)[2]

#Finding the lowest 10% of flows in September
SeptFlowsB<-USGSB$Flow[USGSB$Month==9]
Sept10B<-as.vector(quantile(SeptFlowsB,0.1));rm(SeptFlowsB)

#Finding August Low Flow
AugB<-USGSB[USGSB$Month==8,]
AugFlowsB<-group_by(AugB,Year)
LFsB<-summarize(AugFlowsB,minFlow=min(Flow))
ALFB<-median((LFsB$minFlow))

#Finding Quantiles
q10B<-as.numeric(quantile(USGSB$Flow,0.1))
q25B<-as.numeric(quantile(USGSB$Flow,0.25))
q50B<-as.numeric(quantile(USGSB$Flow,0.5))
q75B<-as.numeric(quantile(USGSB$Flow,0.75))
q90B<-as.numeric(quantile(USGSB$Flow,0.9))
avgB<-mean(USGSB$Flow)
zeroB<-sum(USGSB$Flow==0)
#=======================================================================================================================
#Get 7q10, 30q2 and DOR for After
USGSA_7q10<-xQy_dor(USGSA,x=7,y=10)[1]
USGSA_30q2<-xQy_dor(USGSA,x=30,y=2)[1]
USGSA_dor<-xQy_dor(USGSA,x=7,y=10)[2]

#Finding the lowest 10% of flows in September
SeptFlowsA<-USGSA$Flow[USGSA$Month==9]
Sept10A<-as.vector(quantile(SeptFlowsA,0.1));rm(SeptFlowsA)

#Finding August Low Flow
AugA<-USGSA[USGSA$Month==8,]
AugFlowsA<-group_by(AugA,Year)
LFsA<-summarize(AugFlowsA,minFlow=min(Flow))
ALFA<-median((LFsA$minFlow))

#Finding Quantiles
q10A<-as.numeric(quantile(USGSA$Flow,0.1))
q25A<-as.numeric(quantile(USGSA$Flow,0.25))
q50A<-as.numeric(quantile(USGSA$Flow,0.5))
q75A<-as.numeric(quantile(USGSA$Flow,0.75))
q90A<-as.numeric(quantile(USGSA$Flow,0.9))
avgA<-mean(USGSA$Flow)
zeroA<-sum(USGSA$Flow==0)

#=========================================================================================================================
#Organize above calculations into useful output
BeforeFlows<-data.frame(ALF=ALFB,DoR=USGSB_dor,Sept10=Sept10B,'7q10'=USGSB_7q10,'xQy'=USGSB_30q2,
              "10%"=q10B,"25%"=q25B,"50%"=q50B,"75%"=q75B,"90%"=q90B,"Zero"=zeroB,Mean=avgB)#Organize above calculations into useful output
AfterFlows<-data.frame(ALF=ALFA,DoR=USGSA_dor,Sept10=Sept10A,'7q10'=USGSA_7q10,'xQy'=USGSA_30q2,
              "10%"=q10A,"25%"=q25A,"50%"=q50A,"75%"=q75A,"90%"=q90A,"Zero"=zeroA,Mean=avgA)

#Output
BeforeFlows
AfterFlows

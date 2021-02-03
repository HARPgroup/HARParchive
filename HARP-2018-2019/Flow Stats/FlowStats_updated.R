rm(list=ls())
#Clear Global Envrionment and prepare to load data. First, call all relevant libraries
#IHA and Pearson DS used for 7Q10 calculations
#dplyr and lubridate used for r formatting, particularly for dates
#EcoHydRology called for its baseflow separation algorithim
library('IHA')
library(PearsonDS)
library(dplyr)
library(lubridate)
library(EcoHydRology)

#Call up VA Hydro functions to load data as well as the stormseparation code
#Be sure to use the modified VA Hydro loading function that does not require data to be in zoo format upon output
fxn_locations = 'C:/Users/connorb5/Desktop/GitHub/hydro-tools'

source(paste(fxn_locations,"VAHydro-1.0","fn_vahydro-1.0.R", sep = "/"))
source(list.files()[1])
source(paste(fxn_locations,"LowFlow/fn_iha.R", sep = "/"))
source("G:/My Drive/Impoundment Modeling/Model Runs/R Code/Stormsep.R")

#Load impounded run and formate date variables
elid<-339991
runid<-5021
dat<-fn_get_runfile(elid, runid,outaszoo=FALSE)
dat$timestamp<-as.POSIXct(dat$timestamp,origin='1970-01-01 00:00.00 UTC')
dat$thisdate<-as.Date(dat$thisdate)
dat<-dat[order(dat$timestamp),]

#Find water year
b<-year(dat$thisdate[1])
for (i in 1:length(dat$thisdate)){
  dat$WY[i]<-b
  if(dat$thisdate[i]>=as.Date(paste0(b,"-10-01"))){
    b<-b+1
    dat$WY[i]<-b
  }
}

#Format data for more extensive analysis
dat$Month<-month(dat$thisdate)
dat$Year<-year(dat$thisdate)

#Create a mean-aggregated daily dataset from the hourly data
daily<-as.data.frame(summarize(group_by(dat,thisdate),Qout=mean(Qout),Month=mean(month),Year=mean(Year),WY=mean(WY)))

#Create a function for both hourly and daily xQy analysis (e.g. 7Q10)
#Also compute drought of record, dor
#x is the number of consecutive days in an xQy analysis, y is the return period
xQy_dor<-function(dat,x,y,timestep){
  dor_time<-90
  timecheck<-365
  if(timestep==3600){
    #If hourly data, then run conduct analysis in longer time blocks
    #e.g. instead of 7 days in a 7Q10, use 7*24 hours (equivalent to 7 days)
    x<-x*24
    dor_time<-90*24
    timecheck<-8750#Hours in a day, adjusting for loss of random values near water year
  }
  xQy<-numeric()
  dor<-numeric()
  ndays<-numeric()
  for(i in 1:length(unique(dat$WY))){
    #Find the minimum of the rolling x averages
    #Also compute drought of record, dor
    subst<-dat[dat$WY==unique(dat$WY)[i],]
    xQy[i]<-min(rollapply(subst$Qout,x,mean))
    dor[i]<-min(rollapply(subst$Qout,dor_time,mean))
    ndays[i]<-length(subst$thisdate)
    
  }
  xQy<-xQy[which(ndays>=timecheck)]#Get rid of incompete water years
  xQy<-xQy[xQy>0]
  xQy<-log(xQy)
  n<-length(xQy)
  N<-length(ndays[ndays>=timecheck])
  #Adjust probability as suggested in USGS pub (and by previous HARP team)
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

#Create hourly 7q10-like stats
imp_7q10<-xQy_dor(dat,x=7,y=10,timestep=3600)[1]
imp_30q2<-xQy_dor(dat,x=30,y=2,timestep=3600)[1]
dor_imp<-xQy_dor(dat,x=7,y=10,timestep=3600)[2]


#Calc other hourly stats
#September 10%
SeptFlows<-as.vector((select(filter(dat,Month==9),Qout))$Qout)
Sept10<-as.vector(quantile(SeptFlows,0.1));rm(SeptFlows)
#August Low Flow
ALF<-median(as.vector(summarize(group_by(filter(dat,Month==8),Year),minFlow=min(Qout)))$minFlow)
#Rise, fall, duration from storm separation script
outStorms<-stormSeparate(dat$timestamp,dat$Qout,plt = F)
output<-outStorms$Stats
output<-output[output$RsqR>=0.7&output$RsqF>0.7,]
rise<-mean(output$rising,na.rm=T)
fall<-mean(output$falling,na.rm=T)
durAll<-mean(output$durAll,na.rm=T)
#Mean baseflow
bf<-BaseflowSeparation(dat$Qout,passes=3)
bf<-mean(bf$bt)
#Quantile flow
q10<-as.numeric(quantile(dat$Qout,0.1))
q25<-as.numeric(quantile(dat$Qout,0.25))
q50<-as.numeric(quantile(dat$Qout,0.5))
q75<-as.numeric(quantile(dat$Qout,0.75))
q90<-as.numeric(quantile(dat$Qout,0.9))
q99<-as.numeric(quantile(dat$Qout,0.99))
avg<-mean(dat$Qout)
#Number of timesteps of zero flow
zero<-sum(dat$Qout==0)
a<-data.frame(ALF=ALF,DoR=dor_imp,Sept10=Sept10,'7q10'=imp_7q10,'xQy'=imp_30q2,'Rise'=rise,'Fall'=fall,
              durAll=durAll,bf=bf,"10%"=q10,"25%"=q25,"50%"=q50,"75%"=q75,"90%"=q90,'99%'=q99,"Zero"=zero,Mean=avg)

#Rerun all calculations from a daily temporal resolution
#Create daily 7q10-like stats
imp_7q10<-xQy_dor(daily,x=7,y=10,timestep=86400)[1]
imp_30q2<-xQy_dor(daily,x=30,y=2,timestep=86400)[1]
dor_imp<-xQy_dor(daily,x=7,y=10,timestep=86400)[2]


#Calc other daily stats
SeptFlows<-as.vector((select(filter(daily,Month==9),Qout))$Qout)
Sept10<-as.vector(quantile(SeptFlows,0.1));rm(SeptFlows)
ALF<-median(as.vector(summarize(group_by(filter(daily,Month==8),Year),minFlow=min(Qout)))$minFlow)
outStorms<-stormSeparate(daily$thisdate,daily$Qout,plt = F)
output<-outStorms$Stats
output<-output[output$RsqR>=0.7&output$RsqF>0.7,]
rise<-mean(output$rising,na.rm=T)
fall<-mean(output$falling,na.rm=T)
durAll<-mean(output$durAll,na.rm=T)
bf<-BaseflowSeparation(daily$Qout,passes=3)
bf<-mean(bf$bt)
q10<-as.numeric(quantile(daily$Qout,0.1))
q25<-as.numeric(quantile(daily$Qout,0.25))
q50<-as.numeric(quantile(daily$Qout,0.5))
q75<-as.numeric(quantile(daily$Qout,0.75))
q90<-as.numeric(quantile(daily$Qout,0.9))
q99<-as.numeric(quantile(daily$Qout,0.99))
avg<-mean(daily$Qout)
zero<-sum(daily$Qout==0)
b<-data.frame(ALF=ALF,DoR=dor_imp,Sept10=Sept10,'7q10'=imp_7q10,'xQy'=imp_30q2,'Rise'=rise,'Fall'=fall,
              durAll=durAll,bf=bf,"10%"=q10,"25%"=q25,"50%"=q50,"75%"=q75,"90%"=q90,'99%'=q99,"Zero"=zero,Mean=avg)

stats<-rbind(a,b)
rownames(stats)<-c("Hourly",'Daily');stats

#cleaned up Case study 1
# Kelsey Reitz 24 May 2018]


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



# Overall Hydrograph 1962-2002 --------------------------------------------

# Dry Run California: Gauge number 11465200
# Get Gauge information from USGS
siteNo<-"11465200"
pCode<-'00060'
stat<-'00003'
start.date<-'1962-01-01'  #Start date is 20 years before 1982
end.date<-'2002-12-31'    #End date is 20 years after 1982
USGS<-readNWISdv(siteNo,pCode,start.date,end.date)

#Reorder the column names of USGS into understandable terms. Order the data in order of time.
names(USGS)
names(USGS) <- c('Agency', 'Site', 'Date', 'Flow', 'QC')
USGS <- USGS[order(USGS$Date),] 

#create new columns using the lubridate package (month, year functions)
USGS$Month <- month(USGS$Date)                                    #new column created in USGS for months
USGS$Year <- year(USGS$Date)                                      #new column created in USGS for year
aligroup <- group_by(USGS, Year)                                  #aligroup variable is a data frame in which years are grouped togehter
peaks <- as.data.frame(summarize(aligroup, MaxFlow=max(Flow)))    #finds max (peak) flow for each year by summarizing from aligroup and finding max flow
peakUSGS <- readNWISpeak(siteNo, start.date, end.date)            #USGS does same thing from NWIS raw data

#plot data to generate raw hydrograph for years 
plot(USGS$Date, USGS$Flow) # plot data and make it look pretty
par(mar=c(5,6,2,4),cex.axis=2, lwd=2, cex.lab=2)  
plot(USGS$Date, USGS$Flow, xlab='Date', ylab='Flow (cfs)') #create labels for axes
plot(USGS$Date,USGS$Flow, xlab='Date', ylab='Flow (cfs)',type = 'l') #this is an l not a 1! 
lines(USGS$Date, USGS$Flow, col = 'black')

##NOTE: FROM HERE USGS DATA FRAME IS IGNORED. NO FURTHER CALCULATIONS ARE DONE ON THIS. 

# Pull from NWIS: 1962-1982 and 1982-2002----------------------------------------------------

# Get Gauge information from USGS for 62-82
AsiteNo<-"11465200"
ApCode<-'00060'
Astat<-'00003'
Astart.date<-'1962-01-01'  #Start date is 20 years before 1982
Aend.date<-'1982-12-31'    #End date is 20 years after 1982
predam<-readNWISdv(AsiteNo,ApCode,Astart.date,Aend.date)

# Get Gauge information from USGS for 82-02
BsiteNo<-"11465200"
BpCode<-'00060'
Bstat<-'00003'
Bstart.date<-'1982-01-01'  #Start date is 20 years before 1982
Bend.date<-'2002-12-31'    #End date is 20 years after 1982
postdam<-readNWISdv(BsiteNo,BpCode,Bstart.date,Bend.date)


# Reorder based on date and plot data -------------------------------------

#Reorder the column names of USGS into understandable terms. Order the data in order of time.
names(predam)
names(predam) <- c('Agency', 'Site', 'Date', 'Flow', 'QC')
names(postdam)
names(postdam) <- c('Agency', 'Site', 'Date', 'Flow', 'QC')

predam <- predam[order(predam$Date),]
postdam <- postdam[order(postdam$Date),] 

#plot data to generate pre and post dam hydrograph
par(mfrow=c(1,2))
plot(predam$Date, predam$Flow, type = 'l', ylim = c(0,20000), ylab='Flow before dam (cfs)', xlab = 'year')
plot(postdam$Date, postdam$Flow, type = 'l', ylim = c(0,20000), ylab='Flow after dam (cfs)', xlab = 'year')


# Water Year Calculation --------------------------------------------------
predam$WY<-numeric(length(predam$Date)) #create a new column used to store water year
postdam$WY<-numeric(length(postdam$Date)) #create a new column used to store water year

#predam loop based on water year iteration:
predam$WY<-numeric(length(predam$Date))
b<-year(predam$Date[1])              #initialize b variable to be first year of USGS date column
for (i in 1:length(predam$Date)){    #create for loop to end of Date data, starting at 1, ending at last entry
  predam$WY[i] <- b-1                #wy[i] = date year -1
  if(predam$Date[i]>=as.Date(paste0(b, "-10-01"))){ #if the date is > or = to Oct 1, add one to b (year = year)
    b <- b+1
    predam$WY[i]<-b-1
  }
}
#postdam loop based on water year iteration:
postdam$WY<-numeric(length(postdam$Date))
b<-year(postdam$Date[1])              #initialize b variable to be first year of USGS date column
for (i in 1:length(postdam$Date)){    #create for loop to end of Date data, starting at 1, ending at last entry
  postdam$WY[i] <- b-1                #wy[i] = date year -1
  if(postdam$Date[i]>=as.Date(paste0(b, "-10-01"))){ #if the date is > or = to Oct 1, add one to b (year = year)
    b <- b+1
    postdam$WY[i]<-b-1
  }
}

# Lubridate package: create new columns -----------------------------------

#create new columns using the lubridate package for predam
predam$Month <- month(predam$Date)
predam$Year <- year(predam$Date)
predam_aligroup <- group_by(predam, Year)
predam_peaks <- as.data.frame(summarize(predam_aligroup, MaxFlow=max(Flow))) #finds max flow for each year and stores in data frame
peak_predam <- readNWISpeak(AsiteNo, Astart.date, Aend.date) #USGS does same thing from NWIS

#create new columns using the lubridate package for postdam
postdam$Month <- month(postdam$Date)
postdam$Year <- year(postdam$Date)
postdam_aligroup <- group_by(postdam, Year)
postdam_peaks <- as.data.frame(summarize(postdam_aligroup, MaxFlow=max(Flow))) #finds max flow for each year and stores in data frame
peak_postdam <- readNWISpeak(BsiteNo, Bstart.date, Bend.date) #USGS does same thing from NWIS

#plot peak vs USGS predam and postdam

#first need to create a year column in peak_predam and peak_postdam
peak_predam$Year <- year(peak_predam$peak_dt)
peak_postdam$Year <- year(peak_postdam$peak_dt)

par(mfrow=c(1,2))
plot(peak_predam$Year, peak_predam$peak_va, xlab='Date', ylab='Flow(cfs)', type='l')
lines(predam_peaks$Year, predam_peaks$MaxFlow, col = 'blue')
legend(1960, 5000, legend=c("USGS Peak Flow","Predicted Peak Flow"),col=c('black','blue'), cex=1, bty='n', lty=1, x.intersp = 0.5, y.intersp = 0.75)

plot(peak_postdam$Year, peak_postdam$peak_va, xlab='Date', ylab='Flow (cfs)', type = 'l')
lines(postdam_peaks$Year, postdam_peaks$MaxFlow, col = 'blue', type='l', xlab='Date', ylab='Flow (cfs)')

#match axes
par(mfrow=c(1,2))
plot(peak_predam$Year, peak_predam$peak_va, xlab='Date', ylab='Flow(cfs)', type='l', ylim=c(0,32000))
lines(predam_peaks$Year, predam_peaks$MaxFlow, col = 'blue')
legend(1960, 5000, legend=c("USGS Peak Flow","Predicted Peak Flow"),col=c('black','blue'), cex=1, bty='n', lty=1, x.intersp = 0.5, y.intersp = 0.75)

plot(peak_postdam$Year, peak_postdam$peak_va, xlab='Date', ylab='Flow (cfs)', type = 'l', ylim=c(0,32000))
lines(postdam_peaks$Year, postdam_peaks$MaxFlow, col = 'blue', type='l', xlab='Date', ylab='Flow (cfs)')

# Function to calculate xqy statistics: 7q10, 30q2: ----------------------------------
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
#-------------------------------------------------------------------------------------------

# Flow Metrics  -----------------------------------------------------------

# for the predam
predam_7q10<-xQy_dor(predam,x=7,y=10)[1]
predam_30q2<-xQy_dor(predam,x=30,y=2)[1]
predam_dor<-xQy_dor(predam,x=7,y=10)[2]
predam_SeptFlows <- predam$Flow[predam$Month==9] #Find the flows in the flow column only for september
predam_Sept10 <- as.vector(quantile(predam_SeptFlows, 0.1)); rm(predam_SeptFlows)

predam_Aug <- predam[predam$Month==8 ,]
predam_AugFlows <- group_by(predam_Aug, Year)
predam_LFs <- summarize(predam_AugFlows, minFlow=min(Flow)) #provides a summary  of AugFlows and minimum flows
predam_ALF <- median(predam_LFs$minFlow) # find median minimum flow for August low flow

Aq10<-as.numeric(quantile(predam$Flow,0.1))
Aq25<-as.numeric(quantile(predam$Flow,0.25))
Aq50<-as.numeric(quantile(predam$Flow,0.5))
Aq75<-as.numeric(quantile(predam$Flow,0.75))
Aq90<-as.numeric(quantile(predam$Flow,0.9))
Aavg<-mean(predam$Flow)
Azero<-sum(predam$Flow==0)

#for the postdam
postdam_7q10<-xQy_dor(postdam,x=7,y=10)[1]
postdam_30q2<-xQy_dor(postdam,x=30,y=2)[1]
postdam_dor<-xQy_dor(postdam,x=7,y=10)[2]
postdam_SeptFlows <- postdam$Flow[postdam$Month==9] #Find the flows in the flow column only for september
postdam_Sept10 <- as.vector(quantile(postdam_SeptFlows, 0.1)); rm(postdam_SeptFlows)

postdam_Aug <- postdam[postdam$Month==8 ,]
postdam_AugFlows <- group_by(postdam_Aug, Year)
postdam_LFs <- summarize(postdam_AugFlows, minFlow=min(Flow)) #provides a summary  of AugFlows and minimum flows
postdam_ALF <- median(postdam_LFs$minFlow) # find median minimum flow for August low flow

Bq10<-as.numeric(quantile(postdam$Flow,0.1))
Bq25<-as.numeric(quantile(postdam$Flow,0.25))
Bq50<-as.numeric(quantile(postdam$Flow,0.5))
Bq75<-as.numeric(quantile(postdam$Flow,0.75))
Bq90<-as.numeric(quantile(postdam$Flow,0.9))
Bavg<-mean(postdam$Flow)
Bzero<-sum(postdam$Flow==0)

#produce a dataframe that gives the output of all these values: 
a<-data.frame(ALF=predam_ALF,DoR=predam_dor,Sept10=predam_Sept10,'7q10'=predam_7q10,'xQy'=predam_30q2,
              "10%"=Aq10,"25%"=Aq25,"50%"=Aq50,"75%"=Aq75,"90%"=Aq90,"Zero"=Azero,Mean=Aavg)
b<-data.frame(ALF=postdam_ALF,DoR=postdam_dor,Sept10=postdam_Sept10,'7q10'=postdam_7q10,'xQy'=postdam_30q2,
              "10%"=Bq10,"25%"=Bq25,"50%"=Bq50,"75%"=Bq75,"90%"=Bq90,"Zero"=Bzero,Mean=Bavg)
c<-rbind(a,b); 
#display the difference in flow metrics in one column: postdam - predam
rownames(c)<-c("predam", "postdam"); c
d =  b - a; 
print(c); 
print(d)

# Peak Flow plotting: --------------------------------------------------
#display the difference in pre dam and post dam: on one graph

plot(unique(USGS$WY),dor,type='l',xlab='Water Year',ylab='Flow (cfs)',ylim=c(100,3000))
lines(unique(USGS$WY),xQy,col='blue')
legend(x=2004,y=3000,legend=c("90 day min flow","7 day min flow"),col=c('black','blue'),cex=2,bty='n',lty=1,x.intersp = 0.5,y.intersp = 0.75)



# Plot by predamWY and postdamWY against dor ------------------------------

#initialize variables
#using rollapply:
AxQy<-numeric()
Ador<-numeric()
Andays<-numeric()
BxQy<-numeric()
Bdor<-numeric()
Bndays<-numeric()
x<-7
y<-10
dor_time<-90


#create a dor for predam
for(i in 1:length(unique(predam$WY))){
  Asubst<-predam[predam$WY==unique(predam$WY)[i],]
  AxQy[i]<-min(rollapply(Asubst$Flow,x,mean))
  predam_dor[i]<-min(rollapply(Asubst$Flow,dor_time,mean))
  Andays[i]<-length(Asubst$Date)
}

#create a dor for postdam
for(i in 1:length(unique(postdam$WY))){
  Bsubst<-postdam[postdam$WY==unique(postdam$WY)[i],]
  BxQy[i]<-min(rollapply(Bsubst$Flow,x,mean))
  postdam_dor[i]<-min(rollapply(Bsubst$Flow,dor_time,mean))
  Bndays[i]<-length(Bsubst$Date)
}

layout(matrix(c(1,1,2,2), 2,2, byrow=TRUE), widths=c(2,1), heights=c(1,1))
plot(unique(predam$WY), predam_dor,type='l',xlab='Water Year',ylab='Flow (cfs)',ylim=c(0,100))
lines(unique(predam$WY),AxQy,col='blue')
legend("topleft", legend= c("USGS pre-dam dor","Predicted dor"),col=c('black','blue'), cex=1.5, bty='n', lty=1, x.intersp = 0.5, y.intersp = 0.75)


plot(unique(postdam$WY), postdam_dor,type='l',xlab='Water Year',ylab='Flow (cfs)',ylim=c(0,300))
lines(unique(postdam$WY),BxQy,col='blue')
legend("topleft", legend= c("USGS post-dam dor","Predicted dor"),col=c('black','blue'), cex=1.5, bty='n', lty=1, x.intersp = 0.5, y.intersp = 0.75)

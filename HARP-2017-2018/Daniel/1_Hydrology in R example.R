# Activating Libraries ------------------------------------------------------------------------------------------------

library(IHA)
library(dataRetrieval)
library(PearsonDS)
library(dplyr)
library(lubridate)


# Variable Assignment ----------------------------------------------------------------------------------------------------------

# Assigns USGS gage number
siteNo <- "03168000"

# Assigns parameter identifier -- 00060 for discharge (cfs), 00065 for gage height (ft), 00010 for temperature (C)
  #00045 for precipitation (in.), 00400 for pH
pCode <- '00060'

#Assigns statistical code -- 00001 for Maximum, 00002 for Minimum, 00003 for Mean, 00008 for Median
stat <- '00003'

#Assigns start date of data retrieval
start.date <- '2000-01-01'

#Assigns end date of data retrieval
end.date <- '2010-12-31'


# Data Retrieval ------------------------------------------------------------------------------------------------------

#Retrieves data from USGS website
USGS <- readNWISdv(siteNo, pCode, start.date, end.date)

#Renames columns of dataframe containing retrieved data -- descriptions available in USGS documentation
  #https://pubs.usgs.gov/tm/04/a10/pdf/tm4A10_appendix_1.pdf
names(USGS) <- c('Agency', 'Site', 'Date', 'Flow', 'QC')

#Reorders data in USGS dataframe to be organized by date
USGS <- USGS[order(USGS$Date),]


# Plotting --------------------------------------------------------------------------------------------------------------

#Plots flow over desired period at USGS gage
plot(USGS$Date, USGS$Flow)

#Alters aesthetics of plot -- increase text size, data points, labels, and margins
par(mar = c(5,6,2,4), cex.axis = 2, lwd = 2, cex.lab = 2)
#par: used to set/query graphical parameters
#mar: a numerical vector of the form c(bottom, left, top, right) which gives the number of lines of margin to be
  #specified on the four sides of the plot
#cex.axis: magnification used for axis annotation relative to the current cex setting
#lwd: line width, a positive number, defaulting to 1. Interpretation is device specific.
#cex.lab: magnification used for x and y labels relative to the current cex setting

#Labels axes
plot(USGS$Date, USGS$Flow, xlab = 'Date', ylab = 'Flow (cfs)')

#Turns graph into line graph
plot(USGS$Date, USGS$Flow, xlab = 'Date', ylab = 'Flow (cfs)', type = 'l')
#possible plot types include: p for points, l for lines, b for both, c for only the lines part of both,
  #o for both overplotted, h for histogram-like vertical lines, s for stair steps, S for other steps, n for none


# Further Analysis ------------------------------------------------------------------------------------------------------

# Adds column to USGS data frame to be used for water year classification
USGS$WY <- numeric(length(USGS$Date))
#numeric: creates an object that is interpretable as numbers (in this instance, with the length of the USGS$Date vector)

# Determines water year using iterator 'b' which increases by one every year at October 1st
b <- year(USGS$Date[1])
for (i in 1: length(USGS$Date)) {
  #in: tests whether a number is within a set of bounds
  USGS$WY[i] <- b-1
  if (USGS$Date[i] >= as.Date(paste0(b, "-10-01"))) {
    b <- b+1
    USGS$WY[i] <- b-1
  }
}

# Stores month and year of each data point using lubridate package, then groups by year
USGS$Month <- month(USGS$Date)
USGS$Year <- year(USGS$Date)
aligroup <- group_by(USGS, Year)
#group_by takes an existing table and converts it into a grouped table where operations are performed by group

# Finds maximum flow for each year and stores it in a data frame
peaks <- as.data.frame(summarize(aligroup, MaxFlow = max(Flow)))
#summarize: reduces multiple values down to a single value -- output will have one row for each group

# Retrieves peak flow data from USGS's NWIS system
peakUSGS <- readNWISpeak(siteNo, start.date, end.date)

# Plots calculated max flow and max flow from USGS
plot(peakUSGS$peak_va, xlab = 'Date', ylab = 'Flow (cfs)', type = 'l')
lines(peaks$MaxFlow, col = 'blue')


# Function calculating xQy statistics such as 7q10 and 30q2 -------------------------------------------------------------------

# Function xQy_dor
xQy_dor <- function(dat, x, y) {     #creates the xQy_dor function using inputs dat, x, and y
  dor_time <- 90
  timecheck <- 365
  xQy <- numeric()
  dor <- numeric()
  ndays <- numeric()
  for (i in 1:length(unique(dat$WY))) {
    #unique: returns a vector, data frame, or array with duplicate elements and rows removed
    subst <- dat[dat$WY == unique(dat$WY)[i],]     #creates a subset of data by water year (i)
    xQy[i] <- min(rollapply(subst$Flow, x, mean))
    #rollapply: applies a function to rolling margins of an array (such as a rolling average)
      #rollapply mandatory inputs: data, width, function to be applied
    dor[i] <- min(rollapply(subst$Flow, dor_time, mean))
    ndays[i] <- length(subst$Date)
    #length: returns the length of the () vector
  }
  xQy <- xQy[which(ndays >= timecheck)]     #Gets rid of incomplete water years
  #which: gives the TRUE indices of a logical object, allowing for array indices
  xQy <- xQy[xQy > 0]
  xQy <- log(xQy)
  n <- length(xQy)
  N <- length(ndays[ndays >= timecheck])
  Padj <- ((N/y)/n) - ((N-n)/n)
  if(Padj <= 0 | n <= 1) {
    #| is the or operator
    imp_XQY <- 0
  } else {
    pars <- PearsonDS:::pearsonIIIfitML(xQy)
    #Pearson III is Gamma distribution
    imp_XQY <- exp(qpearsonIII(Padj, params = pars$par))
  }
  dor_imp <- min(dor)
  if(dor_imp < 0) {
    dor_imp <-0
  }
  return(c(imp_XQY, dor_imp))
}

plot(unique(USGS$WY),dor,type='l',xlab='Water Year',ylab='Flow (cfs)',ylim=c(100,3000))
lines(unique(USGS$WY),xQy,col='blue')
legend(x=2004,y=3000,legend=c("90 day min flow","7 day min flow"),col=c('black','blue'),cex=2,bty='n',lty=1,x.intersp = 0.5,y.intersp = 0.75)
#legend: obviously used to add legends to plots
#cex: character expansion factor relative to the current par("cex"). Used for text.
#bty: type of box to be drawn around the legend. Allowed values: 'o' and 'n'
#lty: type of line appearing in the legend.  One of the two must be specified for a line drawing.
#x.intersp: character interspacing factor for horizontal (x) spacing
#y.intersp: the same for vertical (y) line distances

#CALL FUNCTION: OUTPUT <- xQy_dor(USGS, x=_, y=_)[1 for xQy, 2 for DOR]


# Examples within function ------------------------------------------------------------------------------------------

xQy<-numeric()
dor<-numeric()
ndays<-numeric()
x<-7
y<-10
dor_time<-90
unique(USGS$WY)
unique(USGS$WY)[1]
i=1
subst<-USGS[USGS$WY==unique(USGS$WY)[i],]
USGS[USGS$WY==unique(USGS$WY)[i]&USGS$Flow>5000,]
min(rollapply(subst$Flow,x,mean))
min(rollapply(subst$Flow,dor_time,mean))
for(i in 1:length(unique(USGS$WY))){
  subst<-USGS[USGS$WY==unique(USGS$WY)[i],]
  xQy[i]<-min(rollapply(subst$Flow,x,mean))
  dor[i]<-min(rollapply(subst$Flow,dor_time,mean))
  ndays[i]<-length(subst$Date)
}
plot(unique(USGS$WY),dor,type='l',xlab='Water Year',ylab='Flow (cfs)',ylim=c(100,3000))
lines(unique(USGS$WY),xQy,col='blue')
legend(x=2004,y=3000,legend=c("90 day min flow","7 day min flow"),col=c('black','blue'),cex=2,bty='n',lty=1,x.intersp = 0.5,y.intersp = 0.75)
boxplot(xQy,dor,names=c('Min 7 day','Min 90 day'),ylab='Flow (cfs)')
hist(xQy)
#hist: creates histograms
f3<-zoo(USGS$Flow,order.by=USGS$Date)
#zoo: creator for an S3 class of indexed totally ordered observations including irregular time series
g2 <- group2(f3)
#group2: calculates the group2 IHA statistics -- measures the magnitude of monthly water condition and includes 12 parameters


# Calculating additional flow metrics -- September10thPercentile, August Low Flow --------------------------------------

# September 10th Percentile Low Flow
SeptFlows <- USGS$Flow[USGS$Month == 9]
Sept10 <- as.vector(quantile(SeptFlows,0.1));rm(SeptFlows)
#quantile: produces sample quantiles corresponding to given probabilities.  The smallest observation corresponds to
  #a probability of 0 and the largest to a probability of 1.
#rm: removes objects from a specified environment -- specified as character strings, in a character vector "list" or
  #through a combination.  All specified objects are then removed.

#August Low Flow (median minimum August flow)
Aug<-USGS[USGS$Month==8,]
AugFlows<-group_by(Aug,Year)
LFs<-summarize(AugFlows,minFlow=min(Flow))
ALF<-median((LFs$minFlow))

#10%, 25%, 50%, 75%, 90% flows
q10<-as.numeric(quantile(USGS$Flow,0.1))
q25<-as.numeric(quantile(USGS$Flow,0.25))
q50<-as.numeric(quantile(USGS$Flow,0.5))
q75<-as.numeric(quantile(USGS$Flow,0.75))
q90<-as.numeric(quantile(USGS$Flow,0.9))
avg<-mean(USGS$Flow)
zero<-sum(USGS$Flow==0)

#Creating Dataframe and Outputting result
a<-data.frame(ALF=ALF,DoR=USGS_dor,Sept10=Sept10,'7q10'=USGS_7q10,'xQy'=USGS_30q2,
              "10%"=q10,"25%"=q25,"50%"=q50,"75%"=q75,"90%"=q90,"Zero"=zero,Mean=avg)
a


# Getting data from second gauge -- specific to example ------------------
siteNo<-"03171000"
pCode<-'00060'#discharge, table 2
stat<-'00003'#mean, table 3
start.date<-'2000-01-01'
end.date<-'2010-12-31'
USGS_imp<-readNWISdv(siteNo,pCode,start.date,end.date)
USGS_imp<-USGS_imp[order(USGS_imp$Date),]
#order: ordering permutation -- returns a permutation which rearranges its first argument into ascending or
  #descending order, breaking ties by further arguments -- sort.list is the same, only using one argument.
names(USGS_imp)<-c('Agency','Site','Date','Flow','QC')
#names() creates a vector of names for the columns of the matrix specified within the parentheses

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
rm(b,i)

#Calculates specific flow metrics using the previously defined xQy_dor function
USGS_imp_7q10<-xQy_dor(USGS_imp,x=7,y=10)[1]
USGS_imp_30q2<-xQy_dor(USGS_imp,x=30,y=2)[1]
USGS_imp_dor<-xQy_dor(USGS_imp,x=7,y=10)[2]

#Calculates Sep10 flow, ALF, and other metrics
SeptFlows<-as.vector((select(filter(USGS_imp,Month==9),Flow))$Flow)
#select: select() keeps only the variables you mention by name
#filter: finds rows/cases where conditions are true -- rows where the condition evaluates to NA are dropped
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

#Outputs results of both upstream and downstream gage
a;b

prob<-seq(0,1,0.01)
#seq: generates regular sequences
plot(prob,quantile(USGS$Flow,prob),type='l',xlab='Probability',ylab='Flow (cfs)')
USGS$LogFlow<-log10(USGS$Flow)
USGS_imp$LogFlow<-log10(USGS_imp$Flow)
plot(prob,quantile(USGS$LogFlow,prob),type='l',xlab='Probability',ylab=expression('Log'[10]*'(Flow)'),col='red')
lines(prob,quantile(USGS_imp$LogFlow,prob),col='blue')
legend(x=0,y=4.5,legend=c("@ Allisonia","@ Radford"),col=c('red','blue'),cex=2,bty='n',lty=1,x.intersp = 0.5,y.intersp = 0.75)

#Loads sfsmisc library
library(sfsmisc)

par(mar=c(5,12,2,4))
plot(prob,quantile(USGS$Flow,prob),type='l',col='red',ann=F,xaxt='n',yaxt='n',log="y")
#ann: if set to FALSE, high-level plotting functions calling plot.default do not annotate the plots they produce with
  #axis titles and overall titles.  Default is to do annotation.
#xaxt: a character specifying x axis type -- specifying "n" suppresses plotting of the axis. Same for yaxt.
lines(prob,quantile(USGS_imp$Flow,prob),col='blue')
axis(1,cex.axis=2,cex=2)
#axis: Adds an axis to the current plot, allowing specification of side, position, labels, and other options.
eaxis(2,log=T,cex.axis=2,cex=2)
#eaxis: extended/engineering axis for graphics -- labels function more prettily, especially for log-scale axes
mtext("Flow (cfs)",side=2,line=8,cex=2)
mtext("Probability",side=1,line=3,cex=2)
#mtext: writes text into one of the margins of the current figure region or the outer margins of the device region
legend(x=0,y=50000,legend=c("@ Allisonia","@ Radford"),col=c('red','blue'),cex=2,bty='n',lty=1,x.intersp = 0.5,y.intersp = 0.75)

Allisonia<-readNWISsite("03168000")
da_Alli<-Allisonia$drain_area_va

Radford<-readNWISsite("03171000")
names(Radford)

da_Rad<-Radford$drain_area_va
pred<-USGS_imp$Flow*da_Alli/da_Rad
par(mar=c(5,6,2,4),cex.axis=2,lwd=2,cex.lab=2)
plot(USGS$Date,pred,type='l',xlab='Date',ylab='Flow (cfs)')
lines(USGS$Date,USGS$Flow,col='red')
plot(USGS$Flow,pred,xlab="Observed",ylab='Predicted')
reg<-lm(pred~USGS$Flow)
plot(USGS$Flow,pred,xlab="Observed",ylab='Predicted')
lines(USGS$Flow,fitted(reg),lwd=2)
#Adds connected line segments to a plot -- a generic function taking coordinates given in various ways and joining
  #corresponding points with line segments.
mtext(paste0("Predicted = ",round(coef(reg)[1],2),"+",round(coef(reg)[2],2),"*Observed"),3,line=-6,at=12000,cex=1)
mtext(bquote(italic(R)^2 == .(format(as.numeric(summary(reg)[8]),digits = 2))),3,line=-8,cex=1,at=12000)
round(coef(reg)[2],2)
round(da_Alli/da_Rad,2)
# Kelsey Reitz
# Update and QA 2/6/2019
# Original Code writted 9/24/18
# Script to generate point source values for all Southern Rivers Segments in years
# 1984-2005. 
# Will evaluate if there is a value for point source, given 3000, 3007, or 3008. 

## load libraries
library(lubridate)
library(dplyr)
library(RCurl)

## set workspace and filepath
setwd("C:\\Users\\Kelsey\\Desktop\\GitHub\\hydro-tools\\HARP-2018\\PointSource_RoanokeAnalysis")
filepath <- "C:\\Users\\Kelsey\\Desktop\\GitHub\\hydro-tools\\HARP-2018\\PointSource_RoanokeAnalysis"


## Initial Link for deq site ----------------
deq <- "http://deq2.bse.vt.edu/p532c-sova/wdm/river/p532cal_062211/eos/"

#MUST CHANGE WHAT CODE YOU'RE DOING.-- changed by: (use 3000, 3007, or 3008) 
code <- 3000

## Set up link to all river segments in southern rivers ---------------
# (this is link to a csv in the drive) 

rivsegs <- data.frame(read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRoUnounQHFPHRkUhoDax3b9W4HFHr2IZFObifFAuH0DOknc4e-I2kdOT541Vfa9MX4HuupOU7-axtI/pub?output=csv", stringsAsFactors = FALSE))
rivsegs <- c(rivsegs[,1]) # use only column 1 of this data frame (pull out riv segs)



## Start loop to analyze point sources --------------------

i <- 1
years <- 1984:2005

#create dataframe "pointsource" to store point source values from the loop
pointsource <- data.frame(matrix(nrow=length(rivsegs), ncol=(length(years))+1))
pointsource[,1] <- rivsegs
colnames(pointsource) <- c("rivsegs", as.character(years))


# create dataframe to determine whether bash needs to be run for data: 
i <- 1
runbash <- data.frame(matrix(nrow=length(rivsegs), ncol=2))

for (i in 1:nrow(pointsource)){
  #pull out the ith river segment, and specify the csv file from deq2.bse
  study_seg1 <- pointsource$rivsegs[i]
  study_seg <- paste0("ps_sep_div_ams_p532cal_062211_",study_seg1,"_",code,".csv")
  
  #check to make sure that the file exists on the site. 
  #if it doesn't exist, move on to the next segment
  goodtogo <- url.exists(paste0(deq, study_seg)) # check to make sure csv exists
  
  runbash[i,1] <- study_seg1
  runbash[i,2] <- goodtogo
}

if ((sum(runbash$X2)==length(rivsegs))==TRUE) {
  print("Good to Go")
  } else {
  print("Need to Run Bash")
  }



i <- 1
for (i in 1:nrow(pointsource)){

  #pull out the ith river segment, and specify the csv file from deq2.bse
  study_seg <- pointsource$rivsegs[i]
  study_seg <- paste0("ps_sep_div_ams_p532cal_062211_",study_seg,"_",code,".csv")
  
  
  #check to make sure that the file exists on the site. 
  #if it doesn't exist, move on to the next segment
  goodtogo <- url.exists(paste0(deq, study_seg)) # check to make sure csv exists
  
  
  if (goodtogo ==TRUE){
  importdata <- read.csv(paste0(deq, study_seg))
  colnames(importdata) <- c("year", "month", "day", "hour", "ps")
  
  # determine if sum of point source values is 0 or not: 
  # if it is zero, output NA into every year
  # if it is not zero, output mean for every year 
  iszero <- sum(importdata$ps)==0 #True or False? 
  
  if (iszero == TRUE){ #then fill with value of 0 for all years for that seg
    pointsource[i,2:ncol(pointsource)] <- rep(0, length(years))
    
  }else if (iszero==FALSE){ #then calculate mean of all hourly values for that year
    means <- data.frame() #create an empty storage dataframe
    
    k <- 1
    for (k in 1:length(years)){ #loop the mean calculation for every year of analysis
      datayear <- subset(importdata, year==years[k])
      datayear <- datayear[1:nrow(datayear)-1,] #remove the 24th hour of dec 31
      meanval <- summarize(datayear, mean(ps))
      means[1,k] <- meanval
      k <- k + 1
    }
    colnames(means) <- as.character(years)
   pointsource[i,2:ncol(pointsource)] <- means
  }

  }else if (goodtogo == FALSE){
    pointsource[i,2:ncol(pointsource)] <- rep("N.Data", length(years))
  }
  i <- i + 1
  
}

write.csv(pointsource, file=paste0("ps_sep_div_ams_p532cal_062211_", code, ".csv"))



## Evaluate point source data -------------------------------------------

ps3000 <- read.csv("ps_sep_div_ams_p532cal_062211_3000.csv")
ps3007 <- read.csv("ps_sep_div_ams_p532cal_062211_3007.csv")
ps3008 <- read.csv("ps_sep_div_ams_p532cal_062211_3008.csv")

paste0("ps 3000 sum: ", round(sum(ps3000[,3:24]), digits=2))
paste0("ps 3007 sum: ", round(sum(ps3007[,3:24]), digits=2)) 
paste0("ps 3008 sum: ", round(sum(ps3008[,3:24]), digits=2))




# Create sum and mean columns for 3007 and 3008 - this is by watershed
i <- 1
for (i in 1:nrow(ps3007)){
ps3007$sum[i] <- sum(ps3007[i,3:24])
ps3008$sum[i] <- sum(ps3008[i,3:24])
ps3007$mean[i] <- ps3007$sum[i]/21
ps3008$mean[i] <- ps3008$sum[i]/21
ps3007$perdif[i] <- (min(ps3007[i,3:24])-max(ps3007[i,3:24])) / ((min(ps3007[i,3:24]) + max(ps3007[i,3:24]))/2)
ps3008$perdif[i] <- (min(ps3008[i,3:24])-max(ps3008[i,3:24])) / ((min(ps3008[i,3:24]) + max(ps3008[i,3:24]))/2)
ps3007$stdev[i] <- sd(ps3007[i,3:24])
ps3008$stdev[i] <- sd(ps3008[i,3:24])
}

# count <- ps3007$mean
# barplot(count, main="Withdrawal", 
#         xlab="Year")
# count <- ps3007$sum
# barplot(count, main="Withdrawal", 
#         xlab="Watershed")


# create sum by year
k <- 3
ps3007[137,1] <- NA
ps3007[138,1] <- NA
ps3008[137,1] <- NA
ps3008[138,1] <- NA
for (k in 3:24){
  ps3007[137,k] <- sum(ps3007[1:136,k])
  ps3008[137,k] <- sum(ps3008[1:136,k])
  ps3007[138,k] <- mean(ps3007[1:136,k])
  ps3008[138,k] <- mean(ps3008[1:136,k])
}



library(ggplot2)
# Basic barplot
pssdata <- data.frame(as.numeric(ps3007[137,3:24]))
pssdata[,2] <- as.numeric(ps3008[137,3:24])
pssdata[,3] <- as.character(1984:2005)
pssdata[,4] <- as.numeric(pssdata[,1] + pssdata[,2])
names(pssdata) <- c("sumind", "sumag", "year", "sumtot")

plotdata <- data.frame(pssdata$year, pssdata$sumtot)
names(plotdata) <- c("year", "sumtot")
p<-ggplot(data=plotdata, aes(x=as.factor(year), y=sumtot)) +
  geom_bar(stat="identity", fill="grey40") + 
  scale_y_continuous(limits=c(0,490)) +
  theme_bw()+ theme(element_blank()) + 
  labs(title="Sum of Withdrawals by Year for All River Segments", 
       x="Year", y="Total Withdrawals (cfs)")
  # CANT GET 88 AND 94 TO PLOT
p

ggsave("Discharges.jpg", plot=p, path = filepath, width=8, height=4, units="in")

min(pssdata$sumtot) 
max(pssdata$sumtot)

# Create watershed bar graphs: Case study on bad one 
baddata <- data.frame(as.numeric(ps3007[60,3:24]))
baddata[,2] <- as.numeric(ps3008[60,3:24])
baddata[,3] <- as.character(1984:2005)
baddata[,4] <- as.numeric(baddata[,1] + baddata[,2])
names(baddata) <- c("sumind", "sumag", "year", "sumtot")

plot2data <- data.frame(baddata$year, baddata$sumtot)
names(plot2data) <- c("year", "sumtot")
pk<-ggplot(data=plot2data, aes(x=as.factor(year), y=sumtot)) +
  geom_bar(stat="identity", fill="grey40") + 
  scale_y_continuous(limits=c(0,5)) +
  theme_bw()+ theme(element_blank()) + 
  labs(title="Withdrawals by Year for Segment NR2_8600_8700", 
       x="Year", y="Withdrawals (cfs)")

pk

ggsave("BadDischarges22_Wshd.jpg", plot=pk, path = filepath, width=8, height=4, units="in")



# Create watershed bar graphs: Case study on one 
gooddata <- data.frame(as.numeric(ps3007[51,3:24]))
gooddata[,2] <- as.numeric(ps3008[51,3:24])
gooddata[,3] <- as.character(1984:2005)
gooddata[,4] <- as.numeric(gooddata[,1] + gooddata[,2])
names(gooddata) <- c("sumind", "sumag", "year", "sumtot")

plot3data <- data.frame(gooddata$year, gooddata$sumtot)
names(plot3data) <- c("year", "sumtot")
pkk<-ggplot(data=plot3data, aes(x=as.factor(year), y=sumtot)) +
  geom_bar(stat="identity", fill="grey40") + 
  scale_y_continuous(limits=c(0,150)) +
  theme_bw()+ theme(element_blank()) + 
  labs(title="Withdrawals by Year for Segment NR6_8050_8051", 
       x="Year", y="Withdrawals (cfs)")

pkk

ggsave("GoodDischarges22_Wshd.jpg", plot=pkk, path = filepath, width=8, height=4, units="in")


100*(min(gooddata$sumtot) - max(gooddata$sumtot)) / ((min(gooddata$sumtot) + max(gooddata$sumtot))/2)

100*(min(baddata$sumtot) - max(baddata$sumtot)) / ((min(baddata$sumtot) + max(baddata$sumtot))/2)

sd(plotdata$sumtot) / mean(plotdata$sumtot)

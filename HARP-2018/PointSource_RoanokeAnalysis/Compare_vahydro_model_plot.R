#3/20/2019
#Script to compare VAHydro withdrawal data to model for years of interest

# load libraries
library(RCurl)
library(dplyr)
library(lubridate)
library(ggplot2)

setwd('C:\\Users\\Kelsey\\Desktop\\GitHub\\hydro-tools\\HARP-2018\\PointSource_RoanokeAnalysis')


# pull model data from deq ---------------------------------------------------------------------------
deq <- "http://deq2.bse.vt.edu/p532c-sova/wdm/river/p532cal_062211/eos/"

#MUST CHANGE WHAT CODE YOU'RE DOING.-- changed by: 
code <- 3007
rivseg <- "OR2_8130_7900"

study_seg <- paste0("ps_sep_div_ams_p532cal_062211_",rivseg,"_",code,".csv")
#check to make sure that the file exists on the site. 
goodtogo <- url.exists(paste0(deq, study_seg)) # check to make sure csv exists

if (goodtogo ==TRUE){
  modeldata <- read.csv(paste0(deq, study_seg))
  colnames(modeldata) <- c("year", "month", "day", "hour", "w.d [cfs]")
}

#units are in cfs
#modeldata <- subset(modeldata, modeldata$year==yoi)
modeldata$date <- as.Date(paste0(modeldata$year,"-",modeldata$month,"-",modeldata$day))
modeldata <- modeldata[which(modeldata$date=="2001-01-01" & modeldata$hour=="1"):nrow(modeldata),]
rownames(modeldata) <- 1:nrow(modeldata)


#Load wayside withdrawals
vahydro <- read.csv("wayside_updated.csv", stringsAsFactors = FALSE)
vahydro$Date <- as.Date(vahydro$Date)
vahydro <- subset(vahydro, varkey=="wd_mgm" )
vahydro$year <- year(vahydro$Date)
vahydro$Value <- as.numeric(gsub(",", "",vahydro$Value)) 

#pull the data of interest out (mgm monthly withdrawal)
vahydro <- subset(vahydro, year== "2001" | year== "2002" | year== "2003"
                  | year== "2004" | year== "2005")

# units are in mgm, need mgd then cfs
vahydro <- data.frame(vahydro$Date, vahydro$Value) #, spholla$Value, spholla$varkey)
colnames(vahydro) <- c("Date", "way_val_mgm") # "sph_val", "varkey")
vahydro <- vahydro[order(vahydro$Date),]
rownames(vahydro) <- 1:nrow(vahydro)

vahydro$monthdays <- days_in_month(vahydro$Date)
vahydro$mgd <- vahydro$way_val_mgm / vahydro$monthdays

#convert mgd to cfs for vahydro 
#    MGD  <-  gal/Mgal *  ft3/gal    *   day/hr *  hr/sec
unit_conv <-  (10^6)   * (1/7.480519) *  (1/24)  * (1/3600)

vahydro$val_cfs <- vahydro$mgd * unit_conv




# fit the model data for analysis ---------------
#step 1: look at the model data -- is it the same for every month?
# for every month, determine how many unique values exist. if only one, show that value
k <- 1 #k changes the year
l <- 1 #l and m are counters for a dataframe
m <- 12
uniqueval <- data.frame()
uniqueval2 <- data.frame(a = NA, b = NA, c = NA, d=NA, e=NA)
colnames(uniqueval2) <- c("num.uniq", "unique.value[cfs]", "month", "monthname","year")
for (k in 1:length(unique(modeldata$year))){
  studyear <- unique(modeldata$year)[k]
  analyzeyear <- subset(modeldata, year==studyear)
  
  i <- 1 #i changes the month
  for (i in 1:12){
    mon <- subset(analyzeyear, analyzeyear$month==i)
    mon <- mon[1:nrow(mon)-1,] #remove hour 24 of the last day of month (that is next month)
    uniquemon <- length(unique(mon$`w.d [cfs]`))
    
    uniqueval[i,1] <- uniquemon
    
    if (uniqueval[i,1]==1){
      uniqueval[i,2] <- unique(mon$`w.d [cfs]`)
    }else if (uniqueval[i,1]!=1){
      uniqueval[i,2] <- NA
    }
  }
  
  uniqueval2[l:m,1] <- uniqueval[1:12,1]
  uniqueval2[l:m,2] <- uniqueval[1:12,2]
  uniqueval2[l:m,3] <- 1:12
  uniqueval2[l:m,4] <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  uniqueval2[l:m,5] <- studyear
  l <- l+12
  m <- m+12
}
uniqueval2$date <- as.Date(paste0(uniqueval2$year,"-",uniqueval2$month,"-01"))



#compare vahydro and model data

comparedata <- data.frame(uniqueval2$`unique.value[cfs]`, vahydro$val_cfs, uniqueval2$date, uniqueval2$monthname, uniqueval2$year)
colnames(comparedata) <- c("model", "vahydro", "date", "monthname", 'year')




sevyears <- ggplot(comparedata, aes(x=date)) +
  geom_line(aes(y=model, color="Model"))+
  geom_line(aes(y=vahydro, color="VAHydro"))+
  labs(x="Date", y="Withdrawal [cfs]", title="Withdrawals by Month and Year") + 
  scale_color_manual(values=c("blue", "red")) + 
  theme(legend.title = element_blank())
sevyears


ggsave(file="Gage_vs_model_withdrawal.png", width=9, height=5, units="in")

# Kelsey Reitz
#3/20/2019
# Evaluation of withdrawal data
# proper unit notation

#this script looks at the North Fork Rivanna JL segment in question to compare to wayside 

## load libraries
library(lubridate)
library(dplyr)
library(RCurl)
library(ggplot2)
library(dataRetrieval)

# set workspace
dir.create("C:/Users/Kelsey/Desktop/GitHub/hydro-tools/HARP-2018/PointSource_RoanokeAnalysis/JL_segs", showWarnings = F)
filepath <- "C:/Users/Kelsey/Desktop/GitHub/hydro-tools/HARP-2018/PointSource_RoanokeAnalysis/JL_segs"
setwd(filepath)
# SPECIFY UNITS OF ALL DATA AND DOCUMENTATION OF UNITS ----------------------------------------------

# for VAHydro: listed within the csv / right on the site: 
# for model:
#     HSPF user manual: https://drive.google.com/drive/u/1/folders/0B5TT3KOg-7FUMzVENm84NmNsM0U
#     pg 698: "outdgt ft3/s"
#     BUT: uci for seg has ac-ft, but I think this is for some other calculation (not withdrawal stuff)


# pull model data from deq ---------------------------------------------------------------------------
deq <- "http://deq2.bse.vt.edu/p532c-sova/wdm/river/p532cal_062211/eos/"

#MUST CHANGE WHAT CODE YOU'RE DOING.-- changed by: 
code <- 3007
rivseg <- "JL2_6240_6520"
yoi <- 2002

study_seg <- paste0("ps_sep_div_ams_p532cal_062211_",rivseg,"_",code,".csv")
#check to make sure that the file exists on the site. 
goodtogo <- url.exists(paste0(deq, study_seg)) # check to make sure csv exists

if (goodtogo ==TRUE){
  modeldata <- read.csv(paste0(deq, study_seg))
  colnames(modeldata) <- c("year", "month", "day", "hour", "w.d [cfs]")
}

#units are in cfs
modeldata <- subset(modeldata, modeldata$year==yoi)
modeldata$date <- as.Date(paste0(modeldata$year,"-",modeldata$month,"-",modeldata$day))

# pull VAHydro data for Rivanna North Fork (downloaded from VAHydro in stored csv files) ------

#read the csvs
fork <- read.csv("Northfork.csv", stringsAsFactors = FALSE)

#fix format of data
fork$Date <- as.Date(fork$Date) 
fork$Value <- as.numeric(gsub(",", "",fork$Value)) 

#pull the year of interest out
fork <- subset(fork, varkey=="wd_mgm" & year(fork$Date)==yoi)


# units are in mgd
vahydro <- data.frame(fork$Date, fork$Value, fork$varkey)
colnames(vahydro) <- c("Date", "val", "varkey")
vahydro <- vahydro[order(vahydro$Date),]
rownames(vahydro) <- 1:nrow(vahydro)

vahydro$monthdays <- days_in_month(vahydro$Date)
vahydro$mgd <- vahydro$val / vahydro$monthdays

#convert mgd to cfs for vahydro 
#    MGD  <-  gal/Mgal *  ft3/gal    *   day/hr *  hr/sec
unit_conv <-  (10^6)   * (1/7.480519) *  (1/24)  * (1/3600)

vahydro$cfs <- vahydro$mgd * unit_conv



# Begin analysis of data ----------------------------------

#given: model data has units of cfs in hourly timesteps
#      VAHydro data is in mgd


#step 1: look at the model data -- is it the same for every month?
# for every month, determine how many unique values exist. if only one, show that value
i <- 1
uniqueval <- data.frame()
for (i in 1:12){
  mon <- subset(modeldata, modeldata$month==i)
  mon <- mon[1:nrow(mon)-1,] #remove hour 24 of the last day of month (that is next month)
  uniquemon <- length(unique(mon$`w.d [cfs]`))
  
  uniqueval[i,1] <- uniquemon
  
  if (uniqueval[i,1]==1){
    uniqueval[i,2] <- unique(mon$`w.d [cfs]`)}
}
colnames(uniqueval) <- c("num.uniq", "unique.value[cfs]")

#step 2: compare the model cfs to vahydro cfs data

compare <- data.frame(vahydro[], uniqueval$`unique.value[cfs]`)
compare <- na.omit(compare)
compare$cfs_error <- 100*(compare$cfs - compare$uniqueval..unique.value.cfs..) / (compare$cfs)



plot <- ggplot(compare, aes(Date)) + 
  geom_line(aes(y=uniqueval..unique.value.cfs.., colour="Model [cfs]"), size=.5)+
  geom_line(aes(y=cfs, colour="North Fork [cfs]"), size=.5) + 
  scale_colour_manual(values=c("blue", "red")) + 
  labs(x = "Date", y = "Withdrawal (cfs)", 
       title="Model v. VAHydro: Withdrawal Comparisions", colour = "Source")
ggsave(file="cfs_data.png", width=9, height=5, units="in")


resid_plot <- ggplot(compare,(aes(Date))) + 
  geom_point(aes(y=cfs_error, colour="North Fork Error"), size=3) + 
  #geom_point(aes(y=both_error, colour="Combined Error"), size=3) + 
  scale_colour_manual(values=c("blue")) + 
  geom_hline(yintercept=0, linetype=1, color="black", size=1) + 
  labs(x="Date", y="Residual Error", title="Residual Error Plot", colour="Model vs.")
resid_plot
ggsave(file="NorthFork_residual.png", width=9, height=5, units="in")



# what does this look like compared to actual flow values? 
gage <- readNWISdv('02032680', parameterCd='00060', startDate = "1984-01-01", 
                   endDate = "2005-12-31", statCd = "00003")


hydro_view <- ggplot(gage, aes(Date)) + 
  geom_line(aes(y=X_00060_00003, colour="gage"), size=0.5) + 
  geom_line(data=modeldata, aes(x=date, y=`w.d [cfs]`, colour="model"), size=0.7) + 
  scale_colour_manual(values=c("blue", "black")) + 
  labs(x="Date", y="Flow and withdrawals [cfs]", colour="Legend") + 
  coord_cartesian(ylim=c(0,30))
ggsave(file="withdrawal_v_gageflow_zoomed.png", width=9, height=5, units="in")



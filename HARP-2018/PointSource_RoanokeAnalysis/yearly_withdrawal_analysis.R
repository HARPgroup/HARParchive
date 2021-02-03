# Kelsey Reitz
#3/8/2019
# Evaluation of withdrawal data: Wayside vs model for all simulated years
# proper unit notation

#this script only looks at the wayside OR segment in question, rather than the entire dataset.  

## load libraries
library(lubridate)
library(dplyr)
library(RCurl)
library(ggplot2)
library(dataRetrieval)

# set workspace
setwd("C:/Users/Kelsey/Desktop/GitHub/hydro-tools/HARP-2018/PointSource_RoanokeAnalysis")

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
rivseg <- "OR2_8130_7900"
#yoi <- 2002

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

# pull VAHydro data for wayside and spring hollow (downloaded from VAHydro in stored csv files) ------

#read the csvs
#wayside <- read.csv("wayside_mgd.csv", stringsAsFactors = FALSE)
wayside <- read.csv("wayside_updated.csv", stringsAsFactors = F)
#spholla <- read.csv("springhol_mgd.csv", stringsAsFactors = FALSE)

#fix format of data
wayside$Date <- as.Date(wayside$Date) 
#spholla$Date <- as.Date(spholla$Date)
wayside$Value <- as.numeric(gsub(",", "",wayside$Value)) 
#spholla$Value <- as.numeric(gsub(",", "",spholla$Value))

#pull the data of interest out (mgm monthly withdrawal)
wayside <- subset(wayside, varkey=="wd_mgm")


# units are in mgd
vahydro <- data.frame(wayside$Date, wayside$Value) #, spholla$Value, spholla$varkey)
colnames(vahydro) <- c("Date", "way_val_mgm") # "sph_val", "varkey")
vahydro <- vahydro[order(vahydro$Date),]
rownames(vahydro) <- 1:nrow(vahydro)

vahydro$monthdays <- days_in_month(vahydro$Date)
vahydro$mgd <- vahydro$way_val_mgm / vahydro$monthdays

#convert mgd to cfs for vahydro 
#    MGD  <-  gal/Mgal *  ft3/gal    *   day/hr *  hr/sec
unit_conv <-  (10^6)   * (1/7.480519) *  (1/24)  * (1/3600)

vahydro$way_cfs <- vahydro$mgd * unit_conv

#vahydro$sph_cfs <- vahydro$sph_val * unit_conv


# Begin analysis of data ----------------------------------

#given: model data has units of cfs in hourly timesteps
#      VAHydro data is in mgd


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
#issue: feb. of 2000 has two dates (row 194 w/ NA)


#uniqueval2 <- uniqueval2[253:264,]
plot <- ggplot(uniqueval2, aes(date)) + 
  geom_col(aes(y=`unique.value[cfs]`))
plot

plotzoom <- ggplot(uniqueval2, aes(date)) + 
  geom_col(aes(y=`unique.value[cfs]`)) + 
  coord_cartesian(xlim=c(as.Date("1998-01-01"),as.Date("2005-01-01")))
plotzoom

colorbar <- ggplot(uniqueval2, aes(x=month, y=`unique.value[cfs]`, colour=as.factor(year))) + 
  geom_line() + 
  geom_point() + 
  scale_color_hue(l=50, c=300)
colorbar



cols <- c("grey", "grey", "grey", "grey", "grey", "grey", 
          "grey", "grey", "grey", "grey", "grey",
          "grey", "grey", "grey", "grey", "black",
          "red", "brown", "gold", "mediumseagreen", "blue", 
          "mediumpurple3", "darkred")

uniqueval2$monthname <- reorder(uniqueval2$monthname, uniqueval2$month)

ggplot(uniqueval2, aes(x=monthname, y=`unique.value[cfs]`,fill=as.factor(year))) +
  geom_col(position = "dodge", width=2) + 
  scale_fill_manual(values=cols)+
  theme(axis.text.x = element_text(angle=90))


#clip the data to remove 1984-1998 --------------------

# ensure that there are inded no withdrawals for this period: 
sum(uniqueval2$`unique.value[cfs]`[1:which(uniqueval2$date==as.Date("1998-12-01"))]) == 0

ninetynine <- uniqueval2[which(uniqueval2$date==as.Date("1999-01-01")):nrow(uniqueval2),]

cols<-c("black","red", "brown", "gold", "mediumseagreen", "blue", "mediumpurple3", "darkred")

sevyears <- ggplot(ninetynine, aes(x=monthname, y=`unique.value[cfs]`,fill=as.factor(year))) +
  geom_col(position = "dodge", width=.7) + 
  scale_fill_manual(values=cols, name="Year") + 
  labs(x="Month", y="Withdrawal [cfs]", title="Withdrawals by Month and Year") 
sevyears
ggsave(file="Withdrawals by Month and Year.png", width=9, height=5, units="in")

#issue: feb. of 2000 has two dates (row 194 w/ NA)



# Kelsey Reitz
#3/20/2019
# Evaluation of withdrawal data: North Fork vs model for all simulated years
# proper unit notation

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

# pull model data from deq ---------------------------------------------------------------------------
deq <- "http://deq2.bse.vt.edu/p532c-sova/wdm/river/p532cal_062211/eos/"

#MUST CHANGE WHAT CODE YOU'RE DOING.-- changed by: 
code <- 3007
rivseg <- "JL2_6240_6520"

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

# pull VAHydro data for north fork (downloaded from VAHydro in stored csv files) ------

#read the csvs
northfork <- read.csv("Northfork.csv", stringsAsFactors = F)

#fix format of data
northfork$Date <- as.Date(northfork$Date) 
#spholla$Date <- as.Date(spholla$Date)
northfork$Value <- as.numeric(gsub(",", "",northfork$Value)) 
#spholla$Value <- as.numeric(gsub(",", "",spholla$Value))

#pull the data of interest out (mgm monthly withdrawal)
northfork <- subset(northfork, varkey=="wd_mgm")


# units are in mgd
vahydro <- data.frame(northfork$Date, northfork$Value) #, spholla$Value, spholla$varkey)
colnames(vahydro) <- c("Date", "val_mgm") # "sph_val", "varkey")
vahydro <- vahydro[order(vahydro$Date),]
rownames(vahydro) <- 1:nrow(vahydro)

vahydro$monthdays <- days_in_month(vahydro$Date)
vahydro$mgd <- vahydro$val_mgm / vahydro$monthdays

#convert mgd to cfs for vahydro 
#    MGD  <-  gal/Mgal *  ft3/gal    *   day/hr *  hr/sec
unit_conv <-  (10^6)   * (1/7.480519) *  (1/24)  * (1/3600)

vahydro$cfs <- vahydro$mgd * unit_conv

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


colorbar <- ggplot(uniqueval2, aes(x=month, y=`unique.value[cfs]`, colour=as.factor(year))) + 
  geom_line() + 
  geom_point() + 
  scale_color_hue(l=50, c=300)
colorbar



cols <- c("pink", "orange", "darkblue", "darkorange", "magenta", 
          "black", "green", "tomato4", "salmon2", "firebrick4", "grey", "ivory2",
          "lemonchiffon1",  "black",
          "red", "brown", "gold", "mediumseagreen", 
          "slategray3", "blue", "royalblue", 
          "mediumpurple3", "darkred")

uniqueval2$monthname <- reorder(uniqueval2$monthname, uniqueval2$month)

allyrs<- ggplot(uniqueval2, aes(x=monthname, y=`unique.value[cfs]`,fill=as.factor(year))) +
  geom_col(position = "dodge", width=1) + 
  scale_fill_manual(values=cols, name="Year")+
  theme(axis.text.x = element_text(angle=90)) + 
  labs(x="Month", y="Withdrawal [cfs]", title="Withdrawals by Month and Year") 

allyrs
ggsave(file="Withdrawals by Month and Year_allyears.png", width=9, height=5, units="in")



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



ninetysix <- uniqueval2[which(uniqueval2$date==as.Date("1996-01-01")):nrow(uniqueval2),]

cols<-c("olivedrab", "royalblue", "pink" , "black","red", "brown", "gold", "mediumseagreen", "blue", "mediumpurple3", "darkred")

sevyears <- ggplot(ninetysix, aes(x=monthname, y=`unique.value[cfs]`,fill=as.factor(year))) +
  geom_col(position = "dodge", width=.7) + 
  scale_fill_manual(values=cols, name="Year") + 
  labs(x="Month", y="Withdrawal [cfs]", title="Withdrawals by Month and Year") 
sevyears
ggsave(file="Withdrawals by Month and Year_1996.png", width=9, height=5, units="in")


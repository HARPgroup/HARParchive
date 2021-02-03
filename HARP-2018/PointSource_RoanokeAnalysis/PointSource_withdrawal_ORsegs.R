# Kelsey Reitz
#2/6/2019
# Quality Assurance of original Point Source Evaluation code 
# (original named PointSourceEval_SRVA_original in the directory)

#this script only looks at the two OR segments in question, rather than the entire dataset.  

## load libraries
library(lubridate)
library(dplyr)
library(RCurl)
library(ggplot2)

setwd("C:/Users/Kelsey/Desktop/GitHub/hydro-tools/HARP-2018/PointSource_RoanokeAnalysis")


# # Observation of current data listed within the folder --------------
# divr07 <- read.csv("ps_sep_div_ams_p532cal_062211_3007.csv")
# diva08 <- read.csv("ps_sep_div_ams_p532cal_062211_3008.csv")
# 
# divr_segs <- divr07[which(divr07$rivsegs=="OR2_8130_7900" | divr07$rivsegs=="OR2_8020_8130"),]
# diva_segs <- diva08[which(diva08$rivsegs=="OR2_8130_7900" | diva08$rivsegs=="OR2_8020_8130"),]
# 

#Re-analysis using just the two OR Segments -------------

deq <- "http://deq2.bse.vt.edu/p532c-sova/wdm/river/p532cal_062211/eos/"

#MUST CHANGE WHAT CODE YOU'RE DOING.-- changed by: 
code <- 3007

rivsegs <- data.frame()
rivsegs[1,1] <- "OR2_8130_7900"
rivsegs[2,1] <- "OR2_8020_8130"
i <- 1

# pull raw withdrawal data from DEQ
for (i in 1:nrow(rivsegs)){
study_seg <- paste0("ps_sep_div_ams_p532cal_062211_",rivsegs[i,1],"_",code,".csv")
#check to make sure that the file exists on the site. 
#if it doesn't exist, move on to the next segment
goodtogo <- url.exists(paste0(deq, study_seg)) # check to make sure csv exists
  if (goodtogo ==TRUE){
    importdata <- read.csv(paste0(deq, study_seg))
    colnames(importdata) <- c("year", "month", "day", "hour", "ps")
  }
#store the results of this pull for seg 1 and 2
if (i==1){
  seg_8130_7900 <- data.frame(importdata)
} else if (i==2){
  seg_8020_8130 <- data.frame(importdata)
}

}

## Format data into one consolidated data frame named mergesegs
seg_8020_8130$rownum <- 1:nrow(seg_8020_8130)
seg_8130_7900$rownum <- 1:nrow(seg_8130_7900)
mergesegs <- merge(seg_8020_8130, seg_8130_7900, by.x = "rownum", by.y = "rownum", all = TRUE)
mergesegs <- data.frame(mergesegs$rownum, mergesegs$year.x, mergesegs$month.x, mergesegs$day.x, mergesegs$hour.x, mergesegs$ps.x, mergesegs$ps.y)
colnames(mergesegs) <- c("rownum", "year", "month", "day", "hour", "8020_8130", "8130_7900")



## Begin analysis of point sources and where / what year they are in  --------------------
# 
# convert hourly to daily: referencing the data_downloader R script from HARP 2018 DEQ_Model_vs_USGS_comparison

# IMPORTING AND EXPORTING MODEL DATA ----------------------------------------------------
# Splitting the River Segment string into each segment name

RivSegStr <- seg_8130_7900        # not analyzing upstream, just the downstream erroneous 
start.date <- "1984-01-01"
end.date <- "2005-12-31"

model_days <- length(seq(as.Date(start.date):as.Date(end.date))) # how many days are between these 

#Reads data into data frame "ovols", exports each seg's data

# Downloading and exporting hourly model data
model_hourly <- RivSegStr 
  
# Converting hourly to daily data and exporting daily data
# model_hourly <- model_hourly[-1,]
#model_hourly$V1 <- trimws(model_hourly$V1, which = "both")
colnames(model_hourly) <- c("year","month","day","hour","ovol", "rownum")
model_hourly$date <- as.Date(paste0(model_hourly$year,"-",model_hourly$month,"-",model_hourly$day))
model_daily <- aggregate(model_hourly$ovol, list(model_hourly$date), FUN = sum)
colnames(model_daily) <- c("date","mod.flow")
#write.csv(model_daily, file = paste0(container, "/data/new_(updated)_data/raw_data/model_data/daily_data/",RivSeg," - Daily Raw Data.csv"))
  
# convert from ac-ft/day to cfs
model_daily$mod.flow_converted <- model_daily$mod.flow * 0.504167

#TRY AC-IN/DAY to cfs
model_daily$mod.flow_converted_in_tocf <- model_daily$mod.flow * (43560)/(24*3600*12)

model_daily <- subset(model_daily, year(model_daily$date)==2002)

# are there different ways that monthly flow may be interpreted by the model ? maybe??
# look at how withdrawal values are distributed by month: 
# plot(model_daily$date, model_daily$mod.flow_converted, type='l')
# safe to say they're consistent by month. Going with an average monthly withdrawal for the model

# mean of monthly flows -- average these flows by month
model_monthly_bymean <- aggregate(model_daily, list(month(model_daily$date)), FUN = mean)
model_monthly_bymean <- data.frame(select(model_monthly_bymean, Group.1, mod.flow, mod.flow_converted, mod.flow_converted_in_tocf))
colnames(model_monthly_bymean) <- c("month","mod.flow_raw","mod.flow_converted", "mod.flow_convertedinch")



# pull in withdrawal data from wayside and spring hollow --------------------
# mon_wayside <- read.csv("wayside_mon.csv", stringsAsFactors = FALSE)
# mon_spholla <- read.csv("springhol_mon.csv", stringsAsFactors = FALSE) 

#ignore daily values -- its just monthly divided by num days in the month
mon_wayside <- read.csv("wayside_mgd.csv", stringsAsFactors = FALSE)
mon_spholla <- read.csv("springhol_mgd.csv", stringsAsFactors = FALSE)

# mon_wayside$Date <- as.Date(mon_wayside$Date)
# mon_spholla$Date <- as.Date(mon_spholla$Date)
mon_wayside$Date <- as.Date(mon_wayside$Date)
mon_spholla$Date <- as.Date(mon_spholla$Date)

# mon_wayside$Value <- as.numeric(gsub(",", "",mon_wayside$Value))
# mon_spholla$Value <- as.numeric(gsub(",", "",mon_spholla$Value))
mon_wayside$Value <- as.numeric(gsub(",", "",mon_wayside$Value))
mon_spholla$Value <- as.numeric(gsub(",", "",mon_spholla$Value))
                                
# mon_wayside <- subset(mon_wayside, varkey=="wd_mgm" & year(mon_wayside$Date)==2002)
# mon_spholla <- subset(mon_spholla, varkey=="wd_mgm" & year(mon_spholla$Date)==2002)
mon_wayside <- subset(mon_wayside, varkey=="wl_mgd" & year(mon_wayside$Date)==2002)
mon_spholla <- subset(mon_spholla, varkey=="wl_mgd" & year(mon_spholla$Date)==2002)

vahydro <- data.frame(mon_wayside$Date, mon_wayside$Value, mon_spholla$Value)  #day_wayside$Value ,day_spholla$Value)
vahydro <- vahydro[order(vahydro$mon_wayside.Date), ]
colnames(vahydro) <- c("Date", "Wayside_mgd", "Sphol_mgd")  #,"Wayside_mgd","Sphol_mgd")
vahydro$Wayside_cfs <- vahydro$Wayside_mgd * (1/7.4052) * (10^6) * (1/24) * (1/3600)
vahydro$Sphol_cfs <- vahydro$Sphol_mgd * (1/7.4052) * (10^6) * (1/24) * (1/3600)


## compare wayside, spring hollow, and MEAN monthly model data -------------
comparison <- data.frame(vahydro[], model_monthly_bymean[])
comparison$perdif <- 100*(comparison$Wayside_cfs- comparison$mod.flow_convertedinch) / comparison$Wayside_cfs
comparison$twoperdif <- 100*((comparison$Wayside_cfs+comparison$Sphol_cfs) - comparison$mod.flow_convertedinch) / (comparison$Wayside_cfs+comparison$Sphol_cfs)
write.csv(comparison, "segs_vs_meanmonthlyinches.csv")

# remove NA, infinite, or 0
comparison$perdif[!is.finite(comparison$perdif)==TRUE] <- NA
mean(comparison$perdif, na.rm = TRUE); mean(comparison$twoperdif, na.rm = TRUE)

# plot the data: 
alldata <- ggplot(comparison, aes(Date)) + 
  geom_line(aes(y = mod.flow_converted, colour = "Converted"), size=1) + 
  geom_line(aes(y = mod.flow_raw, colour = "Raw"), size=1) + 
  geom_line(aes(y = (Wayside_cfs+Sphol_cfs), colour = "Total"), size=1) + 
  geom_line(aes(y = Wayside_cfs, colour = "Wayside"), size=1) + 
  geom_line(aes(y = Sphol_cfs, colour = "Spring Hollow"), size=1) + 
  scale_colour_manual(values=c("black", "grey", "blue", "green", "red")) + 
  labs(x = "Date", y = "Withdrawal (cfs)", colour = "Source")

data <- ggplot(comparison, aes(Date)) + 
  geom_line(aes(y = mod.flow_converted, colour = "Converted Model"), size=.5) + 
  #geom_line(aes(y = mod.flow_convertedinch, colour = "Converted Model inches"), size=.5, linetype = "longdash") +
  geom_line(aes(y = (Wayside_cfs+Sphol_cfs), colour = "Wayside + Sp. Hol."), size=.5) + 
  geom_line(aes(y = Wayside_cfs, colour = "Wayside"), size=.5) + 
  geom_line(aes(y = Sphol_cfs, colour = "Spring Hollow"), size=.5) + 
  scale_colour_manual(values=c("black","green", "blue", "red")) + 
  labs(x = "Date", y = "Withdrawal (cfs)", colour = "Source")
#ggsave(file="Converted_data.png", width=9, height=5, units="in")

data <- ggplot(comparison, aes(Date)) + 
  geom_line(aes(y = mod.flow_converted, colour = "Converted Model"), size=.5) + 
  geom_line(aes(y = mod.flow_convertedinch, colour = "Converted Model inches"), size=.5, linetype = "longdash") +
  geom_line(aes(y = (Wayside_cfs+Sphol_cfs), colour = "Wayside + Sp. Hol."), size=.5) + 
  geom_line(aes(y = Wayside_cfs, colour = "Wayside"), size=.5) + 
  geom_line(aes(y = Sphol_cfs, colour = "Spring Hollow"), size=.5) + 
  scale_colour_manual(values=c("black", "grey","green", "blue", "red")) + 
  labs(x = "Date", y = "Withdrawal (cfs)", colour = "Source") + 
  coord_cartesian(ylim=c(0,250))
#ggsave(file="Converted_data_cfs.png", width=9, height=5, units="in")







yearofi <- subset(model_hourly, model_hourly$year==2002)
#model_daily <- aggregate(model_hourly$ovol, list(model_hourly$date), FUN = sum)



data <- ggplot(model_hourly, aes(Date)) + 
  geom_line(aes(y = mod.flow_converted, colour = "Converted Model"), size=.5) + 
  geom_line(aes(y = mod.flow_convertedinch, colour = "Converted Model inches"), size=.5, linetype = "longdash") +
  geom_line(aes(y = (Wayside_cfs+Sphol_cfs), colour = "Wayside + Sp. Hol."), size=.5) + 
  geom_line(aes(y = Wayside_cfs, colour = "Wayside"), size=.5) + 
  geom_line(aes(y = Sphol_cfs, colour = "Spring Hollow"), size=.5) + 
  scale_colour_manual(values=c("black", "grey","green", "blue", "red")) + 
  labs(x = "Date", y = "Withdrawal (cfs)", colour = "Source") + 
  coord_cartesian(ylim=c(0,250))
#ggsave(file="Converted_data_cfs.png", width=9, height=5, units="in")

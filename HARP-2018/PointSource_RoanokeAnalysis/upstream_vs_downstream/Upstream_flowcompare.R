# Kelsey Reitz
# 22 March 2019

## load libraries
# library(lubridate)
# library(dplyr)
# library(RCurl)
library(stringr)
library(ggplot2)
library(dataRetrieval)

# set workspace
setwd("C:/Users/Kelsey/Desktop/GitHub/hydro-tools/HARP-2018/PointSource_RoanokeAnalysis")
dir.create("C:/Users/Kelsey/Desktop/GitHub/hydro-tools/HARP-2018/PointSource_RoanokeAnalysis/upstream_vs_downstream", showWarnings = F)
filepath <- "C:/Users/Kelsey/Desktop/GitHub/hydro-tools/HARP-2018/PointSource_RoanokeAnalysis/upstream_vs_downstream"
setwd(filepath)

# SPECIFY UNITS OF ALL DATA AND DOCUMENTATION OF UNITS ----------------------------------------------

# for VAHydro: listed within the csv / right on the site: 
# for model:
#     HSPF user manual: https://drive.google.com/drive/u/1/folders/0B5TT3KOg-7FUMzVENm84NmNsM0U
#     pg 698: "outdgt ft3/s"


# pull model withdrawal data from deq ---------------------------------------------------------------------------
deq <- "http://deq2.bse.vt.edu/p532c-sova/wdm/river/p532cal_062211/eos/"

#MUST CHANGE WHAT CODE YOU'RE DOING.-- changed by: 
code <- 3007
rivseg <- "OR2_8130_7900"
#yoi <- 2002

study_seg <- paste0("ps_sep_div_ams_p532cal_062211_",rivseg,"_",code,".csv")
#check to make sure that the file exists on the site. 
goodtogo <- url.exists(paste0(deq, study_seg)) # check to make sure csv exists

if (goodtogo ==TRUE){
  wayside_wd_data <- read.csv(paste0(deq, study_seg))
  colnames(wayside_wd_data) <- c("year", "month", "day", "hour", "w.d [cfs]")
}


# pull flow data for wayside ---------
deq <- "http://deq2.bse.vt.edu/p532c-sova/wdm/river/p532cal_062211/stream/"
code <- "0111"
rivseg <- "OR2_8130_7900"
#yoi <- 2002

study_seg <- paste0(rivseg,"_",code,".csv")
#check to make sure that the file exists on the site. 
goodtogo <- url.exists(paste0(deq, study_seg)) # check to make sure csv exists

if (goodtogo ==TRUE){
  wayside_flow_data <- read.csv(paste0(deq, study_seg))
  colnames(wayside_flow_data) <- c("year", "month", "day", "hour", "w.d [cfs]")
}

# pull flow data for the upstream segment ----------
deq <- "http://deq2.bse.vt.edu/p532c-sova/wdm/river/p532cal_062211/stream/"
code <- "0111"
rivseg <- "OR2_8020_8130"
#yoi <- 2002

study_seg <- paste0(rivseg,"_",code,".csv")
#check to make sure that the file exists on the site. 
goodtogo <- url.exists(paste0(deq, study_seg)) # check to make sure csv exists

if (goodtogo ==TRUE){
  upstream_flow_data <- read.csv(paste0(deq, study_seg))
  colnames(upstream_flow_data) <- c("year", "month", "day", "hour", "w.d [cfs]")
}

model <- data.frame(matrix(nrow=nrow(upstream_flow_data)))
colnames(model) <- c("Date")
model[,1] <- as.Date(paste0(wayside_flow_data$year,"-",wayside_flow_data$month,"-",wayside_flow_data$day))
model$wayside_flow_raw <- wayside_flow_data$`w.d [cfs]`
model$upstream_flow_raw <- upstream_flow_data$`w.d [cfs]`
model$withdrawal <- wayside_wd_data$`w.d [cfs]`

#raw model is in ac-ft / time interval. need  ft^3 /sec. 

#add up for every day [1:24 increments] for flows
# for withdrawal, take the value of hr 1 of that day. 

#initalize variables
i <- 1
days <- unique(model$Date)
stop <- length(unique(model$Date))
modelcfs <- data.frame(unique(model$Date))
convert <- 43560 /(24*3600)

for (i in 1:stop){
  thisday <- model[which(model$Date==days[i]),]
  
  modelcfs$wayside[i] <- sum(thisday$wayside_flow_raw) * convert
  modelcfs$upstream[i] <- sum(thisday$upstream_flow_raw) * convert
  modelcfs$wd[i] <- thisday$withdrawal[1]
  i <- i + 1
}

modelcfs$flowdif <- modelcfs$wayside - modelcfs$upstream



# Analyze gage and vahydro data: is it the same trend? ------------------
#Load wayside withdrawals
vahydro <- read.csv("C:/Users/Kelsey/Desktop/GitHub/hydro-tools/HARP-2018/PointSource_RoanokeAnalysis/wayside_updated.csv", stringsAsFactors = FALSE)
vahydro$Date <- as.Date(vahydro$Date)
vahydro <- subset(vahydro, varkey=="wd_mgm" )
vahydro$year <- year(vahydro$Date)
vahydro$Value <- as.numeric(gsub(",", "",vahydro$Value)) 
#pull the data of interest out (mgm monthly withdrawal)
#vahydro <- subset(vahydro, year== "2001" | year== "2002" | year== "2003" | year== "2004" | year== "2005")
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


# pull in gage data for upstream and downstream gages: ---------------
#GAGES: upstream 02054500; downstream	02054530

site_id <- c('02054500', '02054530')
startDate <- '1984-01-01'
endDate <- '2005-12-31'
pCode <- '00060'
USGSQ <- readNWISdv(site_id,pCode, startDate, endDate)
upstreamgage <- subset(USGSQ, USGSQ$site_no=="02054500")
waysidegage <- subset(USGSQ, USGSQ$site_no=="02054530")

# pull the vahydro cfs value for each day of the month
upstreamgage$monthyear <- str_sub(as.character(upstreamgage$Date), start=1L, end=7L)
#waysidegage$monthyear <- str_sub(as.character(waysidegage$Date), start=1L, end=7L)
vahydro$monthyear <- str_sub(as.character(vahydro$Date), start=1L, end=7L)


realvals <- data.frame(upstreamgage$Date, upstreamgage$X_00060_00003, upstreamgage$monthyear)
colnames(realvals) <- c("Date", "flow", "monthyear")
wd_val <- vahydro[,5:6]


j <- 1
for (j in 1:nrow(realvals)){
  rowdates <- which(waysidegage$Date==realvals$Date[j])
  if (length(rowdates)==0){
    realvals[j,4] <- NA
  }else 
    realvals[j,4] <- waysidegage$X_00060_00003[rowdates]
}

i <- 1
for (i in 1:nrow(wd_val)){
  rownums <- which(realvals$monthyear==wd_val$monthyear[i])
  realvals[rownums,5] <- wd_val$val_cfs[i] 
}

colnames(realvals) <- c("Date", "upstream_cfs", "monthyear", "wayside_cfs", "VAHydro_wd")
realvals$flowdif <- realvals$wayside_cfs - realvals$upstream_cfs
realvals <- realvals[which(complete.cases(realvals)==TRUE),]




# Generate the plots --------------------------------
ggplot(modelcfs, aes(x=unique.model.Date.)) + 
  geom_line(aes(y=upstream, col="Upstream"), cex=.8) +
  geom_line(aes(y=wayside, col="Wayside"), cex=.8) + 
  geom_line(aes(y=wd, col="Withdrawal"), cex=.8)+
  scale_color_manual(values=c("black", "red", "blue"), name="Flow")+
  coord_cartesian(xlim=c(as.Date("2000-12-31"),as.Date("2002-12-31")), 
                  ylim=c(0,300)) + 
  labs(x="Date", y="Flow [cfs]", title="Model Flows and Withdrawals")
ggsave(file="MODEL_Upstream_vs_downstream_vs_withdrawal.png", width=9, height=5, units="in")



ggplot(modelcfs, aes(x=unique.model.Date.)) + 
  geom_line(aes(y=flowdif, col="Downstream - Upstream"), cex=.8) +
  geom_line(aes(y=wd, col="Withdrawal"), cex=.8) + 
  scale_color_manual(values=c("grey", "blue"), name="Flow")+
  coord_cartesian(xlim=c(as.Date("1984-01-01"),as.Date("2005-12-31")),
                  ylim=c(-400,400)) + 
  labs(x="Date", y="Flow [cfs]", title="Model Flows and Withdrawals")
ggsave(file="MODEL_Flowdif_vs_withdrawal.png", width=9, height=5, units="in")



ggplot(realvals, aes(x=Date)) + 
  geom_line(aes(y=upstream_cfs, col="Upstream Gage"), cex=0.8)+
  geom_line(aes(y=wayside_cfs, col="Wayside Gage"), cex=0.8) + 
  geom_line(aes(y=VAHydro_wd, col="VAHydro Withdrawal"), cex=0.8) + 
  scale_color_manual(values=c("black", "blue", "red"), name="Flow")+
  coord_cartesian(xlim=c(as.Date("2000-12-31"),as.Date("2002-12-31")),
                  ylim=c(0,300)) + 
  labs(x="Date", y="Flow [cfs]", title="Gage Flows and VAHydro Withdrawals")

ggsave(file="GAGE_Upstream_vs_downstream_vs_withdrawal.png", width=9, height=5, units="in")


ggplot(realvals, aes(x=Date)) + 
  geom_line(aes(y=flowdif, col="Downstream - Upstream"), cex=.8) +
  geom_line(aes(y=VAHydro_wd, col="Withdrawal"), cex=.8) + 
  scale_color_manual(values=c("grey", "blue"), name="Flow")+
  coord_cartesian(xlim=c(as.Date("1984-01-01"),as.Date("2005-12-31")),
                  ylim=c(-400,400)) + 
  labs(x="Date", y="Flow [cfs]", title="Gage Flows and Withdrawals")
ggsave(file="GAGE_Flowdif_vs_withdrawal.png", width=9, height=5, units="in")




# Inspect the 2002 year: look how off it is when comparing gage vs model
ggplot(modelcfs, aes(x=unique.model.Date.)) + 
  geom_line(aes(y=upstream, col="Upstream"), cex=.8) +
  geom_line(aes(y=wayside, col="Wayside"), cex=.8) + 
  geom_line(aes(y=wd, col="Withdrawal"), cex=.8)+
  scale_color_manual(values=c("black", "red", "blue"), name="Flow")+
  coord_cartesian(xlim=c(as.Date("2001-12-31"),as.Date("2002-12-31")), 
                  ylim=c(0,300)) + 
  labs(x="Date", y="Flow [cfs]", title="Model Flows and Withdrawals")
ggsave(file="MODEL_2002_Upstream_vs_downstream_vs_withdrawal.png", width=9, height=5, units="in")

ggplot(realvals, aes(x=Date)) + 
  geom_line(aes(y=upstream_cfs, col="Upstream Gage"), cex=0.8)+
  geom_line(aes(y=wayside_cfs, col="Wayside Gage"), cex=0.8) + 
  geom_line(aes(y=VAHydro_wd, col="VAHydro Withdrawal"), cex=0.8) + 
  scale_color_manual(values=c("black", "blue", "red"), name="Flow")+
  coord_cartesian(xlim=c(as.Date("2001-12-31"),as.Date("2002-12-31")),
                  ylim=c(0,300)) + 
  labs(x="Date", y="Flow [cfs]", title="Gage Flows and VAHydro Withdrawals")

ggsave(file="GAGE_2002_Upstream_vs_downstream_vs_withdrawal.png", width=9, height=5, units="in")


#generate flow difference plot for model and gage
ggplot(modelcfs, aes(x=unique.model.Date.)) + 
  geom_line(aes(y=flowdif, col="Downstream - Upstream"), cex=.8) +
  geom_line(aes(y=wd, col="Withdrawal"), cex=.8) + 
  scale_color_manual(values=c("grey", "blue"), name="Flow")+
  coord_cartesian(xlim=c(as.Date("2002-01-01"),as.Date("2002-12-31")),
                  ylim=c(-400,400)) + 
  labs(x="Date", y="Flow [cfs]", title="Model Flows and Withdrawals")
ggsave(file="MODEL_2002_Flowdif_vs_withdrawal.png", width=9, height=5, units="in")


ggplot(realvals, aes(x=Date)) + 
  geom_line(aes(y=flowdif, col="Downstream - Upstream"), cex=.8) +
  geom_line(aes(y=VAHydro_wd, col="Withdrawal"), cex=.8) + 
  scale_color_manual(values=c("grey", "blue"), name="Flow")+
  coord_cartesian(xlim=c(as.Date("2002-01-01"),as.Date("2002-12-31")),
                  ylim=c(-400,400)) + 
  labs(x="Date", y="Flow [cfs]", title="Gage Flows and Withdrawals")
ggsave(file="GAGE_2002_Flowdif_vs_withdrawal.png", width=9, height=5, units="in")



# look at model withdrawal and VAHydro withdrawal ------------- 
#are they off by a factor of something? (is there a decimal point error?)

#pull in model withdrawal and vahydro withdrawal: 
model_withdrawal <- data.frame(modelcfs$unique.model.Date., modelcfs$wd)
colnames(model_withdrawal) <- c("Date", "Mwd")
VAhydro_withdrawal <- data.frame(realvals$Date, realvals$VAHydro_wd)
colnames(VAhydro_withdrawal) <- c("Date", "Vwd")

withdrawal<-merge(model_withdrawal, VAhydro_withdrawal)
withdrawal$div <- withdrawal$Mwd/withdrawal$Vwd


ggplot(withdrawal, aes(x=Date)) + 
  geom_line(aes(y=div, col="Division"), cex=.8) +
  #geom_line(aes(y=VAHydro_wd, col="Withdrawal"), cex=.8) + 
  scale_color_manual(values=c("black"), name="Method")+
  coord_cartesian(xlim=c(as.Date("2002-01-01"),as.Date("2002-12-31")),
                  ylim=c(3.75,4.25)) + 
  labs(x="Date", y="Model/VAHydro wd", title="Withdrawal Differences")
ggsave(file="2002_withdrawal_differences.png", width=9, height=5, units="in")


# Kelsey Reitz
#4/1/2019
# Evaluation of VAHydro_model data to USGS gage data 
# proper unit notation
rm(list = ls())

library(pander);
library(httr);
library(hydroTSM);
save_directory <- "/var/www/html/files/fe/plots"
#----------------------------------------------
site <- "http://deq2.bse.vt.edu/d.dh"    #Specify the site of interest, either d.bet OR d.dh
#----------------------------------------------
# Load Libraries
basepath='/var/www/R';
basepath = 'C:/Users/Kelsey/Desktop/GitHub/hydro-tools';
source(paste(basepath,'config.local.private',sep='/'));
source(paste(hydro_tools,"VAHydro-2.0/rest_functions.R", sep = "/")); 
source(paste(hydro_tools,"VAHydro-1.0/fn_vahydro-1.0.R", sep = "/"));  
source(paste(hydro_tools,"LowFlow/fn_iha.R", sep = "/"));  
#retrieve rest token - DISABLED
#fxn_locations <-  '/usr/local/home/git/r-dh-ecohydro/ELFGEN';
#source(paste(fxn_locations,"elf_rest_token.R", sep = "/"));   
#elf_rest_token (site, token)
# to run in knit'r, need to preload token
#token = 'W-THcwwvstkINd9NIeEMrmNRls-8kVs16mMEcN_-jOA';

#Generate REST token              
rest_uname = FALSE
rest_pw = FALSE
source(paste(hydro_tools,"auth.private", sep = "\\")); #load rest username and password, contained in auth.private file
source(paste(hydro_tools,"VAHydro-2.0","rest_functions.R", sep = "\\")) #load REST functions
token <- rest_token(site, token, rest_uname, rest_pw)
options(timeout=1200); # set timeout to twice default level to avoid abort due to high traffic

# inputs for pulling via REST -----------------------------------
hydrocode = "vahydrosw_wshed_OR2_8020_8130";
ftype = 'vahydro'; # nhd_huc8, nhd_huc10, vahydro
inputs <- list (
  hydrocode = hydrocode,
  bundle = 'watershed',
  ftype = 'vahydro'
)
#property dataframe returned
feature = FALSE;
odata <- getFeature(inputs, token, site, feature);
# Ex: flows <- fn_get_rundata(207885, 402);
#     fn_iha_7q10(flows);
# Get data frame for stashing multirun data
stash <- data.frame();
mostash <- data.frame();
tsstash = FALSE;
featureid <- odata[1,"hydroid"];
fname <- as.character(odata[1,]$name );
inputs <- list(
  varkey = "wshed_local_area_sqmi",
  featureid = featureid,
  entity_type = "dh_feature"
)

da <- getProperty(inputs, site, model) #model has not been specified anywhere above

inputs <- list(
  varkey = "om_model_element",
  featureid = featureid,
  entity_type = "dh_feature",
  propcode = "vahydro-1.0"
)
model <- getProperty(inputs, site, model)
mid = as.numeric(as.character(model[1,]$pid))
inputs <- list(
  varkey = "om_element_connection",
  featureid = mid,
  entity_type = "dh_properties"
)
prop <- getProperty(inputs, site, prop)
elid = as.numeric(as.character(prop[1,]$propvalue))

# Analsyis config
#runids = c(20021,20023);
runid = 2 #original id was 1 -- 
    # 2 will give run using monthly varying values that reflect current demand
targetyear = 2005;
eventstart = paste(targetyear,"-02-15",sep='')
eventend = paste(targetyear,"-06-15",sep='')
bigQ = 50;

wshed_summary_tbl = data.frame(
  "Run ID" = character(), 
  "Segment Name (D. Area)" = character(), 
  "7Q10/ALF/LF-90" = character(), 
  "WD (mean/max)" = character(), 
  stringsAsFactors = FALSE) ;
#pander(odata);
#pander(odata);

omsite = site <- "http://deq2.bse.vt.edu"
dat <- fn_get_runfile(elid, runid, site= omsite,  cached = TRUE);

dat <- window(dat, start = as.Date("1984-01-01"), end = as.Date("2005-12-31"))
setwd("C:/Users/Kelsey/Desktop/GitHub/hydro-tools/HARP-2018/PointSource_RoanokeAnalysis/VAHydro_Model")
write.csv(dat, "vahydro_upstream_flow.csv")

#--------------------------------------
# end rest retrieval
# -------------------------------------


# modify dat dataframe to be more user friendly: 
vahydroflow <- dat[,c(4,5, 21, 32)]
vahydroflow <- data.frame(vahydroflow)
#format data
vahydroflow$Qout <- as.numeric(as.character(vahydroflow$Qout))
vahydroflow$timestamp <- as.Date(as.character(vahydroflow$timestamp))

# ----------------------------------------
# begin data analysis / comparison 
# -----------------------------------------

## load libraries
library(lubridate)
library(dplyr)
library(RCurl)
library(ggplot2)
library(dataRetrieval)

# set workspace


# pull USGS gage data for wayside 

# what does this look like compared to actual flow values? 
gage <- readNWISdv('02054530', parameterCd='00060', startDate = "1984-01-01", 
                   endDate = "2005-12-31", statCd = "00003")

hydro_view <- ggplot(gage, aes(Date)) + 
  geom_line(aes(y=X_00060_00003, colour="gage"), size=0.5) + 
  geom_line(data=vahydroflow, aes(x=timestamp, y=Qout, colour="vahydromodel"), size=0.7) + 
  scale_colour_manual(values=c("blue", "black")) + 
  labs(x="Date", y="Flow [cfs]", colour="Legend") + 
  coord_cartesian(ylim=c(0,300), xlim=c(as.Date("2003-12-31"), as.Date("2005-12-31")))
ggsave(file="vahydro_v_gageflow.png", width=9, height=5, units="in")


# add CPB model data to the mix -----------------

# pull model data from deq ---------------------------------------------------------------------------
deq <- "http://deq2.bse.vt.edu/p532c-sova/wdm/river/p532cal_062211/stream/"

#MUST CHANGE WHAT CODE YOU'RE DOING.-- changed by: 
code <- 3000 #3000 is flow data
rivseg <- "OR2_8130_7900"

study_seg <- paste0(rivseg,"_0111.csv")
#check to make sure that the file exists on the site. 
goodtogo <- url.exists(paste0(deq, study_seg)) # check to make sure csv exists





if (goodtogo ==TRUE){
  modeldata <- read.csv(paste0(deq, study_seg))
  colnames(modeldata) <- c("year", "month", "day", "hour", "flow[cfs]")
}

#units are in cfs
#modeldata <- subset(modeldata, modeldata$year==yoi)
modeldata$date <- as.Date(paste0(modeldata$year,"-",modeldata$month,"-",modeldata$day))
modeldata <- modeldata[which(modeldata$date=="1984-01-01" & modeldata$hour=="1"):nrow(modeldata),]
rownames(modeldata) <- 1:nrow(modeldata)
modeldata$date <- as.Date(with(modeldata, paste(year, month, day,sep="-")), "%Y-%m-%d")

modeldata<- aggregate(modeldata$`flow[cfs]`, list(modeldata$date), FUN = sum)
modeldata$x <- modeldata$x * 0.504167 #convert ac-ft to cfs
colnames(modeldata) <- c("date", "flow")


#plot all three
hydro_view <- ggplot(gage, aes(Date)) + 
  geom_line(aes(y=X_00060_00003, colour="gage"), size=0.5) + 
  geom_line(data=vahydroflow, aes(x=timestamp, y=Qout, colour="vahydro_model"), size=0.7) + 
  geom_line(data=modeldata, aes(x=date, y=flow, colour="CPB_model"), size=0.7) + 
  scale_colour_manual(values=c("black", "blue", "red")) + 
  labs(x="Date", y="Flow [cfs]", colour="Legend") + 
  coord_cartesian(ylim=c(0,300), xlim=c(as.Date("2001-12-31"), as.Date("2002-12-31")))
ggsave(file="CPB_v_vahydro_v_gageflow_2002.png", width=9, height=5, units="in")


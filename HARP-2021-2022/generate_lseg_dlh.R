##### This script is used to generate a dataframe specifying if it is light or dark at a given time
## Last Update 7/26/21
## HARP Group

# load libraries
#.libPaths("/var/www/R/x86_64-pc-linux-gnu-library/")
library(lubridate)
library(sqldf)

# load vahydro functions
site <- "http://deq1.bse.vt.edu:81/d.dh"  #Specify the site of interest, either d.bet OR d.dh
basepath <- '/var/www/R';
source(paste(basepath,'config.R',sep='/'))

# load lseg_functions
source(paste(github_location,"HARParchive/HARP-2021-2022","lseg_functions.R", sep = "/"))


# loop iterates through AllLandsegList and outputs csv file
i <- 1
while(i<=length(AllLandsegList)){
  landseg <- AllLandsegList[i]
  # read in land segment radiation data
  dfRAD <- read.table(paste0("http://deq1.bse.vt.edu:81/met/out/lseg_csv/1984010100-2020123123/",landseg,".RAD"), header = FALSE, sep = ",")
  #dfRAD <- read.table(paste0("/backup/meteorology/out/lseg_csv/1984010100-2020123123/",landseg,".RAD"), header = FALSE, sep = ",")
  colnames(dfRAD) = c("year","month","day","hour","rad")
  dfRAD$date <- as.Date(paste(dfRAD$year,dfRAD$month,dfRAD$day,sep="-"))

  # determine if hour is daylight or dark
  dfDLH <- generate_lseg_dlh(dfRAD = dfRAD)
  
  # create and save DLD file as csv
  write.table(dfDLH,paste0("/Users/katealbi/Desktop/HARP/",landseg,".DLH"), 
              row.names = FALSE, col.names = FALSE, sep = ",")
  #write.table(dfDLH,paste0("/backup/meteorology/out/lseg_csv/1984010100-2020123123/",landseg,".DLH"), 
  #           row.names = FALSE, col.names = FALSE, sep = ",")
  
  i<-i+1
}

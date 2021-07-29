##### This script is an outline of calculating PET using the Hamon Method and downloading csv
## Last Update 7/29/21
## HARP Group

library(lubridate)
library(sqldf)

# load vahydro functions
site <- "http://deq1.bse.vt.edu:81/d.dh"  #Specify the site of interest, either d.bet OR d.dh
basepath <- '/var/www/R';
source(paste(basepath,'config.R',sep='/'))

# load lseg_functions
source(paste(github_location,"HARParchive/HARP-2021-2022","lseg_functions.R", sep = "/"))

# loop iterates through AllLandsegList and outputs 2 csv files, one for each PET method
i <- 1
while(i<=length(AllLandsegList)){
  landseg <- AllLandsegList[i]
  # read in land segment radiation data
  dfRAD <- read.table(paste0("http://deq1.bse.vt.edu:81/met/out/lseg_csv/1984010100-2020123123/",landseg,".RAD"), header = FALSE, sep = ",")
  colnames(dfRAD) = c("year","month","day","hour","RAD")
  dfRAD$date <- as.Date(paste(dfRAD$year,dfRAD$month,dfRAD$day,sep="-"))
  # read in land segment temperature data
  dfTMP <- read.table(paste0("http://deq1.bse.vt.edu:81/met/out/lseg_csv/1984010100-2020123123/",landseg,".TMP"), header = FALSE, sep = ",")
  colnames(dfTMP) = c("year","month","day","hour","temp")
  
  # calculate HET values and create df
  dfHET <- generate_lseg_het(dfTMP = dfTMP, dfRAD = dfRAD)
  
  # create and save HET file as csv
  write.table(dfHET,paste0("C:/Users/kylew/Documents/HARP/NLDAS/lseg_pet_csv/",landseg,".HET"), 
              row.names = FALSE, col.names = FALSE, sep = ",")
  #write.table(dfHET,paste0("/backup/meteorology/out/lseg_csv/1984010100-2020123123/",landseg,".HET"), 
  #           row.names = FALSE, col.names = FALSE, sep = ",")
  
  i<-i+1
}

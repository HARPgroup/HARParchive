##### This script is an outline of calculating PET using the Hamon Method and downloading csv
## Last Update 7/29/21
## HARP Group

library(lubridate)
library(sqldf)

# load lseg_functions


# loop iterates through AllLandsegList and outputs 2 csv files, one for each PET method
i <- 225
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
  #write.csv(dfHET,paste0("/backup/meteorology/out/lseg_csv/1984010100-2020123123/",landseg,".HET"), 
  #           row.names = FALSE, col.names = FALSE, sep = ",")
  
  i<-i+1
}

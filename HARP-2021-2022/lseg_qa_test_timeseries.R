##### This script runs QA on land segment summary stat data. It generates a txt file with list of land segments it flags.
## Last Updated 10/25/21
## HARP Group
## To change metric and QA testing value alter lines 33 and 45
## Change the .XXX label at the end of the paste statement in line 36 to correspond to metric (ex: PRC = precipitation)
## Change the numeric condition in the if statement to fit needs in line 45 (for precipitation > 1 corresponds to > 1 in/hr)

# load packages
#.libPaths("/var/www/R/x86_64-pc-linux-gnu-library/")
library(lubridate)
library(sqldf)
library(IHA)
library(zoo)

# load vahydro functions
site <- "http://deq1.bse.vt.edu:81/d.dh"  #Specify the site of interest, either d.bet OR d.dh
basepath <- '/var/www/R';
source(paste(basepath,'config.R',sep='/'))

# load lseg_functions
source(paste(github_location,"HARParchive/HARP-2021-2022","lseg_functions.R", sep = "/"))

# instantiate data frames and variables for loops
i <- 1
ErrorLsegs <- data.frame()
ErrorYear <- data.frame()
ErrorMonth <- data.frame()
ErrorDay <- data.frame()
ErrorHour <- data.frame()
ErrorValue <- data.frame()
while(i<=length(AllLandsegList)){
  
  landseg <- AllLandsegList[i]
  
  # read in summary stats data table
  timeSeries <- read.table(paste0("http://deq1.bse.vt.edu:81/met/out/lseg_csv/1984010100-2020123123/",landseg,".PRC"), header = FALSE, sep = ",")
  # code with correct input directory if running on deq machine
  #timeSeries <- read.table(paste0("/backup/meteorology/out/lseg_csv/1984010100-2020123123/",landseg,".PRC"), header = FALSE, sep = ",")
  
  
  # loops iterates through to check for abnormally values 
  j <- 1
  while (j <= nrow(timeSeries)) {
    
    if (as.numeric(timeSeries$V5[j]) > 1) {
      ErrorLsegs <- rbind(ErrorLsegs, paste0(landseg))
      ErrorYear <- rbind(ErrorYear, paste0(timeSeries$V1[j]))
      ErrorMonth <- rbind(ErrorMonth, paste0(timeSeries$V2[j]))
      ErrorDay <- rbind(ErrorDay, paste0(timeSeries$V3[j]))
      ErrorHour <- rbind(ErrorHour, paste0(timeSeries$V4[j]))
      ErrorValue <- rbind(ErrorValue, paste0(timeSeries$V5[j]))
    }
    j <- j + 1
  }

  i <- i+ 1
}

# creates data framed of flagged values with corresponding time and land segment
FlaggedData <- data.frame(ErrorYear, ErrorMonth, ErrorDay, ErrorHour, ErrorLsegs, ErrorValue)

# create and save error landsegment file as txt
write.table(FlaggedData,"C:/Users/kylew/Documents/R/HARPSpring2021/NLDAS-2/FlaggedLsegsPRC.txt", 
            row.names = FALSE, col.names = FALSE)
# code to write it to /backup/meteorology directory if we ever decide to
#write.table(ErrorLsegs,/backup/meteorology/error_lsegs.txt"), 
#           row.names = FALSE, col.names = False)

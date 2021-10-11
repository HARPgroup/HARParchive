##### This script runs QA on land segment summary stat data. It generates a txt file with list of land segments it flags.
## Last Updated 10/11/21
## HARP Group

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
ErrorValue <- data.frame()
while(i<=length(AllLandsegList)){
  
  landseg <- AllLandsegList[i]
  
  # read in summary stats data table
  sumStats <- read.table(paste0("http://deq1.bse.vt.edu:81/met/out/lseg_csv/1984010100-2020123123/",landseg,"SummaryStats.csv"), header = FALSE, sep = ",")
  # code with correct input directory if running on deq machine
  #sumStats <- read.table(paste0("/backup/meteorology/out/lseg_csv/1984010100-2020123123/",landseg,"SummaryStats.csv"), header = FALSE, sep = ",")
  
  
  # loops iterates through to check for abnormally high annual precip values 
  j <- 2
  #count <- 0
  while (j <= nrow(sumStats)) {
    
    if (as.numeric(sumStats$V10[j]) > 150) {
      ErrorLsegs <- rbind(ErrorLsegs, paste0(landseg))
      ErrorYear <- rbind(ErrorYear, paste0(sumStats$V1[j]))
      ErrorValue <- rbind(ErrorValue, paste0(sumStats$V10[j]))
      #count <- count + 1
    }
    j <- j + 1
  }

  i <- i+ 1
}

FlaggedData <- data.frame(ErrorYear, ErrorLsegs, ErrorValue)

# create and save error landsegment file as txt
write.table(FlaggedData,"C:/Users/kylew/Documents/R/HARPSpring2021/NLDAS-2/FlaggedLsegsVal.txt", 
            row.names = FALSE, col.names = FALSE)
# code to write it to /backup/meteorology directory if we ever decide to
#write.table(ErrorLsegs,/backup/meteorology/error_lsegs.txt"), 
#           row.names = FALSE, col.names = False)

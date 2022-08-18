##### This script dowloads temp, precip, and pet data and creates/downloads csv of summary stats
## Last Updated 7/29/21
## HARP Group

# load packages
#.libPaths("/var/www/R/x86_64-pc-linux-gnu-library/")
library(lubridate)
library(sqldf)
library(IHA)
library(zoo)
outdir=Sys.getenv(c('NLDAS_ROOT'))[1]

# load vahydro functions
site <- "http://deq1.bse.vt.edu:81/d.dh"  #Specify the site of interest, either d.bet OR d.dh
basepath <- '/var/www/R';
source(paste(basepath,'config.R',sep='/'))

# load lseg_functions
source(paste(github_location,"HARParchive/HARP-2021-2022","lseg_functions.R", sep = "/"))

argst <- commandArgs(trailingOnly = T)
if (length(argst) > 0) {
  lseg_arg = argst[1]
  AllLandsegList = c(lseg_arg)
} else {
  # load landseg list of interest
  AllLandsegList <- scan(file = "https://raw.githubusercontent.com/HARPgroup/HARParchive/master/GIS_layers/p5_landsegments.txt", what = character())
}
if (length(argst) > 1) {
  outdir = argst[2]
}


i <- 1
while(i<=length(AllLandsegList)){
  landseg <- AllLandsegList[i]
  # read in land segment temperature and precipitation data
  dfPRC <- read.table(paste0("http://deq1.bse.vt.edu:81/met/out/lseg_csv/1984010100-2020123123/",landseg,".PRC"), header = FALSE, sep = ",")
  dfTMP <- read.table(paste0("http://deq1.bse.vt.edu:81/met/out/lseg_csv/1984010100-2020123123/",landseg,".TMP"), header = FALSE, sep = ",")
  dfPET <- read.table(paste0("http://deq1.bse.vt.edu:81/met/out/lseg_csv/1984010100-2020123123/",landseg,".PET"), header = FALSE, sep = ",")
  #dfPRC <- read.table(paste0("/backup/meteorology/out/lseg_csv/1984010100-2020123123/",landseg,".PRC"), header = FALSE, sep = ",")
  #dfTMP <- read.table(paste0("/backup/meteorology/out/lseg_csv/1984010100-2020123123/",landseg,".TMP"), header = FALSE, sep = ",")
  #dfPET <- read.table(paste0("/backup/meteorology/out/lseg_csv/1984010100-2020123123/",landseg,".PET"), header = FALSE, sep = ",")
  colnames(dfTMP) = c("year","month","day","hour","temp")
  dfTMP$date <- as.Date(paste(dfTMP$year,dfTMP$month,dfTMP$day, sep="-"))
  colnames(dfPRC) = c("year","month","day","hour","precip")
  dfPRC$date <- as.Date(paste(dfPRC$year,dfPRC$month,dfPRC$day, sep="-"))
  colnames(dfPET) = c("year","month","day","hour","pet")
  dfPET$date <- as.Date(paste(dfPET$year,dfPET$month,dfPET$day, sep="-"))
  
  # run get_lseg_summary_stats function
  summaryStats <- get_lseg_summary_stats(dfTMP=dfTMP,dfPRC=dfPRC,dfPET=dfPET)
  
  # create and save PET file as csv
  message(paste("Exporting ",paste0(outdir,"/",landseg,"SummaryStats.csv")))
  write.table(summaryStats,paste0(outdir,"/",landseg,"SummaryStats.csv"), 
              row.names = FALSE, col.names = TRUE, sep = ",")
  i<-i+1
}



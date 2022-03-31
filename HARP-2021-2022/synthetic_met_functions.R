##### This script houses functions used to create mash-up timeseries data for synthetic meteorological datasets
## Last Updated 3/23/22
## HARP Group


library(lubridate)
library(sqldf)

# load vahydro functions
site <- "http://deq1.bse.vt.edu:81/d.dh"  #Specify the site of interest, either d.bet OR d.dh
basepath <- '/var/www/R';
source(paste(basepath,'config.R',sep='/'))

# load lseg_functions
source(paste(github_location,"HARParchive/HARP-2021-2022","lseg_functions.R", sep = "/"))

# testing variables
landseg <- "N51800"
startdate1 <- "1984-01-01"
enddate1 <- "1984-12-31"
startdate2 <- "2002-01-01"
enddate2 <- "2002-12-31"

# met timeseries data downloading function
# inputs a landsegment 
# outputs a list of lseg_csv timesieries for entire downloaded time period
get_lseg_csv <- function(landseg){
  
  # downloading entire timeseries data
  dfRAD <- read.table(paste0("http://deq1.bse.vt.edu:81/met/out/lseg_csv/1984010100-2020123123/",landseg,".RAD"), header = FALSE, sep = ",")
  dfTMP <- read.table(paste0("http://deq1.bse.vt.edu:81/met/out/lseg_csv/1984010100-2020123123/",landseg,".TMP"), header = FALSE, sep = ",")
  dfPET <- read.table(paste0("http://deq1.bse.vt.edu:81/met/out/lseg_csv/1984010100-2020123123/",landseg,".PET"), header = FALSE, sep = ",")
  dfPRC <- read.table(paste0("http://deq1.bse.vt.edu:81/met/out/lseg_csv/1984010100-2020123123/",landseg,".PRC"), header = FALSE, sep = ",")
  dfWND <- read.table(paste0("http://deq1.bse.vt.edu:81/met/out/lseg_csv/1984010100-2020123123/",landseg,".WND"), header = FALSE, sep = ",")
  dfDPT <- read.table(paste0("http://deq1.bse.vt.edu:81/met/out/lseg_csv/1984010100-2020123123/",landseg,".DPT"), header = FALSE, sep = ",")
  
  #dfRAD <- read.table(paste0("/backup/meteorology/out/lseg_csv/1984010100-2020123123/",landseg,".RAD"), header = FALSE, sep = ",")
  #dfTMP <- read.table(paste0("/backup/meteorology/out/lseg_csv/1984010100-2020123123/",landseg,".TMP"), header = FALSE, sep = ",")
  #dfPET <- read.table(paste0("/backup/meteorology/out/lseg_csv/1984010100-2020123123/",landseg,".PET"), header = FALSE, sep = ",")
  #dfPRC <- read.table(paste0("/backup/meteorology/out/lseg_csv/1984010100-2020123123/",landseg,".PRC"), header = FALSE, sep = ",")
  #dfWND <- read.table(paste0("/backup/meteorology/out/lseg_csv/1984010100-2020123123/",landseg,".WND"), header = FALSE, sep = ",")
  #dfDPT <- read.table(paste0("/backup/meteorology/out/lseg_csv/1984010100-2020123123/",landseg,".DPT"), header = FALSE, sep = ",")
  
  
  # adding date column for date manipulation
  colnames(dfRAD) = c("year","month","day","hour","RAD")
  dfRAD$date <- as.Date(paste(dfRAD$year,dfRAD$month,dfRAD$day,sep="-"))
  
  colnames(dfTMP) = c("year","month","day","hour","TMP")
  dfTMP$date <- as.Date(paste(dfTMP$year,dfTMP$month,dfTMP$day,sep="-"))
  
  colnames(dfPET) = c("year","month","day","hour","PET")
  dfPET$date <- as.Date(paste(dfPET$year,dfPET$month,dfPET$day,sep="-"))
  
  colnames(dfPRC) = c("year","month","day","hour","PRC")
  dfPRC$date <- as.Date(paste(dfPRC$year,dfPRC$month,dfPRC$day,sep="-"))
  
  colnames(dfWND) = c("year","month","day","hour","WND")
  dfWND$date <- as.Date(paste(dfWND$year,dfWND$month,dfWND$day,sep="-"))
  
  colnames(dfDPT) = c("year","month","day","hour","DPT")
  dfDPT$date <- as.Date(paste(dfDPT$year,dfDPT$month,dfDPT$day,sep="-"))
  
  
  # return new time series as list
  dfALL <- list(
    "RAD" = dfRAD,
    "TMP" = dfTMP,
    "PET" = dfPET, 
    "PRC" = dfPRC,
    "WND" = dfWND,
    "DPT" = dfDPT)
  
  return(dfALL)
  
}

# running data downloading function
all_time <- get_lseg_csv(landseg = landseg)


# mash up time series function
# inputs lseg_csv data list for one land segment and entire timeperiod (output of get_lseg_csv function)
# inputs two start dates and end dates in "YYYY-MM-DD" format
# outputs a synthetic timeseries for modeling purposes
generate_synthetic_timeseries <- function(lseg_csv, startdate1, enddate1, startdate2, enddate2){
  
  # seperate list into individual data frames
  dfRAD <- lseg_csv$RAD
  dfTMP <- lseg_csv$TMP
  dfPET <- lseg_csv$PET
  dfPRC <- lseg_csv$PRC
  dfWND <- lseg_csv$WND
  dfDPT <- lseg_csv$DPT
  
  
  # filter by inputted date ranges
  dfRAD1 <- sqldf(paste0("SELECT year, month, day, hour, RAD
                  FROM dfRAD
                  WHERE date between ", 
                         as.numeric(as.Date(startdate1)),
                         " AND ",
                         as.numeric(as.Date(enddate1)),
                         ""))
  dfRAD2 <- sqldf(paste0("SELECT year, month, day, hour, RAD
                  FROM dfRAD
                  WHERE date between ", 
                         as.numeric(as.Date(startdate2)),
                         " AND ",
                         as.numeric(as.Date(enddate2)),
                         ""))
  
  dfTMP1 <- sqldf(paste0("SELECT year, month, day, hour, TMP
                  FROM dfTMP
                  WHERE date between ", 
                         as.numeric(as.Date(startdate1)),
                         " AND ",
                         as.numeric(as.Date(enddate1)),
                         ""))
  dfTMP2 <- sqldf(paste0("SELECT year, month, day, hour, TMP
                  FROM dfTMP
                  WHERE date between ", 
                         as.numeric(as.Date(startdate2)),
                         " AND ",
                         as.numeric(as.Date(enddate2)),
                         ""))
  
  dfPET1 <- sqldf(paste0("SELECT year, month, day, hour, PET
                  FROM dfPET
                  WHERE date between ", 
                         as.numeric(as.Date(startdate1)),
                         " AND ",
                         as.numeric(as.Date(enddate1)),
                         ""))
  dfPET2 <- sqldf(paste0("SELECT year, month, day, hour, PET
                  FROM dfPET
                  WHERE date between ", 
                         as.numeric(as.Date(startdate2)),
                         " AND ",
                         as.numeric(as.Date(enddate2)),
                         ""))
  
  dfPRC1 <- sqldf(paste0("SELECT year, month, day, hour, PRC
                  FROM dfPRC
                  WHERE date between ", 
                         as.numeric(as.Date(startdate1)),
                         " AND ",
                         as.numeric(as.Date(enddate1)),
                         ""))
  dfPRC2 <- sqldf(paste0("SELECT year, month, day, hour, PRC
                  FROM dfPRC
                  WHERE date between ", 
                         as.numeric(as.Date(startdate2)),
                         " AND ",
                         as.numeric(as.Date(enddate2)),
                         ""))
  
  dfWND1 <- sqldf(paste0("SELECT year, month, day, hour, WND
                  FROM dfWND
                  WHERE date between ", 
                         as.numeric(as.Date(startdate1)),
                         " AND ",
                         as.numeric(as.Date(enddate1)),
                         ""))
  dfWND2 <- sqldf(paste0("SELECT year, month, day, hour, WND
                  FROM dfWND
                  WHERE date between ", 
                         as.numeric(as.Date(startdate2)),
                         " AND ",
                         as.numeric(as.Date(enddate2)),
                         ""))
  
  dfDPT1 <- sqldf(paste0("SELECT year, month, day, hour, DPT
                  FROM dfDPT
                  WHERE date between ", 
                         as.numeric(as.Date(startdate1)),
                         " AND ",
                         as.numeric(as.Date(enddate1)),
                         ""))
  dfDPT2 <- sqldf(paste0("SELECT year, month, day, hour, DPT
                  FROM dfDPT
                  WHERE date between ", 
                         as.numeric(as.Date(startdate2)),
                         " AND ",
                         as.numeric(as.Date(enddate2)),
                         ""))
  
  
  # combining two timeseries
  dfRAD_MASH <- rbind(dfRAD1, dfRAD2)
  dfTMP_MASH <- rbind(dfTMP1, dfTMP2)
  dfPET_MASH <- rbind(dfPET1, dfPET2)
  dfPRC_MASH <- rbind(dfPRC1, dfPRC2)
  dfWND_MASH <- rbind(dfWND1, dfWND2)
  dfDPT_MASH <- rbind(dfDPT1, dfDPT2)
  
  # return new time series as list
  dfSYNTHETIC <- list(
    "RAD" = dfRAD_MASH,
    "TMP" = dfTMP_MASH,
    "PET" = dfPET_MASH, 
    "PRC" = dfPRC_MASH,
    "WND" = dfWND_MASH,
    "DPT" = dfDPT_MASH)
  
  return(dfSYNTHETIC)
}


# running mash up function
mash_up <- generate_synthetic_timeseries(lseg_csv = all_time, startdate1 = startdate1, enddate1 = enddate1, startdate2 = startdate2, enddate2 = enddate2)


# posting timeseries function
# inputs a land segment
# inputs two start dates and end dates
# inputs a lseg_csv synthetic timeseries for given dates (output of generate_synthetic_timesieries function)
# posts new synthetic timeseries to terminal for wdm generation
post_synthetic_timeseries <- function(landseg, startdate1, enddate1, startdate2, enddate2, lseg_csv){
  
  # create mashup date format for saving
  mashupdate <- paste0(substring(startdate1, 1, 4), substring(startdate1, 6, 7), substring(startdate1, 9, 10), "00-",
                       substring(enddate2, 1, 4), substring(enddate2, 6, 7), substring(enddate1, 9, 10), "23")
  
  
  # saving and posting new timeseries
  write.table(lseg_csv$RAD, paste0("C:/Users/kylew/Documents/HARP/NLDAS/mashups/", mashupdate, landseg, ".RAD"), 
              row.names = FALSE, col.names = FALSE, sep = ",")
  #write.table(dfSYNTHETIC$RAD,paste0("/backup/meteorology/out/lseg_csv/", mashupdate, landseg,".RAD"), 
  #           row.names = FALSE, col.names = FALSE, sep = ",")
  
  write.table(lseg_csv$TMP, paste0("C:/Users/kylew/Documents/HARP/NLDAS/mashups/", mashupdate, landseg, ".TMP"), 
              row.names = FALSE, col.names = FALSE, sep = ",")
  #write.table(dfSYNTHETIC$TMP,paste0("/backup/meteorology/out/lseg_csv/", mashupdate, landseg,".TMP"), 
  #           row.names = FALSE, col.names = FALSE, sep = ",")
  
  write.table(lseg_csv$PET, paste0("C:/Users/kylew/Documents/HARP/NLDAS/mashups/", mashupdate, landseg, ".PET"), 
              row.names = FALSE, col.names = FALSE, sep = ",")
  #write.table(dfSYNTHETIC$PET,paste0("/backup/meteorology/out/lseg_csv/", mashupdate, landseg,".PET"), 
  #           row.names = FALSE, col.names = FALSE, sep = ",")
  
  write.table(lseg_csv$PRC, paste0("C:/Users/kylew/Documents/HARP/NLDAS/mashups/", mashupdate, landseg, ".PRC"), 
              row.names = FALSE, col.names = FALSE, sep = ",")
  #write.table(dfSYNTHETIC$PRC,paste0("/backup/meteorology/out/lseg_csv/", mashupdate, landseg,".PRC"), 
  #           row.names = FALSE, col.names = FALSE, sep = ",")
  
  write.table(lseg_csv$WND, paste0("C:/Users/kylew/Documents/HARP/NLDAS/mashups/", mashupdate, landseg, ".WND"), 
              row.names = FALSE, col.names = FALSE, sep = ",")
  #write.table(dfSYNTHETIC$WND,paste0("/backup/meteorology/out/lseg_csv/", mashupdate, landseg,".WND"), 
  #           row.names = FALSE, col.names = FALSE, sep = ",")
  
  write.table(lseg_csv$DPT, paste0("C:/Users/kylew/Documents/HARP/NLDAS/mashups/", mashupdate, landseg, ".DPT"), 
              row.names = FALSE, col.names = FALSE, sep = ",")
  #write.table(dfSYNTHETIC$DPT,paste0("/backup/meteorology/out/lseg_csv/", mashupdate, landseg,".DPT"), 
  #           row.names = FALSE, col.names = FALSE, sep = ",")
}


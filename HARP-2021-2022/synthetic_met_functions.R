##### This script houses functions used to create mash-up timeseries data for synthetic meteorological datasets
## Last Updated 4/9/22
## HARP Group



# met timeseries data downloading function
# inputs a landsegment 
# inputs start and end date
# inputs website and linux locations of data
# outputs a list of lseg_csv timesieries for entire downloaded time period
get_lseg_csv <- function(landseg, startdate, enddate, site, dir){
  
  # creating timeframe variable for grabing data
  timeframe <- paste0(substring(startdate, 1, 4), substring(startdate, 6, 7), substring(startdate, 9, 10), "00-",
                      substring(enddate, 1, 4), substring(enddate, 6, 7), substring(enddate, 9, 10), "23")
  
  # downloading entire timeseries data
  # using web directory
  dfRAD <- fread(paste0(site,landseg, ".RAD"))
  dfTMP <- fread(paste0(site,landseg, ".TMP"))
  dfPET <- fread(paste0(site,landseg, ".PET"))
  dfPRC <- fread(paste0(site,landseg, ".PRC"))
  dfWND <- fread(paste0(site,landseg, ".WND"))
  dfDPT <- fread(paste0(site,landseg, ".DPT"))
  
  # using linux terminal directory
  #dfRAD <- read.table(paste0(dir, "/", landseg,".RAD"), header = FALSE, sep = ",")
  #dfTMP <- read.table(paste0(dir, "/", landseg,".TMP"), header = FALSE, sep = ",")
  #dfPET <- read.table(paste0(dir, "/", landseg,".PET"), header = FALSE, sep = ",")
  #dfPRC <- read.table(paste0(dir, "/", landseg,".PRC"), header = FALSE, sep = ",")
  #dfWND <- read.table(paste0(dir, "/", landseg,".WND"), header = FALSE, sep = ",")
  #dfDPT <- read.table(paste0(dir, "/", landseg,".DPT"), header = FALSE, sep = ",")
  
  
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
  
  
  # filter by inputted date range
  dfRAD <- sqldf(paste0("SELECT year, month, day, hour, RAD
                  FROM dfRAD
                  WHERE date between ", 
                        as.numeric(as.Date(startdate)),
                        " AND ",
                        as.numeric(as.Date(enddate)),
                        ""))
  
  dfTMP <- sqldf(paste0("SELECT year, month, day, hour, TMP
                  FROM dfTMP
                  WHERE date between ", 
                        as.numeric(as.Date(startdate)),
                        " AND ",
                        as.numeric(as.Date(enddate)),
                        ""))
  
  dfPET <- sqldf(paste0("SELECT year, month, day, hour, PET
                  FROM dfPET
                  WHERE date between ", 
                        as.numeric(as.Date(startdate)),
                        " AND ",
                        as.numeric(as.Date(enddate)),
                        ""))
  
  dfPRC <- sqldf(paste0("SELECT year, month, day, hour, PRC
                  FROM dfPRC
                  WHERE date between ", 
                        as.numeric(as.Date(startdate)),
                        " AND ",
                        as.numeric(as.Date(enddate)),
                        ""))
  
  dfWND <- sqldf(paste0("SELECT year, month, day, hour, WND
                  FROM dfWND
                  WHERE date between ", 
                        as.numeric(as.Date(startdate)),
                        " AND ",
                        as.numeric(as.Date(enddate)),
                        ""))
  
  dfDPT <- sqldf(paste0("SELECT year, month, day, hour, DPT
                  FROM dfDPT
                  WHERE date between ", 
                        as.numeric(as.Date(startdate)),
                        " AND ",
                        as.numeric(as.Date(enddate)),
                        ""))
  
  
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


# posting timeseries function
# inputs a land segment
# inputs two start dates and end dates
# inputs a lseg_csv synthetic timeseries for given dates (output of generate_synthetic_timesieries function)
# inputs saving directory
# posts new synthetic timeseries to terminal for wdm generation
post_synthetic_timeseries <- function(landseg, startdate1, enddate1, startdate2, enddate2, lseg_csv, dir){
  
  # create mashup date format for saving
  mashupdate <- paste0(substring(startdate1, 1, 4), substring(startdate1, 6, 7), substring(startdate1, 9, 10), "00-",
                       substring(enddate2, 1, 4), substring(enddate2, 6, 7), substring(enddate2, 9, 10), "23")
  
  
  # saving and posting new timeseries
  # first line is for local testing
  # second line saves to ouput directory on linux
  write.table(lseg_csv$RAD, paste0("C:/Users/kylew/Documents/HARP/NLDAS/mashups/", mashupdate, landseg, ".RAD"), 
              row.names = FALSE, col.names = FALSE, sep = ",")
  #write.table(lseg_csv$RAD,paste0(dir, mashupdate, landseg,".RAD"), 
  #           row.names = FALSE, col.names = FALSE, sep = ",")
  
  write.table(lseg_csv$TMP, paste0("C:/Users/kylew/Documents/HARP/NLDAS/mashups/", mashupdate, landseg, ".TMP"), 
              row.names = FALSE, col.names = FALSE, sep = ",")
  #write.table(lseg_csv$TMP,paste0(dir, mashupdate, landseg,".TMP"), 
  #           row.names = FALSE, col.names = FALSE, sep = ",")
  
  write.table(lseg_csv$PET, paste0("C:/Users/kylew/Documents/HARP/NLDAS/mashups/", mashupdate, landseg, ".PET"), 
              row.names = FALSE, col.names = FALSE, sep = ",")
  #write.table(lseg_csv$PET,paste0(dir, mashupdate, landseg,".PET"), 
  #           row.names = FALSE, col.names = FALSE, sep = ",")
  
  write.table(lseg_csv$PRC, paste0("C:/Users/kylew/Documents/HARP/NLDAS/mashups/", mashupdate, landseg, ".PRC"), 
              row.names = FALSE, col.names = FALSE, sep = ",")
  #write.table(lseg_csv$PRC,paste0(dir, mashupdate, landseg,".PRC"), 
  #           row.names = FALSE, col.names = FALSE, sep = ",")
  
  write.table(lseg_csv$WND, paste0("C:/Users/kylew/Documents/HARP/NLDAS/mashups/", mashupdate, landseg, ".WND"), 
              row.names = FALSE, col.names = FALSE, sep = ",")
  #write.table(lseg_csv$WND,paste0(dir, mashupdate, landseg,".WND"), 
  #           row.names = FALSE, col.names = FALSE, sep = ",")
  
  write.table(lseg_csv$DPT, paste0("C:/Users/kylew/Documents/HARP/NLDAS/mashups/", mashupdate, landseg, ".DPT"), 
              row.names = FALSE, col.names = FALSE, sep = ",")
  #write.table(lseg_csv$DPT,paste0(dir, mashupdate, landseg,".DPT"), 
  #           row.names = FALSE, col.names = FALSE, sep = ",")
}


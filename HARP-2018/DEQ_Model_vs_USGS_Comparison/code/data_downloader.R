# DOCUMENTATION -----------------------------------------------------------

# Links inputted USGS gage to corresponding river segment,
# downloads data from USGS, downloads model data from DEQ2.

# LIBRARIES ---------------------------------------------------------------

library(dataRetrieval)
library(lubridate)
library(plyr)

# INPUTS ------------------------------------------------------------------

# Address of "DEQ_Model_vs_USGS_Comparison" folder
# Include "DEQ_Model_vs_USGS_Comparison" in address!
container <- "C:\\Users\\Daniel\\Documents\\HARP\\GitHub\\hydro-tools\\HARP-2018\\DEQ_Model_vs_USGS_Comparison"

# USGS Gage number
siteNo <- "02077500"

# Start and end dates of data (Model: Has data from 1984-01-01 to 2005-12-31)
start.date <- "1984-01-01"
end.date <- "2005-12-31"

# CARRYOVER IF MASTER IS BEING RUN ----------------------------------------
if (exists("container.master") == TRUE) {
  container <- container.master
  siteNo <- siteNo.master
  new.or.original <- new.or.original.master
}

# CREATING DIRECTORIES FOR DATA STORAGE
dir.create(paste0(container, "\\data\\new_(updated)_data"), showWarnings = FALSE)
dir.create(paste0(container,"\\data\\new_(updated)_data\\raw_data"), showWarnings = FALSE);
dir.create(paste0(container,"\\data\\new_(updated)_data\\raw_data\\gage_data"), showWarnings = FALSE)
dir.create(paste0(container,"\\data\\new_(updated)_data\\raw_data\\merged_data"), showWarnings = FALSE)
dir.create(paste0(container,"\\data\\new_(updated)_data\\raw_data\\model_data"), showWarnings = FALSE)
dir.create(paste0(container,"\\data\\new_(updated)_data\\raw_data\\model_data\\hourly_data"), showWarnings = FALSE)
dir.create(paste0(container,"\\data\\new_(updated)_data\\raw_data\\model_data\\daily_data"), showWarnings = FALSE)

# LINKING MODEL SEGMENT ---------------------------------------------------

gage.to.segment <- read.csv(file.path(container, "data", "Gage_To_Segment_Northern.csv"),
                          header = TRUE, sep = ',', stringsAsFactors = FALSE)
gage.to.segment <- subset(gage.to.segment, gage.to.segment$gage_number == as.numeric(siteNo))
RivSeg <- gage.to.segment$river_segment

# IMPORTING AND EXPORTING USGS DATA -----------------------------------------------------

pCode <- "00060"
USGS_daily <- readNWISdv(siteNo, pCode, start.date, end.date)
names(USGS_daily)
USGS_daily <- renameNWISColumns(USGS_daily)
write.csv(USGS_daily, file = paste0(container, "/data/new_(updated)_data/raw_data/gage_data/",siteNo," - Raw Data.csv"))

# IMPORTING AND EXPORTING MODEL DATA ----------------------------------------------------

# Splitting the River Segment string into each segment name
RivSegStr <- strsplit(RivSeg, "\\+")
RivSegStr <- RivSegStr[[1]]
num.segs <- length(RivSegStr)
model_days <- length(seq(as.Date(start.date):as.Date(end.date)))

#Reads data into data frame "ovols", exports each seg's data
if (num.segs == 1) {
  # Downloading and exporting hourly model data
  model_hourly <- read.csv(paste0("http://deq2.bse.vt.edu/p532c-sova/wdm/river/p532cal_062211/stream/", 
                                  RivSeg, "_0111.csv"), header = FALSE, sep = ",", stringsAsFactors = FALSE);  
  write.csv(model_hourly, file = paste0(container, "/data/new_(updated)_data/raw_data/model_data/hourly_data/",RivSeg," - Hourly Raw Data.csv"))
  
  # Converting hourly to daily data and exporting daily data
  model_hourly <- model_hourly[-1,]
  model_hourly$V1 <- trimws(model_hourly$V1, which = "both")
  colnames(model_hourly) <- c("year","month","day","hour","ovol")
  model_hourly$date <- as.Date(paste0(model_hourly$year,"-",model_hourly$month,"-",model_hourly$day))
  model_daily <- aggregate(model_hourly$ovol, list(model_hourly$date), FUN = sum)
  colnames(model_daily) <- c("date","mod.flow")
  write.csv(model_daily, file = paste0(container, "/data/new_(updated)_data/raw_data/model_data/daily_data/",RivSeg," - Daily Raw Data.csv"))
  
  # Merging data and exporting merged data
  combined.flows <- merge(USGS_daily, model_daily, by.x = "Date", by.y = "date", all = TRUE)
  write.csv(combined.flows, file = paste0(container, "/data/new_(updated)_data/raw_data/merged_data/", siteNo, "_vs_", RivSeg," - Raw Data.csv"))
  
} else {
  ovols <- setNames(data.frame(replicate(num.segs,sample(0, model_days, rep = TRUE))), sprintf("flow%s", seq(from = 1, to = num.segs, by = 1)))
  seg_ctr <- 1
  for (seg_ctr in 1:length(RivSegStr)) {
    # Downloading and exporting hourly model data
    model_hourly <- read.csv(paste0("http://deq2.bse.vt.edu/p532c-sova/wdm/river/p532cal_062211/stream/", 
                                    RivSegStr[seg_ctr], "_0111.csv"), header = FALSE, sep = ",", stringsAsFactors = FALSE);
    write.csv(model_hourly, file = paste0(container, "/data/new_(updated)_data/raw_data/model_data/hourly_data/",RivSegStr[seg_ctr]," - Hourly Raw Data.csv"))
    
    # Converting hourly to daily data and exporting daily data
    model_hourly <- model_hourly[-1,]
    model_hourly$V1 <- trimws(model_hourly$V1, which = "both")
    colnames(model_hourly) <- c("year","month","day","hour","ovol")
    model_hourly$date <- as.Date(paste0(model_hourly$year,"-",model_hourly$month,"-",model_hourly$day))
    model_daily <- aggregate(model_hourly$ovol, list(model_hourly$date), FUN = sum)
    colnames(model_daily) <- c("date","mod.flow")
    write.csv(model_daily, file = paste0(container, "/data/new_(updated)_data/raw_data/model_data/daily_data/",RivSegStr[seg_ctr]," - Daily Raw Data.csv"))
    
    # Adding flow to a matrix, for later flow summing
    ovols[,paste0("flow",seg_ctr)] <- model_daily$mod.flow
  }
  
  # Sums ovols, outputs summed daily flows
  model_daily$mod.flow <- rowSums(ovols)
  write.csv(model_daily, file = paste0(container, "/data/new_(updated)_data/raw_data/model_data/daily_data/",RivSeg," - Daily Raw Data.csv"))
  
  # Merging data and exporting merged data
  combined.flows <- merge(USGS_daily, model_daily, by.x = "Date", by.y = "date", all = TRUE)
  write.csv(combined.flows, file = paste0(container, "/data/new_(updated)_data/raw_data/merged_data/", siteNo, "_vs_", RivSeg," - Raw Data.csv"))
  seg_ctr <- seg_ctr + 1
}

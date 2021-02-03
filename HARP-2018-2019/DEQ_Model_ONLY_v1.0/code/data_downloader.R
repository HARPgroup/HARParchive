# DOCUMENTATION -----------------------------------------------------------

# Downloads model data for river segment from DEQ2.

rm(list = ls())

# LIBRARIES ---------------------------------------------------------------

library(dataRetrieval)
library(lubridate)
library(plyr)

# INPUTS ------------------------------------------------------------------

# Address of "DEQ_Model_ONLY" folder
# Include "DEQ_Model_ONLY_v..." in address!
container <- "C:\\Users\\Daniel\\Documents\\HARP\\DEQ_Model_ONLY_v1.0"

# River Segment
RivSeg <- "TU3_8480_8680"

# Start and end dates of data (Model: Has data from 1984-01-01 to 2005-12-31)
start.date <- "1984-01-01"
end.date <- "2005-12-31"

# CARRYOVER IF MASTER IS BEING RUN ----------------------------------------
if (exists("container.master") == TRUE) {
  container <- container.master
  RivSeg <- RivSeg.master
  new.or.original <- new.or.original.master
}

# SOURCING CALCULATING FUNCTION -------------------------------------------
source(paste0(container, "\\code\\fn_hourly2daily.R"))

# CREATING DIRECTORIES FOR DATA STORAGE
dir.create(paste0(container, "\\data\\new_(updated)_data"), showWarnings = FALSE)
dir.create(paste0(container,"\\data\\new_(updated)_data\\daily_data"), showWarnings = FALSE)

# IMPORTING AND EXPORTING MODEL DATA ----------------------------------------------------

model_days <- length(seq(as.Date(start.date):as.Date(end.date)))

# Downloading hourly model data, converting to daily, exporting daily
model_hourly <- try(read.csv(paste0("http://deq2.bse.vt.edu/p532c-sova/wdm/river/p532cal_062211/stream/", 
                                                         RivSeg, "_0111.csv"), header = FALSE, sep = ",", stringsAsFactors = FALSE))
model_daily <- fn_mod.hourly2daily(model_hourly)
write.csv(model_daily, file = paste0(container, "/data/new_(updated)_data/daily_data/",RivSeg," - Daily Raw Data.csv"))
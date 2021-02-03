# DOCUMENTATION -----------------------------------------------------------

# Loads previously downloaded data, trims it to proper time frame

# LOADING LIBRARIES -------------------------------------------------------

library('lubridate')

# INPUTS ------------------------------------------------------------------

# Address of "DEQ_Model_ONLY_v..." folder
# Include "DEQ_Model_ONLY_v..." in address!
container <- "C:\\Users\\Daniel\\Documents\\HARP\\DEQ_Model_ONLY_v1.0"

# River Segment ID
RivSeg <- "MN2_8530_8510"

# Should new or original data be used?
new.or.original <- "new"

# CARRYOVER IF MASTER IS BEING RUN ----------------------------------------
if (exists("container.master") == TRUE) {
  container <- container.master
  RivSeg <- RivSeg.master
  new.or.original <- new.or.original.master
}

# NEW OR ORIGINAL DATA SWITCH ---------------------------------------------

if (new.or.original == "new") {
  container.cont <- "\\data\\new_(updated)_data"
} else if (new.or.original == "original") {
  container.cont <- "\\data\\original_(reproducible)_data"
} else {
  print("ERROR: neither new or original data specified")
}

# CREATING DIRECTORIES FOR DATA STORAGE -----------------------------------
dir.create(paste0(container,"\\data\\new_(updated)_data\\trimmed_data"), showWarnings = FALSE)

# LOADING DATA ------------------------------------------------------------

data <- read.csv(paste0(container, container.cont, "\\daily_data\\", RivSeg," - Daily Raw Data.csv"))

# REMOVING NA DATA --------------------------------------------------------

data <- data[complete.cases(data),]

# TRIMMING TO WATER YEAR --------------------------------------------------

data.length <- length(data$date)
start.month <- month(data$date[1])
end.month <- month(data$date[data.length])
start.day <- day(data$date[1])
end.day <- day(data$date[data.length])

if (start.month <= 9) {
  start.year <- year(data$date[1])
} else if (start.month == 10 & start.day == 1) {
  start.year <- year(data$date[1])
} else {
  start.year <- year(data$date[1]) + 1
}

if (end.month >= 10) {
  end.year <- year(data$date[data.length])
} else if (end.month == 9 & end.day == 30) {
  end.year <- year(data$date[data.length])
} else {
  end.year <- year(data$date[data.length]) - 1
}

start.date <- paste0(start.year, "-10-01")
end.date <- paste0(end.year, "-09-30")

start.line <- which(data$date == start.date)
end.line <- which(data$date == end.date)

data <- data[start.line:end.line,]


# CONVERTING MODEL TO CFS -----------------------------------------------

# Model data is in acre-feet
# USGS gage data is in cfs
# The conversion factor from acre-feet to cfs is 0.504167

data$mod.flow <- data$mod.flow * 0.504167

# EXPORTING "TRIMMED FLOW" ------------------------------------------------

# Exporting "trimmed flow"
write.csv(data, file = paste0(container, "/data/new_(updated)_data/trimmed_data/", RivSeg," - Derived Data.csv"))
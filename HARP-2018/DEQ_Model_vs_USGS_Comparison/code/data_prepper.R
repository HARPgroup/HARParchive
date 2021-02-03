# DOCUMENTATION -----------------------------------------------------------

# Loads previously downloaded data, trims it to proper time frame, removes
# lines of code where gage or model data is NA, area-adjusts data.

# LOADING LIBRARIES -------------------------------------------------------

library('lubridate')
library('dataRetrieval')

# INPUTS ------------------------------------------------------------------

# Address of "DEQ_Model_vs_USGS_Comparison" folder
# Include "DEQ_Model_vs_USGS_Comparison" in address!
container <- "C:\\Users\\Daniel\\Documents\\HARP\\GitHub\\hydro-tools\\HARP-2018\\DEQ_Model_vs_USGS_Comparison"

# USGS Gage number
siteNo <- "03208500"

# Should new or original data be used?
new.or.original <- "new"

site <- "http://deq2.bse.vt.edu/d.bet"    #Specify the site of interest, either d.bet OR d.dh

basepath='C:\\Users\\Daniel\\Documents\\HARP\\GitHub\\hydro-tools';

# SETUP

source(paste(basepath,'config.local.private',sep='/'));
#retrieve rest token
source(paste(hydro_tools,"VAHydro-2.0/rest_functions.R", sep = "/")); 
source(paste(hydro_tools,"auth.private", sep = "/"));#load rest username and password, contained in auth.private file
token <- rest_token(site, token, rest_uname, rest_pw);
options(timeout=120); # set timeout to twice default level to avoid abort due to high traffic

# CARRYOVER IF MASTER IS BEING RUN ----------------------------------------
if (exists("container.master") == TRUE) {
  container <- container.master
  siteNo <- siteNo.master
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

# LINKING MODEL SEGMENT ---------------------------------------------------

gage.to.segment <- read.csv(file.path(container, "data", "Gage_To_Segment.csv"),
                            header = TRUE, sep = ',', stringsAsFactors = FALSE)
gage.to.segment <- subset(gage.to.segment, gage.to.segment$gage_number == as.numeric(siteNo))
RivSeg <- gage.to.segment$river_segment

# CREATING DIRECTORIES FOR DATA STORAGE -----------------------------------
dir.create(paste0(container,"\\data\\new_(updated)_data\\derived_data"), showWarnings = FALSE)
dir.create(paste0(container,"\\data\\new_(updated)_data\\derived_data\\trimmed_data"), showWarnings = FALSE)
dir.create(paste0(container,"\\data\\new_(updated)_data\\derived_data\\trimmed+area-adjusted_data"), showWarnings = FALSE)

# LOADING DATA ------------------------------------------------------------

data <- read.csv(paste0(container, container.cont, "/raw_data/merged_data/", siteNo, "_vs_", RivSeg," - Raw Data.csv"))

# REMOVING NA DATA --------------------------------------------------------

data <- data[complete.cases(data),]

# TRIMMING TO WATER YEAR --------------------------------------------------

data.length <- length(data$Date)
start.month <- month(data$Date[1])
end.month <- month(data$Date[data.length])
start.day <- day(data$Date[1])
end.day <- day(data$Date[data.length])

if (start.month <= 9) {
  start.year <- year(data$Date[1])
} else if (start.month == 10 & start.day == 1) {
  start.year <- year(data$Date[1])
} else {
  start.year <- year(data$Date[1]) + 1
}

if (end.month >= 10) {
  end.year <- year(data$Date[data.length])
} else if (end.month == 9 & end.day == 30) {
  end.year <- year(data$Date[data.length])
} else {
  end.year <- year(data$Date[data.length]) - 1
}

start.date <- paste0(start.year, "-10-01")
end.date <- paste0(end.year, "-09-30")

start.line <- which(data$Date == start.date)
end.line <- which(data$Date == end.date)

data <- data[start.line:end.line,]

# ELIMINATING UNNECCESARY COLUMNS -----------------------------------------

data <- data[,c("Date", "Flow", "mod.flow")]
data <- setNames(data, c("date", "gage.flow", "model.flow"))

# AREA-ADJUSTING MODEL FLOW -----------------------------------------------

# Model data is in acre-feet
# USGS gage data is in cfs
# The conversion factor from acre-feet to cfs is 0.504167

data$model.flow <- data$model.flow * 0.504167

# EXPORTING "TRIMMED FLOW" ------------------------------------------------

# Exporting "trimmed flow"
write.csv(data, file = paste0(container, container.cont, "/derived_data/trimmed_data/", siteNo, "_vs_", RivSeg," - Derived Data.csv"))

# AREA-ADJUSTING FLOW -----------------------------------------------------

gage.area <- readNWISsite(siteNo)
gage.area <- gage.area$drain_area_va

# Splitting the River Segment string into each segment name
RivSegStr <- strsplit(RivSeg, "\\+")
RivSegStr <- RivSegStr[[1]]
num.segs <- length(RivSegStr)
sum.model.area <- 0

for (i in 1:num.segs) {

# GETTING MODEL DATA FROM VA HYDRO
hydrocode = paste("vahydrosw_wshed_",RivSegStr[i],sep="");
ftype = 'vahydro'; # nhd_huc8, nhd_huc10, vahydro
inputs <- list (
  hydrocode = hydrocode,
  bundle = 'watershed',
  ftype = 'vahydro'
)
#property dataframe returned
feature = FALSE;
odata <- getFeature(inputs, token, site, feature);
hydroid <- odata[1,"hydroid"];
fname <- as.character(odata[1,]$name );
print(paste("Retrieved hydroid",hydroid,"for", fname,RivSegStr[i], sep=' '));
# Getting the local drainage area feature
areainfo <- list(
  varkey = "wshed_drainage_area_sqmi",
  featureid = as.integer(as.character(hydroid)),
  entity_type = "dh_feature"
)
model.area <- getProperty(areainfo, site, model.area)
model.area <- model.area$propvalue
sum.model.area <- sum.model.area + model.area
}

ratio <- gage.area/sum.model.area

data$model.flow <- data$model.flow*(gage.area/sum.model.area)

# EXPORTING "TRIMMED + AREA-ADJUSTED FLOW ---------------------------------

write.csv(data, file = paste0(container, "/data/new_(updated)_data/derived_data/trimmed+area-adjusted_data/", siteNo, "_vs_", RivSeg," - Derived Data.csv"))
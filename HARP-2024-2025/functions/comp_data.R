



args <- commandArgs(trailingOnly = TRUE)
#four libraries needed for function
suppressPackageStartupMessages(library(dataRetrieval))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(sqldf))
suppressPackageStartupMessages(library(zoo))


if (length(args) != 4) {
  message("Usage: Rscript usgsdata.R usgs_data.csv precip_data.csv gage_id write_file_location")
  q()
}

usgs_csv <- args[1]
precip_csv <- args[2]
gage_id <- args[3]
file_location <- args[4]

print("Inputting csvs")
#argument 1 is the file path to the usgs csv made earlier
usgs_data <- read.csv(usgs_csv)
#argument 2 is the file path to the precipitation csv made earlier
precip_data <- read.csv(precip_csv)
#argument 3 is the gageid, could be improved, we only need this for the drainage area
gage_info <- readNWISsite(gage_id)
#Extract the drainage area of the gage
da <- gage_info$drain_area_va
print("creating comp_data")
comp_data <- sqldf(
  "select a.obs_date, a.precip_in as  precip_p_in, 
  a.yr, a.mo, a.da, a.wk,
  b.X_00060_00003 as usgs_cfs
  from precip_data as a
  left outer join usgs_data as b 
  on (
    a.yr = b.yr
    and a.mo = b.mo
    and a.da = b.da
  )
  order by a.yr, a.mo, a.da
  "
)

print("computing drainage area")
comp_data$precip_cfs <- 1.572 * (da * 640.0 * comp_data$precip_p_in / 12.0) / 3.07 
#argument 4 is the save location for the comp_data csv
print("outputting csv")
write.csv(comp_data,file_location)

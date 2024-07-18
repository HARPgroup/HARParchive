#Arguments
#Argument 1 is the flow_csv
#Argument 2 is the precip_csv
#argument 3 is the gage_id, this is used to create precip_cfs, could be moved to a different function
#Argument 4 is the location where the file is going to be saved



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

flow_csv<- args[1]
precip_csv <- args[2]
gage_id <- args[3]
file_location <- args[4]

print("Inputting csvs")
#argument 1 is the file path to the flow csv made earlier
usgs_data <- read.csv(flow_csv)
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
  b.obs_flow 
  from precip_data as a
  left outer join flow_data as b 
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
print(paste0("Write csv in new file path: ",write_path))
write.csv(comp_data,file_location)

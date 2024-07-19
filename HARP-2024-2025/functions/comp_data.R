#Arguments
#Argument 1 is the flow_csv
#Argument 2 is the precip_data.csv
#Argument 3 is the location where the file is going to be saved



args <- commandArgs(trailingOnly = TRUE)
#four libraries needed for function
suppressPackageStartupMessages(library(dataRetrieval))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(sqldf))
suppressPackageStartupMessages(library(zoo))


if (length(args) != 3) {
  message("Usage: Rscript usgsdata.R usgs_data.csv precip.csv write_path")
  q()
}

flow_csv<- args[1]
precip_csv <- args[2]
write_path <- args[3]

print("Inputting csvs")
#argument 1 is the file path to the flow csv made earlier
flow_data <- read.csv(flow_csv)
#argument 2 is the file path to the precipitation csv made earlier
precip_data <- read.csv(precip_csv)
print("creating comp_data")
daily_data <- sqldf(
  "select a.obs_date, a.precip_in as  precip_p_in, 
  a.yr, a.mo, a.da, a.wk,
  b.obs_flow, dra 
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
daily_data$precip_p_cfs <- 1.572 * (daily_data$dra * 640.0 * daily_data$precip_p_in / 12.0) / 3.07
#argument 3 is the save location for the comp_data csv
print(paste0("Write csv in new file path: ",write_path))
write.csv(daily_data,write_path)

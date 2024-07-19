# Inputs (args):
# 1 = File path of csv from VA Hydro
# 2 = Data source "nldas2, daymet, prism"
# 3 = End path of new csv
# Outputs:
# Csv file with manipulated data at end filepath

# Library necessary packages
print("Accessing necessary libraries")
suppressPackageStartupMessages(library("dataRetrieval"))
suppressPackageStartupMessages(library("sqldf"))
suppressPackageStartupMessages(library("zoo"))
suppressPackageStartupMessages(library("lubridate"))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3){
  message("Missing or extra inputs. Usage: Rscript hydroimport_daily.R data_csv_location write_path")
  q()
}
# Set up command args
print("Reading command args")
#this accepts links and file locations on device
precip_csv_location <-args[1]
flow_csv_location <- args[2]
write_path <- args[3]

# Pull csv from input file path
print("Reading csv")
hydro_daily <- read.csv(precip_csv_location)
flow_data <- read.csv(flow_csv_location)
# Add in more date information
print("Adding date information")
hydro_daily[,c('yr', 'mo', 'da', 'wk')] <- cbind(year(as.Date(hydro_daily$obs_date)),
                                                 month(as.Date(hydro_daily$obs_date)),
                                                 day(as.Date(hydro_daily$obs_date)),
                                                 week(as.Date(hydro_daily$obs_date)))

# If data comes from nladas2 (hourly), it must be converted into daily data
print("Summing to daily data")
  hydro_daily <- sqldf(
    "select featureid, min(obs_date) as obs_date, yr, mo, da, wk, 
     sum(precip_mm) as precip_mm, sum(precip_in) as precip_in
   from hydro_daily 
   group by yr, mo, da, wk
   order by yr, mo, da, wk
  "
  )
  
daily_data <- sqldf(
  "select a.obs_date, a.precip_in as precip_p_in, 
  a.yr, a.mo, a.da, a.wk,
  b.obs_flow, b.dra
  from hydro_daily as a
  left outer join flow_data as b 
  on (
    a.yr = b.yr
    and a.mo = b.mo
    and a.da = b.da
  )
  order by a.yr, a.mo, a.da
  "
  )
print("test")
daily_data$precip_p_cfs <- 1.572 * (daily_data$dra * 640.0 * daily_data$precip_p_in / 12.0) / 3.07

# Write csv in new file path
  print(paste0("Write csv in new file path: ",write_path))
write.csv(daily_data,write_path)

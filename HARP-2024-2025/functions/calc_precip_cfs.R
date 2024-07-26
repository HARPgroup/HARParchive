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
if (length(args) != 2){
  message("Missing or extra inputs. Usage: Rscript calc_precip_cfs.R input_csv_location output_csv_location")
  q()
}
# Set up command args
print("Reading command args")
#this accepts links and file locations on device
data_csv_location <-args[1]
write_location <- args[2]

# Pull csv from input file path
print("Reading csv")
precip_daily <- read.csv(data_csv_location)

if (!('area_sqmi' %in% names(precip_daily))) {
  message(paste("Error: area_sqmi column is required in input file, given:", names(precip_daily)))
}

# If data comes from nladas2 (hourly), it must be converted into daily data
precip_daily$precip_cfs <- 1.572 * (precip_daily$area_sqmi * 640.0 * precip_daily$precip_in / 12.0) / 3.07 
# Write csv in new file path
print(paste0("Write csv in new file path: ",write_location))
write.csv(precip_daily,write_location)

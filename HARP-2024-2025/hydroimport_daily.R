# Inputs (args):
# 1 = File path of csv from VA Hydro
# 2 = Data source "nldas2, daymet, prism"
# 3 = End path of new csv
# Outputs:
# Csv file with manipulated data at end filepath

# Library necessary packages
print("Accessing necessary libraries")
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(sqldf))

# Set up command args
print("Reading command args")
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3){
  message("Missing or extra inputs. Usage: Rscript hydroimport_daily.R input_path data_source output_path")
  q()
}

input_path <- args[1]
data_source <- args[2]
output_path <- args[3]

# Pull csv from input file path
print("Reading csv")
hydro_daily <- read.csv(input_path)
# Add in more date information
print("Adding date information")
hydro_daily[,c('yr', 'mo', 'da', 'wk')] <- cbind(year(as.Date(hydro_daily$obs_date)),
                                                 month(as.Date(hydro_daily$obs_date)),
                                                 day(as.Date(hydro_daily$obs_date)),
                                                 week(as.Date(hydro_daily$obs_date)))

# If data comes from nladas2 (hourly), it must be converted into daily data
print("Checking for nldas2")
if (data_source=="nldas2"){
  hydro_daily <- sqldf(
    "select featureid, min(obs_date) as obs_date, yr, mo, da, 
     sum(precip_mm) as precip_mm, sum(precip_in) as precip_in
   from hydro_daily 
   group by yr, mo, da
   order by yr, mo, da
  "
  )}

# Write csv in new file path
print("Writing csv")
write.csv(hydro_daily,file = output_path)
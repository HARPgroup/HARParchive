# Inputs (args):
# 1 = File path of csv from VA Hydro
# 2 = Data source "nldas2, daymet, prism"
# 3 = End path of new csv
# Outputs:
# Csv file with manipulated data at end filepath

# Library necessary packages
print("Accessing necessary libraries")
library(lubridate)
library(sqldf)

# Set up command args
print("Reading command args")
args <- commandArgs(trailingOnly = TRUE)

# Pull csv from input file path
print("Reading csv")
hydro_daily <- read.csv(args[1])
# Add in more date information
print("Adding date information")
hydro_daily[,c('yr', 'mo', 'da', 'wk')] <- cbind(year(as.Date(hydro_daily$obs_date)),
                                                 month(as.Date(hydro_daily$obs_date)),
                                                 day(as.Date(hydro_daily$obs_date)),
                                                 week(as.Date(hydro_daily$obs_date)))

# If data comes from nladas2 (hourly), it must be converted into daily data
print("Checking for nldas2")
if (args[2]=="nldas2"){
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
write.csv(hydro_daily,file = args[3])
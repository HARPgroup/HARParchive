# Inputs (args): 
# 1 = comp data csv filepath
# 2 = Output csv path

# Libraries
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(sqldf))
suppressPackageStartupMessages(library(dplyr))


args <- commandArgs(trailingOnly = T)

if (length(args) != 2){
  message("Missing or extra inputs. Usage: Rscript make_weekly_summary_ts.R comp_data_filepath output_filepath")
  q()
}
print("Assigning arguments")
comp_data_filepath <- args[1]
output_filepath <- args[2]



print("Reading in comp data csv")
comp_data <- read.csv(comp_data_filepath)
  
# Check for necessary data there are a lot of 
#required columns and this ensures they are all available
print("Checking columns")
if ("obs_date" %in% colnames(comp_data)){}else{
  message("Missing obs_date column, required columns are obs_date, yr, wk, mo, precip_p_in, precip_cfs, and usgs_cfs")
 q()
}
if ("yr" %in% colnames(comp_data)){}else{
  message("Missing yr column, required columns are obs_date, yr, wk, mo, precip_p_in, precip_cfs, and usgs_cfs")
  q()
}
if ("wk" %in% colnames(comp_data)){}else{
  message("Missing wk column, required columns are obs_date, yr, wk, mo, precip_p_in, precip_cfs, and usgs_cfs")
  q()
}
if ("mo" %in% colnames(comp_data)){}else{
  message("Missing mo column, required columns are obs_date, yr, wk, mo, precip_p_in, precip_cfs, and usgs_cfs")
  q()
}
if ("precip_p_in"%in% colnames(comp_data)){}else{
  message("Missing precip_p_in column, required columns are obs_date, yr, wk, mo, precip_p_in, precip_cfs, and usgs_cfs")
  q()
}
if ("precip_cfs"%in% colnames(comp_data)){}else{
  message("Missing precip_cfs column, required columns are obs_date, yr, wk, mo, precip_p_in, precip_cfs, and usgs_cfs")
  q()
}
if ("usgs_cfs" %in% colnames(comp_data)){}else{
  message("Missing usgs_cfs column, required columns are obs_date, yr, wk, mo, precip_p_in, precip_cfs, and usgs_cfs")
  q()
}


#converts our daily data into weekly
print("Converting to weekly data")
  week_data <- sqldf(
    "select min(obs_date) as start_date, max(obs_date) as end_date, yr, wk,
     avg(precip_p_in) as weekly_mean_p_in, avg(precip_cfs) as weekly_mean_precip_cfs,
     avg(usgs_cfs) as weekly_mean_usgs_cfs
   from comp_data
   group by yr, wk
   order by yr, wk
  "
  )
  

  #Base the month column off of the min day index for that week, or the day that
  #week began

  week_data$mo <- month(week_data$start_date)
  
  
  write.csv(week_data,output_filepath)
  

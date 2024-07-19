# Inputs (args): 
# 1 = comp data csv filepath
# 2 = Output csv path

# Libraries
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(sqldf))
suppressPackageStartupMessages(library(dplyr))


args <- commandArgs(trailingOnly = T)

if (length(args) != 2){
  message("Missing or extra inputs. Usage: Rscript make_weekly_summary_ts.R comp_data_filepath write_path")
  q()
}
print("Assigning arguments")
comp_data_filepath <- args[1]
write_path <- args[2]

required_variables <- c("obs_date", "yr", "wk", "mo", "precip_p_in", "precip_p_cfs","obs_flow")
print("Reading in comp data csv")
comp_data <- read.csv(comp_data_filepath)
  
# Check for necessary data there are a lot of 
#required columns and this ensures they are all available
print("Checking columns")
if (!all(required_variables %in% colnames(comp_data))){
  message("Missing required columns, required columns are obs_date, yr, wk, mo, precip_p_in, precip_cfs, and obs_flow")
 q()
}



#converts our daily data into weekly
print("Converting to weekly data")
  week_data <- sqldf(
    "select min(obs_date) as start_date, max(obs_date) as end_date, yr, wk,
     avg(precip_p_in) as weekly_mean_p_in, avg(precip_p_cfs) as weekly_mean_precip_cfs,
     avg(obs_flow) as weekly_mean_obs_flow
   from comp_data
   group by yr, wk
   order by yr, wk
  "
  )
  

  #Base the month column off of the min day index for that week, or the day that
  #week began

week_data$mo <- month(week_data$start_date)
  
print(paste0("Write csv in new file path: ",write_path))
write.csv(week_data,write_path)
  

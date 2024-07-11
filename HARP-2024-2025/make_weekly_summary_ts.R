# Inputs (args): 
# 1 = comp data csv filepath
# 2 = Output csv path
# 3 = data column

# Libraries
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(sqldf))


args <- commandArgs(trailingOnly = T)

if (length(args) != 3){
  message("Missing or extra inputs. Usage: Rscript make_weekly_summary_ts.R comp_data_filepath output_filepath data_column ")
}
# Missing from github issue:[weekly_column(default=weekly_mean_value)]
print("Assigning arguments")
comp_data_filepath <- args[1]
output_filepath <- args[2]
data_column <- args[3]

print("Reading in comp data csv")
comp_data <- read.csv(comp_data_filepath)
  
# Check for necessary data (Comp data doesnt have start and end date?)
print("Checking columns")
#if ("start_date" %in% colnames(comp_data)){}else{
 # message("start_date column missing")
 # q()
#}

#if ("end_date" %in% colnames(comp_data)){}else{
  #message("end_date column missing")
  #q()
#}

if (data_column %in% colnames(comp_data)){}else{
  message(data_column, "column missing")
  q()
}


#converts our daily data into weekly
print("Converting to weekly data")
  week_data <- sqldf(
    "select min(obs_date) as week_begin, yr, wk, min(dataset_day) as dataset_day_begin,
     avg(dataset_p_in) as dataset_p_in, avg(dataset_cfs) as dataset_cfs,
     avg(usgs_cfs) as usgs_cfs, avg(today_d_cfs) as today_d_cfs, 
     avg(nextday_d_cfs) as nextday_d_cfs
   from comp_data
   group by yr, wk
   order by yr, wk
  "
  )
  #Base the month column off of the min day oindex for that week, or the day that
  #week began
  week_data$mo <- month(week_data$week_begin)
  
  
  write.csv(week_data,output_filepath)
  

#Arguments
# 1 the location of the csv, can be comp_data or week_data
#2 y_variable
#3 x_variable
#4 month variable
#5 csv write_path this is just stats


suppressPackageStartupMessages(library("dataRetrieval"))
suppressPackageStartupMessages(library("lubridate"))
suppressPackageStartupMessages(library("sqldf"))
suppressPackageStartupMessages(library("R6"))
suppressPackageStartupMessages(library("rjson"))
#for testing purposes
source("~/HarpData/HARParchive/HARP-2024-2025/functions/lm_analysis_plots_copy.R")


#mon_lm function
#source("https://raw.githubusercontent.com/HARPgroup/HARParchive/master/HARP-2024-2025/functions/lm_analysis_plots.R")


#checks for proper number of arguments
args <- commandArgs(trailingOnly = T)
if (length(args) != 5){
  message("Missing or extra inputs. Usage: Rscript analysis.R data_csv y_variable x_var month_var write_path")
  q()
}
print("Assigning Arguments to Variables")
data_location <- args[1]
y_var <- args[2]
x_var <- args[3]
mo_var <- args[4]
write_path <- args[5]

print("Reading in data")
sample_data <- read.csv(data_location)

print("Running mon_lm function")
data_lm <- mon_lm_new(sample_data,y_var,x_var,mo_var)
json <- toJSON(data_lm)
print(paste0("Write csv in new file path: ",write_path))
write(json, write_path)









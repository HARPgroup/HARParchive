#Arguments
# 1 the location of the csv, can be comp_data or week_data
#2 y_variable
#3 x_variable
#4 month variable
#5 json write_path this is the full data_lm
#5 csv write_path this is just stats


suppressPackageStartupMessages(library("dataRetrieval"))
suppressPackageStartupMessages(library("lubridate"))
suppressPackageStartupMessages(library("sqldf"))
suppressPackageStartupMessages(library("R6"))
suppressPackageStartupMessages(library("jsonlite"))
#mon_lm function
#for testing purposes
#source("~/HarpData/HARParchive/HARP-2024-2025/functions/lm_analysis_plots_copy.R")
source("https://raw.githubusercontent.com/HARPgroup/HARParchive/master/HARP-2024-2025/functions/lm_analysis_plots.R")


#checks for proper number of arguments
args <- commandArgs(trailingOnly = T)
if (length(args) != 7){
  message("Missing or extra inputs. Usage: Rscript analysis.R data_csv y_variable x_var month_var json_write_path csv_write_path png_location")
  q()
}
print("Assigning Arguments to Variables")
data_location <- args[1]
y_var <- args[2]
x_var <- args[3]
mo_var <- args[4]
json_write_path <- args[5]
stats_write_path <-args[6]
png_location <- args [7]

print("Reading in data")
sample_data <- read.csv(data_location)

print("Running mon_lm function")
data_lm <- mon_lm_stats(sample_data,y_var,x_var,mo_var)
data_meth_json <- data_lm$toJSON()
data_from_json <- plotBin$new(data_meth_json, data_is_json=TRUE)
# compare the plots from the original object and the serialized/unserialized object
plot(data_lm$atts$lms[[1]],1)
plot(data_from_json$atts$lms[[1]],1)
# This outputs our residuals
for (i in 1:12){
  png(paste0(png_location, i,".png"))
  plot(data_from_json$atts$lms[[i]],1)
  dev.off()
}

print(paste0("Write json in new file path: ",json_write_path))
write(json_data_lm, json_write_path)
write.csv(data_lm$atts$stats, stats_write_path)










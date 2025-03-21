#Arguments
#1 the location of the stats file
#2 dataset name, used for the plot
#3 label name, gageid is a good choice
#4location to save the png

suppressPackageStartupMessages(library("dataRetrieval"))
suppressPackageStartupMessages(library("lubridate"))
suppressPackageStartupMessages(library("sqldf"))
suppressPackageStartupMessages(library("R6"))
#for testing purposes only


source("~/HarpData/HARParchive/HARP-2024-2025/functions/lm_analysis_plots_copy.R")
#source("https://raw.githubusercontent.com/HARPgroup/HARParchive/master/HARP-2024-2025/functions/lm_analysis_plots.R")




args <- commandArgs(trailingOnly = T)
if (length(args) != 4){
  message("Missing or extra inputs. Usage: Rscript plot_save.R data_location data_name label_name write_path for png")
  q()
}
print("Assigning Arguments to Variables")
data_location <- args[1]
data_name <- args[2]
label_name <- args[3]
write_path <- args[4]

stats <- read.csv(data_location)
print(paste0("Write png in new file path: ",write_path))
png(filename = write_path)
# plot(testPlot)
mon_lm_plot(stats, data_name, label_name)
dev.off()



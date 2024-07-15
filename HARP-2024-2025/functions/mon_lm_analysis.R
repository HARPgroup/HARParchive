suppressPackageStartupMessages(library("dataRetrieval"))
suppressPackageStartupMessages(library("lubridate"))
suppressPackageStartupMessages(library("sqldf"))
suppressPackageStartupMessages(library("R6"))
source("https://raw.githubusercontent.com/HARPgroup/HARParchive/master/HARP-2024-2025/functions/lm_analysis_plots.R")

args <- commandArgs(trailingOnly = T)
if (length(args) != 8){
  message("Missing or extra inputs. Usage: Rscript analysis.R data_csv x_variable y_var month_var dataset location_for_stats location_for_plot")
  q()
}
print("Assigning Arguments to Variables")
data_location <- args[1]
y_var <- args[2]
x_var <- args[3]
m_var <- args[4]
data_name <- args[5]
label_name <- args[6]
csv_location <- args[7]
plot_location <- args[8]

print("Reading in data")
sample_data <- read.csv(data_location)

print("Running mon_lm function")
data_lm <- mon_lm(sample_data, y_var, x_var, 
                  m_var, data_name,label_name)
print("Creating csv of stats")
write.csv(data_lm$atts$stats,csv_location)

print("Creating plot")
plotMe <- function(data_lm){plot(x = data_lm$atts$stats$mo, y = data_lm$atts$stats$r_PRISM)}
png(file = plot_location)
plotMe(data_lm)
bp <- barplot(
  data_lm$atts$stats$r_PRISM ~ data_lm$atts$stats$mo,
  ylim=c(0,1.0),
  main=paste("lm(Q ~ P), monthly,",data_name,label_name)
)
dev.off()



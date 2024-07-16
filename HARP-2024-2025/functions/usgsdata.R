# Inputs (args):
# 1 = Gage id you want to use
# 2 = End path of new csv
suppressPackageStartupMessages(library(dataRetrieval))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(sqldf))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) {
  message("Usage: Rscript usgsdata.R gage_id output_path")
  q()
}

gage_id <- args[1]
write_path <- args[2]

print("Pull csv from input file path")

usgs_data <- readNWISdv(gage_id,'00060')
print("Extract date information from the gage using lubridate as above")
usgs_data[,c('yr', 'mo', 'da')] <- cbind(year(as.Date(usgs_data$Date)),
                                         month(as.Date(usgs_data$Date)),
                                         day(as.Date(usgs_data$Date)))
gage_info <- readNWISsite(gage_id)


print(paste0("Write csv in new file path: ",write_path))
write.csv(usgs_data,write_path)

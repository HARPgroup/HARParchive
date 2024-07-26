# Inputs (args):
# 1 = Gage id you want to use
# 2 = End path of new csv
suppressPackageStartupMessages(library(dataRetrieval))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(sqldf))
suppressPackageStartupMessages(library(dplyr))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) {
  message("Usage: Rscript usgsdata.R gage_id output_path")
  q()
}

gage_id <- args[1]
write_path <- args[2]

print("Pull csv from input file path")

flow_data <- readNWISdv(gage_id,'00060')
print("Extract date information from the gage using lubridate as above")
flow_data[,c('yr', 'mo', 'da')] <- cbind(year(as.Date(flow_data$Date)),
                                         month(as.Date(flow_data$Date)),
                                         day(as.Date(flow_data$Date)))

#Converts the name for flow from usgs to our generic name of obs_flow
#adds drainage area as a column, to be used in later steps

flow_data <- flow_data |> rename(obs_flow = X_00060_00003)
gage_info <- readNWISsite(gage_id)
flow_data <- flow_data |> mutate(dra = gage_info$drain_area_va)




print(paste0("Write csv in new file path: ",write_path))
write.csv(flow_data,write_path)



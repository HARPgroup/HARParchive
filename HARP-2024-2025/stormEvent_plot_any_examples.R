library("hydrotools")
basepath='/var/www/R'
source(paste(basepath,'config.R',sep='/'))
source("https://raw.githubusercontent.com/HARPgroup/HARParchive/refs/heads/master/HARP-2024-2025/stormEvent_plot_any.R")

p <- storm_plot("usgs_ws_01646000", date = "2003-01-01", bufferDays = 30,
           modelScenarios = c("pubsheds","mubsheds","lubsheds","subsheds"),
           riverseg = "PM7_4581_4580")
p$p


#Pass list of model data frames so we don't have to keep getting data:
#Create an empty list to populate with model data
model_out <- list()
modelScenarios = c("pubsheds","mubsheds","lubsheds","subsheds")
riverseg = "PS3_5100_5080"
for(i in modelScenarios){
  # riverseg <- 'PM7_4581_4580'
  #Compile URL to get model data
  model_output_file <- paste0('http://deq1.bse.vt.edu:81/p6/out/river/',
                              i, '/hydr/', riverseg, '_hydrd_wy.csv')
  message(paste0("Retrieving data for ",i," for ",riverseg," from: ",model_output_file))
  model_data <- read.table(model_output_file, header = TRUE, sep = ",")
  #Set the date column and add the run id as a column
  model_data$date <- as.Date(model_data$date)
  model_data$runid <- i
  #Add data to any exisitng model data
  model_out <- c(model_out,
                 list(model_data))
}
names(model_out) <- modelScenarios
#Need to pass in a named list!
p <- storm_plot("usgs_ws_01634000", date = "1999-07-01", bufferDays = 60,
                modelList = model_out)
p$p

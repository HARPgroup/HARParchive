#Adds columns for year, month, day, and hour to a csv with an 'index' column
suppressPackageStartupMessages(library(data.table)) 
suppressPackageStartupMessages(library(lubridate))

hydr_file_path <- argst[1]

hydr <- fread(hydr_file_path)

origin <- "1970-01-01"
hydr$date <- as.Date(hydr$index, format = "%m/%d/%Y %H:%M", origin = origin)
hydr$hour <- hour(hydr$index)
hydr$day <- day(hydr$index)
hydr$month <- month(hydr$index)
hydr$year <- year(hydr$index)

write.table(hydr,file = hydr_file_path, sep = ",", row.names = FALSE)
# Creating a hydr csv of hspf 0111.csv files

suppressPackageStartupMessages(library(data.table)) 
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(zoo))
suppressPackageStartupMessages(library(R.utils))


#setwd("/Users/VT_SA/Documents/HARP") #for testing
#hspf <- fread('OR1_7700_7980_0111.csv')

# Accepting command arguments:
argst <- commandArgs(trailingOnly = T)
input_file_path <- argst[1] 
#input_file_path <- '/media/model/p532/out/river/p532sova_2021/stream/OR1_7700_7980_0111.csv' #for testing 
output_file_path <- argst[2]
#output_file_path <- '/media/model/p532/out/river/p532sova_2021/stream/OR1_7700_7980_hydr.csv'

hspf <- fread(input_file_path)
colnames(hspf) <- c('year','month','day','hour','ROVOL')

# Creating new hydr table
hspf$Qout <- hspf$ROVOL*12.1 # converting ROVOL to cfs
hspf$index <- seq(mdy_hm("1/1/1984 0:00"), mdy_hm("12/31/2020 23:00"), by = "hour")

hydr <- hspf[,c("index", "ROVOL", "Qout")]

write.table(hydr,file = output_file_path, sep = ",", row.names = FALSE)

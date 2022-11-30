# creating a csv with wanted col and wdm format

basepath='/var/www/R';
source("/var/www/R/config.R")

suppressPackageStartupMessages(library(data.table)) 
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(R.utils))

omsite = "http://deq1.bse.vt.edu:81"

#setwd("/Users/VT_SA/Documents/HARP") #for testing
#hydr <- fread("OR1_7700_7980_hydr.csv") #for testing

argst <- commandArgs(trailingOnly = T)
input_path <- argst[1]
output_path <- argst[2]
column <- argst[3]

hydr <- fread(input_path)
hydr %>% select(column) -> hydr_column

# creating tables with OVOL3 and ROVOL
hydr_df <- data.frame(hydr$year, hydr$month, hydr$day, hydr_column)
#Robustify by adding usage for hourly and daily data 

# exporting the tables
write.table(hydr_df, file = output_path, sep = ',', row.names = FALSE, col.names = FALSE, quote = FALSE)


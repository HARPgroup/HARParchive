# Script to compare hydr tables from hspf and hsp2
basepath='/var/www/R';
source("/var/www/R/config.R")

suppressPackageStartupMessages(library(data.table)) 
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(zoo))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(caTools))
suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(IHA))
suppressPackageStartupMessages(library(PearsonDS))
#suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(R.utils))
#suppressPackageStartupMessages(library(hydrotools))

omsite = "http://deq1.bse.vt.edu:81"

# Accepting command arguments:
argst <- commandArgs(trailingOnly = T)
path_to_hspf <- argst[1]
path_to_hsp2 <- argst[2] 

image_path <- '/media/model/p532/out/river/hsp2_2022/images/OR1_7700_7980_compare.png'
#Importing flow data from hspf:
#hspf_data <- model_import_data_cfs('OR2_7650_8070', 'p532', 'p532sova_2021', '1984-01-01', '2020-12-31', urlbase = omsite )
# We need to find how that function is created to use it 

hydr_f <- fread(path_to_hspf) # Hydr table from hspf
hydr_2 <- fread(path_to_hsp2) # Hydr table from hsp2

colnames(hydr_f) <- c('year', 'month', 'day', 'hour', 'flow')

#Convert ROVOL in hsp2 hydr to cfs for comparison (from ac-ft/hr)
hydr_2$ROVOL_cfs = hydr_2$ROVOL*12.1

#Adding usable dates to both data tables 
hydr_2$date <- as.Date(hydr_2$index, format = "%m/%d/%Y %H:%M")
hydr_2$month <- month(hydr_2$date)
hydr_2$year <- year(hydr_2$date)

hydr_f$date = paste(hydr_f[,1],'-',hydr_f[,2],'-',hydr_f[,3], sep = '')


#Aggregate both tables to get monthly average flows 
monthlyQout2 <- aggregate(hydr_2$ROVOL_cfs, by = list(hydr_2$month, hydr_2$year), FUN = "mean")
colnames(monthlyQout2) <- c("month", "year", "Qout")

monthlyQoutF <- aggregate(hydr_f$flow, by = list(hydr_f$month, hydr_f$year), FUN = "mean")
colnames(monthlyQoutF) <- c("month", "year", "Qout")

monthlyQout2$date <- paste(monthlyQout2$month,'-',monthlyQout2$year, sep = '')
monthlyQoutF$date <- paste(monthlyQoutF$month,'-',monthlyQoutF$year, sep = '')

png(image_path)
plot(monthlyQout2$date,monthlyQout2$Qout, type = 'l', col = 'blue', ylab = 'Qout (cfs)',  xlab = NA,)
lines(monthlyQoutF$date,monthlyQoutF$Qout, col = 'red')
legend(x = "topright", legend = c('HSP2', 'HSPF'), fill = c('blue','red'), bty = 'n')
title(main = 'HSPF vs HSP2 Outflows', sub = 'Monthly average values are plotted')


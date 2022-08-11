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
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(R.utils))


omsite = "http://deq1.bse.vt.edu:81"

#setwd("/Users/glenncampagna/Desktop/HARPteam22/Data") # for testing only
#hydr_2 <- fread("OR1_7700_7980_hydr.csv") # for testing only #
#hydr_f <- fread("OR1_7700_7980_0111.csv") # for testing only 

# Accepting command arguments:
argst <- commandArgs(trailingOnly = T)
riverseg <- argst[1]
#riverseg <- ('OR1_7700_7980_hydr')
path_to_hspf <- argst[2]
#/media/model/p532/out/river/p532sova_2021/stream/OR2_7650_8070_0111.csv
path_to_hsp2 <- argst[3] 
#media/model/p532/out/river/hsp2_2022/hydr/OR2_7650_8070_hydr.csv

image_path <- paste('/media/model/p532/out/river/hsp2_2022/', 'images','/',riverseg,'_compare.pd.box.png', sep='')
image_path2 <- paste('/media/model/p532/out/river/hsp2_2022/', 'images','/',riverseg,'_compare.pd.flow.png', sep='')

hydr_f <- fread(path_to_hspf) # Hydr table from hspf (ac-ft/hr)
hydr_2 <- fread(path_to_hsp2) # Hydr table from hsp2 - ROVOL in ac-ft/hr

colnames(hydr_f) <- c('year', 'month', 'day', 'hour', 'flow')


#Convert ROVOL to cfs for comparison (from ac-ft/hr)
hydr_2$Qout_cfs = hydr_2$ROVOL*12.1
hydr_f$Qout_cfs = hydr_f$flow*12.1

hydr_f_ <- hydr_f[2:nrow(hydr_f)] #Results in hours lining up for hydr_f_ and hydr_2
hydr_f_$Qout2 <- hydr_2$Qout_cfs[2:(nrow(hydr_f_)+1)]
hydr_f_$diff <- (abs(hydr_f_$Qout_cfs - hydr_f_$Qout2))/((hydr_f_$Qout_cfs + hydr_f_$Qout2)/2)*100


#Removing the 1st 9 months of data because of hsp2 'warm up period'
hydr_f_warm <- hydr_f_[6577:nrow(hydr_f_)]
avg_diff <- mean(hydr_f_warm$diff)


#Percent differences:

print(paste('Avg % difference:', round(avg_diff, digits = 7)))

png(image_path)
boxplot(hydr_f_warm$diff, ylab = '% Difference')  # Not useful for hourly data
title(main='Percent difference of Outflow (HSPF - HSP2)')

png(image_path2)
plot((hydr_f_warm$Qout_cfs+hydr_f_warm$Qout2),hydr_f_warm$diff, xlab = 'Magnitude of Flow (cfs)', ylab = '% Difference') +
title(main='% difference as a function of Outflow (HSPF - HSP2)')



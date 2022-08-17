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
#hydr_2 <- fread("JL1_6770_6850_hydr.csv") # for testing only #
#hydr_f <- fread("JL1_6770_6850_0111.csv") # for testing only 

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
image_path3 <- paste('/media/model/p532/out/river/hsp2_2022/', 'images','/',riverseg,'_compare.summer.png', sep='')
image_path4 <- paste('/media/model/p532/out/river/hsp2_2022/', 'images','/',riverseg,'_compare.may.png', sep='')

hydr_f <- fread(path_to_hspf) # Hydr table from hspf (ac-ft/hr)
hydr_2 <- fread(path_to_hsp2) # Hydr table from hsp2 - ROVOL in ac-ft/hr

colnames(hydr_f) <- c('year', 'month', 'day', 'hour', 'flow')


#Convert ROVOL to cfs for comparison (from ac-ft/hr)
hydr_2$Qout_cfs = hydr_2$ROVOL*12.1
hydr_f$Qout_cfs = hydr_f$flow*12.1

hydr_f_ <- hydr_f[2:nrow(hydr_f)] #Results in hours lining up for hydr_f_ and hydr_2
hydr_f_$Qout2 <- hydr_2$Qout_cfs[2:(nrow(hydr_f_)+1)]
hydr_f_$diff <- (abs(hydr_f_$Qout_cfs - hydr_f_$Qout2))/((hydr_f_$Qout_cfs + hydr_f_$Qout2)/2)*100
hydr_f_$date <- seq(ymd_hm("1984-1-1 1:00"), ymd_hm("2020-12-31 23:00"), by = "hour") 

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

#Detailed hydrographs - currently not being exported 
hydr_85 <- filter(hydr_f_warm, year == 1985)
  
hydr_85_summ <- filter(hydr_85, month == 5 | month == 6 | month ==7)
png(image_path3)
plot(hydr_85_summ$date, hydr_85_summ$Qout2, type = 'l', col = 'blue', xlab = 'Date', ylab='Qout(cfs)') +
  lines(hydr_85_summ$date, hydr_85_summ$Qout_cfs, col = 'red') +
  title(main = c('hsp2 vs hspf for 1985 summer - ', riverseg)) +
  legend(x = "topright", legend = c('HSP2', 'HSPF'), fill = c('blue','red'), bty = 'n')

hydr_85_may <- filter(hydr_85, month == 5)
png(image_path4)
plot(hydr_85_may$date, hydr_85_may$Qout2, type = 'l', col = 'blue', xlab = 'Date', ylab='Qout(cfs)') +
  lines(hydr_85_may$date, hydr_85_may$Qout_cfs, col = 'red') +
  title(main = c('hsp2 vs hspf for May, 1985 - ', riverseg)) +
  legend(x = "topright", legend = c('HSP2', 'HSPF'), fill = c('blue','red'), bty = 'n')

# High difference and low flow analysis: 

#diff_95 <- quantile(hydr_f_warm$diff, probs = .95)
#hydr_highpd <- filter(hydr_f_warm, diff > diff_95)

#ggplot(hydr_highpd, aes(date,Qout_cfs, color=diff)) + 
#  geom_point()  +
#  scale_colour_continuous(low = 'cyan', high='blue2')

#flow_10 <- quantile(hydr_f_warm$Qout_cfs, probs = .1)
#hydr_lowflow <- filter(hydr_f_warm, Qout_cfs < flow_10)

#plot((hydr_lowflow$Qout_cfs+hydr_lowflow$Qout2),hydr_lowflow$diff, xlab = 'Magnitude of Flow (cfs)', ylab = '% Difference') +
#  title(main='% difference as a function of Outflow (Lowest 10% of flows)')


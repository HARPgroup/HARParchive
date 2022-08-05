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
#hydr_2 <- fread("OR1_7700_7980_hydr.csv") # for testing only 
#hydr_f <- fread("OR1_7700_7980_0111.csv") # for testing only 

# Accepting command arguments:
argst <- commandArgs(trailingOnly = T)
path_to_hspf <- argst[1]
path_to_hsp2 <- argst[2] 

image_path1 <- '/media/model/p532/out/river/hsp2_2022/images/OR1_7700_7980_compare.png'
image_path2 <- '/media/model/p532/out/river/hsp2_2022/images/OR1_7700_7980_compare.pd.box.png'
image_path3 <- '/media/model/p532/out/river/hsp2_2022/images/OR1_7700_7980_compare.pd.flow.png'
image_path4 <- '/media/model/p532/out/river/hsp2_2022/images/OR1_7700_7980_compare.pd.mon.png'


#Importing flow data from hspf:
#hspf_data <- model_import_data_cfs('OR2_7650_8070', 'p532', 'p532sova_2021', '1984-01-01', '2020-12-31', urlbase = omsite )
# We need to find how that function is created to use it 

hydr_f <- fread(path_to_hspf) # Hydr table from hspf (ac-ft/hr)
hydr_2 <- fread(path_to_hsp2) # Hydr table from hsp2 - ROVOL in ac-ft/hr

colnames(hydr_f) <- c('year', 'month', 'day', 'hour', 'flow')

#Convert ROVOL to cfs for comparison (from ac-ft/hr)
hydr_2$Qout_cfs = hydr_2$ROVOL*12.1
hydr_f$Qout_cfs = hydr_f$flow*12.1

#Adding usable dates to both data tables 
hydr_2$date <- as.Date(hydr_2$index, format = "%m/%d/%Y %H:%M")
hydr_2$month <- month(hydr_2$date)
hydr_2$year <- year(hydr_2$date)
hydr_f$date = paste(hydr_f$year,'-',hydr_f$month,'-',hydr_f$day, sep = '')

#Aggregate both tables to get monthly average flows 
monthlyQout2 <- aggregate(hydr_2$Qout_cfs, by = list(hydr_2$month, hydr_2$year), FUN = "mean")
colnames(monthlyQout2) <- c("month", "year", "Qout")

monthlyQoutF <- aggregate(hydr_f$Qout_cfs, by = list(hydr_f$month, hydr_f$year), FUN = "mean")
colnames(monthlyQoutF) <- c("month", "year", "Qout")

monthlyQout2$date <- paste(monthlyQout2$month,'-',monthlyQout2$year, sep = '')
monthlyQoutF$date <- paste(monthlyQoutF$month,'-',monthlyQoutF$year, sep = '')

monthlyQout2$num <- 1:nrow(monthlyQout2)
monthlyQoutF$num <- 1:nrow(monthlyQoutF)

max2 <- max(monthlyQout2$Qout)
maxf <- max(monthlyQoutF$Qout)
if (max2 > maxf) {
  max = max2
}
if (maxf > max2) {
  max = maxf
}
monthlyQout_clip <- monthlyQout2[1:nrow(monthlyQoutF), ]
monthlyQout_clip$QoutF <- monthlyQoutF$Qout
years <- seq(min(monthlyQout_clip$year), max(monthlyQout_clip$year)) # for graphing

png(image_path1)
plot(monthlyQout_clip$num,monthlyQout_clip$Qout, type = 'l', col = 'blue', ylab = 'Qout (cfs)',  xlab = NA, xaxt = 'n', ylim = c(0,max))
lines(monthlyQoutF$num, monthlyQoutF$Qout, col = 'red')
legend(x = "topright", legend = c('HSP2', 'HSPF'), fill = c('blue','red'), bty = 'n')
title(main = 'HSPF vs HSP2 Outflows', sub = 'Monthly average values are plotted')
axis(1, at = seq(4,nrow(monthlyQout_clip),12), labels = years)


#Percent differences:
monthlyQout_clip$pct_df <- abs((monthlyQout_clip$Qout - monthlyQout_clip$QoutF)/monthlyQout_clip$QoutF)*100
pc_avg = as.numeric(mean(monthlyQout_clip$pct_df))
pc_rd <- round(pc_avg, digits = 2)

print(paste('Avg % difference:', pc_rd))

png(image_path2)
boxplot(monthlyQout_clip$pct_df, ylab = '% Difference')
title(main='Percent difference of Outflow (HSPF - HSP2)')

monthlyQout_clip$sum <- monthlyQout_clip$Qout + monthlyQout_clip$QoutF

png(image_path3)
plot(monthlyQout_clip$sum,monthlyQout_clip$pct_df, xlab = 'Magnitude of Flow (cfs)', ylab = '% Difference', pch =18) +
title(main='% difference as a function of Outflow (HSPF - HSP2)')

pc_mon <- aggregate(monthlyQout_clip$pct_df, by = list(monthlyQout_clip$month), FUN = 'mean')
colnames(pc_mon) <- c('month', 'pc')
pc_mon$mon <- month.abb

png(image_path4)
ggplot(pc_mon, aes(x=reorder(month,mon),pc)) + geom_bar(stat='identity') + 
  ggtitle('Avg % difference in outflow by month (HSPF - HSP2)') +
  labs(x=NULL,y='% Difference') + 
  annotate(geom = "label", x=3, y=2.5, label = paste("Overall avg % diff:", pc_rd)) +
  scale_x_discrete( labels = month.abb) 


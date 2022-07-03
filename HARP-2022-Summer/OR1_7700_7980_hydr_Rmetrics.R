# Using data from a model run (OR1_7700_7980.h5) to practice analyzing flow metrics 
# More specifically, using the table found in RESULTS/RCHRES_R001/HYDR/table 
# The 'RO' column represents total rate of outflow from RCHRES 

library(data.table)
library(zoo)
library(IHA)
library(PearsonDS)
library(ggplot2)
library(dplyr)
library(lubridate)


#setwd("/Users/glenncampagna/Documents/HARPteam22/Data") # Set working directory to location of .csv file 
# Add your directory path to a new command and comment it out when finished for future use (csv is too big to upload to GitHub)

hydr <- fread("OR1_7700_7980_hydr.csv") # fread() is faster than read.csv() for larger datasets 
hydr$date <- as.Date(hydr$index, format = "%m/%d/%y") # Creating new column for date (no time), index column originally in character format
hydr_daily <- aggregate(hydr$RO, by = list(hydr$date), FUN = "mean") # Creating table with average outflow per day for use in IHA package
colnames(hydr_daily) <- c("date", "flow") # Changing column names
hydr_dailyz <- zoo(hydr_daily$flow, order.by = hydr_daily$date)# Conversion to zoo object for use in IHA package 

  # IHA metric 1: Monthly magnitude of flow 
hydr_g1 <- as.data.frame(group1(hydr_dailyz, year = 'calendar',FUN = median)) # group1() is IHA function for metric 1
boxplot(hydr_g1, ylim = c(0,325), ylab = "Monthly magnitude (cfs)", las =2)

  # IHA metric 2: 
hydr_g2 <- as.data.frame(group2(hydr_dailyz))

# Plotting minimums over different time periods 
plot(hydr_g2$year, hydr_g2$`1 Day Min`, type = 'l', ylab = 'Flow (cfs)', xlab = 'Year', col = 'blue', ylim = c(0,125))
lines(hydr_g2$year, hydr_g2$`3 Day Min`, col = 'red', type = 'l')
lines(hydr_g2$year, hydr_g2$`7 Day Min`, col = 'green', type = 'l')
lines(hydr_g2$year, hydr_g2$`30 Day Min`, col = 'black', type = 'l')
lines(hydr_g2$year, hydr_g2$`90 Day Min`, col = 'purple', type = 'l')
title(main = 'Min Flows Post Dam')
legend(x= 1990 ,y= 120 ,legend=c("1 Day","3 Day", "7 Day", "30 Day", "90 Day"), col=c('blue','red', 'green', 'black', 'purple'), bty='n',lty=1, xjust = 0.25, yjust=0.85)

# Plotting baseflow
plot(hydr_g2$year, hydr_g2$`Base index`, type = "l", xlab = NA, ylab = 'Base Index', col = 'blue')

  # IHA metric 3:
hydr_g3 <- as.data.frame(group3(hydr_dailyz, year = 'water'))
hydr_g3$year <- sequence(37, 1984, 1)

# Plotting timing of 1 day minimum flows 
plot(hydr_g3$year, hydr_g3$Min, type = 'p', xlab = NA, ylab = 'Day of Water Year') %>% title(main = 'Timing of 1 day Min')

# Plottingtiming of 1 day maximum flows 
plot(hydr_g3$year, hydr_g3$Max, type = 'p', xlab = NA, ylab = 'Day of Water Year') %>% title(main = 'Timing of 1 day Max')

  # IHA metric 4:
hydr_g4 <- as.data.frame(group4(hydr_dailyz))
# Interesting: there were 0 low pulses in 2003, which I think I remember Rob saying was a very wet year 
hydr_g4$year <- sequence(37, 1984, 1)

# Plotting number and duration of low pulses 
par(mfrow = c(1,2))
plot(hydr_g4$year, hydr_g4$`Low pulse number`, ylab = '# of Low Pulses', xlab = NA, las = 2) %>% title(main = 'Low Pulses')
plot(hydr_g4$year, hydr_g4$`Low pulse length`, ylab = 'Length of Low Pulses', xlab = NA, las =2 ) %>% title(main = 'Low Pulse Length')

# The year with the greatest number of low pulses:
lowPulse_max <- max(hydr_g4$`Low pulse number`)
yearLow_max <- hydr_g4$year[lowPulse_max] 
print(yearLow_max) # year
print(lowPulse_max) # number of low pulses

# Plotting number and duration of high pulses 
par(mfrow = c(1,2))
plot(hydr_g4$year, hydr_g4$`High pulse number`, ylab = '# of High Pulses', xlab = NA, las = 2) %>% title(main = 'High Pulses')
plot(hydr_g4$year, hydr_g4$`High pulse length`, ylab = 'Length of High Pulses', xlab = NA, las =2) %>% title(main = 'High Pulse Length')

  # IHA metric 5:
hydr_g5 <- as.data.frame(group5(hydr_dailyz))
hydr_g5$year <- sequence(37, 1984, 1)

# Plotting reversals 
par(mfrow = c(1,1))
plot(hydr_g5$year, hydr_g5$Reversals, xlab = NA, ylab = ' # of Reversals', las = 2) %>% title(main = 'Hydrologic Reversals')

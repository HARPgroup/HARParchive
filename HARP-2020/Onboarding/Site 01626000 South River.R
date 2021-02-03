# Gage Data for Site 01626000 on the South River near Waynesboro, VA
# Author: Sarah Loomis
# Last Updated: June 4, 2020

library(dataRetrieval)
library(ggplot2)

site_id <- '01626000'
pCode <- '00060'


# 2019 water year

startDate1 <- '2018-10-01'
endDate1 <- '2019-09-30'

rawDailyQ2019 <- readNWISdv(site_id,pCode, startDate1, endDate1)

siteinfo <- readNWISsite(site_id)

a <- siteinfo$drain_area_va * 5280 * 5280
# converting drainage area to square feet

rawDailyQ2019conv <- (rawDailyQ2019$X_00060_00003 / a) * 3600 * 24 * 365 * 12
# converting to inches / yr

plot(rawDailyQ2019$Date, rawDailyQ2019conv,
     main = "South River Stream Discharge, 2019 Water Year", 
     xlab = "2019 Water Year", 
     ylab = "Stream Discharge (in/yr)",
     type = 'l')
        

ave2019cfs <- mean(rawDailyQ2019$X_00060_00003)
ave2019in <- mean(rawDailyQ2019conv)
max2019cfs <- max(rawDailyQ2019$X_00060_00003)
min2019cfs <- min(rawDailyQ2019$X_00060_00003)
max2019in <- max(rawDailyQ2019conv)
min2019in <- min(rawDailyQ2019conv)


# 2001 water year

startDate2 <- '2000-10-01'
endDate2 <- '2001-09-30'

rawDailyQ2001 <- readNWISdv(site_id,pCode, startDate2, endDate2)

b <- siteinfo$drain_area_va * 5280 * 5280
# converting drainage area to square feet

rawDailyQ2001conv <- (rawDailyQ2001$X_00060_00003 / b) * 3600 * 24 * 365 * 12
# converting to inches / yr

plot(rawDailyQ2001$Date, rawDailyQ2001conv,
     main = "South River Stream Discharge, 2001 Water Year", 
     xlab = "2001 Water Year", 
     ylab = "Stream Discharge (in/yr)",
     type = 'l')

ave2001cfs <- mean(rawDailyQ2001$X_00060_00003)
ave2001in <- mean(rawDailyQ2001conv)
max2001cfs <- max(rawDailyQ2001$X_00060_00003)
min2001cfs <- min(rawDailyQ2001$X_00060_00003)
max2001in <- max(rawDailyQ2001conv)
min2001in <- min(rawDailyQ2001conv)


# comparison chart of 2001 vs 2019, linear scale

library(scales)

ggplot(rawDailyQ2019, aes(x = rawDailyQ2019$Date)) +
        geom_line(aes(y = rawDailyQ2019conv, color = '2019')) +
        geom_line(aes(y = rawDailyQ2001conv, color = '2001')) +
        theme_bw() + 
        scale_x_date(labels = date_format("%B")) +
        labs(title = "South River Stream Discharge, 2001 vs 2019",
             x = "Water Year",
             y = "Stream Discharge (in/yr)")


# comparison chart of 2001 vs 2019, logarithmic scale

ggplot(rawDailyQ2019, aes(x = rawDailyQ2019$Date)) +
        geom_line(aes(y = rawDailyQ2019conv, color = '2019')) +
        geom_line(aes(y = rawDailyQ2001conv, color = '2001')) +
        theme_bw() + 
        scale_x_date(labels = date_format("%B")) +
        scale_y_log10() +
        labs(title = "South River Stream Discharge, 2001 vs 2019",
             x = "Water Year",
             y = "Stream Discharge (in/yr)")


# South River gage site data manipulation and summer/winter comparison boxplots

library(dplyr)
library(tidyr)

startDate <- '2000-10-01'
endDate <- '2019-09-30'

rawDailyQ <- readNWISdv(site_id,pCode, startDate, endDate)

Date = rawDailyQ$Date
Year = format(as.Date(Date, format = '%d/%m/%Y'), '%Y')
Month = format(as.Date(Date, format = '%d/%m/%Y'), '%m')
Month <- as.numeric(Month)
Year <- as.numeric(Year)
rawDailyQ$Year = Year

for (i in 1:length(rawDailyQ)) {
        if (Month[i] >= 10) {
                rawDailyQ$Year[i] <- rawDailyQ$Year[i] + 1
        }
} #Assigning water year as a separate column in rawDailyQ data frame

rawDailyQSummer <- filter(rawDailyQ, Month %in% c(6:8))
rawDailyQSummer$Year <- as.factor(rawDailyQSummer$Year)

ggplot(rawDailyQSummer,
       aes(x = rawDailyQSummer$Year, y = rawDailyQSummer$X_00060_00003)) +
        geom_boxplot(outlier.shape = NA) +
        geom_point(
                color = 'red',
                stat = 'summary',
                fun = 'mean') +
        coord_cartesian(ylim = c(0,600)) +
        theme_bw() + 
        labs(title = "South River Stream Discharge (June-August)",
             x = "Water Year",
             y = "Stream Discharge (cfs)")

rawDailyQWinter <- filter(rawDailyQ, Month %in% c(1:3))
rawDailyQWinter$Year <- as.factor(rawDailyQWinter$Year)

ggplot(rawDailyQWinter, 
       aes(x = rawDailyQWinter$Year, y = rawDailyQWinter$X_00060_00003)) +
        geom_boxplot(outlier.shape = NA) +
        geom_point(
                color = 'red',
                stat = 'summary',
                fun = 'mean') +
        coord_cartesian(ylim = c(0,800)) +
        theme_bw() + 
        labs(title = "South River Stream Discharge (January-March)",
             x = "Water Year",
             y = "Stream Discharge (cfs)")


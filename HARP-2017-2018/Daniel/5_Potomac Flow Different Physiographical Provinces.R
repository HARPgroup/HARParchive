# Activating libraries -----------------------------------------------------------------------------------------------

library(dataRetrieval)


# Assigning variables ------------------------------------------------------------------------------------------------

RiverName <- "Potomac"
siteNo1 <- '01631000'
siteNo2 <- '01638500'
siteNo3 <- '01660400'
pCode <- '00060'
start.date <- '2007-01-01'
end.date <- '2017-12-31'


# Retrieving data ---------------------------------------------------------------------------------------------------

gage1.USGS <- readNWISdv(siteNo1, pCode, start.date, end.date)
gage1.siteInfo <- readNWISsite(siteNo1)
gage1.area <- gage1.siteInfo$drain_area_va
gage2.USGS <- readNWISdv(siteNo2, pCode, start.date, end.date)
gage2.siteInfo <- readNWISsite(siteNo2)
gage2.area <- gage2.siteInfo$drain_area_va
gage3.USGS <- readNWISdv(siteNo3, pCode, start.date, end.date)
gage3.siteInfo <- readNWISsite(siteNo3)
gage3.area <- gage3.siteInfo$drain_area_va

names(gage1.USGS) <- c('Agency', 'Site', 'Date', 'Flow', 'QC')
names(gage2.USGS) <- c('Agency', 'Site', 'Date', 'Flow', 'QC')
names(gage3.USGS) <- c('Agency', 'Site', 'Date', 'Flow', 'QC')

gage1.USGS <- gage1.USGS[order(gage1.USGS$Date),]
gage2.USGS <- gage2.USGS[order(gage2.USGS$Date),]
gage3.USGS <- gage3.USGS[order(gage3.USGS$Date),]


# Plotting flow ----------------------------------------------------------------------------------------------------
plot(gage3.USGS$Date, gage3.USGS$Flow/gage3.area, type='l', ylim = c(0,25), ylab = '', xlab = '', col = 'green', axes = FALSE)
par(new=T)
plot(gage1.USGS$Date, gage1.USGS$Flow/gage1.area, type='l', ylim = c(0,25), ylab = 'Flow (cfs)/unit area', xlab = 'Year', main = ('Potomac River Flow at USGS Gages'), col = 'red')
par(new=T)
plot(gage2.USGS$Date, gage2.USGS$Flow/gage2.area, type='l', ylim = c(0,25), ylab = '', xlab = '', col = 'blue', axes = FALSE)


legend("topright", lwd = 2, col = c('red', 'blue', 'green'), legend = c("Flow at Gage 1 (Valley and Ridge)", "Flow at Gage 2 (Piedmont)", "Flow at Gage 3 (Coastal Plain)"), cex = 1.25)

# Creating barplots -----------------------------------------------------------------------------------------------

par(mfrow = c(1,3))
boxplot(gage1.USGS$Flow/gage1.area, ylim = c(0,3), ylab = "Flow per Unit Area (cfs/unit area)", main = 'Gage 1 (Shen. River near Front Royal, VA)')
boxplot(gage2.USGS$Flow/gage2.area, ylim = c(0,3), ylab = "Flow per Unit Area (cfs/unit area)", main = 'Gage 2 (near Point of Rocks, MD)')
boxplot(gage3.USGS$Flow/gage3.area, ylim = c(0,3), ylab = "Flow per Unit Area (cfs/unit area)", main = 'Gage 3 (Aquia Creek near Garris., VA)')
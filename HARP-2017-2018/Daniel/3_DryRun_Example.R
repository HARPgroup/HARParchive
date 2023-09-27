# Activating libraries -------------------------------------------------------------------------------------------------

library(dataRetrieval)


# Assigning variables --------------------------------------------------------------------------------------------------

siteNo <- '11465200'     #assigning USGS gage number
pCode <- '00060'     #assigns parameter identifier of discharge (cfs)
stat <- '00003'     #assigns statistical code of mean (00003) -- should I use statistical code of maximum (00001) instead for peak flow?
prior.start.year <- '1961'
prior.start.date <- paste0(prior.start.year, '-01-01')
prior.end.date <- '1981-12-31'
after.start.year <- '1983'
after.start.date <- paste0(after.start.year, '-01-01')
after.end.date <- '2003-01-01'


# Retrieving data ------------------------------------------------------------------------------------------------------

prior.USGS <- readNWISdv(siteNo, pCode, prior.start.date, prior.end.date)     #retrieves data prior to dam construction
after.USGS <- readNWISdv(siteNo, pCode, after.start.date, after.end.date)     #retrieves data after dam construction

names(prior.USGS) <- c('Agency', 'Site', 'Date', 'Flow', 'QC')     #renames columns of prior data frame
names(after.USGS) <- c('Agency', 'Site', 'Date', 'Flow', 'QC')     #renames columns of after data frame

prior.USGS <- prior.USGS[order(prior.USGS$Date),]     #reorders prior data by date
after.USGS <- after.USGS[order(after.USGS$Date),]     #reorders after data by date

# Plotting flow --------------------------------------------------------------------------------------------------------

par(mfrow = c(1,2))     #makes two graphs appear on same output
plot(prior.USGS$Date, prior.USGS$Flow, type = 'l', ylim = c(0, 20000), ylab = 'Flow (cfs)', xlab = 'Year', main = 'Dry Run Avg. Daily Flow Prior to Dam (1961-1981)')
plot(after.USGS$Date, after.USGS$Flow, type = 'l', ylim = c(0, 20000), ylab = 'Flow (cfs)', xlab = 'Year', main = 'Dry Run Avg. Daily Flow After Dam (1983-2003)')

# Calculating maximum daily flow each year --------------------------------------------------------------------------------------------
count <- 1
prior.start.year <- 1961
prior.peaks <- vector(mode = 'numeric', length=21)
prior.years <- vector(mode = 'numeric', length=21)
while (count <= 21) {
  prior.start.date <- paste0(prior.start.year, '-01-01')
  prior.end.date <- paste0(prior.start.year, '-12-31')
  prior.USGS.peaks <- readNWISdv(siteNo, pCode, prior.start.date, prior.end.date)
  names(prior.USGS.peaks) <- c('Agency', 'Site', 'Date', 'Flow', 'QC')
  prior.peaks[count] = max(prior.USGS.peaks$Flow)
  prior.years[count] = prior.start.year
  count = count + 1
  prior.start.year <- prior.start.year + 1
}

count <- 1
after.start.year <- 1983
after.peaks <- vector(mode = 'numeric', length=21)
after.years <- vector(mode = 'numeric', length=21)
while (count <= 21) {
  after.start.date <- paste0(after.start.year, '-01-01')
  after.end.date <- paste0(after.start.year, '-12-31')
  after.USGS.peaks <- readNWISdv(siteNo, pCode, after.start.date, after.end.date)
  names(after.USGS.peaks) <- c('Agency', 'Site', 'Date', 'Flow', 'QC')
  after.peaks[count] = max(after.USGS.peaks$Flow)
  after.years[count] = after.start.year
  count = count + 1
  after.start.year <- after.start.year + 1
}


# Creating comparative boxplots of daily flow ------------------------------------------------------------------------
par(mfrow = c(1,2))     #makes two graphs appear on same output
boxplot(prior.peaks, ylim = c(0, 20000), ylab = 'Maximum Average Daily Flow per Year (cfs)', main = 'Dry River Run Prior to Dam')
boxplot(after.peaks, ylim = c(0, 20000), ylab = 'Maximum Average Daily Flow per Year (cfs)', main = 'Dry River Run After Dam')
# Retrieving 2008+ timescale data from 15min and daily scales

new.start.date <- '2008-01-01'
new.end.date <- '2008-02-29'

new.dryrun.daily <- readNWISdv(siteNo, pCode, new.start.date, new.end.date)
new.dryrun.15min <- readNWISuv(siteNo, pCode, new.start.date, new.end.date)

names(new.dryrun.daily) <- c('Agency', 'Site', 'Date', 'Flow', 'QC')
names(new.dryrun.15min) <- c('Agency', 'Site', 'Date', 'Flow', 'QC')

new.dryrun.daily <- new.dryrun.daily[order(new.dryrun.daily$Date),]
new.dryrun.15min <- new.dryrun.15min[order(new.dryrun.15min$Date),]

par(mfrow = c(1,1))
plot(new.dryrun.daily$Date, new.dryrun.daily$Flow, col='red', type = 'l', ylim = c(0, 3000), ylab = 'Flow (cfs)', xlab = 'Date', main = 'Dry Run Flow (Early 2008)', cex=1.5)
par(new=T)
plot(new.dryrun.15min$Date, new.dryrun.15min$Flow, col='blue', type = 'l', ylim = c(0, 3000), ylab = '', xlab='', axes=F)
legend("topright", lwd = 2, col = c('red', 'blue'), legend = c("Daily Flow", "15-Minute Flow"), cex=1.25)

daily.mean <- mean(new.dryrun.daily$Flow)
daily.mean
min15.mean <- mean(new.dryrun.15min$Flow)
min15.mean

par(mfrow = c(1,2))
boxplot(new.dryrun.daily$Flow, ylim = c(0, 1500), ylab = 'Flow (cfs)', main = "Dry Run Daily Flow, Jan-Feb 2008")
boxplot(new.dryrun.15min$Flow, ylim = c(0, 1500), ylab = 'Flow (cfs)', main = "Dry Run 15-Minute Flow, Jan-Feb 2008")
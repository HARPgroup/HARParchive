# Test datasets CSV
library("sqldf")
library("dataRetrieval")
library("lubridate")

#Set the id for the USGS gage from which we will pull data and use to summarize
#our meteorology data. Currently, we have data set up for:
#Culpepper 01667500, Strasburg 01634000, Ruckersville 01665500
gageid = '01665500' 
# We redefine the gage id as a "hydrocode", which is how it is stored in the VA
#Hydro (DEQ) database
hydrocode = paste0('usgs_ws_', gageid)

#Pull data from VA Hydro, grabbing some precip data for the gage watershed
prism_data <- read.csv(paste0("http://deq1.bse.vt.edu:81/files/met/", hydrocode, "-prism-all.csv"))

#Use lubridate to add on some additional information about the date. These dates
#come in as posix and thus have timestamps, which make them a little trickier to
#work with. So, we can disaggregate them for easier joins and manipulation
prism_data[,c('yr', 'mo', 'da', 'wk')] <- cbind(year(as.Date(prism_data$obs_date)),
                                                month(as.Date(prism_data$obs_date)),
                                                day(as.Date(prism_data$obs_date)),
                                                week(as.Date(prism_data$obs_date)))

#Repeat the download and date disaggregation for daymet, a different precip
#source:
daymet_data <- read.csv(paste0("http://deq1.bse.vt.edu:81/files/met/", hydrocode, "-daymet-all.csv"))
daymet_data[,c('yr', 'mo', 'da', 'wk')] <- cbind(year(as.Date(daymet_data$obs_date)),
                                                 month(as.Date(daymet_data$obs_date)),
                                                 day(as.Date(daymet_data$obs_date)),
                                                 week(as.Date(daymet_data$obs_date)))
#Repeat the download and date disaggregation for NLDAS2, an HOURLY precip source
nldas2_data <- read.csv(paste0("http://deq1.bse.vt.edu:81/files/met/", hydrocode, "-nldas2-all.csv"))
nldas2_data[,c('yr', 'mo', 'da', 'wk')] <- cbind(year(as.Date(nldas2_data$obs_date)),
                                                 month(as.Date(nldas2_data$obs_date)),
                                                 day(as.Date(nldas2_data$obs_date)),
                                                 week(as.Date(nldas2_data$obs_date)))
#NLDAS is an hourly precip data set. We should aggregate it to be daily sums to
#match the data in PRISM and daymet for the purpose of comparison. We can do
#this in SQL by grouping by the the date columns we created for month, day, and
#year
nldas2_data <- sqldf(
  "select featureid, min(obs_date) as obs_date, yr, mo, da, 
     sum(precip_mm) as precip_mm, sum(precip_in) as precip_in
   from nldas2_data 
   group by yr, mo, da
   order by yr, mo, da
  "
)


# Get USGS gage data from NWIS, the USGS REST services. We pull this data using
# the dataRetrieval R package which has extensive documentation on the options
# therewithin. See https://waterdata.usgs.gov/blog/dataretrieval/. Below, we
# a cached version of the function defined in our config.R file:
gage_info <- memo_readNWISsite(gageid)
#Extract the drainage area of the gage
da <- gage_info$drain_area_va
#Get daily, mean streamflow (pcode 000060, see dataRetrieval::parameterCdFile)
#from the gage specified in gageid
usgs_data <- memo_readNWISdv(gageid,'00060')
#Extract date information from the gage using lubridate as above
usgs_data[,c('yr', 'mo', 'da')] <- cbind(year(as.Date(usgs_data$Date)),
                                         month(as.Date(usgs_data$Date)),
                                         day(as.Date(usgs_data$Date)))

#Join all data together. In other words, extract the relevant fields from the
#three data sources and combine them based on the day, month, and year. We could
#do this by joining by the dates, but you may notice that the USGS daily data
#may not come in as POSIX and doesn't have a time associated with it. We could
#do this via tidyverse verbs, but our team typically prefers SQL as it tends to
#change a lot less frequently i.e. will require less long-term maintenance
comp_data <- sqldf(
  "select a.obs_date, a.precip_in as prism_p_in, 
  a.yr, a.mo, a.da, a.wk,
  b.precip_in as daymet_p_in, d.precip_in as nldas2_p_in,
  c.X_00060_00003 as usgs_cfs
  from prism_data as a
  left outer join daymet_data as b 
  on (
    a.yr = b.yr
    and a.mo = b.mo
    and a.da = b.da
  )
  left outer join usgs_data as c 
  on (
    a.yr = c.yr
    and a.mo = c.mo
    and a.da = c.da
  )
  left outer join nldas2_data as d 
  on (
    a.yr = d.yr
    and a.mo = d.mo
    and a.da = d.da
  )
  order by a.yr, a.mo, a.da
  "
)

# Index the dataset, giving a unique number to each row. Please note that these
# are already available via rownames(comp_data) but here we add it as a column
# to call them in SQLDF. You can see how SQL changes our R workflows. As the
# project moves forward, feel free to branch out and try other workflows but
# please note we will often encourage the use of SQL as we assist in tasks. But
# all code is acceptable if it is documented and works!
comp_data$dataset_day <- index(comp_data) 

#Add a column for the date since this is now daily data
comp_data$date <- as.Date(comp_data$obs_date)

#Here, we add in columns to represent tomorrow's flow and the change in flow
#from today to yesterday and today and tomorrow. This give an indication of how
#flow is changing. We can do so using R's indexing, simply calling the next rows
#down where needed
# comp_data$nextday_usgs_cfs <- c(comp_data$usgs_cfs[2:nrow(comp_data)],NA)
# comp_data$nextday_d_cfs <- comp_data$nextday_usgs_cfs - comp_data$usgs_cfs
# comp_data$today_d_cfs <- comp_data$usgs_cfs - c(NA,comp_data$usgs_cfs[1:(nrow(comp_data) - 1)])

#We could alternatively get this via an SQL query that runs just about as
#quickly! Note the use of the Lag() function and its inputs in the subquery used
#to pull in yesterdays and todays data
comp_data <- sqldf(
  "select b.*,
  b.nextday_usgs_cfs - b.usgs_cfs as nextday_d_cfs,
  b.usgs_cfs - b.yesterday_usgs_cfs as today_d_cfs
  FROM (
    select 
    a.*,
    Lag(a.usgs_cfs,1) OVER(ORDER BY a.dataset_day) as yesterday_usgs_cfs,
    Lag(a.usgs_cfs,-1) OVER(ORDER BY a.dataset_day) as nextday_usgs_cfs
    from comp_data as a
    order by a.dataset_day
  ) as b
  "
)

# Add a cfs Precip column using the following conversions for EACH of the precip
# data sources:
# acre-feet/day = 3.07 MGD
# cfs = 1.57 MGD
# DAsqmi * 640 ac/sqmi * p_in_per_day / 12.0 = Precip ac-ft/day
# P acft/day / 3.07 = P mgd
# P mgd * 1.572 = P cfs
# 1.572 * (DAsqmi * 640.0 * p_in / 12.0) / 3.07 
comp_data$prism_p_cfs <- 1.572 * (da * 640.0 * comp_data$prism_p_in / 12.0) / 3.07 
comp_data$daymet_p_cfs <- 1.572 * (da * 640.0 * comp_data$daymet_p_in / 12.0) / 3.07 
comp_data$nldas2_p_cfs <- 1.572 * (da * 640.0 * comp_data$nldas2_p_in / 12.0) / 3.07 
#Can we learn anything based on what stormSep gives us? e.g. the number of
#storms that occur in a given week, month, day, etc.?
#First, create a dataset where USGS flow is not NA
source("C:/Users/gcw73279.COV/Desktop/gitBackups/OWS/HARParchive/HARP-2024-2025/stormSep_USGS.R")

stormCompData <- comp_data[!is.na(comp_data$usgs_cfs),]
stormOut <- stormSeparate(as.Date(stormCompData$obs_date),
                          inflow = stormCompData$usgs_cfs,
                          plt = FALSE,path = paste0(getwd(),"/StormPlots/"),
                          allMinimaStorms = TRUE,
                          baselineFlowOption = "Month")

#For each storm, sum precip leading up to it (past 3, 5, 7, and 14 days?). Go
#through each storm and find the sum of precip of each dataset. Also find past
#3, 5, 7, and 14 days from storm endpoint including its full duration? What
#about stream length? What about DA or other NHDPLus factors? Can we get at
#travel time and or lag? Land use?
stormStats <- stormOut$Stats
#Throw out any storms that have inconsistent durations compared to start and end
#date. This can occur when a gage goes offline as StormSep doesn't check for
#this
stormEvents <- stormOut$Storms[(as.Date(stormStats$endDate) - as.Date(stormStats$startDate) + 1) == stormStats$durAll]
stormStats <- stormStats[(as.Date(stormStats$endDate) - as.Date(stormStats$startDate) + 1) == stormStats$durAll,]

#Get rolling sums of precip over 3 day periods:
comp_data$roll3PRISM_cfs <- rollapply(comp_data$prism_p_cfs,3,sum,fill = NA,align = "right")

#A function that gets precip from the rollingDur period but will include the
#full stormDuration
getRollPrecip <- function(comp_data,stormDuration,
                          rollingDur,endDate,
                          precipColName = "prism_p_cfs"){
  #Convert input date to date, just in case
  sDate <- as.Date(endDate)
  #Get the index in comp_date df where the endDate occurs
  dateIndex <- grep(endDate,comp_data$date)
  #Get all values from the precipColName in comp_data for the target duration
  #adjusted for the storm duration. So, if there is a five-day storm,
  #stormDuration is 5. If we are interested in rolling 7-day precip prior to and
  #throughout the storm, we'd want rollingDur = 7. So, we need dateIndex - 7 - 5
  #The precip data is in:
  precipData <- comp_data[,precipColName]
  precipStorm <- precipData[(dateIndex - rollingDur - stormDuration + 2) : dateIndex]
  #Return total precip. Adjust for NAs that may occur due to indexing numbers
  #prior to start of comp_data
  totalPrecip <- sum(precipStorm,na.rm = TRUE)
  return(totalPrecip)
}

#Add to stormStats the sum of precipitation from the 3-, 7-, and 14-day periods
#leading up to the storm and including the full storm duration. Convert to MG
cfsToMGD <- 86400 * 12*12*12/231/1000000
#Only precip during the storm iteself
stormStats$roll1DayWStormPRISM_MG <- mapply(SIMPLIFY = TRUE, USE.NAMES = FALSE,
                                            FUN = getRollPrecip, stormDuration = stormStats$durAll,
                                            endDate = stormStats$endDate,
                                            MoreArgs = list(comp_data = comp_data,rollingDur = 1,
                                                            precipColName = "prism_p_cfs")
)
stormStats$roll1DayWStormPRISM_MG <- stormStats$roll1DayWStormPRISM_MG * cfsToMGD
#Includes 1-day prior to the storm:
stormStats$roll2DayWStormPRISM_MG <- mapply(SIMPLIFY = TRUE, USE.NAMES = FALSE,
                                            FUN = getRollPrecip, stormDuration = stormStats$durAll,
                                            endDate = stormStats$endDate,
                                            MoreArgs = list(comp_data = comp_data,rollingDur = 2,
                                                            precipColName = "prism_p_cfs")
)
stormStats$roll2DayWStormPRISM_MG <- stormStats$roll2DayWStormPRISM_MG * cfsToMGD

#Plots of total storm volume to rolling 3-, 7-, 14-day periods with storm duration
plot(stormStats$roll1DayWStormPRISM_MG,stormStats$volumeTotalMG,
     log = "xy")
#Plots of total storm volume above base Q to rolling 3-, 7-, 14-day periods with
#storm duration
plot(stormStats$roll1DayWStormPRISM_MG,stormStats$volumeAboveBaseQMG)

#Linear models with storm duration
withStorm1 <- lm(volumeTotalMG ~ roll1DayWStormPRISM_MG,
                 data = stormStats)

#Linear models with stormDuration above base Q
withStorm1_aboveBaseQ <- lm(volumeAboveBaseQMG ~ roll1DayWStormPRISM_MG,
                            data = stormStats)
withStorm2_aboveBaseQ <- lm(volumeAboveBaseQMG ~ roll2DayWStormPRISM_MG,
                            data = stormStats)

summary(withStorm1)
summary(withStorm1_aboveBaseQ)
summary(withStorm2_aboveBaseQ)

#The following showed a strong correlation. What if we look at power regression
#to smooth over small and large differences e.g. take power away from larger
#storms? This also removes storms in which no precip was found in the target
#duration. This may skew results and should be reviewed carefully due to the
#bias of picking only data where this method "works"
withStorm1_aboveBaseQ_log <- lm(log(volumeAboveBaseQMG) ~ log(roll1DayWStormPRISM_MG),
                                data = stormStats[stormStats$roll1DayWStormPRISM_MG > 0,])
#The log power regression in this case actually performs worse:
summary(withStorm1_aboveBaseQ_log)

#However, are we weighted by large storm volumes? It appears so, with one value
#even falling outside of Cooks Distance
plot(withStorm1_aboveBaseQ)

#Does relationsip improve at all when we look only at specific months. Add month
#based on the startDate of the storm
stormStats$beginMonth <- as.numeric(format(as.Date(stormStats$startDate),"%m"))

#What are the monthly relationships?
monthEventOut1Day <- mon_lm(stormStats, y_var = "volumeAboveBaseQMG",
                            x_var = "roll1DayWStormPRISM_MG",
                            mo_var = "beginMonth", "Storm Event Vol")
monthEventOut2Day <- mon_lm(stormStats, y_var = "volumeAboveBaseQMG",
                            x_var = "roll2DayWStormPRISM_MG",
                            mo_var = "beginMonth", "Storm Event Vol")

#Data counts?
table(stormStats$beginMonth)
sum(stormStats$beginMonth == 10)
testReg <- lm(log(volumeAboveBaseQMG) ~ log(roll1DayWStormPRISM_MG),
              data = stormStats[stormStats$beginMonth==10,])
summary(testReg)

#There may be some heavy influence from high storm events. May want to consider
#power regressions or non-linear exponential regressions to reduce their
#influence or evens a Cooks Distance analysis to remove errant data points

#For each storm in stormOut$Storms, use the storm start and end-date to find the
#7-day period leading up to the storm and the 7-day period following the storm.
#Show precip during this period and throughout storm duration. Then, highlight
#the separated storm hydrograph
for(i in 1:length(stormEvents)){
  print(i)
  #Get the current storm flow data which will be used for highlighting that
  #hydrograph
  stormi <- stormEvents[[1]]
  #Only need non-NA values since stormSep will output full timeseries but leave
  #values as NA if they are not included in storm
  stormi <- stormi[!is.na(stormi$flow),]
  
  #Get the start and end date of the storm from stormStats:
  stormStart <- as.Date(stormStats$startDate[i])
  stormEnd <- as.Date(stormStats$endDate[i])
  #Adjust these values to increase the window:
  plotStart <- stormStart - 7
  plotEnd <- stormEnd + 7
  
  #Get the stream flow and baseflow from stormSep
  flowData <- stormOut$flowData[stormOut$flowData$timestamp >= plotStart & 
                                  stormOut$flowData$timestamp <= plotEnd,]
  #Join in the precip "flow" from comp_data:
  flowDataAll <- sqldf("SELECT flowData.*,
                          comp.prism_p_in,
                          comp.daymet_p_in,
                          comp.nldas2_p_in,
                          comp.prism_p_cfs,
                          comp.daymet_p_cfs,
                          comp.nldas2_p_cfs
                        FROM flowData
                        LEFT JOIN comp_data as comp
                          ON flowData.timeStamp = comp.Date")
  pathOut <- paste0("StormPlotsNew/stormPlot",i,".PNG")
  plotStorm(pathOut,stormi,flowDataAll)
}

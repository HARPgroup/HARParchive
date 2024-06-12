# Test datasets CSV
library("sqldf")
library("dataRetrieval")
#Set the id for the USGS gage from which we will pull data and use to summarize
#our meteorology data
gageid = '01634000'
#We redefine the gage id as a "hydrocode", which is how it is stored in the VA
#Hydro (DEQ) database
hydrocode = paste0('usgs_ws_', gageid)
#Pull data from VA Hydro, grabbing some precip data for the gage watershed
prism_data <- read.csv("http://deq1.bse.vt.edu:81/files/met/usgs_ws_01634000-prism-2022-2023.csv")
daymet_data <- read.csv("http://deq1.bse.vt.edu:81/files/met/usgs_ws_01634000-daymet-2022-2023.csv")

#Use lubridate to add on some additional information about the date. These dates
#come in as posix and thus have timestamps, which make them a little trickier to
#work with. So, we can disaggregate them for easier joins
prism_data[,c('yr', 'mo', 'da', 'wk')] <- cbind(year(as.Date(prism_data$obs_date)),
                                                month(as.Date(prism_data$obs_date)),
                                                day(as.Date(prism_data$obs_date)),
                                                week(as.Date(prism_data$obs_date)) )
#Repeat for daymet:
daymet_data[,c('yr', 'mo', 'da', 'wk')] <- cbind(year(as.Date(daymet_data$obs_date)),
                                                 month(as.Date(daymet_data$obs_date)),
                                                 day(as.Date(daymet_data$obs_date)),
                                                 week(as.Date(daymet_data$obs_date)) )
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
  b.precip_in as daymet_p_in, c.X_00060_00003 as usgs_cfs
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
  "
)

# Index the dataset, giving a unique number to each row. Please note that these
# are already available via rownames(comp_data) but here we add it as a column
# to call them in SQLDF. You can see how SQL changes our R workflows. As the
# project moves forward, feel free to branch out and try other workflows but
# please note we will often encourage the use of SQL as we assist in tasks. But
# all code is acceptable if it is documented and works!
comp_data$dataset_day <- index(comp_data) 
#Here, we add in columns to represent tomorrow's flow and the change in flow
#from today to yesterday and today and tomorrow. This give an indication of how
#flow is changing. We can do so using R's indexing, simply calling the next rows
#down where needed
comp_data$nextday_usgs_cfs <- c(comp_data$usgs_cfs[2:nrow(comp_data)],NA)
comp_data$nextday_d_cfs <- comp_data$nextday_usgs_cfs - comp_data$usgs_cfs
comp_data$today_d_cfs <- comp_data$usgs_cfs - c(NA,comp_data$usgs_cfs[1:(nrow(comp_data) - 1)])

#We could alternatively get this via an SQL query that runs just about as
#quickly! Note the use of the Lag() function and its inputs in the subquery used
#to pull in yesterdays and todays data
comp_data2 <- sqldf(
  "select b.*,
  b.nextday_usgs_cfs - b.usgs_cfs as nextday_d_cfs,
  b.usgs_cfs - b.yesterday_usgs_cfs as today_d_cfs
  FROM (
    select 
    a.obs_date, a.prism_p_in,a.yr,a.mo,a.da, a.wk,
    a.daymet_p_in,a.usgs_cfs,a.dataset_day,
    Lag(a.usgs_cfs,1) OVER(ORDER BY a.dataset_day) as yesterday_usgs_cfs,
    Lag(a.usgs_cfs,-1) OVER(ORDER BY a.dataset_day) as nextday_usgs_cfs
    from comp_data as a
    order by a.dataset_day
  ) as b
  "
)

# Add a cfs Precip column via the following conversions:
# acre-feet/day = 3.07 MGD
# cfs = 1.57 MGD
# DAsqmi * 640 ac/sqmi * p_in_per_day / 12.0 = Precip ac-ft/day
# P acft/day / 3.07 = P mgd
# P mgd * 1.572 = P cfs
# 1.572 * (DAsqmi * 640.0 * p_in / 12.0) / 3.07 
comp_data$prism_p_cfs <- 1.572 * (da * 640.0 * comp_data$prism_p_in / 12.0) / 3.07 
comp_data$daymet_p_cfs <- 1.572 * (da * 640.0 * comp_data$daymet_p_in / 12.0) / 3.07 

#We have a lot of daily data in comp_data, but we suspect to see more noteable
#trends in weekly data compared to daily data given travel time and
#abstraction/storage considerations. So, we can summarize our data by grouping
#it via the week variable we added in earlier using lubridate::week.
#(again, you may have done this previously via dplyr::group_by and summarize)
week_data <- sqldf(
  "select min(obs_date) as week_begin, yr, wk, min(dataset_day) as dataset_day_begin,
     avg(daymet_p_in) as daymet_p_in, avg(daymet_p_cfs) as daymet_p_cfs,
     avg(prism_p_in) as prism_p_in, avg(prism_p_cfs) as prism_p_cfs,
     avg(usgs_cfs) as usgs_cfs, avg(today_d_cfs) as today_d_cfs, 
     avg(nextday_d_cfs) as nextday_d_cfs
   from comp_data
   group by yr, wk
   order by yr, wk
  "
)

#Create a simple linear regression of flow and precip with flow as the dependent
#variable to test if there is a clear and easy relationship between flow and
#precip. Using the formula notation "~" we can specify the regression as
#dependent ~ independent variable as long as the data option is specified in the
#function
mod_prism <- lm(usgs_cfs ~ prism_p_cfs, data=comp_data)
#Look at the regression paramters. Identify how well the regression performed
#(spoiler alert, it performed poorly, which makes sense given time of travel and
#abstraction/storage. We wouldn't want it to be easy!)
summary(mod_prism)

#Repeated for daymet, running the linear regression between the USGS data and
#daymet
mod_daymet <- lm(usgs_cfs ~ daymet_p_cfs, data=comp_data)
#Evaluate the performance of the regression
summary(mod_daymet)


# Weekly cfs vs P. Above, we created linear regressions for flow and
# precipitations. Now we repeat this process to see if weekly flow/precip is
# correlated more strongly. We'd suspect this to be a bit more reliably related,
# but how much stronger?
mod_week_prism <- lm(usgs_cfs ~ prism_p_cfs, data=week_data)
mod_week_daymet <- lm(usgs_cfs ~ daymet_p_cfs, data=week_data)
#Looking at the results, we see some correlation but it's still not very strong
summary(mod_week_prism)
summary(mod_week_daymet)


# Let's repeat the regression anlysis now for ONLY January, which tends to be a
# wetter month. We start with the daily data. For PRISM:
mod_prism_jan <- lm(usgs_cfs ~ prism_p_cfs, data=comp_data[comp_data$mo == 1,])
summary(mod_prism_jan)
# daymet
mod_daymet_jan <- lm(usgs_cfs ~ daymet_p_cfs, data=comp_data[comp_data$mo == 1,])
summary(mod_daymet_jan)
#Plot the daily data for january and see how the flow relates to the summarized
#precip data. This is equivalent to comp_data$usgs_cfs and comp_data$daymet_p_cfs
plot(mod_daymet_jan$model$usgs_cfs ~ mod_daymet_jan$model$daymet_p_cfs)


# What if we repeat the regression, but now we compare TOMORROW's flow with
# TODAYs precipitation? Let's stick with January for now
mod_prism_jan_nd <- lm(nextday_usgs_cfs ~ prism_p_cfs, data=comp_data[comp_data$mo == 1,])
#Moderately improved fit:
summary(mod_prism_jan_nd)

#Repeat regression, but now compare the CHANGE in flow between today and
#tomorrow with TODAYs precipitation. Again, we stick with just January for now.
mod_prism_jan_ndd <- lm(nextday_d_cfs ~ prism_p_cfs, data=comp_data[comp_data$mo == 1,])
summary(mod_prism_jan_ndd)
plot(mod_prism_jan_ndd$model$nextday_d_cfs ~ mod_prism_jan_ndd$model$prism_p_cfs)

# Repeat the above analysis for daymet e.g. the CHANGE in flow b/t today and
# tomorrow and TODAY's daymet precipitation
mod_daymet_jan_ndd <- lm(nextday_d_cfs ~ daymet_p_cfs, data=comp_data[which(comp_data$mo == 1),])
summary(mod_daymet_jan_ndd)
plot(mod_daymet_jan_ndd$model$nextday_d_cfs ~ mod_daymet_jan_ndd$model$daymet_p_cfs)

# Days with zero rainfall may be skewing our results based on the plots above.
# So, repeat the analysis now only comparing against change in flows on the
# same day as non-zero rain
# *** DAYMET
mod_daymet_jan_nz_ndd <- lm(nextday_d_cfs ~ daymet_p_cfs, 
                            data=comp_data[((comp_data$mo == 1) & (comp_data$daymet_p_cfs > 0)),])
summary(mod_daymet_jan_nz_ndd)
plot(mod_daymet_jan_nz_ndd$model$nextday_d_cfs ~ mod_daymet_jan_nz_ndd$model$daymet_p_cfs)
# *** PRISM
mod_prism_jan_nz_ndd <- lm(nextday_d_cfs ~ prism_p_cfs, data=comp_data[((comp_data$mo == 1) & (comp_data$prism_p_cfs > 0)),])
summary(mod_prism_jan_nz_ndd)
plot(mod_prism_jan_nz_ndd$model$nextday_d_cfs ~ mod_prism_jan_nz_ndd$model$prism_p_cfs)

# Now, we look at the change in flow on the day after it rains to the summarized
# precipitation. So, repeate the same ananlysis but now compare to nextday flow
# difference
# *** DAYMET
mod_daymet_jan_nz_ndd <- lm(nextday_d_cfs ~ daymet_p_cfs, 
                            data=comp_data[((comp_data$mo == 1) & (comp_data$daymet_p_cfs > 0)),])
summary(mod_daymet_jan_nz_ndd)
plot(mod_daymet_jan_nz_ndd$model$nextday_d_cfs ~ mod_daymet_jan_nz_ndd$model$daymet_p_cfs)
# *** PRISM
mod_prism_jan_nz_ndd <- lm(nextday_d_cfs ~ prism_p_cfs, 
                           data=comp_data[((comp_data$mo == 1) & (comp_data$prism_p_cfs > 0)),])
summary(mod_prism_jan_nz_ndd)
plot(mod_prism_jan_nz_ndd$model$nextday_d_cfs ~ mod_prism_jan_nz_ndd$model$prism_p_cfs)



sbset <- comp_data[comp_data$obs_date <= as.Date("2013-12-31") & comp_data$obs_date >= "2003-10-01",]
plot(as.Date(sbset$obs_date),sbset$daymet_p_cfs,type = "l",col = "darkred")
lines(as.Date(sbset$obs_date),sbset$prism_p_cfs,col= "darkblue")
lines(as.Date(sbset$obs_date),sbset$usgs_cfs,col= "black",lwd = 2)

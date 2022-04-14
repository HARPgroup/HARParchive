#### This script contains all of the functions we have used for generating land segment data
## Last Updated 4/14/2022
## HARP Group


# Function that creates a dataframe of summary statistics for a given landsegment
# Requires an hourly temperature, precipitation, and potential evapotranspiration dfs
# Containing columns year, date, and respective metric (temp, precip, pet)
# Outputs a summary statistic dataframe
get_lseg_summary_stats <- function(dfTMP, dfPRC, dfPET){
  # create df of daily values
  dailyPrecip <- sqldf("SELECT year, date, sum(precip) daily_precip
                           FROM dfPRC
                           GROUP BY date")
  dailyTemp <- sqldf("SELECT year, date, avg(temp) daily_temp
                       FROM dfTMP
                       GROUP BY date")
  dailyPET <- sqldf("SELECT year, date, sum(pet) daily_pet
                       FROM dfPET
                       GROUP BY date")
  
  # calculate min and max yearly temperature
  minTemp <- sqldf("SELECT date, min(temp) min_temp 
                       FROM dfTMP
                       GROUP BY year")
  maxTemp <- sqldf("SELECT year, date, max(temp) max_temp
                       FROM dfTMP
                       GROUP BY year")
  # calculate min and max and total yearly precipitation
  minPrecip <- sqldf("SELECT date, min(precip) min_precip
                       FROM dfPRC
                       GROUP BY year")
  maxPrecip <- sqldf("SELECT date, max(precip) max_precip
                       FROM dfPRC
                       GROUP BY year")
  annualPrecip <- sqldf("SELECT date, sum(precip) annual_precip
                       FROM dfPRC
                       GROUP BY year")
  # calculate number of consecutive 0 days of precipitation
  repeats <- rle(dfPRC$precip)
  repeats <- data.frame(lengths = repeats[1],
                        values = repeats[2]) #turn repeats rle into a dataframe for sql
  dfPRC$consec <- rep(repeats$lengths, repeats$lengths) #adding repeats lengths to precip df
  maxConsec <- sqldf("SELECT max(consec) max_consec_hours, max(consec)/24.00 max_consec_days
                         FROM dfPRC
                         WHERE precip = 0
                         GROUP BY year")
  # calculate number of consecutive days evapotranspiration > precipitation
  dailyPET$PETvsPrecip <- dailyPET$daily_pet - dailyPrecip$daily_precip
  dailyPET <- sqldf("SELECT year, date, daily_pet,
                    CASE
                    WHEN PETvsPRECIP > 0 then 1
                    ELSE 0
                    END PETvsPrecip
                    FROM dailyPET")
  repeats3 <- rle(dailyPET$PETvsPrecip)
  repeats3 <- data.frame(lengths = repeats3[1],
                         values = repeats3[2]) #turn repeats rle into a dataframe for sql
  dailyPET$consec <- rep(repeats3$lengths, repeats3$lengths) #adding repeats lengths to pet df
  maxConsec2 <- sqldf("SELECT max(consec) max_consec_days
                         FROM dailyPET
                         WHERE PETvsPrecip = 1
                         GROUP BY year")
  # calculate number of no precip days and precip days
  noPrecipDays <- sqldf("SELECT count(daily_precip) no_precip_days
                            FROM dailyPrecip
                            WHERE daily_precip = 0
                            GROUP BY year")
  precipDays <- sqldf("SELECT count(daily_precip) no_precip_days
                            FROM dailyPrecip
                            WHERE daily_precip > 0
                            GROUP BY year")
  # max consec take 2 (this calculates the maximum consecutive calendar days w/o precip rather than max 24 hour periods w/o precip)
  # repeats2 <- rle(dailyPrecip$daily_precip)
  # repeats2 <- data.frame(lengths = repeats2[1],
  #                        values = repeats2[2])
  # dailyPrecip$consec <- rep(repeats2$lengths, repeats2$lengths)
  # maxConsec2 <- sqldf("SELECT max(consec) max_consec_days
  #                        FROM dailyPrecip
  #                        WHERE daily_precip = 0
  #                        GROUP BY year")
  
  # IHA lowflow metrics applied to temperature
  zooTMP <- zoo(x=dailyTemp$daily_temp, order.by=dailyTemp$date)
  group2TMP <- group2(zooTMP, year=c('calendar'),mimic.tnc = T)
  # IHA lowflow metrics applied to precipitation
  zooPRC <- zoo(x=dailyPrecip$daily_precip, order.by=dailyPrecip$date)
  group2PRC <-  group2(zooPRC, year=c('calendar'),mimic.tnc = T)
  
  # create a summary data frame
  summaryStats <- cbind(maxTemp, minTemp, maxPrecip, minPrecip, annualPrecip$annual_precip, maxConsec, maxConsec2, noPrecipDays, precipDays,
                        group2TMP$`7 Day Min`, group2PRC$`7 Day Min`, group2TMP$`30 Day Min`, group2PRC$`30 Day Min`,
                        group2PRC$`90 Day Min`, group2PRC$`90 Day Max`)
  colnames(summaryStats) <- c("year", "max_temp_date", "max_temp", "min_temp_date", "min_temp", 
                              "max_precip_date", "max_precip", "min_precip_date", "min_precip", "precip_annual",
                              "max_consec_no_precip_hours", "no_precip_max", "water_deficit_max", "no_precip_days", 
                              "precip_days", "7_day_min_temp", "7_day_min_precip", "30_day_min_temp",
                              "30_day_min_precip", "90_day_min_precip", "90_day_max_precip")
  return(summaryStats)
}


# Function that calculates Hamon method of potential evapotranspiration
# Requires an hourly temperature and radiation dfs
# Containing columns year, month, day, hour, date, and respective metric (temp, rad)
# Outputs a dataframe of ET values with columns year, month, day, hour, PET
generate_lseg_het <- function(dfTMP, dfRAD){
  # create dataframe with number of daylight hours for each day
  dfDLH <- sqldf("SELECT year, month, day, count(RAD) daylight_hrs
                    FROM dfRAD
                    WHERE rad > 0 
                    GROUP BY date")
  
  # hours with no radiation get daylight values of 0 while hours with radiation get values of 1
  # these will be used later to make PET 0 if there is no radiation
  hourlyPET <- data.frame(date = dfRAD$date,
                          hour = dfRAD$hour,
                          rad = dfRAD$rad)
  hourlyPET <- sqldf("SELECT date, hour, rad,
                       CASE
                       when rad == 0 then 0
                       else 1
                       end daylight
                       FROM hourlyPET")
  
  # Hamon Equation
  #N = daytime length
  N <- (rep(dfDLH$daylight_hrs, 24))/12
  # e = saturation vapor pressur
  e = 6.108*exp((17.27*(dfTMP$temp)/((dfTMP$temp)+273.3)))
  # k = 1 = proportioanlity coefficient (always equal to 1)
  k <- 1
  # t = average daily temperature 
  t <- dfTMP$temp
  # calculate PET
  ET_H <- k*0.165*216.7*N*(e/(t+273.3))
  
  # make PET values 0 if no solar radiation for given hour
  hourlyPET$PET <- ET_H*as.numeric(hourlyPET$daylight)
  # currently in mm/day, convert to in/hr
  hourlyPET$PET <- hourlyPET$PET*0.0393701/24
  
  # add Hamon ET data to table with same convention as other metrics
  dfHET <- data.frame(year = dfTMP$year, 
                      month = dfTMP$month, 
                      day = dfTMP$day,
                      hour = dfTMP$hour,
                      PET = hourlyPET$PET)
}

# Function that calculates Hargreaves-Samani method of potential evapotranspiration
# Requires an hourly temperature and radiation dfs
# Containing columns year, month, day, hour, date, and respective metric (temp, rad)
# Outputs a dataframe of ET values with columns year, month, day, hour, PET
generate_lseg_hset <- function(dfTMP, dfRAD){
  # calculate daily maximum and minimum temperature values
  dailyMetrics <- sqldf("SELECT date, max(temp) max_temp, min(temp) min_temp
                       FROM dfTMP
                       GROUP BY date")
  # repeat daily maximum and minimum 24 times to match up with hourly values
  dailyMetrics$freq <- 24
  hourlyMetrics <- data.frame(date = dfTMP$date,
                              max_temp = rep(dailyMetrics$max_temp, dailyMetrics$freq),
                              min_temp = rep(dailyMetrics$min_temp, dailyMetrics$freq))
  # calculate PET (mm/day)
  k <- 0.19 # empirical coefficient
  hsPET <- (0.0135*(k)*(dfTMP$temp+17.8)*((hourlyMetrics$max_temp - hourlyMetrics$min_temp)^0.5)*0.408*(dfRAD$rad))
  # convert PET to inch/hour
  hsPET <- hsPET*0.0393701/24
  # create PET dataframe
  dfHSET <- data.frame(year = dfTMP$year,
                       month = dfTMP$month,
                       day = dfTMP$day,
                       hour = dfTMP$hour,
                       PET = hsPET)
}


# This function determines if a given time is dark or light
# A hourly radiation dataframe containing year, month, day, hour, rad is needed
# Outputs a dataframe containing year, month, day, hour, light (1 if it is light, 0 if it is dark)
generate_lseg_dlh <- function(dfRAD){
  # determine if hour is daylight or dark
  dfDLH <- sqldf("SELECT year, month, day, hour,
                   CASE
                   WHEN rad > 0 then 1
                   WHEN rad = 0 then 0
                   END light
                   FROM dfRAD")
}

#function that creates the parent rolling average dataset (old as of 3/31/2022)
#inputs: temperature and precipitation dataframes formatted with specific columns
#         dfTMP - year, month, day, hour, temp, date
#         dfPRC - year, month, day, hour, precip, date
get_rolling_avgs <- function(dfTMP, dfPRC){
  
  # create df of daily values
  dailyPrecip <- sqldf("SELECT year, date, month, sum(precip) daily_precip
                           FROM dfPRC
                           GROUP BY date") 
  dailyTemp <- sqldf("SELECT date, avg(temp) daily_temp
                       FROM dfTMP
                       GROUP BY date") %>% select(daily_temp)
  df <- cbind(dailyPrecip, dailyTemp)
  
  #rolling averages for precip
  rolling_7day_PRC <- sqldf(paste('SELECT *, AVG(daily_precip)
                           OVER (ORDER BY date ASC ROWS BETWEEN 6 PRECEDING AND CURRENT ROW)
                           AS rolling_7day_PRC
                          FROM df',sep="")) %>% select(rolling_7day_PRC)
  rolling_30day_PRC <- sqldf(paste('SELECT *, AVG(daily_precip)
                           OVER (ORDER BY date ASC ROWS BETWEEN 29 PRECEDING AND CURRENT ROW)
                           AS rolling_30day_PRC
                          FROM df',sep="")) %>% select(rolling_30day_PRC)
  rolling_90day_PRC <- sqldf(paste('SELECT *, AVG(daily_precip)
                           OVER (ORDER BY date ASC ROWS BETWEEN 89 PRECEDING AND CURRENT ROW)
                           AS rolling_90day_PRC
                          FROM df',sep="")) %>% select(rolling_90day_PRC)
  
  #rolling averages for temperature
  rolling_7day_TEMP <- sqldf(paste('SELECT *, AVG(daily_temp)
                           OVER (ORDER BY date ASC ROWS BETWEEN 6 PRECEDING AND CURRENT ROW)
                           AS rolling_7day_TEMP
                          FROM df',sep="")) %>% select(rolling_7day_TEMP)
  rolling_30day_TEMP <- sqldf(paste('SELECT *, AVG(daily_temp)
                           OVER (ORDER BY date ASC ROWS BETWEEN 29 PRECEDING AND CURRENT ROW)
                           AS rolling_30day_TEMP
                          FROM df',sep="")) %>% select(rolling_30day_TEMP)
  rolling_90day_TEMP <- sqldf(paste('SELECT *, AVG(daily_temp)
                           OVER (ORDER BY date ASC ROWS BETWEEN 89 PRECEDING AND CURRENT ROW)
                           AS rolling_90day_TEMP
                          FROM df',sep="")) %>% select(rolling_90day_TEMP)
  
  rollingAVG <- cbind(df, rolling_7day_PRC, rolling_30day_PRC, rolling_90day_PRC, 
                      rolling_7day_TEMP,rolling_30day_TEMP, rolling_90day_TEMP) 
  return(rollingAVG)
}

#function that creates rolling average dataframe for desired metric
#inputs: dataframe formatted with specific columns, and metric of data
#         df - year, month, day, hour, 'metric', date
#         metric - 'metric' in df (as a character string, must match column name)
get_rolling_avg_df <- function(df, metric){
  # create df of daily values
  daily <- fn$sqldf("SELECT year, date, month, sum($metric) daily_$metric
                           FROM df
                           GROUP BY date") 
  
  #rolling averages for precip
  rolling_7day <- fn$sqldf(paste('SELECT *, AVG(daily_$metric)
                           OVER (ORDER BY date ASC ROWS BETWEEN 6 PRECEDING AND CURRENT ROW)
                           AS rolling_7day_$metric
                          FROM daily',sep="")) %>% select(paste0("rolling_7day_",metric))
  rolling_30day <- fn$sqldf(paste('SELECT *, AVG(daily_$metric)
                           OVER (ORDER BY date ASC ROWS BETWEEN 29 PRECEDING AND CURRENT ROW)
                           AS rolling_30day_$metric
                          FROM daily',sep="")) %>% select(paste0("rolling_30day_",metric))
  rolling_90day <- fn$sqldf(paste('SELECT *, AVG(daily_$metric)
                           OVER (ORDER BY date ASC ROWS BETWEEN 89 PRECEDING AND CURRENT ROW)
                           AS rolling_90day_$metric
                          FROM daily',sep="")) %>% select(paste0("rolling_90day_",metric))
  
  
  rolling_avg_df <- cbind(daily, rolling_7day, rolling_30day, rolling_90day) 
  return(rolling_avg_df)
}

#function that creates plots from rolling average dataframe
#inputs: rolling averages dataframe, grid/landsegment of dataframe
#         rolling_avg_df - rolling average data frame from get_rolling_avgs
#         spatial_unit - the name of the grid/land segment/spatial unit
generate_rolling_avg_precip_plots <- function(rolling_avg_df, spatial_unit){
  #calculate the daily precip 
  dailyPrecip <-  sqldf("SELECT year, month, date, daily_precip
                               FROM rolling_avg_df 
                               WHERE daily_precip > 0.01")
  #calculate precip days
  precipDays <- sqldf("SELECT year, daily_precip, count(daily_precip) precip_days
                              FROM dailyPrecip
                              WHERE daily_precip > 0
                              GROUP BY year")
  #calculate total annual precip
  precip <- sqldf("SELECT year, sum(daily_precip) total_precip
                         FROM dailyPrecip
                         GROUP BY year")
  #create precip graph
  p1 <- ggplot() + 
    geom_bar(data = precipDays, aes(x = year, y = precip_days), stat = "identity") + 
    geom_bar(data = precip, aes(x = year, y = total_precip), stat = "identity", fill= "darkblue") +
    xlab("Year") + 
    ylab("Preciptation Days (number of days with measurable precipitation > 0.01 in) 
         and Annual Precipitation Depth (in)") +
    ggtitle("Number of Precipitation Days and Annual Precip",
            subtitle = paste0("Landseg/Grid: ",spatial_unit))
  p1
  
  #create yearly precip graph
  p2 <- ggplot() +
    geom_line(data = precip, aes(x = year, y = total_precip)) +
    xlab("Year") +
    ylab("Annual Precipitation Depth (in)") +
    ggtitle("Annual Precipitation",
            subtitle = paste0("Landseg/Grid: ",spatial_unit))
  p2
  
  #create daily precip graph (best when looking at small sets of data)
  p3 <- ggplot() +
    geom_line(data = dailyPrecip, aes(x = date, y = daily_precip)) +
    xlab("Date") +
    ylab("Daily Precipitation Depth (in)") +
    ggtitle("Daily Precipitation",
            subtitle = paste0("Landseg/Grid: ",spatial_unit))
  p3
  
  plots <- list(p1,p2,p3)
  return(plots)
}

#example of reading in p5 land segment list
p5_landseg_list <- scan(file = "https://raw.githubusercontent.com/HARPgroup/HARParchive/master/GIS_layers/p5_landsegments.txt", what = character())
#example of reading in p6 land segment list
p6_landseg_list <- scan(file = "https://raw.githubusercontent.com/HARPgroup/HARParchive/master/GIS_layers/p6_landsegments.txt", what = character())

#Script that runs rolling averages function for one segment, and creates plots
#functions created and used: get_rolling_avg_df, generate_rolling_avg_plots
#last updated 4/9/2022
#inputs: land segment, startDate, and endDate
#         landseg or grid
#         startDate formatted YYYYMMDDHH or year
#         endDate formatted YYYYMMDDHH

#load libraries
library(dplyr)
library(sqldf)
library(ggplot2)

#reads in data, uses get_rolling_avgs function to create data set, and creates graphs
#inputs
landseg <- 'A51810'
grid <- 'x393y97'
startDate <- "1984010100" #formatted YYYYMMDDHH
endDate <- "2020123123" #formatted YYYYMMDDHH
year <- "1984"

#example of reading in land segment list (not used in this function)
p5_landseg_list <- scan(file = "https://raw.githubusercontent.com/HARPgroup/HARParchive/master/GIS_layers/p5_landsegments.txt", what = character())

#############################################################################Functions
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
#         metric - metric' in df (as a character string)
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
#inputs: rolling averages dataframe
#         rolling_avg_df - rolling average data frame from get_rolling_avgs
generate_rolling_avg_precip_plots <- function(rolling_avg_df){
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
            subtitle = paste0("Landseg/Grid: ",grid))
  p1
  
  #create yearly precip graph
  p2 <- ggplot() +
    geom_line(data = precip, aes(x = year, y = total_precip)) +
    xlab("Year") +
    ylab("Annual Precipitation Depth (in)") +
    ggtitle("Annual Precipitation",
            subtitle = paste0("Landseg/Grid: ",grid))
  p2
  
  #create daily precip graph (best when looking at small sets of data)
  p3 <- ggplot() +
    geom_line(data = dailyPrecip, aes(x = date, y = daily_precip)) +
    xlab("Date") +
    ylab("Daily Precipitation Depth (in)") +
    ggtitle("Daily Precipitation",
            subtitle = paste0("Landseg/Grid: ",grid))
  p3
  
  plots <- list(p1,p2,p3)
  return(plots)
}
###############################################################################

#reading in precip, and temp land segment data sets
dfPRC <- read.table(paste0("http://deq1.bse.vt.edu:81/met/out/lseg_csv/",startDate,"-",endDate,"/",landseg,".PRC"), header = FALSE, sep = ",")
dfTMP <- read.table(paste0("http://deq1.bse.vt.edu:81/met/out/lseg_csv/",startDate,"-",endDate,"/",landseg,".TMP"), header = FALSE, sep = ",")
#reading in precip data from grid cell
dfPRC <- read.table(paste0("http://deq1.bse.vt.edu:81/met/out/grid_met_csv/",year,"/",grid,"zPP.txt"), header = FALSE)

#formatting columns
colnames(dfTMP) = c("year","month","day","hour","temp")
dfTMP$date <- as.Date(paste(dfTMP$year,dfTMP$month,dfTMP$day, sep="-")) 
colnames(dfPRC) = c("year","month","day","hour","precip")
dfPRC$date <- as.Date(paste(dfPRC$year,dfPRC$month,dfPRC$day, sep="-"))

#creating table from function usign rolling_avg_df
#rollingAVG <- get_rolling_avgs(dfTMP = dfTMP, dfPRC = dfPRC)
rolling_avg_df <- get_rolling_avg_df(df = dfPRC, metric = "precip")

#creating plots from generate_rolling_avg_precip_plots
plots <- generate_rolling_avg_precip_plots(rolling_avg_df = rolling_avg_df)
plots[1]
plots[2]
plots[3]

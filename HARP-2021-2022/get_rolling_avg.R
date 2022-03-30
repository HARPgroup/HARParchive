#Script that runs rolling averages function for one segment, and creates plots
#functions created and used: get_rolling_avgs, generate_rolling_avg_plots
#inputs: land segment, startDate, and endDate
#         landseg
#         startDate formatted YYYYMMDDHH
#         endDate formatted YYYYMMDDHH

#load libraries
library(dplyr)
library(sqldf)
library(ggplot2)

#reads in data, uses get_rolling_avgs function to create data set, and creates graphs
#inputs
landseg <- 'A51810'
startDate <- "1984010100" #formatted YYYYMMDDHH
endDate <- "2020123123" #formatted YYYYMMDDHH

#function that creates the parent rolling average dataset
#inputs: temperature and precipitation dataframes formatted with specific columns
#         dfTMP - year, month, day, hour, temp
#         dfPRC - year, month, day, hour, precip
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


#reading in precip, pet, and temp data sets
dfPRC <- read.table(paste0("http://deq1.bse.vt.edu:81/met/out/lseg_csv/",startDate,"-",endDate,"/",landseg,".PRC"), header = FALSE, sep = ",")
dfTMP <- read.table(paste0("http://deq1.bse.vt.edu:81/met/out/lseg_csv/",startDate,"-",endDate,"/",landseg,".TMP"), header = FALSE, sep = ",")

#formatting columns
colnames(dfTMP) = c("year","month","day","hour","temp")
dfTMP$date <- as.Date(paste(dfTMP$year,dfTMP$month,dfTMP$day, sep="-")) 
colnames(dfPRC) = c("year","month","day","hour","precip")
dfPRC$date <- as.Date(paste(dfPRC$year,dfPRC$month,dfPRC$day, sep="-"))

#creating table from function
rollingAVG <- get_rolling_avgs(dfTMP = dfTMP, dfPRC = dfPRC)

# create and save PET file as csv
# write.table(rollingAVG_PET_P6,paste0("/Users/katealbi/Desktop/HARP/",landseg6,"rollingAVG_PET_P6.csv"), 
#             row.names = FALSE, col.names = TRUE, sep = ",")
# 
# rollingAvgP6 <- read.csv(paste0("/Users/katealbi/Desktop/HARP/",landseg6,"rollingAVG_PET_P6.csv"))

#calculate the daily precip 
dailyPrecip <-  sqldf("SELECT year, month, date, daily_precip
                             FROM rollingAVG 
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
          subtitle = paste0("Landseg: ",landseg))
p1

#create yearly precip graph
p2 <- ggplot() +
  geom_line(data = precip, aes(x = year, y = total_precip)) +
  xlab("Year") +
  ylab("Annual Precipitation Depth (in)") +
  ggtitle("Annual Precipitation",
          subtitle = paste0("Landseg: ",landseg))
p2


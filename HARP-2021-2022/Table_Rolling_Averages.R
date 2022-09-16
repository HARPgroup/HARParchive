library(dplyr)
site <- "http://deq1.bse.vt.edu:81/d.dh"  #Specify the site of interest, either d.bet OR d.dh
basepath <- '/var/www/R';
source(paste(basepath,'config.R',sep='/'))

get_rolling_avgs_PET <- function(dfTMP, dfPRC, dfHET, dfHSET){
  
  # create df of daily values
  dailyPrecip <- sqldf("SELECT year, date, month, sum(precip) daily_precip
                           FROM dfTOTAL
                           GROUP BY Date")
  dailyTemp <- sqldf("SELECT date, avg(temp) daily_temp
                       FROM dfTOTAL
                       GROUP BY date") %>% select(daily_temp)
  dailyHPET <- sqldf("SELECT date, sum(Hpet) daily_Hpet
                       FROM dfTOTAL
                       GROUP BY date") %>% select(daily_Hpet)
  dailyHSPET <- sqldf("SELECT date, sum(HSpet) daily_HSpet
                       FROM dfTOTAL
                       GROUP BY date") %>% select(daily_HSpet)
  #creating daily water deficit values 
  
  dailyHPET$daily_water_deficit_HAMON <- dailyHPET$daily_Hpet - dailyPrecip$daily_precip
  dailyHSPET$daily_water_deficit_HS <- dailyHSPET$daily_HSpet - dailyPrecip$daily_precip
  
  df <- cbind(dailyPrecip, dailyHPET, dailyTemp, dailyHSPET)
  
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
  
  #rolling avergaes for Hamon PET 
  rolling_7day_HPET <- sqldf(paste('SELECT *, AVG(daily_Hpet)
                           OVER (ORDER BY date ASC ROWS BETWEEN 6 PRECEDING AND CURRENT ROW)
                           AS rolling_7day_HPET
                          FROM df',sep="")) %>% select(rolling_7day_HPET)
  rolling_30day_HPET <- sqldf(paste('SELECT *, AVG(daily_Hpet)
                           OVER (ORDER BY date ASC ROWS BETWEEN 29 PRECEDING AND CURRENT ROW)
                           AS rolling_30day_HPET
                          FROM df',sep="")) %>% select(rolling_30day_HPET)
  rolling_90day_HPET <- sqldf(paste('SELECT *, AVG(daily_Hpet)
                           OVER (ORDER BY date ASC ROWS BETWEEN 89 PRECEDING AND CURRENT ROW)
                           AS rolling_90day_HPET
                          FROM df',sep="")) %>% select(rolling_90day_HPET)
  
  #rolling avergaes for Hargreaves Samani PET 
  rolling_7day_HSPET <- sqldf(paste('SELECT *, AVG(daily_HSpet)
                           OVER (ORDER BY date ASC ROWS BETWEEN 6 PRECEDING AND CURRENT ROW)
                           AS rolling_7day_HSPET
                          FROM df',sep="")) %>% select(rolling_7day_HSPET)
  rolling_30day_HSPET <- sqldf(paste('SELECT *, AVG(daily_HSpet)
                           OVER (ORDER BY date ASC ROWS BETWEEN 29 PRECEDING AND CURRENT ROW)
                           AS rolling_30day_HSPET
                          FROM df',sep="")) %>% select(rolling_30day_HSPET)
  rolling_90day_HSPET <- sqldf(paste('SELECT *, AVG(daily_HSpet)
                           OVER (ORDER BY date ASC ROWS BETWEEN 89 PRECEDING AND CURRENT ROW)
                           AS rolling_90day_HSPET
                          FROM df',sep="")) %>% select(rolling_90day_HSPET)
  
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
  
  #rolling averages for water deficit (HamonPET-Precip)
  rolling_7day_WATERDEF_HAMON <- sqldf(paste('SELECT *, AVG(daily_water_deficit_HAMON)
                           OVER (ORDER BY date ASC ROWS BETWEEN 6 PRECEDING AND CURRENT ROW)
                           AS rolling_7day_WATERDEF_HAMON
                          FROM df',sep="")) %>% select(rolling_7day_WATERDEF_HAMON)
  rolling_30day_WATERDEF_HAMON <- sqldf(paste('SELECT *, AVG(daily_water_deficit_HAMON)
                           OVER (ORDER BY date ASC ROWS BETWEEN 29 PRECEDING AND CURRENT ROW)
                           AS rolling_30day_WATERDEF_HAMON
                          FROM df',sep="")) %>% select(rolling_30day_WATERDEF_HAMON)
  rolling_90day_WATERDEF_HAMON <- sqldf(paste('SELECT *, AVG(daily_water_deficit_HAMON)
                           OVER (ORDER BY date ASC ROWS BETWEEN 89 PRECEDING AND CURRENT ROW)
                           AS rolling_90day_WATERDEF_HAMON
                          FROM df',sep="")) %>% select(rolling_90day_WATERDEF_HAMON)
  
  #rolling averages for water deficit (HSPET-Precip)
  rolling_7day_WATERDEF_HS <- sqldf(paste('SELECT *, AVG(daily_water_deficit_HS)
                           OVER (ORDER BY date ASC ROWS BETWEEN 6 PRECEDING AND CURRENT ROW)
                           AS rolling_7day_WATERDEF_HS
                          FROM df',sep="")) %>% select(rolling_7day_WATERDEF_HS)
  rolling_30day_WATERDEF_HS <- sqldf(paste('SELECT *, AVG(daily_water_deficit_HS)
                           OVER (ORDER BY date ASC ROWS BETWEEN 29 PRECEDING AND CURRENT ROW)
                           AS rolling_30day_WATERDEF_HS
                          FROM df',sep="")) %>% select(rolling_30day_WATERDEF_HS)
  rolling_90day_WATERDEF_HS <- sqldf(paste('SELECT *, AVG(daily_water_deficit_HS)
                           OVER (ORDER BY date ASC ROWS BETWEEN 89 PRECEDING AND CURRENT ROW)
                           AS rolling_90day_WATERDEF_HS
                          FROM df',sep="")) %>% select(rolling_90day_WATERDEF_HS)
  
  #final table with 7-day, 30-day, and 90-day rolling averages for precipitation, temperature, and water deficit
  rollingAVG <- cbind(df, rolling_7day_PRC, rolling_30day_PRC, rolling_90day_PRC, rolling_7day_HPET, 
                      rolling_30day_HPET, rolling_90day_HPET, rolling_7day_HSPET, rolling_30day_HSPET, rolling_90day_HSPET, 
                      rolling_7day_TEMP,rolling_30day_TEMP, rolling_90day_TEMP, rolling_7day_WATERDEF_HAMON, 
                      rolling_30day_WATERDEF_HAMON,rolling_90day_WATERDEF_HAMON, rolling_7day_WATERDEF_HS, rolling_30day_WATERDEF_HS,
                      rolling_90day_WATERDEF_HS) 
  return(rollingAVG)
}

i <- 1
while(i<=length(AllLandsegList)){
  landseg <- AllLandsegList[i]
  
  #reading in precip, pet, and temp data sets
  dfPRC <- read.table(paste0(site,"/met/out/lseg_csv/",dataset,"/",landseg,".PRC"), header = FALSE, sep = ",")
  dfHET <- read.table(paste0(site,"/met/out/lseg_csv/",dataset,"/",landseg,".HET"), header = FALSE, sep = ",")
  dfTMP <- read.table(paste0(site,"/met/out/lseg_csv/",dataset,"/",landseg,".TMP"), header = FALSE, sep = ",")
  dfHSET <- read.table(paste0(site,"/met/out/lseg_csv/",dataset,"/",landseg,".HSET"), header = FALSE, sep = ",")
  colnames(dfTMP) = c("year","month","day","hour","temp")
  dfTMP$date <- as.Date(paste(dfTMP$year,dfTMP$month,dfTMP$day, sep="-")) 
  colnames(dfHET) = c("year1","month1","day1","hour1","Hpet")
  dfHET$date1 <- as.Date(paste(dfHET$year,dfHET$month,dfHET$day, sep="-"))                      
  colnames(dfPRC) = c("year2","month2","day2","hour2","precip")
  dfPRC$date2 <- as.Date(paste(dfPRC$year,dfPRC$month,dfPRC$day, sep="-"))
  colnames(dfHSET) = c("year3","month3","day3","hour3","HSpet")
  dfHSET$date3 <- as.Date(paste(dfTMP$year,dfTMP$month,dfTMP$day, sep="-"))
  dfTOTAL <- cbind(dfTMP, dfHET, dfPRC,dfHSET) %>% select(date, year, month, day, hour, temp, precip, Hpet, HSpet) 
  #creating table from function
  rollingAVG_PET <- get_rolling_avgs_PET(dfTMP = dfTMP, dfPRC = dfPRC, dfHET = dfHET, dfHSET = dfHSET)
  
  # create and save PET file as csv
  write.table(rollingAVG_PET,paste0("/Users/katealbi/Desktop/HARP/",landseg,"rollingAVG_PET.csv"), 
              row.names = FALSE, col.names = TRUE, sep = ",")
  
  i<-i+1
}

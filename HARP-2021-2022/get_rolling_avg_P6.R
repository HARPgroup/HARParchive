library(dplyr)
#in order to run the new land segment commands for each phase, 
#the new files must be created on the users computer
#log into deq1

site <- "http://deq1.bse.vt.edu:81/d.dh"  #Specify the site of interest, either d.bet OR d.dh
basepath <- '/Users/katealbi/Desktop/HARP/var/www/R';
source(paste(basepath,'config.R',sep='/'))

#loading phase 6 land segments 
LandsegP6 <- c("N51800","N51550","N51810","N51037","N51093","N51111","N51053","N51740","N51710","N51121","N51135","N51149",
               "N51181","N51650","N51700","N51161","N51019","N51031","N51147","N51199","N51735","N51131","N51071","N51770",
               "N51011","N51007","N51041","N51570","N51730","N51036","N51095","N51830","N51073","N51045","N51775","N51023",
               "N51515","N51680","N51029","N51049","N51087","N51670","N51115","H51023","N51009","N51145","N51760","N51127",
               "N51097","N51001","H51045","N51005","L51023","N51163","N51125","N51075","N51085","N51101","N51119","N51103",
               "N51580","L51163","N51530","H51009","N51003","N51065","N51109","N51057","N51133","N51017","N51678","H51125",
               "N51033","N51159","N51015","H51015","N51177","N51193","L51015","N51790","N51820","H51003","N51540","N51137",
               "N24037","N51091","L51091","H51165","N51165","L51079","N51079","N51113","N51179","N51099","H51079","N51047",
               "N51630","N24017","N54071","N51660","N51139","H51139","H51113","N51061","N51153","H54071","L54071","N51171",
               "H51157","N51157","N51059","N24033","L51157","N51683","L54023","N54023","N54031","H54031","N51187","N51107",
               "N51685","N51600","N51013","N51510","N51610","N11001","N54093","H54023","L54031","N51043","N24031","N54027",
               "N51069","N54077","H24023","N24023","N54057","N51840","N54037","N24021","N24005","N54003","N24043", "N24013",
               "N24001", "N54065","H24021","N42111","N42009","N42057","N42055","N42001","N42041","N42099","N10001","N10005",
               "N24011","N24019","N24039","N24045","N24047")
#161 land segments in phase 6

#creating the parent dataset for Phase 6 
get_rolling_avgs_PET_P6 <- function(dfTMP, dfPRC, dfHET, dfHSET){
  
  # create df of daily values
  dailyPrecip <- sqldf("SELECT year, date, month, sum(precip) daily_precip
                           FROM dfTOTAL
                           GROUP BY date") 
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

i <- 161
while(i<=length(LandsegP6)){
  landseg6 <- LandsegP6[i]
  
  #reading in precip, pet, and temp data sets
  dfPRC <- read.table(paste0("http://deq1.bse.vt.edu:81/met/out/lseg_csv/1984010100-2020123123/",landseg6,".PRC"), header = FALSE, sep = ",")
  dfHET <- read.table(paste0("http://deq1.bse.vt.edu:81/met/out/lseg_csv/1984010100-2020123123/",landseg6,".HET"), header = FALSE, sep = ",")
  dfTMP <- read.table(paste0("http://deq1.bse.vt.edu:81/met/out/lseg_csv/1984010100-2020123123/",landseg6,".TMP"), header = FALSE, sep = ",")
  dfHSET <- read.table(paste0("http://deq1.bse.vt.edu:81/met/out/lseg_csv/1984010100-2020123123/",landseg6,".HSET"), header = FALSE, sep = ",")
  colnames(dfTMP) = c("year","month","day","hour","temp")
  dfTMP$date <- as.Date(paste(dfTMP$year,dfTMP$month,dfTMP$day, sep="-")) 
  colnames(dfHET) = c("year1","month1","day1","hour1","Hpet")
  dfHET$date1 <- as.Date(paste(dfHET$year,dfHET$month,dfHET$day, sep="-"))                      
  colnames(dfPRC) = c("year2","month2","day2","hour2","precip")
  dfPRC$date2 <- as.Date(paste(dfPRC$year,dfPRC$month,dfPRC$day, sep="-"))
  colnames(dfHSET) = c("year3","month3","day3","hour3","HSpet")
  dfHSET$date3 <- as.Date(paste(dfTMP$year,dfTMP$month,dfTMP$day, sep="-"))
  dfTOTAL <- cbind(dfTMP, dfHET, dfPRC, dfHSET) %>% select(date, year, month, day, hour, temp, precip, Hpet, HSpet) 
  #creating table from function
  rollingAVG_PET_P6 <- get_rolling_avgs_PET_P6(dfTMP = dfTMP, dfPRC = dfPRC, dfHET = dfHET, dfHSET = dfHSET)
  
  # create and save PET file as csv
  write.table(rollingAVG_PET_P6,paste0("/Users/katealbi/Desktop/HARP/",landseg6,"rollingAVG_PET_P6.csv"), 
              row.names = FALSE, col.names = TRUE, sep = ",")
  
  rollingAvgP6 <- read.csv(paste0("/Users/katealbi/Desktop/HARP/",landseg6,"rollingAVG_PET_P6.csv"))
  #calculate the daily precip for the Phase 6 land segs 
  dailyPrecipP6 <-  sqldf("SELECT year, month, date, daily_precip
                             FROM rollingAvgP6 
                             WHERE daily_precip > 0.01")
  #precip dats for Phase 6 land segs 
  precipDaysP6 <- sqldf("SELECT year, daily_precip, count(daily_precip) precip_days
                            FROM dailyPrecipP6
                            WHERE daily_precip > 0
                            GROUP BY year")
  #total precip per year for Phase 6 land segs 
  precipP6 <- sqldf("SELECT year, sum(daily_precip) total_precip
                       FROM dailyPrecipP6
                       GROUP BY year")
  #create precip graph 
  precip6 <- ggplot() + 
    geom_bar(data = precipDaysP6, aes(x = year, y = precip_days), stat = "identity") + 
    geom_bar(data = precipP6, aes(x = year, y = total_precip), stat = "identity", fill= "darkblue") +
    xlab("Year") + 
    ylab("Preciptation Days (number of days with measurable precipitation > 0.01 in) 
       and Annual Precipitation Depth (in)") +
    ggtitle(paste0("Number of Precipitation Days and Annual Precip For Phase 6(Lseg ",landseg6,")"))
  precip6
  
  i<-i+1
}
# stopped at Lseg 51135

#Rolling Averages 
library(sqldf)
library(dplyr)

site <- "http://deq1.bse.vt.edu:81/d.dh"  #Specify the site of interest, either d.bet OR d.dh
basepath <- '/Users/katealbi/Desktop/var/www/R';
source(paste(basepath,'config.R',sep='/'))

#Loading all LandSegs List
AllLandsegList <- c("N51800", "N51550", "N51810", "N51037", "N51093", "N51111", "N51053", "N51740", "N51710", "N51121", "N51135", "N51149", 
                    "N51181", "N51650", "N51700", "N51161", "N51019", "N51031", "N51147", "N51199", "N51735", "N51131", "N51071", "N51770", 
                    "N51011", "N51007", "N51041", "N51570", "N51730", "N51036", "N51095", "N51830", "N51073", "N51045", "N51775", "N51023", 
                    "N51515", "N51680", "N51029", "N51049", "N51087", "N51670", "N51115", "H51023", "N51009", "N51145", "N51760", "N51127", 
                    "N51097", "N51001", "H51045", "N51005", "L51023", "N51163", "N51125", "N51075", "N51085", "N51101", "N51119", "N51103", 
                    "N51580", "L51163", "N51530", "H51009", "N51003", "N51065", "N51109", "N51057", "N51133", "N51017", "N51678", "H51125", 
                    "N51033", "N51159", "N51015", "H51015", "N51177", "N51193", "L51015", "N51790", "N51820", "H51003", "N51540", "N51137", 
                    "N24037", "N51091", "L51091", "H51165", "N51165", "L51079", "N51079", "N51113", "N51179", "N51099", "H51079", "N51047", 
                    "N51630", "N24017", "N54071", "N51660", "N51139", "H51139", "H51113", "N51061", "N51153", "H54071", "L54071", "N51171", 
                    "H51157", "N51157", "N51059", "N24033", "L51157", "N51683", "L54023", "N54023", "N54031", "H54031", "N51187", "N51107", 
                    "N51685", "N51600", "N51013", "N51510", "N51610", "N11001", "N54093", "H54023", "L54031", "N51043", "N24031", "N54027", 
                    "N51069", "N54077", "H24023", "N24023", "N54057", "N51840", "N54037", "N24021", "N24005", "N54003", "N24043", "N24013", 
                    "N24001", "N54065", "H24021", "N42111", "N42009", "N42057", "N42055", "N42001", "N42041", "N42099", "N10001", "N10005", 
                    "N24011", "N24019", "N24039", "N24045", "N24047", "A37001", "A37005", "A37009", "A37033", "A37067", "A37077", "A37081", 
                    "A37131", "A37135", "A37145", "A37157", "A37169", "A37171", "A37181", "A37185", "A37189", "A47091", "A47163", "A51021", 
                    "A51025", "A51027", "A51035", "A51051", "A51063", "A51067", "A51077", "A51081", "A51083", "A51089", "A51105", "A51117", 
                    "A51141", "A51143", "A51155", "A51167", "A51169", "A51173", "A51175", "A51183", "A51185", "A51191", "B51197", "B51195", 
                    "B51191", "B51185", "B51173", "B51169", "B51167", "B51141", "B51105", "B51077", "B51067", "B51035", "B37009", "B37005", 
                    "A51750", "A51720", "A51690", "A51640", "A51620", "A51595", "A51590", "A51520", "A51197", "A51195", "A51019", "A51023", 
                    "A51031", "A51045", "A51121", "A51161", "A51515", "A51770", "A51775", "B51019", "B51071", "B51161", "C51071", "F51045", 
                    "F51121", "F51161")
#Variable to declare LandSeg
i <- 1

landseg <- AllLandsegList[i]

#reading in precip, pet, and temp data sets
dfPRC <- read.table(paste0("http://deq1.bse.vt.edu:81/met/out/lseg_csv/1984010100-2020123123/",landseg,".PRC"), header = FALSE, sep = ",")
dfHET <- read.table(paste0("http://deq1.bse.vt.edu:81/met/out/lseg_csv/1984010100-2020123123/",landseg,".HET"), header = FALSE, sep = ",")
dfTMP <- read.table(paste0("http://deq1.bse.vt.edu:81/met/out/lseg_csv/1984010100-2020123123/",landseg,".TMP"), header = FALSE, sep = ",")
colnames(dfTMP) = c("year","month","day","hour","temp")
dfTMP$date <- as.Date(paste(dfTMP$year,dfTMP$month,dfTMP$day, sep="-")) 
colnames(dfHET) = c("year1","month1","day1","hour1","pet")
dfHET$date1 <- as.Date(paste(dfHET$year,dfHET$month,dfHET$day, sep="-"))                      
colnames(dfPRC) = c("year2","month2","day2","hour2","precip")
dfPRC$date2 <- as.Date(paste(dfPRC$year,dfPRC$month,dfPRC$day, sep="-"))

#precip, pet, and temp hourly table 
dfTOTAL <- cbind(dfTMP, dfHET, dfPRC) %>% select(date, year, month, day, hour, temp, pet, precip) 

#creating daily and rolling average tables 
dailyPrecip <- sqldf("SELECT year, date, sum(precip) daily_precip
                           FROM dfTOTAL
                           GROUP BY date")
dailyPET <- sqldf("SELECT date, sum(pet) daily_pet
                       FROM dfTOTAL
                       GROUP BY date") %>% select(daily_pet)
dailyTMP <- sqldf("SELECT date, avg(temp) daily_temp
                       FROM dfTOTAL
                       GROUP BY date") %>% select(daily_temp)
dailyPET$daily_water_deficit <- dailyPET$daily_pet - dailyPrecip$daily_precip

df <- cbind(dailyPrecip, dailyPET, dailyTMP)

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

#rolling avergaes for PET 
rolling_7day_PET <- sqldf(paste('SELECT *, AVG(daily_pet)
                           OVER (ORDER BY date ASC ROWS BETWEEN 6 PRECEDING AND CURRENT ROW)
                           AS rolling_7day_PET
                          FROM df',sep="")) %>% select(rolling_7day_PET)
rolling_30day_PET <- sqldf(paste('SELECT *, AVG(daily_pet)
                           OVER (ORDER BY date ASC ROWS BETWEEN 29 PRECEDING AND CURRENT ROW)
                           AS rolling_30day_PET
                          FROM df',sep="")) %>% select(rolling_30day_PET)
rolling_90day_PET <- sqldf(paste('SELECT *, AVG(daily_pet)
                           OVER (ORDER BY date ASC ROWS BETWEEN 89 PRECEDING AND CURRENT ROW)
                           AS rolling_90day_PET
                          FROM df',sep="")) %>% select(rolling_90day_PET)

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

#rolling averages for water deficit (PET-Precip)
rolling_7day_WATERDEF <- sqldf(paste('SELECT *, AVG(daily_water_deficit)
                           OVER (ORDER BY date ASC ROWS BETWEEN 6 PRECEDING AND CURRENT ROW)
                           AS rolling_7day_WATERDEF
                          FROM df',sep="")) %>% select(rolling_7day_WATERDEF)
rolling_30day_WATERDEF <- sqldf(paste('SELECT *, AVG(daily_water_deficit)
                           OVER (ORDER BY date ASC ROWS BETWEEN 29 PRECEDING AND CURRENT ROW)
                           AS rolling_30day_WATERDEF
                          FROM df',sep="")) %>% select(rolling_30day_WATERDEF)
rolling_90day_WATERDEF <- sqldf(paste('SELECT *, AVG(daily_water_deficit)
                           OVER (ORDER BY date ASC ROWS BETWEEN 89 PRECEDING AND CURRENT ROW)
                           AS rolling_90day_WATERDEF
                          FROM df',sep="")) %>% select(rolling_90day_WATERDEF)

#final table with 7-day, 30-day, and 90-day rolling averages for precipitation, temperature, and water deficit
dfFINAL <- cbind(df, rolling_7day_PRC, rolling_30day_PRC, rolling_90day_PRC, rolling_7day_PET, 
                 rolling_30day_PET, rolling_90day_PET, rolling_7day_TEMP,rolling_30day_TEMP, 
                 rolling_90day_TEMP, rolling_7day_WATERDEF, rolling_30day_WATERDEF,
                 rolling_90day_WATERDEF) 

  
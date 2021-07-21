##### This script is an outline of calculating PET for land segments
## Last Updated 7/21/21
## HARP Group

# load packages
library(lubridate)
library(sqldf)
library(IHA)
library(zoo)

# list of all land segments in VA's minor basins
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
                    "A51750", "A51720", "A51690", "A51640", "A51620", "A51595", "A51590", "A51520", "A51197", "A51195")


i <- 1
while(i<=90){
  landseg <- AllLandsegList[i]
  # read in land segment temperature and precipitation data
  dfPRC <- read.table(paste0("http://deq1.bse.vt.edu:81/met/out/lseg_csv/1984010100-2020123123/",landseg,".PRC"), header = FALSE, sep = ",")
  dfTMP <- read.table(paste0("http://deq1.bse.vt.edu:81/met/out/lseg_csv/1984010100-2020123123/",landseg,".TMP"), header = FALSE, sep = ",")
  #dfPRC <- read.table(paste0("/backup/meteorology/out/lseg_csv/1984010100-2020123123/",landseg,".PRC"), header = FALSE, sep = ",")
  #dfTMP <- read.table(paste0("/backup/meteorology/out/lseg_csv/1984010100-2020123123/",landseg,".TMP"), header = FALSE, sep = ",")
  colnames(dfTMP) = c("year","month","day","hour","temp")
  dfTMP$date <- as.Date(paste(dfTMP$year,dfTMP$month,dfTMP$day, sep="-"))
  colnames(dfPRC) = c("year","month","day","hour","precip")
  dfPRC$date <- as.Date(paste(dfPRC$year,dfPRC$month,dfPRC$day, sep="-"))
  
  # create df of daily values
  dailyPrecip <- sqldf("SELECT year, date, sum(precip) daily_precip
                         FROM dfPRC
                         GROUP BY date")
  dailyTemp <- sqldf("SELECT year, date, avg(temp) daily_temp
                     FROM dfTMP
                     GROUP BY date")
  
  # calculate min and max yearly temperature
  minTemp <- sqldf("SELECT date, min(temp) min_temp 
                     FROM dfTMP
                     GROUP BY year")
  maxTemp <- sqldf("SELECT year, date, max(temp) max_temp
                     FROM dfTMP
                     GROUP BY year")
  # calculate min and max yearly precipitation
  minPrecip <- sqldf("SELECT date, min(precip) min_precip
                     FROM dfPRC
                     GROUP BY year")
  maxPrecip <- sqldf("SELECT date, max(precip) max_precip
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
  repeats2 <- rle(dailyPrecip$daily_precip)
  repeats2 <- data.frame(lengths = repeats2[1],
                         values = repeats2[2])
  dailyPrecip$consec <- rep(repeats2$lengths, repeats2$lengths)
  maxConsec2 <- sqldf("SELECT max(consec) max_consec_days
                       FROM dailyPrecip
                       WHERE daily_precip = 0
                       GROUP BY year")
  
  # IHA lowflow metrics applied to temperature
  zooTMP <- zoo(x=dailyTemp$daily_temp, order.by=dailyTemp$date)
  group2TMP <- group2(zooTMP, year=c('calendar'),mimic.tnc = T)
  # IHA lowflow metrics applied to precipitation
  zooPRC <- zoo(x=dailyPrecip$daily_precip, order.by=dailyPrecip$date)
  group2PRC <-  group2(zooPRC, year=c('calendar'),mimic.tnc = T)
  
  # create a summary data frame
  summaryStats <- cbind(maxTemp, minTemp, maxPrecip, minPrecip, maxConsec, noPrecipDays, precipDays,
                        group2TMP$`7 Day Min`, group2PRC$`7 Day Min`, group2TMP$`30 Day Min`, group2PRC$`30 Day Min`)
  colnames(summaryStats) <- c("year", "max_temp_date", "max_temp", "min_temp_date", "min_temp", 
                              "max_precip_date", "max_precip", "min_precip_date", "min_precip",
                              "max_consec_no_precip_hours", "max_consec_no_precip_days", "no_precip_days", 
                              "precip_days", "7_day_min_temp", "7_day_min_precip", "30_day_min_temp",
                              "30_day_min_precip")
  
  # create and save PET file as csv
  write.table(summaryStats,paste0("C:/Users/alexw/Documents/R/HARP/Summer 2021/landsegPETfiles/",landseg,"SummaryStats.csv"), 
              row.names = FALSE, col.names = TRUE, sep = ",")
  #write.table(summaryStats,paste0("/backup/meteorology/out/lseg_csv/1984010100-2020123123/",landseg,"SummaryStats.csv"), 
  #           row.names = FALSE, col.names = FALSE, sep = ",")
  
  i<-i+1
}

##### This script is an outline of calculating PET for land segments
#### It uses temperature and radiation data to calculate PET for each landsegment
#### The two methods for calculating PET are the Hamon and the Hargreaves-Samani 
#### The temperature and radiation data is located in the /backup/meteorology/out/lseg_csv
#### The generated csv files for PET will be outputed and stored in the same /backup/meteorology/out/lseg_csv directory
#### The csv files for the Hamon method follow the naming convention lseg.HET
#### The csv files for the Hargreaves-Samani method follow the naming convention lseg.HSET

## Last Updated 7/21/21
## HARP Group


# load packages
library(lubridate)
library(sqldf)

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

# dataframe of all land segments in VA's minor basins with corresponding centroid latitude
AllLandsegLat <- data.frame("lseg" = c("A37001", "A37005", "A37009", "A37033", "A37067", "A37077", "A37081", "A37131", "A37135", "A37145",
                                  "A37157", "A37169", "A37171", "A37181", "A37185", "A37189", "A47091", "A47163", "A51021", "A51025",
                                  "A51027", "A51035", "A51051", "A51063", "A51067", "A51077", "A51081", "A51083", "A51089", "A51105",
                                  "A51117", "A51141", "A51143", "A51155", "A51167", "A51169", "A51173", "A51175", "A51183", "A51185",
                                  "A51191", "A51195", "A51197", "A51520", "A51590", "A51595", "A51620", "A51640", "A51690", "A51720",
                                  "A51750", "B37005", "B37009", "B51035", "B51067", "B51077", "B51105", "B51141", "B51167", "B51169",
                                  "B51173", "B51185", "B51191", "B51195", "B51197", "H24021", "H24023", "H51003", "H51009", "H51015",
                                  "H51023", "H51045", "H51079", "H51113", "H51125", "H51139", "H51157", "H51165", "H54023", "H54031",
                                  "H54071", "L51015", "L51023", "L51079", "L51091", "L51157", "L51163", "L54023", "L54031", "L54071",
                                  "N10001", "N10005", "N11001", "N24001", "N24005", "N24011", "N24013", "N24017", "N24019", "N24021",
                                  "N24023", "N24031", "N24033", "N24037", "N24039", "N24043", "N24045", "N24047", "N42001", "N42009",
                                  "N42041", "N42055", "N42057", "N42099", "N42111", "N51001", "N51003", "N51005", "N51007", "N51009",
                                  "N51011", "N51013", "N51015", "N51017", "N51019", "N51023", "N51029", "N51031", "N51033", "N51036",
                                  "N51037", "N51041", "N51043", "N51045", "N51047", "N51049", "N51053", "N51057", "N51059", "N51061",
                                  "N51065", "N51069", "N51071", "N51073", "N51075", "N51079", "N51085", "N51087", "N51091", "N51093",
                                  "N51095", "N51097", "N51099", "N51101", "N51103", "N51107", "N51109", "N51111", "N51113", "N51115",
                                  "N51119", "N51121", "N51125", "N51127", "N51131", "N51133", "N51135", "N51137", "N51139", "N51145",
                                  "N51147", "N51149", "N51153", "N51157", "N51159", "N51161", "N51163", "N51165", "N51171", "N51177",
                                  "N51179", "N51181", "N51187", "N51193", "N51199", "N51510", "N51515", "N51530", "N51540", "N51550",
                                  "N51570", "N51580", "N51600", "N51610", "N51630", "N51650", "N51660", "N51670", "N51678", "N51680",
                                  "N51683", "N51685", "N51700", "N51710", "N51730", "N51735", "N51740", "N51760", "N51770", "N51775",
                                  "N51790", "N51800", "N51810", "N51820", "N51830", "N51840", "N54003", "N54023", "N54027", "N54031",
                                  "N54037", "N54057", "N54065", "N54071", "N54077", "N54093"), 
                       "lat" = c(36.04387, 36.45337, 36.44364, 36.39331, 36.13040, 36.30378, 36.07937, 36.41780, 36.06125, 36.39008,
                                 36.39609, 36.40181, 36.41480, 36.36464, 36.39652, 36.23128, 36.45486, 36.51304, 37.13398, 36.76462,
                                 37.26649, 36.74228, 37.12542, 36.93155, 36.98074, 36.64702, 36.67572, 36.76683, 36.68275, 36.67999,
                                 36.68039, 36.66468, 36.82108, 37.06344, 36.90871, 36.69343, 36.86292, 36.72034, 36.92172, 37.11165,
                                 36.73108, 36.98789, 36.92531, 36.61296, 36.58319, 36.69526, 36.68300, 36.66620, 36.68266, 36.93177,
                                 37.12285, 36.50372, 36.42233, 36.59146, 37.07157, 36.67028, 36.81470, 36.72532, 37.02552, 36.82504,
                                 36.71565, 37.18617, 36.64281, 36.89063, 36.78541, 39.61142, 39.28778, 38.17332, 37.75904, 38.00100,
                                 37.50236, 37.58157, 38.35373, 38.51360, 37.90280, 38.56499, 38.70366, 38.43170, 39.19315, 39.03372,
                                 38.70378, 38.26887, 37.70986, 38.24891, 38.27180, 38.71312, 37.74121, 39.02205, 39.07307, 38.73890,
                                 39.08587, 38.66234, 38.90984, 39.62149, 39.46696, 38.87290, 39.56286, 38.50745, 38.48201, 39.46068,
                                 39.54280, 39.13688, 38.83279, 38.30423, 38.12074, 39.60368, 38.37292, 38.22028, 39.87146, 40.00656,
                                 40.16364, 39.92743, 39.92535, 40.39842, 39.97245, 37.75030, 38.01095, 37.78763, 37.33603, 37.58936,
                                 37.37230, 38.87859, 38.16578, 38.05870, 37.31495, 37.54476, 37.57225, 37.20564, 38.02593, 37.36049,
                                 37.01162, 37.37899, 39.11223, 37.47140, 38.48607, 37.51210, 37.07591, 37.93943, 38.83871, 38.73855,
                                 37.84188, 39.20456, 37.31404, 37.42445, 37.72199, 38.30676, 37.76022, 37.54111, 38.37073, 36.89046,
                                 37.32594, 37.72408, 38.27471, 37.71059, 37.73700, 39.09071, 37.97822, 36.94622, 38.39081, 37.43685,
                                 37.63374, 37.17404, 37.77597, 37.50469, 37.34606, 37.88758, 37.14307, 38.24624, 38.62891, 37.55021,
                                 37.22431, 37.18046, 38.70380, 38.66579, 37.94311, 37.26929, 37.82090, 38.52020, 38.85840, 38.18503,
                                 38.42167, 37.10867, 38.90875, 38.11084, 37.23718, 38.81875, 37.33786, 37.73160, 38.03743, 36.67678,
                                 37.26502, 37.77862, 38.85312, 38.88434, 38.29920, 37.05344, 38.43621, 37.29051, 37.78252, 37.40042,
                                 38.74797, 38.77163, 37.10501, 36.89419, 37.20419, 37.13219, 36.84549, 37.52984, 37.27823, 37.28641,
                                 38.15929, 36.69241, 36.73627, 38.06732, 37.26920, 39.17339, 39.46407, 39.11609, 39.31707, 38.99709,
                                 39.30762, 39.42322, 39.56044, 38.66057, 39.46930, 39.11359)                          
)

# loop iterates through AllLandsegList and outputs 2 csv files, one for each PET method
i <- 223
while(i<=length(AllLandsegList)){
  landseg <- AllLandsegList[i]
  # read in land segment temperature and radiation data
  dfTMP <- read.table(paste0("http://deq1.bse.vt.edu:81/met/out/lseg_csv/1984010100-2020123123/",landseg,".TMP"), header = FALSE, sep = ",")
  dfRAD <- read.table(paste0("http://deq1.bse.vt.edu:81/met/out/lseg_csv/1984010100-2020123123/",landseg,".RAD"), header = FALSE, sep = ",")
  #dfTMP <- read.table(paste0("/backup/meteorology/out/lseg_csv/1984010100-2020123123/",landseg,".TMP"), header = FALSE, sep = ",")
  #dfRAD <- read.table(paste0("/backup/meteorology/out/lseg_csv/1984010100-2020123123/",landseg,".RAD"), header = FALSE, sep = ",")
  colnames(dfTMP) = c("year","month","day","hour","temp")
  dfTMP$date <- as.Date(paste(dfTMP$year,dfTMP$month,dfTMP$day, sep="-"))
  colnames(dfRAD) = c("year","month","day","hour","rad")
  dfRAD$date <- as.Date(paste(dfRAD$year,dfRAD$month,dfRAD$day, sep="-"))
  
  ###################################################################### Hamon's Method
  # find average daily temperate
  dailyMean <- sqldf("SELECT date, avg(temp) avg_temp
                     FROM dfTMP
                     GROUP BY date")
  names(dailyMean)[names(dailyMean) == "date"] <- "date_2"
  # calculate Julian Day
  dailyMean$J <- yday(dailyMean$date_2)
  # calculate declination (radians)
  d <- 23.45*(pi/180)*sin(((dailyMean$J-80)/365)*360*(pi/180))
  # calculate latitude (radians)
  L <- (AllLandsegLat[AllLandsegLat$lseg == landseg, "lat"])*(pi/180)
  # calculate daytime length fraciton
  N <- (1/90)*acos((-tan(L))*(tan(d)))
  # calculate potential evapotranspiration (mm/day)
  hPET <- 0.63*(N^2)*(10^((7.5*dailyMean$avg_temp)/(dailyMean$avg_temp+273)))
  dailyMean$PET <- hPET
  dailyMean$freq <- 24
  hourlyPET <- data.frame(date = dfTMP$date,
                          PET = rep(dailyMean$PET, dailyMean$freq))
  hourlyPET$PET <- hourlyPET$PET*0.0393701/24
  # create PET dataframe
  dfHET <- data.frame(year = dfTMP$year,
                      month = dfTMP$month,
                      day = dfTMP$day,
                      hour = dfTMP$hour,
                      PET = hourlyPET$PET)
  #####################################################################################
  
  ############################################################ Hargreaves-Samani Method
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
  #####################################################################################
 
  # create and save PET file as csv
  write.csv(dfHET,paste0("C:/Users/kylew/Documents/HARP/NLDAS/lseg_pet_csv/",landseg,".HET"), 
              row.names = FALSE)
  write.csv(dfHSET,paste0("C:/Users/kylew/Documents/HARP/NLDAS/lseg_pet_csv/",landseg,".HSET"), 
              row.names = FALSE)
  #write.csv(dfHET,paste0("/backup/meteorology/out/lseg_csv/1984010100-2020123123/",landseg,".HET.csv"), 
  #           row.names = FALSE)
  #write.csv(dfHSET,paste0("/backup/meteorology/out/lseg_csv/1984010100-2020123123/",landseg,".HSET.csv"), 
  #           row.names = FALSE)
  
  i<-i+1
}

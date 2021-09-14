library(lubridate)
library(ggplot2)
library(sqldf)

#loading Land Segs list
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

#chose landseg
i <- 2
landseg <- AllLandsegList[i]

#loading table 
table1 <- read.csv(paste0("/Users/katealbi/Desktop/HARP/",landseg,"rollingAVG_PET.csv"))

#Adding Julien day column 
table1$jday <- yday(table1$date)

#sorting tables
table.7day.HAMON <- sqldf("SELECT Year, Date, jday, rolling_7day_WATERDEF_HAMON
                    FROM table1")
table.30day.HAMON <- sqldf("SELECT Year, Date, jday, rolling_30day_WATERDEF_HAMON
                    FROM table1")
table.90day.HAMON <- sqldf("SELECT Year, Date, jday, rolling_90day_WATERDEF_HAMON
                    FROM table1") 
table.7day.HS <- sqldf("SELECT Year, Date, jday, rolling_7day_WATERDEF_HS
                    FROM table1")
table.30day.HS <- sqldf("SELECT Year, Date, jday, rolling_30day_WATERDEF_HS
                    FROM table1")
table.90day.HS <- sqldf("SELECT Year, Date, jday, rolling_90day_WATERDEF_HS
                    FROM table1") 

table.HAMONPET <- sqldf("SELECT Year, Date, jday, daily_Hpet
                    FROM table1") 
table.HSPET <- sqldf("SELECT Year, Date, jday, daily_HSpet
                    FROM table1") 
table.precip <- sqldf("SELECT Year, Date, jday, daily_precip, rolling_7day_PRC, rolling_30day_PRC, rolling_90day_PRC
                    FROM table1") 

#Creating tables with just 2020 data 
table.7day.HAMON.2020 <- filter(table.7day.HAMON, year == 2020)
table.30day.HAMON.2020 <- filter(table.30day.HAMON, year == 2020)
table.90day.HAMON.2020 <- filter(table.90day.HAMON, year == 2020)
table.7day.HS.2020 <- filter(table.7day.HS, year == 2020)
table.30day.HS.2020 <- filter(table.30day.HS, year == 2020)
table.90day.HS.2020 <- filter(table.90day.HS, year == 2020)
table.HAMONPET.2020 <- filter(table.HAMONPET, year == 2020)
table.HSPET.2020 <- filter(table.HSPET, year == 2020)
table.precip.2020 <- filter(table.precip, year == 2020)

#Water Deficit Graphs: rolling 7,30, and 90 day averages for water deficit using
#both the Hamon and Hargreaves Samani methods 
df.7day.HAMON <- ggplot() + 
  geom_point(data = table.7day.HAMON, mapping = aes(x = jday, y = rolling_7day_WATERDEF_HAMON,color = "1984-2019"), 
             size = 0.5, stroke = 0.5, shape = 16, col = "black") + 
  geom_point(data = table.7day.HAMON.2020, aes(x=jday, y = rolling_7day_WATERDEF_HAMON, color= "2020"),
             size = 1, stroke = 0.5, shape = 16) +
  labs(x = "Julien Day (1-365)", 
       y = "Rolling 7Day Hamon PET Water Deficit (HPET-precip) (inches)", 
       color = "Legend", 
       caption ="Rolling 7 Day average water deficit for the years 1984-2020. Water deficit
       is defined as the potential evapotranspiration calculated by the Hamon method (inches) 
       minus the precipitation (inches). The data for 2020 is highlighted.") + 
  ggtitle(paste0("Rolling 7Day Hamon PET Water Deficit (Lseg ",landseg,") 1984-2020"))
df.7day.HAMON

df.7day.HS <- ggplot() + 
  geom_point(data = table.7day.HS, mapping = aes(x = jday, y = rolling_7day_WATERDEF_HS,color = "1984-2019"), 
             size = 0.5, stroke = 0.5, shape = 16, col = "black") + 
  geom_point(data = table.7day.HS.2020, aes(x=jday, y = rolling_7day_WATERDEF_HS, color= "2020"),
             size = 1, stroke = 0.5, shape = 16) +
  labs(x = "Julien Day (1-365)", 
       y = "Rolling 7Day Hargreaves Samani PET Water Deficit (HPET-precip) (inches)", 
       color = "Legend", 
       caption ="Rolling 7 Day average water deficit for the years 1984-2020. Water deficit
       is defined as the potential evapotranspiration calculated by the Hargreaves Samani method (inches) 
       minus the precipitation (inches). The data for 2020 is highlighted.") + 
  ggtitle(paste0("Rolling 7Day Hargreaves Samani PET Water Deficit (Lseg ",landseg,") 1984-2020"))
df.7day.HS

df.30day.HAMON <- ggplot() + 
  geom_point(data = table.30day.HAMON, mapping = aes(x = jday, y = rolling_30day_WATERDEF_HAMON,color = "1984-2019"), 
             size = 0.5, stroke = 0.5, shape = 16, col = "black") + 
  geom_point(data = table.30day.HAMON.2020, aes(x=jday, y = rolling_30day_WATERDEF_HAMON, color= "2020"),
             size = 1, stroke = 0.5, shape = 16) +
  labs(x = "Julien Day (1-365)", 
       y = "Rolling 30Day Hamon PET Water Deficit (HPET-precip) (inches)", 
       color = "Legend", 
       caption ="Rolling 30 Day average water deficit for the years 1984-2020. Water deficit
       is defined as the potential evapotranspiration calculated by the Hamon method (inches) 
       minus the precipitation (inches). The data for 2020 is highlighted.") + 
  ggtitle(paste0("Rolling 30Day Hamon PET Water Deficit (Lseg ",landseg,") 1984-2020"))
df.30day.HAMON

df.30day.HS <- ggplot() + 
  geom_point(data = table.30day.HS, mapping = aes(x = jday, y = rolling_30day_WATERDEF_HS,color = "1984-2019"), 
             size = 0.5, stroke = 0.5, shape = 16, col = "black") + 
  geom_point(data = table.30day.HS.2020, aes(x=jday, y = rolling_30day_WATERDEF_HS, color= "2020"),
             size = 1, stroke = 0.5, shape = 16) +
  labs(x = "Julien Day (1-365)", 
       y = "Rolling 30Day Hargreaves Samani PET Water Deficit (HPET-precip) (inches)", 
       color = "Legend", 
       caption ="Rolling 30 Day average water deficit for the years 1984-2020. Water deficit
       is defined as the potential evapotranspiration calculated by the Hargreaves Samani method (inches) 
       minus the precipitation (inches). The data for 2020 is highlighted.") + 
  ggtitle(paste0("Rolling 30Day Hargreaves Samani PET Water Deficit (Lseg ",landseg,") 1984-2020"))
df.30day.HS

df.90day.HAMON <- ggplot() + 
  geom_point(data = table.90day.HAMON, mapping = aes(x = jday, y = rolling_90day_WATERDEF_HAMON,color = "1984-2019"), 
             size = 0.5, stroke = 0.5, shape = 16, col = "black") + 
  geom_point(data = table.90day.HAMON.2020, aes(x=jday, y = rolling_90day_WATERDEF_HAMON, color= "2020"),
             size = 1, stroke = 0.5, shape = 16) +
  labs(x = "Julien Day (1-365)", 
       y = "Rolling 90Day Hamon PET Water Deficit (HPET-precip) (inches)", 
       color = "Legend", 
       caption ="Rolling 90 Day average water deficit for the years 1984-2020. Water deficit
       is defined as the potential evapotranspiration calculated by the Hamon method (inches) 
       minus the precipitation (inches). The data for 2020 is highlighted.") + 
  ggtitle(paste0("Rolling 90Day Hamon PET Water Deficit (Lseg ",landseg,") 1984-2020"))
df.90day.HAMON

df.90day.HS <- ggplot() + 
  geom_point(data = table.90day.HS, mapping = aes(x = jday, y = rolling_90day_WATERDEF_HS,color = "1984-2019"), 
             size = 0.5, stroke = 0.5, shape = 16, col = "black") + 
  geom_point(data = table.90day.HS.2020, aes(x=jday, y = rolling_90day_WATERDEF_HS, color= "2020"),
             size = 1, stroke = 0.5, shape = 16) +
  labs(x = "Julien Day (1-365)", 
       y = "Rolling 90Day Hargreaves Samani PET Water Deficit (HPET-precip) (inches)", 
       color = "Legend", 
       caption ="Rolling 90 Day average water deficit for the years 1984-2020. Water deficit
       is defined as the potential evapotranspiration calculated by the Hargr4eaves Samani method (inches) 
       minus the precipitation (inches). The data for 2020 is highlighted.") + 
  ggtitle(paste0("Rolling 90Day Hargreaves Samani PET Water Deficit (Lseg ",landseg,") 1984-2020"))
df.90day.HS

#Precipitation Graphs: rolling 7,30, and 90 day averages 
df.7day.precip <- ggplot() + 
  geom_point(data = table.precip, mapping = aes(x = jday, y = rolling_7day_PRC,color = "1984-2019"),
             size = 0.5, stroke = 0.5, shape = 16, col = "black") +
  geom_point(data = table.precip.2020, aes(x=jday, y = rolling_7day_PRC, color= "2020"),
             size = 1, stroke = 0.5, shape = 16) +
  labs(x = "Julien Day (1-365)", 
       y = "Rolling 7Day Precipitation (inches)", 
       color = "Legend", 
       caption ="Rolling 7 day average precipitation (inches).The data for 2020 is highlighted.") + 
  ggtitle(paste0("Rolling 7Day Precipitation (Lseg ",landseg,") 1984-2020"))
df.7day.precip

df.30day.precip <- ggplot() + 
  geom_point(data = table.precip, mapping = aes(x = jday, y = rolling_30day_PRC,color = "1984-2019"),
             size = 0.5, stroke = 0.5, shape = 16, col = "black") +
  geom_point(data = table.precip.2020, aes(x=jday, y = rolling_30day_PRC, color= "2020"),
             size = 1, stroke = 0.5, shape = 16) +
  labs(x = "Julien Day (1-365)", 
       y = "Rolling 30Day Precipitation (inches)", 
       color = "Legend", 
       caption ="Rolling 30 day average precipitation (inches).The data for 2020 is highlighted.") + 
  ggtitle(paste0("Rolling 30Day Precipitation (Lseg ",landseg,") 1984-2020"))
df.30day.precip

df.90day.precip <- ggplot() + 
  geom_point(data = table.precip, mapping = aes(x = jday, y = rolling_90day_PRC,color = "1984-2019"),
             size = 0.5, stroke = 0.5, shape = 16, col = "black") +
  geom_point(data = table.precip.2020, aes(x=jday, y = rolling_90day_PRC, color= "2020"),
             size = 1, stroke = 0.5, shape = 16) +
  labs(x = "Julien Day (1-365)", 
       y = "Rolling 90Day Precipitation (inches)", 
       color = "Legend", 
       caption ="Rolling 90 day average precipitation (inches).The data for 2020 is highlighted.") + 
  ggtitle(paste0("Rolling 90Day Precipitation (Lseg ",landseg,") 1984-2020"))
df.90day.precip

library(lubridate)
library(ggplot2)
library(sqldf)

argst <- commandArgs(trailingOnly = T)
if (length(argst) < 4) {
  message("Use: Rscript lseg_rolling_avg_graphs.R landseg dataset landseg_ftype model_version_code ")
  message("Ex: Rscript lseg_rolling_avg_graphs.R A51011 1984010100-2020123123 cbp532_landseg cbp-5.3.2 ")
  message("USTABE: Rolling_Averages_Graphs_Updated.R")
  quit()
}

landseg = argst[1]
dataset = argst[2]
landseg_ftype = argst[3]
model_version_code = argst[4]

# 
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

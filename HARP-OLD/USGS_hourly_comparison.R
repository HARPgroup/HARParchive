#This script will translate the USGS 15-minute interval data to hourly data by taking the average of the flow for each hour. Once the
#hourly average has been found, the USGS data and the model data will be compared in a line plot.

URI_model_hourly <- 'https://docs.google.com/spreadsheets/d/e/2PACX-1vSbD4ZuD68FK9ZqESd-_kWlsMhd1a66LWr1N8Xd5oo5zFZm3niigHhFiUJ98b2Y9ayQdncu-4q5VvL4/pub?output=csv'
model_hourly = read.csv(URI_model_hourly, header = TRUE, sep = ",", stringsAsFactors = FALSE);
model_hourly_year <- model_hourly[(184106:192064),] #gets just 2003 data
#1984: [1:8785]
#1985: [8786:17545]
#1986: [17546:26305]
#1987: [26306:35065]
#1988: [35066:43850]
#1989: [43851:52609]
#1990: [52610:61369]
#1991: [61370:70129]
#1992: [70130:78913]
#1993: [78914:87673]
#1994: [87674:96433]
#1995: [96434:105193]
#1996: [105194:113977]
#1997: [113978:122737]
#1998: [122738:131497]
#1999: [131498:140257]
#2000: [140258:149041]
#2001: [149042:157801]
#2002: [157802:166561]
#2003: [166562:175321]
#2004: [175322:184105]
#2005: [184106:192064]

library(dataRetrieval)
vignette("dataRetrieval",package = "dataRetrieval")

# 3. Import data, select site, code, start/end dates
# example for the Dan River at Paces, VA
siteNo <- "02075500"
pCode <- "00060"
start.date <- "2005-01-01"
end.date <- "2005-12-31"

yahara <- readNWISuv(siteNumbers = siteNo,
                     parameterCd = pCode,
                     startDate = start.date,
                     endDate = end.date)
# names with codes
names(yahara)
# cleans up names
yahara <- renameNWISColumns(yahara)
names(yahara)
head(yahara)
summary(yahara) # shows basic stats, median, mean, max, min

#transform 15-minute-interval data to hourly data 
USGS_hourly <- data.frame(matrix(nrow = 1, ncol = 6))

colnames(USGS_hourly) <- c("year", "month", "day", "hour", "Flow", "Average")
i <- 1
j <- 4
h <- 1
k <- nrow(yahara)
for (i in 1:k){
  x <- yahara[i,]
  z <- x$dateTime
  USGS_hourly[i,1] <- format(z, "%Y")
  USGS_hourly[i,2] <- format(z, "%m")
  USGS_hourly[i,3] <- format(z, "%d")
  USGS_hourly[i,4] <- format(z, "%H")
  USGS_hourly[i,5] <- x$Flow_Inst
}

USGS_hour_average <- data.frame(matrix(nrow = 1, ncol = 6))
colnames(USGS_hour_average) <- c("year", "month", "day", "hour", "Flow" ,"Average")

i <- 1
k <- nrow(yahara)
count <- 1
for (i in 1:k) {
  m <- i+1
  n <- i+2
  p <- i+3
  q <- as.numeric(USGS_hourly[i,4])
  r <- as.numeric(USGS_hourly[m,4])
  s <- as.numeric(USGS_hourly[n,4])
  t <- as.numeric(USGS_hourly[p,4])
  u <- as.numeric(USGS_hourly[i,5])
  v <- as.numeric(USGS_hourly[m,5])
  w <- as.numeric(USGS_hourly[n,5])
  y <- as.numeric(USGS_hourly[p,5])
  if (q==r && q==s && q==t){
        average <- (u+v+w+y)/4
        USGS_hourly[p,6] <- average
        USGS_hour_average[count, 1] <- USGS_hourly[i,1]
        USGS_hour_average[count, 2] <- USGS_hourly[i,2]
        USGS_hour_average[count, 3] <- USGS_hourly[i,3]
        USGS_hour_average[count, 4] <- USGS_hourly[i,4]
        USGS_hour_average[count, 5] <- USGS_hourly[i,5]
        USGS_hour_average[count, 6] <- average 
        i <- i+4
        count <- count+1
  } 

}
  


USGS_data <- data.frame(yahara$dateTime, yahara$Flow_Inst)
model_data <- data.frame(model_hourly_year$date, model_hourly_year$ovol)
USGS_data$data_source <- "USGS"
model_data$data_source <- "Model"
colnames(model_data) <- c("Date", "Flow", "data_source")
colnames(USGS_data) <- c("Date", "Flow", "data_source")

total_data <- rbind(USGS_data, model_data)

# 4. basic plot
library(ggplot2)
ts <- ggplot(data = total_data,
             aes(Date, Flow, group = data_source, color = data_source)) +
  geom_line(size = 1)
ts

parameterInfo <- attr(yahara, "variableInfo")
siteInfo <- attr(yahara, "siteInfo")

ts <- ts +
  xlab("Date") +
  ylab(parameterInfo$variableDescription) +
  ggtitle(siteInfo$station_nm)
ts

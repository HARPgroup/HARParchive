# This script uses the data retrieval tool and ggplot to compare the daily data for USGS and the model output 

URI_model_daily <- 'https://docs.google.com/spreadsheets/d/e/2PACX-1vRxODFehfqbofJfrIwLFHGnoCrrpo0QUeSbhl5hehBJrbpqATfjlAuH3sOKT5f86wKVXbxMjUGzb4vY/pub?output=csv'
model_daily = read.csv(URI_model_daily, header = TRUE, sep = ",", stringsAsFactors = FALSE);
model_daily_year <- model_daily[(6576:6940),] #gets just 2003 data
#1984: [1:366]
#1985: [367:731]
#1986: [732:1096]
#1987: [1097:1461]
#1988: [1462:1827]
#1989: [1828:2192]
#1990: [2193:2556]
#1991: [2557:2922]
#1992: [2923:3288]
#1993: [3289:3653]
#1994: [3654:4018]
#1995: [4019:4383]
#1996: [4384:4749]
#1997: [4750:5114]
#1998: [5115:5479]
#1999: [5480:5844]
#2000: [5845:6210]
#2001: [6211:6575]
#2002: [6576:6940]
#2003: [6941:7305]
#2004: [7306:7671]
#2005: [7672:8036]

library(dataRetrieval)
vignette("dataRetrieval",package = "dataRetrieval")

# 3. Import data, select site, code, start/end dates
# example for the Dan River at Paces, VA
siteNo <- "02075500"
pCode <- "00060"
start.date <- "2002-01-01"
end.date <- "2002-12-31"

#for 15 minute time step: readNWIuv
#for daily time step: readNWIdv
yahara <- readNWISdv(siteNumbers = siteNo,
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
#USGS_data <- data.frame(yahara$dateTime, yahara$Flow_Inst) ##for 15 minute time step
USGS_data <- data.frame(yahara$Date, yahara$Flow)
model_data <- data.frame(model_daily_year$date, model_daily_year$ovol)
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
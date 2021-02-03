#Alyssa Ford
#2-12-2018
#This script will find the monthly non-excedence % for the Chesapeake Bay Watershed Model data and USGS gage data

#Load packages 
library(dataRetrieval)
vignette("dataRetrieval",package = "dataRetrieval")

## DAN RIVER NEAR FRANCISCO, NC

#inputs model data 
URI_all_flows_model <- 'https://docs.google.com/spreadsheets/d/e/2PACX-1vRXEGI0n2G_ssvGiuXSaAKYST9Mu8JeuHjAyPBg2pfFpmImOb04U2iRoqjSV7ched35KTzoBiZdb0rJ/pub?output=csv'
all_flows_model <- read.csv(URI_all_flows_model, header = TRUE, sep = ",", stringsAsFactors = FALSE)

 
# Import data, select site, code, start/end dates
#Change site number for different USGS gages
siteNo <- "02068500"
pCode <- "00060"
start.date <- "1984-01-01"
end.date <- "2005-12-31"
yahara <- readNWISdv(siteNumbers = siteNo,
                     parameterCd = pCode,
                     startDate = start.date,
                     endDate = end.date)

#creates a new data frame from yahara that will only contain the date, month, and flow
all_flows_USGS <- data.frame(matrix(nrow = 1, ncol = 3))
colnames(all_flows_USGS) <- c("date","month","flow")
i <- 1
j <- nrow(yahara)
for (i in 1:j){
  k <- yahara[i,]
  all_flows_USGS[i,1] <- k$Date
  all_flows_USGS[i,2] <- format(k$Date, "%m")
  all_flows_USGS[i,3] <- k$X_00060_00003
}

#creates a new data frame that only contains the flow data for the month of September 
sep_flows_model <-  subset(all_flows_model, month == "9")
x10_model <- quantile(sep_flows_model$ovol, 0.10) #calculates the September 10% flow 

sep_flows_USGS <- subset(all_flows_USGS, month == "09")
x10_USGS <- quantile(sep_flows_USGS$flow, 0.10)
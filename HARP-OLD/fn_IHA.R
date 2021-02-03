# Load Libraries
fxn_locations = "C:\\Users\\alexd15\\Documents\\ArcGIS\\HARP\\R\\7Q10";
source(paste(fxn_locations,"fn_iha.R", sep = "/"));  

library(pander);
library(httr);
library(dataRetrieval)
library(zoo)
vignette("dataRetrieval",package = "dataRetrieval")

# Loads model data 
#For river segment OD5_8780_8660
#URI_model_daily <- 'https://docs.google.com/spreadsheets/d/e/2PACX-1vRxODFehfqbofJfrIwLFHGnoCrrpo0QUeSbhl5hehBJrbpqATfjlAuH3sOKT5f86wKVXbxMjUGzb4vY/pub?output=csv'
#For river segment OD3_8630_8720
URI_model_daily <- 'https://docs.google.com/spreadsheets/d/e/2PACX-1vRXEGI0n2G_ssvGiuXSaAKYST9Mu8JeuHjAyPBg2pfFpmImOb04U2iRoqjSV7ched35KTzoBiZdb0rJ/pub?output=csv'
model_daily = read.csv(URI_model_daily, header = TRUE, sep = ",", stringsAsFactors = FALSE);
model_daily <- model_daily[(1:7944),] #This must end on September 30 fo the last year included

Flow_model <- model_daily[,c(6,5)]
Flow_modelv <- as.vector(Flow_model)
colnames(Flow_modelv) <- c("Date", "Flow")

Flow_modelx <- zoo(Flow_modelv$Flow, order.by = Flow_modelv$Date);
model_7Q10 <- fn_iha_7q10(Flow_modelx)
model_alf <- fn_iha_mlf(Flow_modelx,8)
model_DOR <- fn_iha_DOR(Flow_modelx)
model_DOR_Year <- fn_iha_DOR_Year(Flow_modelx)



# Import data, select site, code, start/end dates
# example for the Dan River at Paces, VA
siteNo <- "02068500"
pCode <- "00060"
start.date <- "1984-01-01"
# NOTE: fn_iha_mlf will fail if it lacks a complete water year, so date must be restricted to end on 9-30 
end.date <- "2005-09-30"
yahara <- readNWISdv(siteNumbers = siteNo,
                     parameterCd = pCode,
                     startDate = start.date,
                     endDate = end.date)

# names with codes
names(yahara)
# cleans up names
yahara <- renameNWISColumns(yahara)
# make date posix
#datv$thisdate <- as.POSIXct(datv$thisdate)
flows_USGS <- zoo(yahara[,"Flow"], order.by = yahara$Date);
# get 7Q10
USGS_7Q10 <- fn_iha_7q10(flows_USGS)
# calculate August Low Flow (8 = August)
# NOTE: fn_iha_mlf will fail if it lacks a complete water year, so date must be restricted to end on 9-30 
USGS_alf <- fn_iha_mlf(flows_USGS,8)
#Calculates Drought of Record Flow
USGS_DOR <- fn_iha_DOR(flows_USGS)
#Returns the year for the Drought of Record
USGS_DOR_Year <- fn_iha_DOR_Year(flows_USGS)

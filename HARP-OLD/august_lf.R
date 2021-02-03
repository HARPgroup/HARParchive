#This script will calculate the august low flow values for the CBP model data and the USGS gage data


#Loads USGS gage data -----------------------------------------------
library(dataRetrieval)
vignette("dataRetrieval",package = "dataRetrieval")
library(pander)
library(httr)
library(ggplot2)
library(zoo)
library(IHA)
# Load Libraries
site <- "http://deq1.bse.vt.edu/d.dh"

fxn_locations = '/usr/local/home/git/r-dh-ecohydro/Analysis';
source(paste(fxn_locations,"fn_vahydro-1.0.R", sep = "/"));  
source(paste(fxn_locations,"fn_iha.R", sep = "/"));  

siteNo <- "02075500"
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

flows <- zoo(yahara[,"Flow"], order.by = yahara$Date)
#get 7Q10
fn_iha_7q10(flows)
#calculate August Low Flow (8 = August)
fn_iha_mlf(flows,8)
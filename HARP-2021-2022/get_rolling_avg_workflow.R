#Script that runs rolling averages function for one segment, and creates plots
#functions used: get_rolling_avg_df, generate_rolling_avg_plots
#last updated 4/14/2022
#inputs: land segment, startDate, and endDate OR grid and year
#         startDate formatted YYYYMMDDHH
#         endDate formatted YYYYMMDDHH

#load libraries
library(dplyr)
library(sqldf)
library(ggplot2)

#load vahydro functions
site <- "http://deq1.bse.vt.edu:81/d.dh"
basepath <- '/var/www/R';
source(paste(basepath,'config.R',sep='/'))
#source nldas functions
source("https://raw.githubusercontent.com/HARPgroup/HARParchive/master/HARP-2021-2022/lseg_functions.R")
source("https://raw.githubusercontent.com/HARPgroup/HARParchive/master/HARP-2021-2022/get_lseg_data.R")

###########################################if analyzing land segment run these lines below
#declare inputs
landseg <- 'A51810'
startDate <- "1984010100" #formatted YYYYMMDDHH
endDate <- "2020123123" #formatted YYYYMMDDHH

#reading in precip land segment data sets (run these lines if you want to analyze a land segment)
#dfPRC <- read.table(paste0("http://deq1.bse.vt.edu:81/met/out/lseg_csv/",startDate,"-",endDate,"/",landseg,".PRC"), header = FALSE, sep = ",")
dfLSEG <- get_lseg_data (landseg)
dfPRC <- as.data.frame(dfLSEG$dfPRC)
dfn <- names(dfPRC)
dfn[which(dfn == 'PRC')] <- "precip"
names(dfPRC) <- dfn

#creating table from function using rolling_avg_df
rolling_avg_df <- get_rolling_avg_df(df = dfPRC, metric = "precip")

#creating plots from generate_rolling_avg_precip_plots (change spatial unit input according to your dataframe
plots <- generate_rolling_avg_precip_plots(rolling_avg_df = rolling_avg_df, spatial_unit = landseg)
plots[1]
plots[2]



##########################################if analyzing grid cell run these lines below
#declare inputs
grid <- 'x393y97'
syear <- substr(startDate,1,4)
eyear <- substr(endDate,1,4)
dfPRCall = FALSE
for (iyr in syear:eyear) {
  #reading in precip data from grid cell
  #dfPRC <- read.table(paste0("http://deq1.bse.vt.edu:81/met/out/grid_met_csv/",year,"/",grid,"zPP.txt"), header = FALSE)
  site <- paste0("http://deq1.bse.vt.edu:81/met/out/grid_met_csv/",iyr,"/")
  dfPRC <- data.table::fread(paste0(site,grid,"zPP.txt"))
  #formatting columns
  colnames(dfPRC) = c("year","month","day","hour","precip")
  dfPRC$date <- as.Date(
    paste(dfPRC$year,dfPRC$month,dfPRC$day, 
    sep="-")
  )
  if (is.logical(dfPRCall)) {
    dfPRCall <- dfPRC
  } else {
    dfPRCall <- sqldf(
      "SELECT * from (
         SELECT * from dfPRCall
         UNION
         SELECT * from dfPRC
       )
       ORDER BY date
      "
    )
  }
  
}

#creating table from function using rolling_avg_df
rolling_avg_df <- get_rolling_avg_df(df = dfPRCall, metric = "precip")

#creating plots from generate_rolling_avg_precip_plots (change spatial unit input according to your dataframe
plots <- generate_rolling_avg_precip_plots(rolling_avg_df = rolling_avg_df, spatial_unit = grid)
plots[1]
plots[3]

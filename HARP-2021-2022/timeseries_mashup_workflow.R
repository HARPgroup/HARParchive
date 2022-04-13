##### This script creates mash-up timeseries data for synthetic meteorological datasets
## Last Updated 4/9/22
## HARP Group

library(lubridate)
library(sqldf)
library(data.table)

# load vahydro functions
site <- "http://deq1.bse.vt.edu:81/d.dh"
basepath <- '/var/www/R';
source(paste(basepath,'config.R',sep='/'))


# load lseg_functions and synthetic_met_functions
source(paste(github_location,"HARParchive/HARP-2021-2022","lseg_functions.R", sep = "/"))
source(paste(github_location,"HARParchive/HARP-2021-2022","synthetic_met_functions.R", sep = "/"))


# declare location of directories housing data, and desired saving place
in_dir <- "/backup/meteorology/out/lseg_csv/1984010100-2020123123/" # linux directory
out_dir <- "/backup/meteorology/out/lseg_csv/"
site <- "http://deq1.bse.vt.edu:81/met/out/lseg_csv/1984010100-2020123123/" # temporary cloud url
#site <- paste(site, "met", "out", "lseg_csv", "1984010100-2020123123/", sep = "/")

# declare land segment and date range variables
# startdate1 needs to include warm up period of two years
# enddate1 is date of interest for year of modeling interest (could be the current date)
# startdate2 is next day in year but for a year of record with significant data for modeling purposes(ex: drought year)
# enddate2 is end day in year of record
landseg <- "N51800"
startdate1 <- "1984-01-01"  
enddate1 <- "1984-12-31"
startdate2 <- "2002-01-01"
enddate2 <- "2002-12-31"


# running data downloading function
# make sure to download a timeseries that includes entire mash up period
lseg_csv <- get_lseg_csv(landseg = landseg, startdate = "1984-01-01", enddate = "2020-12-31", site = site, dir = in_dir)


# running mash up function
mash_up <- generate_synthetic_timeseries(lseg_csv = lseg_csv, startdate1 = startdate1, enddate1 = enddate1, startdate2 = startdate2, enddate2 = enddate2)


# running posting function
post_synthetic_timeseries(landseg = landseg, startdate1 = startdate1, enddate1 = enddate1, startdate2 = startdate2, enddate2 = enddate2, lseg_csv = mash_up, dir = out_dir)
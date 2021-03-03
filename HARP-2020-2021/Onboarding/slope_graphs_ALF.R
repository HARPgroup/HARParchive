
library(CGPfunctions)
library(dataRetrieval)
library(ggplot2)

#potomac site
site_id <- '01646502'
start_date <-'1949-10-01'
end_date <- '2019-9-30'

# Function for decade alf values from 1950s to 2010s
ALF_decades <- function(site_id,start_date,end_date,site_name){
  flow <- readNWISdv(site_id,'00060',start_date,end_date)
  info <- readNWISsite(site_id)
  # add the month and water year column
  date <- flow$Date
  month <- format(as.Date(date,format = '%d-%m-%Y'), '%m')
  month <- as.numeric(month)
  flow$month <- month
  flow <- addWaterYear(flow)
  # Normalize by area
  area <- info$drain_area_va
  flow$spec_dis <- flow$X_00060_00003/area
  #initialize mins_ap vector
  aug_mins <- c()
  w_year <- flow$waterYear[1]
  # Now evaluate
  while (w_year <= 2019) {
    w_year <- toString(w_year)
    aug <- flow[(flow$waterYear == w_year) & (flow$month == 8), ]
    min_val <- min(aug$spec_dis)
    aug_mins <- append(aug_mins, min_val)
    w_year <- strtoi(w_year) + 1
  }
  val <- 0
  ALF_dec <- c()
  #Now find med for each decade and add it to vector
  decade <- aug_mins[1:10]
  med_dec <- median(decade)
  ALF_dec <- append(ALF_dec, med_dec)
  decade <- aug_mins[11:20]
  med_dec <- median(decade)
  ALF_dec <- append(ALF_dec, med_dec)
  decade <- aug_mins[21:30]
  med_dec <- median(decade)
  ALF_dec <- append(ALF_dec, med_dec)
  decade <- aug_mins[31:40]
  med_dec <- median(decade)
  ALF_dec <- append(ALF_dec, med_dec)
  decade <- aug_mins[41:50]
  med_dec <- median(decade)
  ALF_dec <- append(ALF_dec, med_dec)
  decade <- aug_mins[51:60]
  med_dec <- median(decade)
  ALF_dec <- append(ALF_dec, med_dec)
  decade <- aug_mins[61:70]
  med_dec <- median(decade)
  ALF_dec <- append(ALF_dec, med_dec)
  decades <- c('1950s','1960s','1970s','1980s','1990s','2000s','2010s')
  Site <- rep(c(site_name), times = 7)
  ALF_dec <- round(ALF_dec, digits=2)
  df <- data.frame(Site, ALF_dec, decades)
  return(df)
}

potomac <- ALF_decades(site_id,start_date,end_date,'Potomac')


#same with additional site from 1950-2020
# Holston River near Damascus
site_id <- '03473000'
start_date <-'1949-10-01'
end_date <- '2019-9-30'
holston <- ALF_decades(site_id,start_date,end_date,'Holston')


# Now add appomattox, site 02040000
site_id <- '02040000'
appomattox <- ALF_decades(site_id,start_date,end_date,'Appomattox')

# Add Blackwater River Site
site_id <- '02047500'
blackwater <- ALF_decades(site_id,start_date,end_date,'Blackwater')

combo_df <- rbind(potomac,holston, appomattox, blackwater)


newggslopegraph(combo_df, decades, ALF_dec,Site, DataTextSize = 3.2, WiderLabels= TRUE, 
                YTextSize = 4, TitleTextSize=18, XTextSize=14,SubTitleText=14 ) + 
  labs(title="Change in ALF values [cfs/mi^2]", 
       subtitle="Virginia USGS Gage Sites", 
       caption="source: USGS")    


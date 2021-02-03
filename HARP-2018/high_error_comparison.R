#The purpose of this code is to calculate the average daily flow of model and gage outputs
#Not only is this a useful metric but it allows us to make sure we have the correct river segments and gage associated

#Load Libraries---------------------------------------------------------------------
library(pander);
library(httr);
library(dataRetrieval);
library(zoo);
library(lubridate);

#Info that needs to be changed: river segment, gage number--------------------------
#RETRIEVE MODEL DATA
river_seg<-'TU2_8950_9040'
siteNo <- "03471500"
start.date <- "1984-10-01"
end.date <- "2005-12-05"  # we analyze to 09-30, but need to pull data through 11-30
river_seg_deq <- paste0(river_seg,'_0111')
URL_model_hourly <-paste("http://deq2.bse.vt.edu/p532c-sova/wdm/river/p532cal_062211/stream/",river_seg_deq,".csv",sep="")
model_hourly <- read.csv(URL_model_hourly, header = FALSE, sep = ",", stringsAsFactors = FALSE);
model_hourly <- model_hourly[-1,]

# make a folder connection to save your plots
folder_location<- 'C:\\Users\\HaileyMae\\Downloads'
dir.create(paste0(folder_location,"\\Error Hydrographs\\",siteNo, " vs ", river_seg), showWarnings = TRUE)


# Converting hourly to daily data
colnames(model_hourly) <- c("year","month","day","hour","flow")
model_hourly$date <- as.Date(paste0(model_hourly$year,"-",model_hourly$month,"-",model_hourly$day))


model_daily_flow <- aggregate(model_hourly$flow, list(model_hourly$date), FUN = sum)
colnames(model_daily_flow) <- c("date","flow")

# Converts from acre-feet to cfs (which USGS uses) -- 0.504167 is conversion factor
model_daily_flow$flow <- model_daily_flow$flow * 0.504167

# Creates model_daily dataframe, used throughout this script
model_daily_WY <- data.frame(c(1:length(model_daily_flow$date)),year(model_daily_flow$date), 
                             month(model_daily_flow$date), day(model_daily_flow$date), 
                             model_daily_flow$flow, model_daily_flow$date)
colnames(model_daily_WY) <- c("X","year", "month", "day", "flow", "date")

# specify dates of interest in model
start_model_daily = subset(model_daily_WY, as.Date(model_daily_WY$date)==as.Date(start.date));
start_model_daily = start_model_daily$X;
end_model_daily = subset(model_daily_WY, as.Date(model_daily_WY$date)==as.Date(end.date));
end_model_daily = end_model_daily$X;
model_daily_WY = model_daily_WY[c(start_model_daily:end_model_daily),];


# LOADS GAGE DATA---------------------------------------------------------------
pCode <- "00060"
gage_data <- readNWISdv(siteNumbers = siteNo,
                        parameterCd = pCode,
                        startDate = start.date,
                        endDate = end.date)
# Cleans up names of gage data columns
#names(gage_data)
gage_data <- renameNWISColumns(gage_data)


# AREA WEIGHTED CALCULATION ---------------------------------------------------
#Pull in data on river segment areas - originates from GIS
URL_model_area <- 'https://docs.google.com/spreadsheets/d/e/2PACX-1vRY5BkQ0Ha269jchgbuZYaND5_rxaSTV1rBoW6dDtr7NnlrCIcMaODmST-WAfSzUwUWwOuv3ghVUwNH/pub?output=csv'
model_area_all = read.csv(URL_model_area, header = TRUE, sep = ",", stringsAsFactors = FALSE);


model_area_row_number<-which(model_area_all$RiverSeg == paste(river_seg)) #Pull column number of river segement
model_area_row_info<-data.frame(matrix(nrow=1,ncol=1))                    #Create empty vector to store desired river segment data
model_area_row_info<-model_area_all[model_area_row_number,]               #Fill vector with desired data
model_area<-model_area_row_info$GISArea                                   #Pull just the area of the river segment

gage_area<-readNWISsite(paste(siteNo)) #Pulls data on the size of the watershed and locations
area_of_model<-model_daily_WY$flow*(gage_area$drain_area_va/model_area) #To get area adjust model data do model flow *gage area/model area
area_weighted_model<-data.frame(area_of_model,as.Date(model_daily_WY$date))
colnames(area_weighted_model) <- c("flow","date")

# CALCULATES AVERAGE DAILY FLOW -----------------------------------------------------
avg_model_area<-mean(area_of_model)
avg_model_norm<-mean(model_daily_WY$flow)
avg_gage<-mean(gage_data$Flow)


# we want to make a hydrograph that will zoom in on 3 month segments where error is high
# find how many we need to count

# initialize variables ----------------------------------------------------

#all_data puts model gage flows and corresponding dates in one data frame
all_data <- data.frame (gage_data$Date, area_weighted_model$flow, gage_data$Flow) 
all_data$counter <- 1:length(all_data$gage_data.Date) # counter fixes issues with row numbers later on in script
colnames(all_data) <- c('Date', 'Model Flow', 'Gage Flow', 'Counter')

# find the first date for which data is collected, (in date format)
# and a date that is roughly one year and two months past the first date
YearStart <- all_data$Date[1]     
YearEnd <- all_data$Date[1] + 430 

# YearStart_Row and YearEnd_Row are the rows corresponding the the YearStart and YearEnd dates
YearStart_Row <- which(all_data$Date== YearStart) 
YearEnd_Row <- which(all_data$Date == YearEnd)

# initalize dataframes and counters, assign names for dataframe columns
#AvgMonthlyError: used within nested for loop to create a 1x12 matrix that holds 1 year of 3 month error segments
#Timespan_Error: used in large loop to store values from AvgMonthlyError; holds entire timespan of 3 month error segments
AvgMonthlyError <- data.frame(matrix(nrow=1,ncol=1));  
names(AvgMonthlyError)<-'Error' 
Timespan_Error <- data.frame(matrix(nrow=1, ncol=1)); 
names(Timespan_Error)<-'Error'
i <- 1; # used for first for loop to advance a year
x <- 1 # x and y used to advance dataframes
y <- 12

# start loops  -------------------------------------------------------------

for (i in 1:21){                                # run loop for an entire data series
  year <- all_data[YearStart_Row:YearEnd_Row ,] # specify year: 10-01-year1 to 12-05-year2
  m <- 1                                        # counter for nested loop
  
  
  MonthStart <- YearStart  # first date for 3 month timespan                      
  doi <- as.Date(MonthStart) + seq(0,365,31) 
  # doi= date of interest. dummy variable just to create function next.month
  next.month <- function(doi) as.Date(as.yearmon(doi) + 1/12) + as.numeric(doi-as.Date(as.yearmon(doi)))
  
  MonthEnd <-data.frame(next.month(doi));  # last date in 3 month timespan - used function to determine 3rd month
  MonthEnd <- MonthEnd[3,1] # specifies end of month 3 as last date
  # (technically specifies 01 of month 4)
  
  # row numbers corresponding with start and end dates, as a number. See note below 
  MonthStart_Row <- as.numeric(which(all_data$Date==MonthStart))
  MonthEnd_Row <- as.numeric(which(all_data$Date==MonthEnd))
  
  # Note: Counter column is used here to specify which row starts MonthStart and MonthEnd_Row.
  # When rows are pulled from year row numbers are also pulled, 
  # so a counter must be used for proper row numbers. 
  Start_new <- which(year$Counter==MonthStart_Row)
  End_new <- which(year$Counter==MonthEnd_Row)
  
  
  for (m in 1:12){
    month_time <- year[Start_new:End_new ,]  #extract data for 3 month timespan within year of interest
    avgmonth_gage <- mean(month_time$`Gage Flow`)   # find average of gage flow for 3 months
    avgmonth_model <- mean(month_time$`Model Flow`) # find average of model flow for 3 months
    AvgMonthlyError[m,1] <- abs(avgmonth_gage - avgmonth_model)/ avgmonth_gage * 100  # percent error between gage and model
    
    
    MonthEndyear <- year(MonthEnd) # Year associated with last month of extracted data 
    MonthEndmonth <- month(MonthEnd) # Month associated with last month of extracted data 
    
    # the next three lines are for the error calculations -- stop on 1st of month 4 (31 of month 3)
    # Note: this does include the 1st of the next month in error calculation
    # Put a control on what date the script advances by - if date is not 1st of month, reset it
    DateCheck <- as.Date(paste0(MonthEndyear,'-',MonthEndmonth,'-01'))
    if (MonthEnd != DateCheck)
      MonthEnd <- as.Date(paste0(MonthEndyear,'-', MonthEndmonth, '-01'))
    
    # These lines are to create a date column with the correct dates for visual purposes
    # Used to aid in visualization of last month of calculation
    # Ex: Analysis 10-01 to 12-31 will stop on 01-01, but the lines below output 12-30.
    stopmonth <- MonthEndmonth-1
    stopyear <- MonthEndyear
    stopdate <- MonthEnd
    if (stopmonth ==0)
      stopyear<- MonthEndyear-1
    if (stopmonth == 0) # make sure January says December as stopmonth
      stopmonth <- 12 
    VisualDate <- as.Date(paste0(stopyear,'-',stopmonth,'-27'))
    if (stopdate != VisualDate)
      stopdate <- as.Date(paste0(stopyear,'-', stopmonth, '-27'))
    AvgMonthlyError[m,2] <- stopdate
    
    # Advance to next month or count
    MonthStart <- next.month(MonthStart)
    MonthEnd <- next.month(MonthEnd)
    StartMonth_Row <- which(all_data$Date==MonthStart);     
    StartMonth_Row <- as.numeric(which(all_data$Date==MonthStart))
    EndMonth_Row <- which(all_data$Date==MonthEnd);     
    EndMonth_Row <- as.numeric(which(all_data$Date==MonthEnd))
    Start_new <- which(year$Counter==StartMonth_Row)
    End_new <- which(year$Counter==EndMonth_Row)
    m <- m + 1
  }
  
  Timespan_Error[x:y, 1] <- AvgMonthlyError[,1] # save the error entries from AvgMonthlyError
  Timespan_Error[x:y, 2] <- AvgMonthlyError[,2] # save the dates 
  
  
  # advance Timespan_Error for next run
  x <- x + 12  
  y <- y + 12
  
  
  YearStart <- YearStart + 365  # Advance 1 year
  YearEnd <- YearEnd + 365     # Advance 1 year & 2 months (from 10-01 to 12-05)
  
  # Put a control on what date the script advances by - if end date is not 12-05, reset it
  # if begin date is not 
  YearBeginyear <- year(YearStart)  # pull year of ending year
  YearBeginCheck <- as.Date(paste0(YearBeginyear,'-10-01'))
  if (YearBeginyear != YearBeginCheck)     
    YearStart <- as.Date(paste0(YearBeginyear,'-10-01'))
  
  
  YearEndyear <- year(YearEnd)  # pull year of ending year
  YearEndCheck <- as.Date(paste0(YearEndyear,'-12-05'))
  if (YearEnd != YearEndCheck)     
    YearEnd <- as.Date(paste0(YearEndyear,'-12-05'))
  YearStart_Row <- which(all_data$Date== YearStart)
  YearEnd_Row <- which(all_data$Date == YearEnd)
  
  i <- i + 1
}

# This section of code will plot timeframes with high error.
# count the number of 3 month periods over 20% error, plot the highest 3 periods.

Timespan_Error$Logic <- Timespan_Error$Error>=20
HighError <- Timespan_Error[Timespan_Error$Logic=='TRUE',]
HighError<- HighError[order(HighError$Error, decreasing = TRUE),]
names(HighError)<-c('Error', 'Date', 'Logic')

HighestErrors <- HighError[1:3,]
# pull data for each of these 3 month segments. 

erroryear <- data.frame(matrix(nrow=1,ncol=6))
errordates <- data.frame(matrix(nrow=1, ncol=2))
names(erroryear)<- c('endyear', 'endmonth', 'enddate', 'startyear', 'startmonth', 'startdate')
names(errordates)<- c('start date row', 'end date row')
q <- 1

for (q in 1:length(HighestErrors)){
  erroryear[q,1] <- year(HighestErrors$Date[q])  # ending year
  erroryear[q,2]<- as.numeric(month(HighestErrors$Date[q]) + 1) # ending month
  erroryear[q,4]<- year(HighestErrors$Date[q]) #startyear
  erroryear[q,5]<- month(HighestErrors$Date[q])-2 #startmonth
  
  if (erroryear[q,2] > 12) { # if end month is jan, must move year up
    erroryear[q,4] <- erroryear[q,1]
    erroryear[q,1]<- erroryear[q,1] + 1 # year for jan moves
    erroryear[q,2] <- 1
  }else if (erroryear[q,5] == -1) {
    erroryear[q,4] <- erroryear[q,4] - 1 # if january, go back a year and start november
    erroryear[q,5] <- 11
  }else if (erroryear[q,5] == 0) {
    erroryear[q,4] <- erroryear[q,4] - 1 # if january, go back a year and start november
    erroryear[q,5] <- 12
  } else{
    erroryear[q,1]<- erroryear[q,1]  #endyear
    erroryear[q,2]<- erroryear[q,2]  #endmonth
    erroryear[q,4]<- erroryear[q,4]  #startyear
    erroryear[q,5]<- erroryear[q,5]  #startmonth
  }
  erroryear[q,3]<- paste0(erroryear[q,1], '-',erroryear[q,2], '-01') #enddate
  erroryear[q,6]<- paste0(erroryear[q,4], '-', erroryear[q,5], '-01')#startdate
  
  errordates[q,3]<- as.Date(erroryear$startdate[q])
  errordates[q,4]<- as.Date(erroryear$enddate[q])
  errordates[q,1]<- which(all_data$Date==errordates$V3[q])
  errordates[q,2]<- which(all_data$Date==errordates$V4[q])
  
  plot1<-all_data[errordates$`start date row`[q]:errordates$`end date row`[q],]
  
  # # create and export a plot: 
  png(filename=paste0(folder_location,"\\Error Hydrographs\\",siteNo, " vs ", river_seg, "\\Fig.", q, "- Error.png"), 
      width=1400, height=950, units="px")
  
  gage_legend<- paste('Gage',siteNo)                        #Creates legend title for gage
  model_legend<-paste('Model River Seg. \n', river_seg)     #Creates legend title for model
  error <- round(HighestErrors$Error[q], digits=2)
  #Both data sets on one graph
  par(cex=3, lwd=2, mar=c(4.5,4.2,1.5,0.5))
  plot(plot1$Date, plot1$`Gage Flow`, type='l', col="red", ylim=c(0,(max(plot1$`Gage Flow`))),
       xlab=paste('Date Range:', errordates[q,3], ':', errordates[q,4], ' ', 'Error:', error,'%'), 
       ylab='Flow (cfs)', lwd=2) #USGS gage data
  lines(plot1$Date, plot1$`Model Flow`, col="blue", type='l', lwd=2)                 #Model data
  legend ("topright", legend=c(gage_legend,model_legend), col=c('red','blue'), lty=1, bty='n', cex=1)
  dev.off()
  q <- q+1
}


# CREATES OUTPUT MATRIX -------------------------------------------------------

# also want to list the number of timespans that were over 20% error. 
Over_20<- round(nrow(HighError) / nrow(Timespan_Error) * 100, digits=2)
OUTPUT_MATRIX <- matrix(c(avg_gage, avg_model_area, Over_20), nrow=1, ncol=3)
rownames(OUTPUT_MATRIX) = c("Area Weighted Flow")
colnames(OUTPUT_MATRIX) = c('USGS', 'Model', 'Error>20 (%)')
overall_error <- round(abs(OUTPUT_MATRIX[1,1]-OUTPUT_MATRIX[1,2])/OUTPUT_MATRIX[1,1]*100, digits=2)
OUTPUT_MATRIX <- matrix(c(avg_gage, avg_model_area, Over_20, overall_error), nrow=1, ncol=4)
rownames(OUTPUT_MATRIX) = c("Area Weighted Flow")
colnames(OUTPUT_MATRIX) = c('USGS', 'Model', 'Error>20 (%)', 'Overall Error')

# # PLOT AVERAGE DAILY FLOWS FOR HIGH ERROR SEGMENTS -------------------------------------------------------
# gage_legend<- paste('Gage',siteNo)                        #Creates legend title for gage
# model_legend<-paste('Model River Seg. \n', river_seg)     #Creates legend title for model
# error <- abs(OUTPUT_MATRIX[1,1]-OUTPUT_MATRIX[1,2])/OUTPUT_MATRIX[1,1]*100
# error <- round(error, digits = 2)
# par(mfrow=c(1,1))                                                                                #Both data sets on one graph
# par(cex=1.1)
# plot(gage_data$Date, gage_data$Flow, type='l', col="red", 
#      xlab=paste('Date Range:', start.date, ':', end.date, ' ', 'Error:', error,'%'), 
#      ylab='Flow (cfs)', lwd=1.5) #USGS gage data
# lines(area_weighted_model$date, area_weighted_model$flow, col="blue", type='l', lwd=1.5)                 #Model data
# legend ("topright", legend=c(gage_legend,model_legend), col=c('red','blue'), lty=1, bty='n') 

# OUTPUT MATRIX------------------------------------------------------------------------
OUTPUT_MATRIX
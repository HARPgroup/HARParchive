# DOCUMENTATION -----------------------------------------------------------

# Creates plots zoomed in three-month periods of highest error, also calculates
# a few metrics pertaining to error -- such as percent of observations above 20%

library(zoo)
library(lubridate)

# INPUTS ------------------------------------------------------------------

# Address of "DEQ_Model_vs_USGS_Comparison" folder
# Include "DEQ_Model_vs_USGS_Comparison" in address!
container <- "C:\\Users\\FujitsuT\\Downloads\\HARP\\GitHub\\hydro-tools\\HARP-2018\\DEQ_Model_vs_USGS_Comparison_Northern"

# USGS Gage number
siteNo <- "02039500"

# Should new or original data be used?
new.or.original <- "new"

# CARRYOVER IF MASTER IS BEING RUN ----------------------------------------
if (exists("container.master") == TRUE) {
  container <- container.master
  siteNo <- siteNo.master
  new.or.original <- new.or.original.master
}

# LINKING MODEL SEGMENT ---------------------------------------------------

gage.to.segment <- read.csv(file.path(container, "data", "Gage_To_Segment_Northern.csv"),
                            header = TRUE, sep = ',', stringsAsFactors = FALSE)
gage.to.segment <- subset(gage.to.segment, gage.to.segment$gage_number == as.numeric(siteNo))
RivSeg <- gage.to.segment$river_segment

# NEW OR ORIGINAL DATA SWITCH ---------------------------------------------

if (new.or.original == "new") {
  container.cont <- "\\data\\new_(updated)_data"
} else if (new.or.original == "original") {
  container.cont <- "\\data\\original_(reproducible)_data"
} else {
  print("ERROR: neither new or original data specified")
}

# LOADING DATA ------------------------------------------------------------

data <- read.csv(paste0(container, container.cont, "\\derived_data\\trimmed+area-adjusted_data\\",siteNo,"_vs_",RivSeg, " - Derived Data.csv"))

# OVERALL MEAN FLOW
met00_Gage_MeanFlow <- signif(mean(data$gage.flow), digits=3);
met00_Model_MeanFlow <- signif(mean(data$model.flow), digits=3);
met00_PctError <- -(signif(((met00_Model_MeanFlow - met00_Gage_MeanFlow) / met00_Gage_MeanFlow)*100, digits=3));

# Calculating mean flow of gage and model
avg_gage <- mean(data$gage.flow)
avg_model <- mean(data$model.flow)

# ADDING ADDITIONAL DATA COLUMNS ------------------------------------------
data$year <- year(ymd(data$date))
data$month <- month(ymd(data$date))
data$day <- day(ymd(data$date))

data$date <- as.Date(data$date)
start.date <- data$date[1]
end.date <- data$date[length(data$date)]
DumYear <- data.frame(as.Date(seq(as.Date(start.date), as.Date(end.date), by = 'day')))
colnames(DumYear) <- 'date'
data <- merge.data.frame(data, DumYear, by="date", all=TRUE)



# initialize variables ----------------------------------------------------
# This section will create a hydrograph that will zoom in on 3 month segments where error is high
# It does so for the top three highest error periods

#all_data puts model gage flows and corresponding dates in one data frame
all_data <- data.frame(data$date, data$model.flow, data$gage.flow) 
all_data$counter <- 1:length(all_data$data.date) # counter fixes issues with row numbers later on in script
colnames(all_data) <- c('Date', 'Model Flow', 'Gage Flow', 'Counter')

# find the first date for which data is collected, (in date format)
# and a date that is roughly one year and two months past the first date
YearStart <- as.character(as.Date(all_data$Date[1]))
fixer <- which(all_data$Date == paste0(year(YearStart)+1,"-11-30"))
YearEnd <- as.character(as.Date(all_data$Date[fixer]))

# YearStart_Row and YearEnd_Row are the rows corresponding the the YearStart and YearEnd dates
YearStart_Row <- as.numeric(which(all_data$Date==YearStart)) 
YearEnd_Row <- as.numeric(which(all_data$Date==YearEnd))

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

# start loops used for yearly and monthly data  -------------------------------------------------------------

loop <- as.numeric(round(length(data$date)/365, digits = 0))-1
for (i in 1:loop){                                # run loop for an entire data series
  year <- all_data[YearStart_Row:YearEnd_Row,] # specify year: 10-01-year1 to 11-30-year2
  m <- 1                                        # counter for nested loop
  
  MonthStart <- YearStart  # first date for 3 month timespan                      
  doi <- as.Date(MonthStart) 
  doi <- doi + seq(0,365,31) 
  # doi= date of interest. dummy variable just to create the function next.month
  next.month <- function(doi) as.Date(as.yearmon(doi) + 1/12) + as.numeric(as.Date(doi)-(as.Date(as.yearmon(doi))))
  
  #(re)initalize variables for the nested loop
  MonthEnd <-data.frame(next.month(doi));  # last date in 3 month timespan - used function to determine 3rd month
  MonthEnd <- MonthEnd[3,1]-2 # specifies end of month 3 as last date
  # (technically specifies 01 of month 4)
  
  # row numbers corresponding with start and end dates, as a number. See note below 
  MonthStart_Row <- as.numeric(which(as.Date(all_data$Date)==as.Date(MonthStart)))
  MonthEnd_Row <- as.numeric(which(as.Date(all_data$Date)==as.Date(MonthEnd)))
  
  # Note: Counter column is used here to specify which row starts MonthStart and MonthEnd_Row.
  # When rows are pulled from year row numbers are also pulled, 
  # so a counter must be used for proper row numbers.
  Start_new <- which(year$Counter==MonthStart_Row)
  End_new <- which(year$Counter==MonthEnd_Row)
  
  # begin nested loop
  for (m in 1:12){
    month_time <- year[Start_new:End_new ,]         #extract data for 3 month timespan within year of interest
    avgmonth_gage <- mean(month_time$`Gage Flow`)   # find average of gage flow for 3 months
    avgmonth_model <- mean(month_time$`Model Flow`) # find average of model flow for 3 months
    AvgMonthlyError[m,1] <- (avgmonth_gage - avgmonth_model)/ avgmonth_gage * 100  # percent error between gage and model
    
    MonthEnd<-as.Date(MonthEnd)
    MonthEnd<-MonthEnd+1
    MonthEndyear <- year(MonthEnd)   # Year associated with last month of extracted data 
    MonthEndmonth <- month(MonthEnd) # Month associated with last month of extracted data 
    
    # the next three lines are for the error calculations -- stop on 1st of month 4 (31 of month 3)
    # Note: this DOES include the 1st of the next month in error calculation
    # Put a control on what date the script advances by - if date is not 1st of month, reset it
    DateCheck <- as.Date(paste0(MonthEndyear,'-',MonthEndmonth,'-01'))
    if (MonthEnd != DateCheck)
      MonthEnd <- as.Date(paste0(MonthEndyear,'-', MonthEndmonth, '-01'))
    
    MonthEnd <- MonthEnd-1
    stopdate <- as.Date(MonthEnd)
    AvgMonthlyError[m,2] <- stopdate
    
    # Advance to next month or count
    MonthStart <- next.month(MonthStart)
    MonthEnd <- MonthEnd+1
    MonthEnd <- next.month(MonthEnd)
    MonthEnd <- MonthEnd-1
    StartMonth_Row <- which(as.Date(all_data$Date)==as.Date(MonthStart));     
    StartMonth_Row <- as.numeric(StartMonth_Row)
    EndMonth_Row <- which(as.Date(all_data$Date)==as.Date(MonthEnd));     
    EndMonth_Row <- as.numeric(EndMonth_Row)
    Start_new <- which(year$Counter==StartMonth_Row)
    End_new <- which(year$Counter==EndMonth_Row)
    m <- m + 1
  }
  
  Timespan_Error[x:y, 1] <- AvgMonthlyError[,1] # save the error entries from AvgMonthlyError
  Timespan_Error[x:y, 2] <- AvgMonthlyError[,2] # save the dates 
  
  # advance Timespan_Error for next run
  x <- x + 12  
  y <- y + 12
  
  YearStart <- as.Date(YearStart) + 365  # Advance 1 year
  YearEnd <- as.Date(YearEnd) + 365     # Advance 1 year & 2 months (from 10-01 to 11-30)
  
  # Put a control on what date the script advances by - if end date is not 11-30, reset it
  # - if begin date is not -10-01, reset it
  YearBeginyear <- year(YearStart)  # pull year of beginning year
  YearBeginCheck <- as.Date(paste0(YearBeginyear,'-10-01'))
  if (YearBeginyear != YearBeginCheck)     
    YearStart <- as.Date(paste0(YearBeginyear,'-10-01'))
  
  YearEndyear <- year(YearEnd)  # pull year of ending year
  YearEndCheck <- as.Date(paste0(YearEndyear,'-11-30'))
  if (YearEnd != YearEndCheck)     
    YearEnd <- as.Date(paste0(YearEndyear,'-11-30'))
  YearStart_Row <- which(as.Date(all_data$Date)== as.Date(YearStart))
  YearEnd_Row <- which(as.Date(all_data$Date) == as.Date(YearEnd))
  
  i <- i + 1
}

# This section of code will plot timeframes with high error.
# count the number of 3 month periods over 20% error, plot the highest 3 periods.

Timespan_Error$Logic <- Timespan_Error$Error>=20 | Timespan_Error$Error<= -20
HighError <- Timespan_Error[Timespan_Error$Logic=='TRUE',]
HighError<- HighError[order(abs(HighError$Error), decreasing = TRUE),]
names(HighError)<-c('Error', 'Date', 'Logic')

# pull data for each of these 3 month segments.
HighestErrors <- HighError[1:3,]
HighestErrors$Date <- as.Date(HighestErrors$Date)

# initalize variables for loop
erroryear <- data.frame(matrix(nrow=1,ncol=6))
errordates <- data.frame(matrix(nrow=1, ncol=2))
names(erroryear)<- c('endyear', 'endmonth', 'enddate', 'startyear', 'startmonth', 'startdate')
names(errordates)<- c('start date row', 'end date row')
q <- 1

for (q in 1:length(HighestErrors)){
  erroryear[q,1] <- year(HighestErrors$Date[q])  # ending year
  erroryear[q,2]<- month(HighestErrors$Date[q]) + 1 # ending month
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
  erroryear$enddate <- as.Date(erroryear$enddate)
  erroryear[q,6]<- as.Date(paste0(erroryear[q,4], '-', erroryear[q,5], '-01')) #startdate
  erroryear$startdate <- as.Date(erroryear$startdate)
  
  errordates[q,1]<- as.character(erroryear$startdate[q])
  errordates[q,2]<- as.character(erroryear$enddate[q]-1)
  errordates[q,3]<- which(as.Date(all_data$Date)==as.Date(errordates$`start date row`[q]))
  errordates[q,4]<- which(as.Date(all_data$Date)==as.Date(errordates$`end date row`[q]))
  
  plot1<-all_data[errordates$V3[q]:errordates$V4[q],]
  
  # # create and export a plot: 
  png(filename=paste0(container,"\\results\\user's_results\\",siteNo,"_vs_",RivSeg,
                     "\\Plots\\Fig.", q+7, "- Error.png"), 
      width=1400, height=950, units="px")
  
  gage_legend<- paste('Gage',siteNo)                        #Creates legend title for gage
  model_legend<-paste('Model River Seg. \n', RivSeg)     #Creates legend title for model
  error <- signif(HighestErrors$Error[q], digits=3)          #Create error variable to display on graph
  par(cex=3, lwd=2, mar=c(4.5,4.2,1.5,0.5))
  
  # for plotting: determine if gage or model is the max for y axis limits:
  yaxis<- max(plot1$`Model Flow`) > max(plot1$`Gage Flow`)
  if (yaxis == TRUE){
    ymax <- max(plot1$`Model Flow`)
  } else {
    ymax <- max(plot1$`Gage Flow`)
  }
  
  # plot for info of row q
  plot(as.Date(plot1$Date), plot1$`Gage Flow`, type='l', col="black", ylim=c(0,ymax),
       xlab=paste('Date Range:', errordates[q,1], ':', errordates[q,2], ' ', 'Error:', error,'%'), 
       ylab='Flow (cfs)', lwd=2) #USGS gage data
  lines(as.Date(plot1$Date), plot1$`Model Flow`, col="red", type='l', lwd=2)                 #Model data
  legend ("topright", legend=c(gage_legend,model_legend), col=c('black','red'), lty=1, bty='n', cex=1)
  dev.off()
  q <- q+1
}



# CREATES OUTPUT MATRIX -------------------------------------------------------
# also want to list the number of timespans that were over 20% error.
over20 <- signif(nrow(HighError)/nrow(Timespan_Error)*100, digits=3)
OUTPUT_MATRIX <- matrix(c(avg_gage, avg_model, over20), nrow=1, ncol=3)
rownames(OUTPUT_MATRIX) = c("Area Weighted Flow")
colnames(OUTPUT_MATRIX) = c('USGS', 'Model', 'Error>20 (%)')
overall_error <- signif((OUTPUT_MATRIX[1,1]-OUTPUT_MATRIX[1,2])/OUTPUT_MATRIX[1,1]*100, digits=3)
OUTPUT_MATRIX <- matrix(c(avg_gage, avg_model, over20, met00_PctError), nrow=1, ncol=4)
rownames(OUTPUT_MATRIX) = c("Area Weighted Flow")
colnames(OUTPUT_MATRIX) = c('USGS', 'Model', 'Error>20 (%)', 'Overall Error')

# OUTPUT MATRIX------------------------------------------------------------------------
OUTPUT_MATRIX
write.csv(OUTPUT_MATRIX, paste0(container,"\\results\\user's_results\\",siteNo,"_vs_",RivSeg,
                                "\\Tables\\Tab. 0 - High Errors.csv"))

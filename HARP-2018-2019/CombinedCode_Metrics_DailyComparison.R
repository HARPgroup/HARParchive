# DESCRIPTION -----
# Calculates numerous flow metrics (monthly minimums, monthly means, low flows, etc.) and outputs/saves plots.
# Compiled by Daniel Hildebrand
# 6/11/18
# Updated 6/22/18

# LOADING LIBRARIES -----
library(pander);
library(IHA);
library(PearsonDS);
library(httr);
library(dataRetrieval);
library(zoo);
library(lubridate);
library(lfstat);

# INPUTS -----
# Location of "USGStoModel" folder:
folder_location = "C:\\Users\\HaileyMae\\Downloads";

# USGS Gage Number
siteNo <- '02056900';

# Start and end dates for data to be imported and for calculation
start_date <- '1984-10-01';
end_date <- '2005-09-30';
# NOTE: fn_iha_mlf will fail if it lacks a complete water year, so date must end on 9-30

# start and end dates for hydrograph calculation
start.date <- start_date
end.date <- '2005-12-05'

# SOURCING CALCULATING FUNCTION -----
source(paste0(folder_location, "\\USGStoModel\\7Q10_ALF.R"));

# IMPORTING USGS DATA -----
pCode <- "00060";

# Importing data
USGS_daily <- readNWISdv(siteNo, pCode, start_date, end_date);
names(USGS_daily);
USGS_daily <- renameNWISColumns(USGS_daily);

# LINKING RIVER SEGMENT -----
# Finding river segment corresponding to USGS gage
GageToSegment <- read.csv(paste0(folder_location,"\\USGStoModel\\GageToSeg.csv"), 
                          header=TRUE, sep=',', stringsAsFactors=FALSE);
GageToSegment <- subset(GageToSegment, GageToSegment$Gage.Num==as.numeric(siteNo));
RivSeg <- GageToSegment$River.Segment;

#CREATE FOLDER 
dir.create(paste0(folder_location,"\\USGStoModel\\OUTPUT\\",siteNo, " vs ", RivSeg), showWarnings = TRUE)


# LOADING MODEL DATA FOR METRICS-----
# Splitting the River Segment string into each segment name
RivSegStr <- strsplit(RivSeg, "\\+")
RivSegStr <- RivSegStr[[1]]
num.segs <- length(RivSegStr)
model_days <- seq(as.Date(start_date):as.Date(end_date))
model_days <- 24*length(model_days) + 8784
# 8784 is the difference in the number of hours between a water year and the julian year containing it


# Reads data into data frame "ovols"
ovols <- setNames(data.frame(replicate(num.segs,sample(0, model_days, rep = TRUE))), sprintf("flow%s", seq(from = 1, to = num.segs, by = 1)))
seg_ctr <- 1
for (seg_ctr in 1:length(RivSegStr)) {
  model_hourly <- read.csv(paste0("http://deq2.bse.vt.edu/p532c-sova/wdm/river/p532cal_062211/stream/", 
                                  RivSegStr[seg_ctr], "_0111.csv"), header = FALSE, sep = ",", stringsAsFactors = FALSE);
  ovols[,paste0("flow",seg_ctr)] <- model_hourly$V5
  seg_ctr <- seg_ctr + 1
}

# Sums ovols into ovol column of model_hourly
model_hourly$V5 <- rowSums(ovols)

# Converting hourly to daily data
model_hourly <- model_hourly[-1,]
colnames(model_hourly) <- c("year","month","day","hour","ovol")
model_hourly$date <- as.Date(paste0(model_hourly$year,"-",model_hourly$month,"-",model_hourly$day))
model_daily_flow <- aggregate(model_hourly$ovol, list(model_hourly$date), FUN = sum)
colnames(model_daily_flow) <- c("date","ovol")

# Converts from acre-feet to cfs (which USGS uses) -- 0.504167 is conversion factor
model_daily_flow$ovol <- model_daily_flow$ovol * 0.504167

# Creates model_daily dataframe, used throughout this script
model_daily <- data.frame(c(1:length(model_daily_flow$date)),year(model_daily_flow$date), 
                          month(model_daily_flow$date), day(model_daily_flow$date), 
                          model_daily_flow$ovol, model_daily_flow$date)
colnames(model_daily) <- c("X","year", "month", "day", "ovol", "date")

# Subsetting proper time frame
start_model_daily = subset(model_daily, as.Date(model_daily$date)==as.Date(start_date));
start_model_daily = start_model_daily$X;
end_model_daily = subset(model_daily, as.Date(model_daily$date)==as.Date(end_date));
end_model_daily = end_model_daily$X;
model_daily = model_daily[c(start_model_daily:end_model_daily),];

# AREA-ADJUSTING MODEL DATA
# Loading USGS gage drainage area
GageArea <- readNWISsite(siteNo);
GageArea <- GageArea$drain_area_va;

# Loading model river segment area
ModelArea <- GageToSegment$GIS.Area;

# Area-adjusts model flow
model_daily$ovol <- model_daily$ovol*(GageArea/ModelArea);


# LOADING MODEL DATA FOR HYDRO --------------------------------------------

# Creates model_daily dataframe, used throughout this script
model_daily_WY <- data.frame(c(1:length(model_daily_flow$date)),year(model_daily_flow$date), 
                             month(model_daily_flow$date), day(model_daily_flow$date), 
                             model_daily_flow$ovol, model_daily_flow$date)
colnames(model_daily_WY) <- c("X","year", "month", "day", "flow", "date")

# subsetting the proper time frame for hydro
start_hydro_daily = subset(model_daily_WY, as.Date(model_daily_WY$date)==as.Date(start.date));
start_hydro_daily = start_hydro_daily$X;
end_hydro_daily = subset(model_daily_WY, as.Date(model_daily_WY$date)==as.Date(end.date));
end_hydro_daily = end_hydro_daily$X;
model_daily_WY = model_daily_WY[c(start_hydro_daily:end_hydro_daily),];

# load gage data for hydro
gage_data <- readNWISdv(siteNumbers = siteNo,
                        parameterCd = pCode,
                        startDate = start.date,
                        endDate = end.date)
# Cleans up names of gage data columns
#names(gage_data)
gage_data <- renameNWISColumns(gage_data)

# Area-adjusts model flow for hydro 
area_of_model <- model_daily_WY$flow*(GageArea/ModelArea);

# CALCULATES AVERAGE DAILY FLOW -----------------------------------------------------
avg_model_area<-mean(area_of_model)
avg_model_norm<-mean(model_daily_WY$flow) # without area weighting factor
avg_gage<-mean(gage_data$Flow)

# initialize variables ----------------------------------------------------
# This section will create a hydrograph that will zoom in on 3 month segments where error is high
# It does so for the top three highest error periods

#all_data puts model gage flows and corresponding dates in one data frame
all_data <- data.frame (gage_data$Date, area_of_model, gage_data$Flow) 
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

# start loops used for yearly and monthly data  -------------------------------------------------------------

# for gage 02068500, an error will be thrown -- it still gives the 13 years that are required. ignore. 
for (i in 1:21){                                # run loop for an entire data series
  year <- all_data[YearStart_Row:YearEnd_Row ,] # specify year: 10-01-year1 to 12-05-year2
  m <- 1                                        # counter for nested loop
  
  MonthStart <- YearStart  # first date for 3 month timespan                      
  doi <- as.Date(MonthStart) + seq(0,365,31) 
  # doi= date of interest. dummy variable just to create the function next.month
  next.month <- function(doi) as.Date(as.yearmon(doi) + 1/12) + as.numeric(doi-as.Date(as.yearmon(doi)))
  
  #(re)initalize variables for the nested loop
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
  
  # begin nested loop
  for (m in 1:12){
    month_time <- year[Start_new:End_new ,]         #extract data for 3 month timespan within year of interest
    avgmonth_gage <- mean(month_time$`Gage Flow`)   # find average of gage flow for 3 months
    avgmonth_model <- mean(month_time$`Model Flow`) # find average of model flow for 3 months
    AvgMonthlyError[m,1] <- (avgmonth_gage - avgmonth_model)/ avgmonth_gage * 100  # percent error between gage and model
    
    
    MonthEndyear <- year(MonthEnd)   # Year associated with last month of extracted data 
    MonthEndmonth <- month(MonthEnd) # Month associated with last month of extracted data 
    
    # the next three lines are for the error calculations -- stop on 1st of month 4 (31 of month 3)
    # Note: this DOES include the 1st of the next month in error calculation
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
  # - if begin date is not -10-01, reset it
  YearBeginyear <- year(YearStart)  # pull year of beginning year
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

Timespan_Error$Logic <- Timespan_Error$Error>=20 | Timespan_Error$Error<= -20
HighError <- Timespan_Error[Timespan_Error$Logic=='TRUE',]
HighError<- HighError[order(abs(HighError$Error), decreasing = TRUE),]
names(HighError)<-c('Error', 'Date', 'Logic')

# pull data for each of these 3 month segments.
HighestErrors <- HighError[1:3,]

# initalize variables for loop
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
  png(filename=paste0(folder_location,"\\USGStoModel\\OUTPUT\\",siteNo, " vs ", RivSeg, "\\Fig.", q, "- Error.png"), 
      width=1400, height=950, units="px")
  
  gage_legend<- paste('Gage',siteNo)                        #Creates legend title for gage
  model_legend<-paste('Model River Seg. \n', RivSeg)     #Creates legend title for model
  error <- round(HighestErrors$Error[q], digits=2)          #Create error variable to display on graph
  par(cex=3, lwd=2, mar=c(4.5,4.2,1.5,0.5))
  
  # for plotting: determine if gage or model is the max for y axis limits:
  yaxis<- max(plot1$`Model Flow`) > max(plot1$`Gage Flow`)
  if (yaxis == TRUE){
    ymax <- max(plot1$`Model Flow`)
  } else {
    ymax <- max(plot1$`Gage Flow`)
  }
  
  # plot for info of row q
  plot(plot1$Date, plot1$`Gage Flow`, type='l', col="red", ylim=c(0,ymax),
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
overall_error <- round((OUTPUT_MATRIX[1,1]-OUTPUT_MATRIX[1,2])/OUTPUT_MATRIX[1,1]*100, digits=2)
OUTPUT_MATRIX <- matrix(c(avg_gage, avg_model_area, Over_20, overall_error), nrow=1, ncol=4)
rownames(OUTPUT_MATRIX) = c("Area Weighted Flow")
colnames(OUTPUT_MATRIX) = c('USGS', 'Model', 'Error>20 (%)', 'Overall Error')


# OUTPUT MATRIX------------------------------------------------------------------------
OUTPUT_MATRIX
write.csv(OUTPUT_MATRIX, paste0(folder_location,"\\USGStoModel\\OUTPUT\\",siteNo," vs ",RivSeg,
                                "\\Output Matrix.csv"))













# SETUP FOR ___ DAY MIN CALCULATIONS
# Trimming gage data, converting to matrix
Flow_USGS <- USGS_daily[,c(3,4)];
Flow_USGS <- as.vector(Flow_USGS);
colnames(Flow_USGS) <- c("Date", "Flow");

# Running gage calculations for ___ day min
f3_USGS <- zoo(Flow_USGS$Flow, order.by = Flow_USGS$Date);
g2_USGS <- group2(f3_USGS, year = 'water');

# Trimming model data, converting to matrix
Flow_model <- model_daily[,c(6,5)];
Flow_model <- as.vector(Flow_model);
colnames(Flow_model) <- c("Date", "Flow");

# Running model calculations for ___ day min
f3_model <- zoo(Flow_model$Flow, order.by = Flow_model$Date);
g2_model <- group2(f3_model, year = 'water');

# SETUP FOR MONTHLY AVERAGES CALCULATION -----
USGS_daily$Month <- month(ymd(USGS_daily$Date));
USGS_Monthly_Means <- aggregate(USGS_daily$Flow, list(USGS_daily$Month), FUN = mean);
Model_Monthly_Means <- aggregate(model_daily$ovol, list(model_daily$month), FUN = mean);

# SETUP FOR MONTHLY MINS CALCULATION -----
flows_USGS <- zoo(USGS_daily[,"Flow"], order.by = USGS_daily$Date);
flows_model <- zoo(model_daily$ovol, order.by = model_daily$date);

# SETUP FOR FLOW EXCEEDENCE CALCULATIONS -----
# Creating vectors of decreasing flow magnitude
dec_flows_USGS <- sort(USGS_daily$Flow, decreasing = TRUE);
dec_flows_model <- sort(model_daily$ovol, decreasing = TRUE);

# Determining the "rank" (0-1) of the flow value
num_observations_model <- length(dec_flows_model);
num_observations_model <- as.numeric(num_observations_model);
rank_vec_model <- c(1:num_observations_model);
rank_vec_model <- as.numeric(rank_vec_model);

num_observations_USGS <- length(dec_flows_USGS);
num_observations_USGS <- as.numeric(num_observations_USGS);
rank_vec_USGS <- c(1:num_observations_USGS);
rank_vec_USGS <- as.numeric(rank_vec_USGS);

# Calculating exceedence probability
prob_exceedance_model <- 100*((rank_vec_model) / (num_observations_model + 1));
prob_exceedance_USGS <- 100*((rank_vec_USGS) / (num_observations_USGS + 1));

# Creating a vector of relevant quantiles
model_prob_exceedancev <- quantile(dec_flows_model, probs = c(0.01, 0.05, 0.5, 0.95, 0.99));
USGS_prob_exceedancev <- quantile(dec_flows_USGS, probs = c(0.01, 0.05, 0.5, 0.95, 0.99));

# SETUP FOR SEPT. 10% FLOW
sept_flows_USGS <- subset(USGS_daily, Month == "9");
sept_flows_model <- subset(model_daily, month == "9");
sept_quant_USGS <- quantile(sept_flows_USGS$Flow, 0.10);
sept_quant_model <- quantile(sept_flows_model$ovol, 0.10);

# SETUP FOR BASEFLOW CALCULATIONS -----
USGSgages <- data.frame(day(USGS_daily$Date),month(USGS_daily$Date),year(USGS_daily$Date),USGS_daily$Flow);
names(USGSgages) <- c('day', 'month', 'year', 'flow');

modeldata <- data.frame(model_daily$day, model_daily$month, model_daily$year, model_daily$ovol);
names(modeldata) <- c('day', 'month', 'year', 'flow');

# Creating low-flow objects
USGSriver <- createlfobj(USGSgages, hyearstart = 10, baseflow = TRUE, meta = NULL);
modelriver <- createlfobj(modeldata, hyearstart = 10, baseflow = TRUE, meta = NULL);

# removing NA values
USGSriver[is.na(USGSriver)] <- 0;
modelriver[is.na(modelriver)] <- 0;

# Adding date vectors
USGSriver$Date <- as.Date(paste0(USGSriver$year,"-",USGSriver$month,"-",USGSriver$day));
modelriver$Date <- as.Date(paste0(modelriver$year,"-",modelriver$month,"-",modelriver$day));

# CALCULATIONS -----
# OVERALL MEAN FLOW
met00_Gage_MeanFlow <- round(mean(USGS_daily$Flow)/0.1)*0.1;
met00_Model_MeanFlow <- round(mean(model_daily$ovol)/0.1)*0.1;
met00_PctError <- -(round(((met00_Model_MeanFlow - met00_Gage_MeanFlow) / met00_Gage_MeanFlow)*1000)*0.1);

# JANUARY LOW FLOW ------------------------------------------------------------
met01_Gage_JanLF <- round(fn_iha_mlf(flows_USGS,1)/0.1)*0.1;
met01_Mod_JanLF <- round(fn_iha_mlf(flows_model,1)/0.1)*0.1;
met01_PctError <- -(round(((met01_Mod_JanLF - met01_Gage_JanLF) / met01_Gage_JanLF)*1000)*0.1);

# FEBRUARY LOW FLOW -----------------------------------------------------------
met02_Gage_FebLF <- round(fn_iha_mlf(flows_USGS,2)/0.1)*0.1;
met02_Mod_FebLF <- round(fn_iha_mlf(flows_model,2)/0.1)*0.1;
met02_PctError <- -(round(((met02_Mod_FebLF - met02_Gage_FebLF) / met02_Gage_FebLF)*1000)*0.1);

# MARCH LOW FLOW --------------------------------------------------------------
met03_Gage_MarLF <- round(fn_iha_mlf(flows_USGS,3)/0.1)*0.1;
met03_Mod_MarLF <- round(fn_iha_mlf(flows_model,3)/0.1)*0.1;
met03_PctError <- -(round(((met03_Mod_MarLF - met03_Gage_MarLF) / met03_Gage_MarLF)*1000)*0.1);

# APRIL LOW FLOW --------------------------------------------------------------
met04_Gage_AprLF <- round(fn_iha_mlf(flows_USGS,4)/0.1)*0.1;
met04_Mod_AprLF <- round(fn_iha_mlf(flows_model,4)/0.1)*0.1;
met04_PctError <- -(round(((met04_Mod_AprLF - met04_Gage_AprLF) / met04_Gage_AprLF)*1000)*0.1);

# MAY LOW FLOW ----------------------------------------------------------------
met05_Gage_MayLF <- round(fn_iha_mlf(flows_USGS,5)/0.1)*0.1;
met05_Mod_MayLF <- round(fn_iha_mlf(flows_model,5)/0.1)*0.1;
met05_PctError <- -(round(((met05_Mod_MayLF - met05_Gage_MayLF) / met05_Gage_MayLF)*1000)*0.1);

# JUNE LOW FLOW ---------------------------------------------------------------
met06_Gage_JunLF <- round(fn_iha_mlf(flows_USGS,6)/0.1)*0.1;
met06_Mod_JunLF <- round(fn_iha_mlf(flows_model,6)/0.1)*0.1;
met06_PctError <- -(round(((met06_Mod_JunLF - met06_Gage_JunLF) / met06_Gage_JunLF)*1000)*0.1);

# JULY LOW FLOW ---------------------------------------------------------------
met07_Gage_JulLF <- round(fn_iha_mlf(flows_USGS,7)/0.1)*0.1;
met07_Mod_JulLF <- round(fn_iha_mlf(flows_model,7)/0.1)*0.1;
met07_PctError <- -(round(((met07_Mod_JulLF - met07_Gage_JulLF) / met07_Gage_JulLF)*1000)*0.1);

# AUGUST LOW FLOW -------------------------------------------------------------
met08_Gage_AugLF <- round(fn_iha_mlf(flows_USGS,8)/0.1)*0.1;
met08_Mod_AugLF <- round(fn_iha_mlf(flows_model,8)/0.1)*0.1;
met08_PctError <- -(round(((met08_Mod_AugLF - met08_Gage_AugLF) / met08_Gage_AugLF)*1000)*0.1);

# SEPTEMBER LOW FLOW ----------------------------------------------------------
met09_Gage_SepLF <- round(fn_iha_mlf(flows_USGS,9)/0.1)*0.1;
met09_Mod_SepLF <- round(fn_iha_mlf(flows_model,9)/0.1)*0.1;
met09_PctError <- -(round(((met09_Mod_SepLF - met09_Gage_SepLF) / met09_Gage_SepLF)*1000)*0.1);

# OCTOBER LOW FLOW ------------------------------------------------------------
met10_Gage_OctLF <- round(fn_iha_mlf(flows_USGS,10)/0.1)*0.1;
met10_Mod_OctLF <- round(fn_iha_mlf(flows_model,10)/0.1)*0.1;
met10_PctError <- -(round(((met10_Mod_OctLF - met10_Gage_OctLF) / met10_Gage_OctLF)*1000)*0.1);

# NOVEMBER LOW FLOW -----------------------------------------------------------
met11_Gage_NovLF <- round(fn_iha_mlf(flows_USGS,11)/0.1)*0.1;
met11_Mod_NovLF <- round(fn_iha_mlf(flows_model,11)/0.1)*0.1;
met11_PctError <- -(round(((met11_Mod_NovLF - met11_Gage_NovLF) / met11_Gage_NovLF)*1000)*0.1);

# DECEMBER LOW FLOW ------------------------------------------------------------
met12_Gage_DecLF <- round(fn_iha_mlf(flows_USGS,12)/0.1)*0.1;
met12_Mod_DecLF <- round(fn_iha_mlf(flows_model,12)/0.1)*0.1;
met12_PctError <- -(round(((met12_Mod_DecLF - met12_Gage_DecLF) / met12_Gage_DecLF)*1000)*0.1);

# JANUARY MEAN FLOW -----------------------------------------------------------
met13_Gage_JanMF <- round(USGS_Monthly_Means[1,2]/0.1)*0.1;
met13_Model_JanMF <- round(Model_Monthly_Means[1,2]/0.1)*0.1;
met13_PctError <- -(round(((met13_Model_JanMF - met13_Gage_JanMF) / met13_Gage_JanMF)*1000)*0.1);

# FEBRUARY MEAN FLOW ----------------------------------------------------------
met14_Gage_FebMF <- round(USGS_Monthly_Means[2,2]/0.1)*0.1;
met14_Model_FebMF <- round(Model_Monthly_Means[2,2]/0.1)*0.1;
met14_PctError <- -(round(((met14_Model_FebMF - met14_Gage_FebMF) / met14_Gage_FebMF)*1000)*0.1);

# MARCH MEAN FLOW -------------------------------------------------------------
met15_Gage_MarMF <- round(USGS_Monthly_Means[3,2]/0.1)*0.1;
met15_Model_MarMF <- round(Model_Monthly_Means[3,2]/0.1)*0.1;
met15_PctError <- -(round(((met15_Model_MarMF - met15_Gage_MarMF) / met15_Gage_MarMF)*1000)*0.1);

# APRIL MEAN FLOW -------------------------------------------------------------
met16_Gage_AprMF <- round(USGS_Monthly_Means[4,2]/0.1)*0.1;
met16_Model_AprMF <- round(Model_Monthly_Means[4,2]/0.1)*0.1;
met16_PctError <- -(round(((met16_Model_AprMF - met16_Gage_AprMF) / met16_Gage_AprMF)*1000)*0.1);

# MAY MEAN FLOW ---------------------------------------------------------------
met17_Gage_MayMF <- round(USGS_Monthly_Means[5,2]/0.1)*0.1;
met17_Model_MayMF <- round(Model_Monthly_Means[5,2]/0.1)*0.1;
met17_PctError <- -(round(((met17_Model_MayMF - met17_Gage_MayMF) / met17_Gage_MayMF)*1000)*0.1);

# JUNE MEAN FLOW --------------------------------------------------------------
met18_Gage_JunMF <- round(USGS_Monthly_Means[6,2]/0.1)*0.1;
met18_Model_JunMF <- round(Model_Monthly_Means[6,2]/0.1)*0.1;
met18_PctError <- -(round(((met18_Model_JunMF - met18_Gage_JunMF) / met18_Gage_JunMF)*1000)*0.1);

# JULY MEAN FLOW --------------------------------------------------------------
met19_Gage_JulMF <- round(USGS_Monthly_Means[7,2]/0.1)*0.1;
met19_Model_JulMF <- round(Model_Monthly_Means[7,2]/0.1)*0.1;
met19_PctError <- -(round(((met19_Model_JulMF - met19_Gage_JulMF) / met19_Gage_JulMF)*1000)*0.1);

# AUGUST MEAN FLOW ------------------------------------------------------------
met20_Gage_AugMF <- round(USGS_Monthly_Means[8,2]/0.1)*0.1;
met20_Model_AugMF <- round(Model_Monthly_Means[8,2]/0.1)*0.1;
met20_PctError <- -(round(((met20_Model_AugMF - met20_Gage_AugMF) / met20_Gage_AugMF)*1000)*0.1);

# SEPTEMBER MEAN FLOW ---------------------------------------------------------
met21_Gage_SepMF <- round(USGS_Monthly_Means[9,2]/0.1)*0.1;
met21_Model_SepMF <- round(Model_Monthly_Means[9,2]/0.1)*0.1;
met21_PctError <- -(round(((met21_Model_SepMF - met21_Gage_SepMF) / met21_Gage_SepMF)*1000)*0.1);

# OCTOBER MEAN FLOW -----------------------------------------------------------
met22_Gage_OctMF <- round(USGS_Monthly_Means[10,2]/0.1)*0.1;
met22_Model_OctMF <- round(Model_Monthly_Means[10,2]/0.1)*0.1;
met22_PctError <- -(round(((met22_Model_OctMF - met22_Gage_OctMF) / met22_Gage_OctMF)*1000)*0.1);

# NOVEMBER MEAN FLOW ----------------------------------------------------------
met23_Gage_NovMF <- round(USGS_Monthly_Means[11,2]/0.1)*0.1;
met23_Model_NovMF <- round(Model_Monthly_Means[11,2]/0.1)*0.1;
met23_PctError <- -(round(((met23_Model_NovMF - met23_Gage_NovMF) / met23_Gage_NovMF)*1000)*0.1);

# DECEMBER MEAN FLOW ----------------------------------------------------------
met24_Gage_DecMF <- round(USGS_Monthly_Means[12,2]/0.1)*0.1;
met24_Model_DecMF <- round(Model_Monthly_Means[12,2]/0.1)*0.1;
met24_PctError <- -(round(((met24_Model_DecMF - met24_Gage_DecMF) / met24_Gage_DecMF)*1000)*0.1);

# 1 DAY MIN --------------------------------------------------------------------
yearly_Gage_1DayMin <- g2_USGS[,c(1,2)];
met25_Gage_1DayMinMin <- round(min(yearly_Gage_1DayMin$`1 Day Min`)/0.1)*0.1;
yearly_Mod_1DayMin <- g2_model[,c(1,2)];
met25_Mod_1DayMinMin <- round(min(yearly_Mod_1DayMin$`1 Day Min`)/0.1)*0.1;
met25_PctError <- -(round(((met25_Mod_1DayMinMin - met25_Gage_1DayMinMin) / met25_Gage_1DayMinMin)*1000)*0.1);
met26_Gage_1DayMinMed <- round(median(yearly_Gage_1DayMin$`1 Day Min`)/0.1)*0.1;
met26_Mod_1DayMinMed <- round(median(yearly_Mod_1DayMin$`1 Day Min`)/0.1)*0.1;
met26_PctError <- -(round(((met26_Mod_1DayMinMed - met26_Gage_1DayMinMed) / met26_Gage_1DayMinMed)*1000)*0.1);

# 3 DAY MIN --------------------------------------------------------------------
yearly_Gage_3DayMin <- g2_USGS[,c(1,4)];
met27_Gage_3DayMinMin <- round(min(yearly_Gage_3DayMin$`3 Day Min`)/0.1)*0.1;
yearly_Mod_3DayMin <- g2_model[,c(1,4)];
met27_Mod_3DayMinMin <- round(min(yearly_Mod_3DayMin$`3 Day Min`)/0.1)*0.1;
met27_PctError <- -(round(((met27_Mod_3DayMinMin - met27_Gage_3DayMinMin) / met27_Gage_3DayMinMin)*1000)*0.1);
met28_Gage_3DayMinMed <- round(median(yearly_Gage_3DayMin$`3 Day Min`)/0.1)*0.1;
met28_Mod_3DayMinMed <- round(median(yearly_Mod_3DayMin$`3 Day Min`)/0.1)*0.1;
met28_PctError <- -(round(((met28_Mod_3DayMinMed - met28_Gage_3DayMinMed) / met28_Gage_3DayMinMed)*1000)*0.1);

# 7 DAY MIN --------------------------------------------------------------------
yearly_Gage_7DayMin <- g2_USGS[,c(1,6)];
met29_Gage_7DayMinMin <- round(min(yearly_Gage_7DayMin$`7 Day Min`)/0.1)*0.1;
yearly_Mod_7DayMin <- g2_model[,c(1,6)];
met29_Mod_7DayMinMin <- round(min(yearly_Mod_7DayMin$`7 Day Min`)/0.1)*0.1;
met29_PctError <- -(round(((met29_Mod_7DayMinMin - met29_Gage_7DayMinMin) / met29_Gage_7DayMinMin)*1000)*0.1);
met30_Gage_7DayMinMed <- round(median(yearly_Gage_7DayMin$`7 Day Min`)/0.1)*0.1;
met30_Mod_7DayMinMed <- round(median(yearly_Mod_7DayMin$`7 Day Min`)/0.1)*0.1;
met30_PctError <- -(round(((met30_Mod_7DayMinMed - met30_Gage_7DayMinMed) / met30_Gage_7DayMinMed)*1000)*0.1);

# 30 DAY MIN ------------------------------------------------------------------
yearly_Gage_30DayMin <- g2_USGS[,c(1,8)];
met31_Gage_30DayMinMin <- round(min(yearly_Gage_30DayMin$`30 Day Min`)/0.1)*0.1;
yearly_Mod_30DayMin <- g2_model[,c(1,8)];
met31_Mod_30DayMinMin <- round(min(yearly_Mod_30DayMin$`30 Day Min`)/0.1)*0.1;
met31_PctError <- -(round(((met31_Mod_30DayMinMin - met31_Gage_30DayMinMin) / met31_Gage_30DayMinMin)*1000)*0.1);
met32_Gage_30DayMinMed <- round(median(yearly_Gage_30DayMin$`30 Day Min`)/0.1)*0.1;
met32_Mod_30DayMinMed <- round(median(yearly_Mod_30DayMin$`30 Day Min`)/0.1)*0.1;
met32_PctError <- -(round(((met32_Mod_30DayMinMed - met32_Gage_30DayMinMed) / met32_Gage_30DayMinMed)*1000)*0.1);

# 90 DAY MIN ------------------------------------------------------------------
yearly_Gage_90DayMin <- g2_USGS[,c(1,10)];
met33_Gage_90DayMinMin <- round(min(yearly_Gage_90DayMin$`90 Day Min`)/0.1)*0.1;
yearly_Mod_90DayMin <- g2_model[,c(1,10)];
met33_Mod_90DayMinMin <- round(min(yearly_Mod_90DayMin$`90 Day Min`)/0.1)*0.1;
met33_PctError <- -(round(((met33_Mod_90DayMinMin - met33_Gage_90DayMinMin) / met33_Gage_90DayMinMin)*1000)*0.1);
met34_Gage_90DayMinMed <- round(median(yearly_Gage_90DayMin$`90 Day Min`)/0.1)*0.1;
met34_Mod_90DayMinMed <- round(median(yearly_Mod_90DayMin$`90 Day Min`)/0.1)*0.1;
met34_PctError <- -(round(((met34_Mod_90DayMinMed - met34_Gage_90DayMinMed) / met34_Gage_90DayMinMed)*1000)*0.1);

# 7Q10 ------------------------------------------------------------------------
met35_Gage_7Q10 <- round(fn_iha_7q10(flows_USGS)/0.1)*0.1;
met35_Model_7Q10 <- round(fn_iha_7q10(flows_model)/0.1)*0.1;
met35_PctError <- -(round(((met35_Model_7Q10 - met35_Gage_7Q10) / met35_Gage_7Q10)*1000)*0.1);

# DROUGHT OF RECORD (MIN. 90 DAY MIN.) YEAR ------------------------------------------------------
met37_Gage_DoR <- fn_iha_DOR_Year(flows_USGS);
met37_Model_DoR <- fn_iha_DOR_Year(flows_model);
if (met37_Gage_DoR == met37_Model_DoR) {
  met37_PctError <- 0;
} else {
  met37_PctError <- 100;
}

# 1% Non-exceedance Flow
met38_Gage_1NonEx <- round(USGS_prob_exceedancev[["1%"]]/0.1)*0.1;
met38_Model_1NonEx <- round(model_prob_exceedancev[["1%"]]/0.1)*0.1;
met38_PctError <- -(round(((met38_Model_1NonEx - met38_Gage_1NonEx) / met38_Gage_1NonEx)*1000)*0.1);

# 5% Non-exceedance Flow
met39_Gage_5NonEx <- round(USGS_prob_exceedancev[["5%"]]/0.1)*0.1;
met39_Model_5NonEx <- round(model_prob_exceedancev[["5%"]]/0.1)*0.1;
met39_PctError <- -(round(((met39_Model_5NonEx - met39_Gage_5NonEx) / met39_Gage_5NonEx)*1000)*0.1);

# 50% Non-exceedance Flow
met40_Gage_50NonEx <- round(USGS_prob_exceedancev[["50%"]]/0.1)*0.1;
met40_Model_50NonEx <- round(model_prob_exceedancev[["50%"]]/0.1)*0.1;
met40_PctError <- -(round(((met40_Model_50NonEx - met40_Gage_50NonEx) / met40_Gage_50NonEx)*1000)*0.1);

# 95% Non-exceedance Flow
met41_Gage_95NonEx <- round(USGS_prob_exceedancev[["95%"]]/0.1)*0.1;
met41_Model_95NonEx <- round(model_prob_exceedancev[["95%"]]/0.1)*0.1;
met41_PctError <- -(round(((met41_Model_95NonEx - met41_Gage_95NonEx) / met41_Gage_95NonEx)*1000)*0.1);

# 99% Non-exceedance Flow
met42_Gage_99NonEx <- round(USGS_prob_exceedancev[["99%"]]/0.1)*0.1;
met42_Model_99NonEx <- round(model_prob_exceedancev[["99%"]]/0.1)*0.1;
met42_PctError <- -(round(((met42_Model_99NonEx - met42_Gage_99NonEx) / met42_Gage_99NonEx)*1000)*0.1);

# Sept. 10% Flow
met43_Gage_Sep10 <- round(sept_quant_model[["10%"]]/0.1)*0.1;
met43_Model_Sep10 <- round(sept_quant_USGS[["10%"]]/0.1)*0.1;
met43_PctError <- -(round(((met43_Model_Sep10 - met43_Gage_Sep10) / met43_Gage_Sep10)*1000)*0.1);

# Baseflow (Average)
met44_Gage_Base <- round(mean(USGSriver$baseflow)/0.1)*0.1;
met44_Model_Base <- round(mean(modelriver$baseflow)/0.1)*0.1;
met44_PctError <- -(round(((met44_Model_Base - met44_Gage_Base) / met44_Gage_Base)*1000)*0.1);

# JANUARY HIGH FLOW ------------------------------------------------------------
met45_Gage_JanHF <- round(fn_iha_mhf(flows_USGS,1)/0.1)*0.1;
met45_Mod_JanHF <- round(fn_iha_mhf(flows_model,1)/0.1)*0.1;
met45_PctError <- -(round(((met45_Mod_JanHF - met45_Gage_JanHF) / met45_Gage_JanHF)*1000)*0.1);

# FEBRUARY HIGH FLOW -----------------------------------------------------------
met46_Gage_FebHF <- round(fn_iha_mhf(flows_USGS,2)/0.1)*0.1;
met46_Mod_FebHF <- round(fn_iha_mhf(flows_model,2)/0.1)*0.1;
met46_PctError <- -(round(((met46_Mod_FebHF - met46_Gage_FebHF) / met46_Gage_FebHF)*1000)*0.1);

# MARCH HIGH FLOW --------------------------------------------------------------
met47_Gage_MarHF <- round(fn_iha_mhf(flows_USGS,3)/0.1)*0.1;
met47_Mod_MarHF <- round(fn_iha_mhf(flows_model,3)/0.1)*0.1;
met47_PctError <- -(round(((met47_Mod_MarHF - met47_Gage_MarHF) / met47_Gage_MarHF)*1000)*0.1);

# APRIL HIGH FLOW --------------------------------------------------------------
met48_Gage_AprHF <- round(fn_iha_mhf(flows_USGS,4)/0.1)*0.1;
met48_Mod_AprHF <- round(fn_iha_mhf(flows_model,4)/0.1)*0.1;
met48_PctError <- -(round(((met48_Mod_AprHF - met48_Gage_AprHF) / met48_Gage_AprHF)*1000)*0.1);

# MAY HIGH FLOW ----------------------------------------------------------------
met49_Gage_MayHF <- round(fn_iha_mhf(flows_USGS,5)/0.1)*0.1;
met49_Mod_MayHF <- round(fn_iha_mhf(flows_model,5)/0.1)*0.1;
met49_PctError <- -(round(((met49_Mod_MayHF - met49_Gage_MayHF) / met49_Gage_MayHF)*1000)*0.1);

# JUNE HIGH FLOW ---------------------------------------------------------------
met50_Gage_JunHF <- round(fn_iha_mhf(flows_USGS,6)/0.1)*0.1;
met50_Mod_JunHF <- round(fn_iha_mhf(flows_model,6)/0.1)*0.1;
met50_PctError <- -(round(((met50_Mod_JunHF - met50_Gage_JunHF) / met50_Gage_JunHF)*1000)*0.1);

# JULY HIGH FLOW ---------------------------------------------------------------
met51_Gage_JulHF <- round(fn_iha_mhf(flows_USGS,7)/0.1)*0.1;
met51_Mod_JulHF <- round(fn_iha_mhf(flows_model,7)/0.1)*0.1;
met51_PctError <- -(round(((met51_Mod_JulHF - met51_Gage_JulHF) / met51_Gage_JulHF)*1000)*0.1);

# AUGUST HIGH FLOW -------------------------------------------------------------
met52_Gage_AugHF <- round(fn_iha_mhf(flows_USGS,8)/0.1)*0.1;
met52_Mod_AugHF <- round(fn_iha_mhf(flows_model,8)/0.1)*0.1;
met52_PctError <- -(round(((met52_Mod_AugHF - met52_Gage_AugHF) / met52_Gage_AugHF)*1000)*0.1);

# SEPTEMBER HIGH FLOW ----------------------------------------------------------
met53_Gage_SepHF <- round(fn_iha_mhf(flows_USGS,9)/0.1)*0.1;
met53_Mod_SepHF <- round(fn_iha_mhf(flows_model,9)/0.1)*0.1;
met53_PctError <- -(round(((met53_Mod_SepHF - met53_Gage_SepHF) / met53_Gage_SepHF)*1000)*0.1);

# OCTOBER HIGH FLOW ------------------------------------------------------------
met54_Gage_OctHF <- round(fn_iha_mhf(flows_USGS,10)/0.1)*0.1;
met54_Mod_OctHF <- round(fn_iha_mhf(flows_model,10)/0.1)*0.1;
met54_PctError <- -(round(((met54_Mod_OctHF - met54_Gage_OctHF) / met54_Gage_OctHF)*1000)*0.1);

# NOVEMBER HIGH FLOW -----------------------------------------------------------
met55_Gage_NovHF <- round(fn_iha_mhf(flows_USGS,11)/0.1)*0.1;
met55_Mod_NovHF <- round(fn_iha_mhf(flows_model,11)/0.1)*0.1;
met55_PctError <- -(round(((met55_Mod_NovHF - met55_Gage_NovHF) / met55_Gage_NovHF)*1000)*0.1);

# DECEMBER HIGH FLOW ------------------------------------------------------------
met56_Gage_DecHF <- round(fn_iha_mhf(flows_USGS,12)/0.1)*0.1;
met56_Mod_DecHF <- round(fn_iha_mhf(flows_model,12)/0.1)*0.1;
met56_PctError <- -(round(((met56_Mod_DecHF - met56_Gage_DecHF) / met56_Gage_DecHF)*1000)*0.1);

# 1 DAY MAX --------------------------------------------------------------------
yearly_Gage_1DayMax <- g2_USGS[,c(1,3)];
met57_Gage_1DayMaxMax <- round(max(yearly_Gage_1DayMax$`1 Day Max`)/0.1)*0.1;
yearly_Mod_1DayMax <- g2_model[,c(1,3)];
met57_Mod_1DayMaxMax <- round(max(yearly_Mod_1DayMax$`1 Day Max`)/0.1)*0.1;
met57_PctError <- -(round(((met57_Mod_1DayMaxMax - met57_Gage_1DayMaxMax) / met57_Gage_1DayMaxMax)*1000)*0.1);
met58_Gage_1DayMaxMed <- round(median(yearly_Gage_1DayMax$`1 Day Max`)/0.1)*0.1;
met58_Mod_1DayMaxMed <- round(median(yearly_Mod_1DayMax$`1 Day Max`)/0.1)*0.1;
met58_PctError <- -(round(((met58_Mod_1DayMaxMed - met58_Gage_1DayMaxMed) / met58_Gage_1DayMaxMed)*1000)*0.1);

# 3 DAY MAX --------------------------------------------------------------------
yearly_Gage_3DayMax <- g2_USGS[,c(1,5)];
met59_Gage_3DayMaxMax <- round(max(yearly_Gage_3DayMax$`3 Day Max`)/0.1)*0.1;
yearly_Mod_3DayMax <- g2_model[,c(1,5)];
met59_Mod_3DayMaxMax <- round(max(yearly_Mod_3DayMax$`3 Day Max`)/0.1)*0.1;
met59_PctError <- -(round(((met59_Mod_3DayMaxMax - met59_Gage_3DayMaxMax) / met59_Gage_3DayMaxMax)*1000)*0.1);
met60_Gage_3DayMaxMed <- round(median(yearly_Gage_3DayMax$`3 Day Max`)/0.1)*0.1;
met60_Mod_3DayMaxMed <- round(median(yearly_Mod_3DayMax$`3 Day Max`)/0.1)*0.1;
met60_PctError <- -(round(((met60_Mod_3DayMaxMed - met60_Gage_3DayMaxMed) / met60_Gage_3DayMaxMed)*1000)*0.1);

# 7 DAY MAX --------------------------------------------------------------------
yearly_Gage_7DayMax <- g2_USGS[,c(1,7)];
met61_Gage_7DayMaxMax <- round(max(yearly_Gage_7DayMax$`7 Day Max`)/0.1)*0.1;
yearly_Mod_7DayMax <- g2_model[,c(1,7)];
met61_Mod_7DayMaxMax <- round(max(yearly_Mod_7DayMax$`7 Day Max`)/0.1)*0.1;
met61_PctError <- -(round(((met61_Mod_7DayMaxMax - met61_Gage_7DayMaxMax) / met61_Gage_7DayMaxMax)*1000)*0.1);
met62_Gage_7DayMaxMed <- round(median(yearly_Gage_7DayMax$`7 Day Max`)/0.1)*0.1;
met62_Mod_7DayMaxMed <- round(median(yearly_Mod_7DayMax$`7 Day Max`)/0.1)*0.1;
met62_PctError <- -(round(((met62_Mod_7DayMaxMed - met62_Gage_7DayMaxMed) / met62_Gage_7DayMaxMed)*1000)*0.1);

# 30 DAY MAX ------------------------------------------------------------------
yearly_Gage_30DayMax <- g2_USGS[,c(1,9)];
met63_Gage_30DayMaxMax <- round(max(yearly_Gage_30DayMax$`30 Day Max`)/0.1)*0.1;
yearly_Mod_30DayMax <- g2_model[,c(1,9)];
met63_Mod_30DayMaxMax <- round(max(yearly_Mod_30DayMax$`30 Day Max`)/0.1)*0.1;
met63_PctError <- -(round(((met63_Mod_30DayMaxMax - met63_Gage_30DayMaxMax) / met63_Gage_30DayMaxMax)*1000)*0.1);
met64_Gage_30DayMaxMed <- round(median(yearly_Gage_30DayMax$`30 Day Max`)/0.1)*0.1;
met64_Mod_30DayMaxMed <- round(median(yearly_Mod_30DayMax$`30 Day Max`)/0.1)*0.1;
met64_PctError <- -(round(((met64_Mod_30DayMaxMed - met64_Gage_30DayMaxMed) / met64_Gage_30DayMaxMed)*1000)*0.1);

# 90 DAY MAX ------------------------------------------------------------------
yearly_Gage_90DayMax <- g2_USGS[,c(1,11)];
met65_Gage_90DayMaxMax <- round(max(yearly_Gage_90DayMax$`90 Day Max`)/0.1)*0.1;
yearly_Mod_90DayMax <- g2_model[,c(1,11)];
met65_Mod_90DayMaxMax <- round(max(yearly_Mod_90DayMax$`90 Day Max`)/0.1)*0.1;
met65_PctError <- -(round(((met65_Mod_90DayMaxMax - met65_Gage_90DayMaxMax) / met65_Gage_90DayMaxMax)*1000)*0.1);
met66_Gage_90DayMaxMed <- round(median(yearly_Gage_90DayMax$`90 Day Max`)/0.1)*0.1;
met66_Mod_90DayMaxMed <- round(median(yearly_Mod_90DayMax$`90 Day Max`)/0.1)*0.1;
met66_PctError <- -(round(((met66_Mod_90DayMaxMed - met66_Gage_90DayMaxMed) / met66_Gage_90DayMaxMed)*1000)*0.1);

# PLOTTING -----
# Creates directory to store plots and tables
dir.create(paste0(folder_location,"\\USGStoModel\\OUTPUT\\",siteNo," vs ",RivSeg), showWarnings = FALSE);

# Creating names for plot legends
name_USGS <- paste('Gage', siteNo);
name_model <- paste('Model: River Seg.\n', RivSeg);

# Basic hydrograph -----
# Max/min for y axis scaling
max <- max(c(max(USGS_daily$Flow), max(model_daily$ovol)));
min <- min(c(min(USGS_daily$Flow), min(model_daily$ovol)));
if (min < 1) {
  min <- 1;
}

# Creating and exporting plot
png(filename=paste0(folder_location,"\\USGStoModel\\OUTPUT\\",siteNo," vs ",RivSeg,
                    "\\Fig. 2 - Hydrograph.png"),
    width = 1400, height = 950, units = "px");
par(cex = 3, lwd = 2, mar = c(4.5,4.2,1.5,0.5));
plot(as.Date(model_daily$date), model_daily$ovol, type = 'l', col = 'blue', 
     ylim = c(min, max),
     xlab = 'Date', ylab = 'Flow (cfs)', log = 'y');
lines(as.Date(USGS_daily$Date), USGS_daily$Flow, col = 'red');
legend('topright', legend = c(name_USGS, name_model),
       bty = 'y', col = c('red','blue'), lwd = 2, cex = 0.7, 
       y.intersp = 1, x.intersp = 0.8);
dev.off();

# Zoomed hydrograph in year of lowest 90-year flow -----
low.year <- subset(yearly_Gage_90DayMin, yearly_Gage_90DayMin$`90 Day Min`==min(yearly_Gage_90DayMin$`90 Day Min`));
low.year <- low.year$year;
low.year.USGS <- subset(USGS_daily, year(as.Date(USGS_daily$Date))==low.year);
low.year.model <- subset(model_daily, model_daily$year == low.year);

# Scaling using max/min
max <- max(c(max(low.year.USGS$Flow), max(low.year.model$ovol)));
min <- min(c(min(low.year.USGS$Flow), min(low.year.model$ovol)));
if (min < 1) {
  min <- 1;
}

# Creating and exporting plot
png(filename=paste0(folder_location,"\\USGStoModel\\OUTPUT\\",siteNo," vs ",RivSeg,
                    "\\Fig. 4 - Zoomed Hydrograph.png"),
    width = 1400, height = 600, units = "px");
par(cex = 3, lwd = 2, mar = c(4.5,4.2,1.5,0.5));
plot(as.Date(low.year.model$date), low.year.model$ovol, type = 'l', col = 'blue', 
     ylim = c(min, max),
     xlab = paste('Drought of Record Year:', low.year), ylab = 'Flow (cfs)', log = 'y');
lines(as.Date(low.year.USGS$Date), low.year.USGS$Flow, col = 'red');
legend('top', legend = c(name_USGS, name_model),
       bty = 'y', col = c('red','blue'), lwd = 2, cex = 0.5, 
       y.intersp = 1.1, x.intersp = 0.25);
dev.off();

# Flow exceedance plot -----
# Determining max flow value for exceedance plot scale
max <- max(c(max(dec_flows_model), max(dec_flows_USGS)));
min <- min(c(min(dec_flows_model), min(dec_flows_USGS)));
if (min < 1) {
  min <- 1;
}

# Creating and exporting plot
png(filename=paste0(folder_location,"\\USGStoModel\\OUTPUT\\",siteNo," vs ",RivSeg,
                    "\\Fig. 3  - Prob. Exceedance.png"),
    width = 1400, height = 900, units = "px");
par(cex = 3, lwd = 2.75, mar = c(4.5,4.2,1.5,0.5));
plot(prob_exceedance_model, dec_flows_model, type = 'l', col = 'blue', 
     log = 'y', ylim = c(min, max), xlim = c(0,100),
     xlab = 'Probability of Exceedance (%)', ylab = 'Flow (cfs)');
lines(prob_exceedance_USGS, dec_flows_USGS, col = 'red');
legend('topright', legend = c(name_USGS, name_model),
       bty = 'y', col = c('red','blue'), lwd = 2, cex = 0.8, 
       y.intersp = 1, x.intersp = 0.6);
dev.off();

# Baseflow Indiviudal Graph -----
# Determining max flow value for plot scale
max <- 10*max(c(max(USGSriver$baseflow), max(modelriver$baseflow)));
min <- min(c(min(USGSriver$baseflow), min(modelriver$baseflow)));
if (min < 1) {
  min <- 1;
}

# Creating and exporting plot
png(filename=paste0(folder_location,"\\USGStoModel\\OUTPUT\\",siteNo," vs ",RivSeg,
                    "\\Fig. 5 - Baseflow.png"),
    width = 1400, height = 900, units = "px");
par(mfrow = c(1,1));
par(cex = 3, lwd = 2.5, mar = c(4.5,4.2,1.5,0.5));
plot(modelriver$Date, modelriver$baseflow, type = 'l', col = 'blue', log = 'y', ylim = c(min, max),
     xlab = 'Date', ylab = 'Flow (cfs)');
lines(USGSriver$Date, USGSriver$baseflow, type = 'l', col = 'red');
legend('bottomright', legend = c(name_USGS, name_model),
       bty = 'y', col = c('red','blue'), lwd = 2, cex = 0.6, 
       y.intersp = 1.19, x.intersp = 0.5);
dev.off();

# Baseflow Combined Graph -----
# Determining max flow value for plot scale
max <- max(c(max(USGSriver$baseflow), max(modelriver$baseflow), max(USGSriver$flow), max(modelriver$flow)));
min <- min(c(min(USGSriver$baseflow), min(modelriver$baseflow), min(USGSriver$flow), min(modelriver$flow)));
if (min < 1) {
  min <- 1;
}

# Creating and exporting plot
png(filename=paste0(folder_location,"\\USGStoModel\\OUTPUT\\",siteNo," vs ",RivSeg,
                    "\\Fig. 6 - Baseflow and Flow.png"),
    width = 1400, height = 900, units = "px");
par(mfrow = c(1,1));
par(cex = 3, lwd = 2, mar = c(4.5,4.2,1.5,0.5));
plot(USGSriver$Date, USGSriver$flow, type = 'l', col = 'black', ylim = c(min, max),
     xlab = 'Date', ylab = 'Flow (cfs)', log = 'y');
lines(modelriver$Date, modelriver$flow, type = 'l', col = 'grey');
lines(USGSriver$Date, USGSriver$baseflow, type = 'l', col = 'red');
lines(modelriver$Date, modelriver$baseflow, type = 'l', col = 'blue');
legend('bottomright', legend = c(paste("Flow:", name_USGS), paste("Flow:", name_model), 
                                 paste("Baseflow:", name_USGS), paste("Baseflow:", name_model)),
       bty = 'y', col = c('black', 'grey', 'red','blue'), lwd = 2, cex = 0.6, 
       y.intersp = 1.19, x.intersp = 0.5);
dev.off();

# OUTPUTTING MATRICES -----
# All metrics, in a row
ALL_METRICS <- matrix(c(met00_Gage_MeanFlow, met00_Model_MeanFlow, met00_PctError,
                        met01_Gage_JanLF, met01_Mod_JanLF, met01_PctError,
                        met02_Gage_FebLF, met02_Mod_FebLF, met02_PctError,
                        met03_Gage_MarLF, met03_Mod_MarLF, met03_PctError,
                        met04_Gage_AprLF, met04_Mod_AprLF, met04_PctError,
                        met05_Gage_MayLF, met05_Mod_MayLF, met05_PctError,
                        met06_Gage_JunLF, met06_Mod_JunLF, met06_PctError,
                        met07_Gage_JulLF, met07_Mod_JulLF, met07_PctError,
                        met08_Gage_AugLF, met08_Mod_AugLF, met08_PctError,
                        met09_Gage_SepLF, met09_Mod_SepLF, met09_PctError,
                        met10_Gage_OctLF, met10_Mod_OctLF, met10_PctError,
                        met11_Gage_NovLF, met11_Mod_NovLF, met11_PctError,
                        met12_Gage_DecLF, met12_Mod_DecLF, met12_PctError,
                        met13_Gage_JanMF, met13_Model_JanMF, met13_PctError,
                        met14_Gage_FebMF, met14_Model_FebMF, met14_PctError,
                        met15_Gage_MarMF, met15_Model_MarMF, met15_PctError,
                        met16_Gage_AprMF, met16_Model_AprMF, met16_PctError,
                        met17_Gage_MayMF, met17_Model_MayMF, met17_PctError,
                        met18_Gage_JunMF, met18_Model_JunMF, met18_PctError,
                        met19_Gage_JulMF, met19_Model_JulMF, met19_PctError,
                        met20_Gage_AugMF, met20_Model_AugMF, met20_PctError,
                        met21_Gage_SepMF, met21_Model_SepMF, met21_PctError,
                        met22_Gage_OctMF, met22_Model_OctMF, met22_PctError,
                        met23_Gage_NovMF, met23_Model_NovMF, met23_PctError,
                        met24_Gage_DecMF, met24_Model_DecMF, met24_PctError,
                        met25_Gage_1DayMinMin, met25_Mod_1DayMinMin, met25_PctError,
                        met26_Gage_1DayMinMed, met26_Mod_1DayMinMed, met26_PctError,
                        met27_Gage_3DayMinMin, met27_Mod_3DayMinMin, met27_PctError,
                        met28_Gage_3DayMinMed, met28_Mod_3DayMinMed, met28_PctError,
                        met29_Gage_7DayMinMin, met29_Mod_7DayMinMin, met29_PctError,
                        met30_Gage_7DayMinMed, met30_Mod_7DayMinMed, met30_PctError,
                        met31_Gage_30DayMinMin, met31_Mod_30DayMinMin, met31_PctError,
                        met32_Gage_30DayMinMed, met32_Mod_30DayMinMed, met32_PctError,
                        met33_Gage_90DayMinMin, met33_Mod_90DayMinMin, met33_PctError,
                        met34_Gage_90DayMinMed, met34_Mod_90DayMinMed, met34_PctError,
                        met35_Gage_7Q10, met35_Model_7Q10, met35_PctError,
                        met37_Gage_DoR, met37_Model_DoR, met37_PctError,
                        met38_Gage_1NonEx, met38_Model_1NonEx, met38_PctError,
                        met39_Gage_5NonEx, met39_Model_5NonEx, met39_PctError,
                        met40_Gage_50NonEx, met40_Model_50NonEx, met40_PctError,
                        met41_Gage_95NonEx, met41_Model_95NonEx, met41_PctError,
                        met42_Gage_99NonEx, met42_Model_99NonEx, met42_PctError,
                        met43_Gage_Sep10, met43_Model_Sep10, met43_PctError,
                        met44_Gage_Base, met44_Model_Base, met44_PctError,
                        met45_Gage_JanHF, met45_Mod_JanHF, met45_PctError,
                        met46_Gage_FebHF, met46_Mod_FebHF, met46_PctError,
                        met47_Gage_MarHF, met47_Mod_MarHF, met47_PctError,
                        met48_Gage_AprHF, met48_Mod_AprHF, met48_PctError,
                        met49_Gage_MayHF, met49_Mod_MayHF, met49_PctError,
                        met50_Gage_JunHF, met50_Mod_JunHF, met50_PctError,
                        met51_Gage_JulHF, met51_Mod_JulHF, met51_PctError,
                        met52_Gage_AugHF, met52_Mod_AugHF, met52_PctError,
                        met53_Gage_SepHF, met53_Mod_SepHF, met53_PctError,
                        met54_Gage_OctHF, met54_Mod_OctHF, met54_PctError,
                        met55_Gage_NovHF, met55_Mod_NovHF, met55_PctError,
                        met56_Gage_DecHF, met56_Mod_DecHF, met56_PctError,
                        met57_Gage_1DayMaxMax, met57_Mod_1DayMaxMax, met57_PctError,
                        met58_Gage_1DayMaxMed, met58_Mod_1DayMaxMed, met58_PctError,
                        met59_Gage_3DayMaxMax, met59_Mod_3DayMaxMax, met59_PctError,
                        met60_Gage_3DayMaxMed, met60_Mod_3DayMaxMed, met60_PctError,
                        met61_Gage_7DayMaxMax, met61_Mod_7DayMaxMax, met61_PctError,
                        met62_Gage_7DayMaxMed, met62_Mod_7DayMaxMed, met62_PctError,
                        met63_Gage_30DayMaxMax, met63_Mod_30DayMaxMax, met63_PctError,
                        met64_Gage_30DayMaxMed, met64_Mod_30DayMaxMed, met64_PctError,
                        met65_Gage_90DayMaxMax, met65_Mod_90DayMaxMax, met65_PctError,
                        met66_Gage_90DayMaxMed, met66_Mod_90DayMaxMed, met66_PctError),
                      nrow = 3, ncol = 66);
rownames(ALL_METRICS) = c("USGS Gage", "Model", "Pct. Error");
colnames(ALL_METRICS) = c("Overall Mean Flow", "Jan. Low Flow", 
                          "Feb. Low Flow",
                          "Mar. Low Flow", "Apr. Low Flow",
                          "May Low Flow", "Jun. Low Flow",
                          "Jul. Low Flow", "Aug. Low Flow",
                          "Sep. Low Flow", "Oct. Low Flow",
                          "Nov. Low Flow", "Dec. Low Flow",
                          "Jan. Mean Flow", "Feb. Mean Flow",
                          "Mar. Mean Flow", "Apr. Mean Flow",
                          "May Mean Flow", "Jun. Mean Flow",
                          "Jul. Mean Flow", "Aug. Mean Flow",
                          "Sep. Mean Flow", "Oct. Mean Flow",
                          "Nov. Mean Flow", "Dec. Mean Flow",
                          "Min. 1 Day Min", "Med. 1 Day Min.", 
                          "Min. 3 Day Min", "Med. 3 Day Min.",
                          "Min. 7 Day Min", "Med. 7 Day Min.",
                          "Min. 30 Day Min", "Med. 30 Day Min.",
                          "Min. 90 Day Min", "Med. 90 Day Min.", 
                          "7Q10", "DoR Year",
                          "1% Non-Exceedance", "5% Non-Exceedance",
                          "50% Non-Exceedance", "95% Non-Exceedance",
                          "99% Non-Exceedance", "Sept. 10% Non-Exceedance",
                          "Mean Baseflow", "Jan. High Flow", 
                          "Feb. High Flow",
                          "Mar. High Flow", "Apr. High Flow",
                          "May High Flow", "Jun. High Flow",
                          "Jul. High Flow", "Aug. High Flow",
                          "Sep. High Flow", "Oct. High Flow",
                          "Nov. High Flow", "Dec. High Flow",
                          "Max. 1 Day Max", "Med. 1 Day Max.", 
                          "Max. 3 Day Max", "Med. 3 Day Max.",
                          "Max. 7 Day Max", "Med. 7 Day Max.",
                          "Max. 30 Day Max", "Med. 30 Day Max.",
                          "Max. 90 Day Max", "Med. 90 Day Max.");
write.csv(ALL_METRICS, paste0(folder_location,"\\USGStoModel\\OUTPUT\\",siteNo," vs ",RivSeg,
                              "\\All_Metrics.csv"));

# Table 1: Monthly Average Flow
Table1 <- matrix(c(met00_Gage_MeanFlow, met13_Gage_JanMF, met14_Gage_FebMF,
                   met15_Gage_MarMF, met16_Gage_AprMF, met17_Gage_MayMF,
                   met18_Gage_JunMF, met19_Gage_JulMF, met20_Gage_AugMF,
                   met21_Gage_SepMF, met22_Gage_OctMF, met23_Gage_NovMF,
                   met24_Gage_DecMF, met00_Model_MeanFlow, met13_Model_JanMF,
                   met14_Model_FebMF, met15_Model_MarMF, met16_Model_AprMF,
                   met17_Model_MayMF, met18_Model_JunMF, met19_Model_JulMF,
                   met20_Model_AugMF, met21_Model_SepMF, met22_Model_OctMF,
                   met23_Model_NovMF, met24_Model_DecMF, met00_PctError,
                   met13_PctError, met14_PctError, met15_PctError,
                   met16_PctError, met17_PctError, met18_PctError,
                   met19_PctError, met20_PctError, met21_PctError,
                   met22_PctError, met23_PctError, met24_PctError),
                 nrow = 13, ncol = 3);
colnames(Table1) = c("USGS Gage", "Model", "Pct. Error");
rownames(Table1) = c("Overall Mean Flow", 
                     "Jan. Mean Flow", "Feb. Mean Flow",
                     "Mar. Mean Flow", "Apr. Mean Flow",
                     "May Mean Flow", "Jun. Mean Flow",
                     "Jul. Mean Flow", "Aug. Mean Flow",
                     "Sep. Mean Flow", "Oct. Mean Flow",
                     "Nov. Mean Flow", "Dec. Mean Flow");
write.csv(Table1, paste0(folder_location,"\\USGStoModel\\OUTPUT\\",siteNo," vs ",RivSeg,
                         "\\Tab. 1 - Monthly Mean Flows.csv"));

# Table 2: Monthly Low Flow
Table2 <- matrix(c(met01_Gage_JanLF, met02_Gage_FebLF, met03_Gage_MarLF,
                   met04_Gage_AprLF, met05_Gage_MayLF, met06_Gage_JunLF,
                   met07_Gage_JulLF, met08_Gage_AugLF, met09_Gage_SepLF,
                   met10_Gage_OctLF, met11_Gage_NovLF, met12_Gage_DecLF,
                   met01_Mod_JanLF, met02_Mod_FebLF, met03_Mod_MarLF, 
                   met04_Mod_AprLF, met05_Mod_MayLF, met06_Mod_JunLF,
                   met07_Mod_JulLF, met08_Mod_AugLF, met09_Mod_SepLF,
                   met10_Mod_OctLF, met11_Mod_NovLF, met12_Mod_DecLF,
                   met01_PctError, met02_PctError, met03_PctError,
                   met04_PctError, met05_PctError, met06_PctError,
                   met07_PctError, met08_PctError, met09_PctError,
                   met10_PctError, met11_PctError, met12_PctError),
                 nrow = 12, ncol = 3);
colnames(Table2) = c("USGS Gage", "Model", "Pct. Error");
rownames(Table2) = c("Jan. Low Flow", "Feb. Low Flow",
                     "Mar. Low Flow", "Apr. Low Flow",
                     "May Low Flow", "Jun. Low Flow",
                     "Jul. Low Flow", "Aug. Low Flow",
                     "Sep. Low Flow", "Oct. Low Flow",
                     "Nov. Low Flow", "Dec. Low Flow");
write.csv(Table2, paste0(folder_location,"\\USGStoModel\\OUTPUT\\",siteNo," vs ",RivSeg,
                         "\\Tab. 2 - Monthly Low Flows.csv"));

# Table 3: Monthly High Flow
Table3 <- matrix(c(met45_Gage_JanHF, met46_Gage_FebHF, met47_Gage_MarHF,
                   met48_Gage_AprHF, met49_Gage_MayHF, met50_Gage_JunHF,
                   met51_Gage_JulHF, met52_Gage_AugHF, met53_Gage_SepHF,
                   met54_Gage_OctHF, met55_Gage_NovHF, met56_Gage_DecHF,
                   met45_Mod_JanHF, met46_Mod_FebHF, met47_Mod_MarHF,
                   met48_Mod_AprHF, met49_Mod_MayHF, met50_Mod_JunHF,
                   met51_Mod_JulHF, met52_Mod_AugHF, met53_Mod_SepHF,
                   met54_Mod_OctHF, met55_Mod_NovHF, met56_Mod_DecHF,
                   met45_PctError, met46_PctError, met47_PctError,
                   met48_PctError, met49_PctError, met50_PctError,
                   met51_PctError, met52_PctError, met53_PctError,
                   met54_PctError, met55_PctError, met56_PctError),
                 nrow = 12, ncol = 3);
colnames(Table3) = c("USGS Gage", "Model", "Pct. Error");
rownames(Table3) = c("Jan. High Flow", "Feb. High Flow",
                     "Mar. High Flow", "Apr. High Flow",
                     "May High Flow", "Jun. High Flow",
                     "Jul. High Flow", "Aug. High Flow",
                     "Sep. High Flow", "Oct. High Flow",
                     "Nov. High Flow", "Dec. High Flow");
write.csv(Table3, paste0(folder_location,"\\USGStoModel\\OUTPUT\\",siteNo," vs ",RivSeg,
                         "\\Tab. 3 - Monthly High Flows.csv"));

# Table 4: Period Low Flows
Table4 <- matrix(c(met25_Gage_1DayMinMin, met26_Gage_1DayMinMed,
                   met27_Gage_3DayMinMin, met28_Gage_3DayMinMed,
                   met29_Gage_7DayMinMin, met30_Gage_7DayMinMed,
                   met31_Gage_30DayMinMin, met32_Gage_30DayMinMed,
                   met33_Gage_90DayMinMin, met34_Gage_90DayMinMed,
                   met35_Gage_7Q10, met37_Gage_DoR,
                   met44_Gage_Base, met25_Mod_1DayMinMin, met26_Mod_1DayMinMed,
                   met27_Mod_3DayMinMin, met28_Mod_3DayMinMed,
                   met29_Mod_7DayMinMin, met30_Mod_7DayMinMed,
                   met31_Mod_30DayMinMin, met32_Mod_30DayMinMed,
                   met33_Mod_90DayMinMin, met34_Mod_90DayMinMed,
                   met35_Model_7Q10, met37_Model_DoR,
                   met44_Model_Base, met25_PctError, met26_PctError, met27_PctError,
                   met28_PctError, met29_PctError, met30_PctError,
                   met31_PctError, met32_PctError, met33_PctError,
                   met34_PctError, met35_PctError,
                   met37_PctError, met44_PctError), nrow = 13, ncol = 3);
colnames(Table4) = c("USGS Gage", "Model", "Pct. Error");
rownames(Table4) = c("Min. 1 Day Min", "Med. 1 Day Min", 
                     "Min. 3 Day Min", "Med. 3 Day Min",
                     "Min. 7 Day Min", "Med. 7 Day Min",
                     "Min. 30 Day Min", "Med. 30 Day Min",
                     "Min. 90 Day Min", "Med. 90 Day Min", 
                     "7Q10", "Drought of Record (Min. 90-Day Min.) Year",
                     "Mean Baseflow");
write.csv(Table4, paste0(folder_location,"\\USGStoModel\\OUTPUT\\",siteNo," vs ",RivSeg,
                         "\\Tab. 4 - Period Low Flows.csv"));

# Table 5: Period High Flows
Table5 <- matrix(c(met57_Gage_1DayMaxMax, met58_Gage_1DayMaxMed,
                   met59_Gage_3DayMaxMax, met60_Gage_3DayMaxMed,
                   met61_Gage_7DayMaxMax, met62_Gage_7DayMaxMed,
                   met63_Gage_30DayMaxMax, met64_Gage_30DayMaxMed,
                   met65_Gage_90DayMaxMax, met66_Gage_90DayMaxMed,
                   met57_Mod_1DayMaxMax, met58_Mod_1DayMaxMed,
                   met59_Mod_3DayMaxMax, met60_Mod_3DayMaxMed,
                   met61_Mod_7DayMaxMax, met62_Mod_7DayMaxMed,
                   met63_Mod_30DayMaxMax, met64_Mod_30DayMaxMed,
                   met65_Mod_90DayMaxMax, met66_Mod_90DayMaxMed,
                   met57_PctError, met58_PctError,
                   met59_PctError, met60_PctError,
                   met61_PctError, met62_PctError,
                   met63_PctError, met64_PctError,
                   met65_PctError, met66_PctError), 
                 nrow = 10, ncol = 3);
colnames(Table5) = c("USGS Gage", "Model", "Pct. Error");
rownames(Table5) = c("Max. 1 Day Max", "Med. 1 Day Max", 
                     "Max. 3 Day Max", "Med. 3 Day Max",
                     "Max. 7 Day Max", "Med. 7 Day Max",
                     "Max. 30 Day Max", "Med. 30 Day Max",
                     "Max. 90 Day Max", "Med. 90 Day Max");
write.csv(Table5, paste0(folder_location,"\\USGStoModel\\OUTPUT\\",siteNo," vs ",RivSeg,
                         "\\Tab. 5 - Period High Flows.csv"));

# Table 6: Non-Exceedance Flows
Table6 <- matrix(c(met38_Gage_1NonEx, met39_Gage_5NonEx, met40_Gage_50NonEx,
                   met41_Gage_95NonEx, met42_Gage_99NonEx, met43_Gage_Sep10,
                   met38_Model_1NonEx, met39_Model_5NonEx, met40_Model_50NonEx,
                   met41_Model_95NonEx, met42_Model_99NonEx, met43_Model_Sep10,
                   met38_PctError, met39_PctError, met40_PctError,
                   met41_PctError, met42_PctError, met43_PctError), nrow = 6, ncol = 3);
colnames(Table6) = c("USGS Gage", "Model", "Pct. Error");
rownames(Table6) = c("1% Non-Exceedance", "5% Non-Exceedance",
                     "50% Non-Exceedance", "95% Non-Exceedance",
                     "99% Non-Exceedance", "Sept. 10% Non-Exceedance");
write.csv(Table6, paste0(folder_location,"\\USGStoModel\\OUTPUT\\",siteNo," vs ",RivSeg,
                         "\\Tab. 6 - Non-Exceedance Flows.csv"))
#The purpose of this code is to calculate the average daily flow of model and gage outputs
#Not only is this a useful metric but it allows us to make sure we have the correct river segments and gage associated

#Load Libraries---------------------------------------------------------------------
library(pander);
library(httr);
library(dataRetrieval);
library(zoo);
library(lubridate);

#Info that needs to be changed, model URL, river segment, gage number--------------------------
URL_model_daily <- 'https://docs.google.com/spreadsheets/d/11gh7lb8DqZR6frLeVgIXld4Y1rFErJrPXZUCOTfL5YY/pub?output=csv'
river_seg<-'OD3_8630_8720'
siteNo <- "02073000"
start.date <- "1984-10-01"
end.date <- "2005-12-05"

# LOADS MODEL DATA ------------------------------------------------------------
model_daily = read.csv(URL_model_daily, header = TRUE, sep = ",", stringsAsFactors = FALSE);

#Adjust model data to be correct date range (start.date to end.date)
start_date<-(which(model_daily$date == start.date))
end_date<-(which(model_daily$date== end.date))
model_daily_WY<-model_daily[start_date:end_date,]

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
#Pull in data on river segment areas
URL_model_area <- 'https://docs.google.com/spreadsheets/d/e/2PACX-1vRY5BkQ0Ha269jchgbuZYaND5_rxaSTV1rBoW6dDtr7NnlrCIcMaODmST-WAfSzUwUWwOuv3ghVUwNH/pub?output=csv'
model_area_all = read.csv(URL_model_area, header = TRUE, sep = ",", stringsAsFactors = FALSE);


model_area_row_number<-which(model_area_all$RiverSeg == paste(river_seg)) #Pull column number of river segement
model_area_row_info<-data.frame(matrix(nrow=1,ncol=1))                    #Create empty vector to store desired river segment data
model_area_row_info<-model_area_all[model_area_row_number,]               #Fill vector with desired data
model_area<-model_area_row_info$GISArea                                   #Pull just the area of the river segment

gage_area<-readNWISsite(paste(siteNo)) #Pulls data on the size of the watershed and locations
area_of_model<-model_daily_WY$ovol*(gage_area$drain_area_va/model_area) #To get area adjust model data do model flow *gage area/model area
area_weighted_model<-data.frame(area_of_model,as.Date(model_daily_WY$date))
colnames(area_weighted_model) <- c("flow","date")

# CALCULATES AVERAGE DAILY FLOW -----------------------------------------------------
avg_model_area<-mean(area_of_model)
avg_model_norm<-mean(model_daily_WY$ovol)
avg_gage<-mean(gage_data$Flow)


# CREATES OUTPUT MATRIX -------------------------------------------------------
OUTPUT_MATRIX <- matrix(c(avg_gage, avg_gage,avg_model_area, avg_model_norm), nrow=2, ncol=2)
rownames(OUTPUT_MATRIX) = c("Area Weighted Flow", "UnWeighted Flow")
colnames(OUTPUT_MATRIX) = c('USGS', 'Model')

# PLOT AVERAGE DAILY FLOWS -------------------------------------------------------
gage_legend<- paste('Gage',siteNo)                        #Creates legend title for gage
model_legend<-paste('Model River Seg. \n', river_seg)     #Creates legend title for model
error <- abs(OUTPUT_MATRIX[1,1]-OUTPUT_MATRIX[1,2])/OUTPUT_MATRIX[1,1]*100
error <- round(error, digits = 2)
par(mfrow=c(1,1))                                                                                #Both data sets on one graph
par(cex=1.1)
plot(gage_data$Date, gage_data$Flow, type='l', col="red", xlab=paste('Date Range:', start.date, ':', end.date, ' ', 'Error:', error,'%'), ylab='Flow (cfs)', lwd=1.5) #USGS gage data
lines(area_weighted_model$date, area_weighted_model$flow, col="blue", type='l', lwd=1.5)                 #Model data
legend ("topright", legend=c(gage_legend,model_legend), col=c('red','blue'), lty=1, bty='n')

# OUTPUT MATRIX------------------------------------------------------------------------
OUTPUT_MATRIX

# DETERMINING PERIODS OF ERROR --------------------------------------------------------

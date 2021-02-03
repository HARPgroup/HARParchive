#This code is to calculate the 7Q10s from the USGS gage data and the Chesapeake Bay Watershed Model and compare
#the two


#Loads USGS gage data and calculates 7Q10s -----------------------------------------------
library(dataRetrieval)
vignette("dataRetrieval",package = "dataRetrieval")
library('IHA')
library('zoo')
library('PearsonDS')

siteNo <- "02075500"
pCode <- "00060"
start.date <- "1984-01-01"
end.date <- "2005-12-31"

yahara <- readNWISdv(siteNumbers = siteNo,
                     parameterCd = pCode,
                     startDate = start.date,
                     endDate = end.date)
# names with codes
names(yahara)
# cleans up names
yahara <- renameNWISColumns(yahara)
names(yahara)
head(yahara)
summary(yahara) # shows basic stats, median, mean, max, min

Flow_USGS <- yahara[,c(3,4)]
Flow_USGSv <- as.vector(Flow_USGS)

Flow_USGSv$Date <- as.POSIXct(Flow_USGSv$Date)

f3_USGS <- zoo(Flow_USGSv$Flow, order.by = Flow_USGSv$Date)

g2_USGS <- group2(f3_USGS)
x <- as.vector(as.matrix(g2_USGS["7 Day Min"]))
x <- log(x)
pars <- PearsonDS:::pearsonIIIfitML(x)
exp(qpearsonIII(0.1,params = pars$par))


#Loads model data and calculates 7Q10s ----------------------------------------------
#For OD5_8780_8660 river segment 
#URI_model_daily <- 'https://docs.google.com/spreadsheets/d/e/2PACX-1vRxODFehfqbofJfrIwLFHGnoCrrpo0QUeSbhl5hehBJrbpqATfjlAuH3sOKT5f86wKVXbxMjUGzb4vY/pub?output=csv'
#For OD3_8638_8720 river segment 
URI_model_daily <- 'https://docs.google.com/spreadsheets/d/e/2PACX-1vRSt9LQQHtGK8kwfiOriEsPolDt_77JoXYW-KVMebP2GKptI8tzWsacshjqEWv0oWgR0J2zYYUx6ZSv/pub?output=csv'
model_daily = read.csv(URI_model_daily, header = TRUE, sep = ",", stringsAsFactors = FALSE);

Flow_model <- model_daily[,c(6,5)]
Flow_modelv <- as.vector(Flow_model)
colnames(Flow_modelv) <- c("Date", "Flow")

Flow_modelv$Date <- as.POSIXct(Flow_modelv$Date)

f3_model <- zoo(Flow_modelv$Flow, order.by = Flow_modelv$Date)

g2_model <- group2(f3_model)
y <- as.vector(as.matrix(g2_model["7 Day Min"]))
y <- log(y)
pars <- PearsonDS:::pearsonIIIfitML(y)
exp(qpearsonIII(0.1,params = pars$par))


#Compares USGS data and model data --------------------------------------------------
#1 Day Min ---------------
one_day_min_model <- g2_model[,c(1,2)]
one_day_min_USGS <- g2_USGS[,c(1,2)]
one_day_min_model["data_source"] <- "Model"
one_day_min_USGS["data_source"] <- "USGS"
one_day_min_total <- rbind(one_day_min_model, one_day_min_USGS)
colnames(one_day_min_total) <- c("Year", "OneDayMin", "data_source")
dev.new()
library(ggplot2)
ts <- ggplot(data = one_day_min_total,
             aes(Year, OneDayMin, group = data_source, color = data_source)) +
  geom_line(size = 1)
ts

ts <- ts +
  xlab("Year") +
  ylab("Flow (cfs)") +
  ggtitle("USGS and CBP Model 1 Day Minimum Comparison")
ts
# 1 Day Max ------------
one_day_max_model <- g2_model[,c(1,3)]
one_day_max_USGS <- g2_USGS[,c(1,3)]
one_day_max_model["data_source"] <- "Model"
one_day_max_USGS["data_source"] <- "USGS"
one_day_max_total <- rbind(one_day_max_model, one_day_max_USGS)
colnames(one_day_max_total) <- c("Year", "OneDayMax", "data_source")
dev.new()
library(ggplot2)
ts <- ggplot(data = one_day_max_total,
             aes(Year, OneDayMax, group = data_source, color = data_source)) +
  geom_line(size = 1)
ts

ts <- ts +
  xlab("Year") +
  ylab("Flow (cfs)") +
  ggtitle("USGS and CBP Model 1 Day Maximum Comparison")
ts

# 3 Day Min ------------
three_day_min_model <- g2_model[,c(1,4)]
three_day_min_USGS <- g2_USGS[,c(1,4)]
three_day_min_model["data_source"] <- "Model"
three_day_min_USGS["data_source"] <- "USGS"
three_day_min_total <- rbind(three_day_min_model, three_day_min_USGS)
colnames(three_day_min_total) <- c("Year", "ThreeDayMin", "data_source")
dev.new()
library(ggplot2)
ts <- ggplot(data = three_day_min_total,
             aes(Year, ThreeDayMin, group = data_source, color = data_source)) +
  geom_line(size = 1)
ts

ts <- ts +
  xlab("Year") +
  ylab("Flow (cfs)") +
  ggtitle("USGS and CBP Model 3 Day Minimum Comparison")
ts

# 3 Day Max ------------
three_day_max_model <- g2_model[,c(1,5)]
three_day_max_USGS <- g2_USGS[,c(1,5)]
three_day_max_model["data_source"] <- "Model"
three_day_max_USGS["data_source"] <- "USGS"
three_day_max_total <- rbind(three_day_max_model, three_day_max_USGS)
colnames(three_day_max_total) <- c("Year", "ThreeDayMax", "data_source")
dev.new()
library(ggplot2)
ts <- ggplot(data = three_day_max_total,
             aes(Year, ThreeDayMax, group = data_source, color = data_source)) +
  geom_line(size = 1)
ts

ts <- ts +
  xlab("Year") +
  ylab("Flow (cfs)") +
  ggtitle("USGS and CBP Model 3 Day Maximum Comparison")
ts

# 7 Day Min ------------
seven_day_min_model <- g2_model[,c(1,6)]
seven_day_min_USGS <- g2_USGS[,c(1,6)]
seven_day_min_model["data_source"] <- "Model"
seven_day_min_USGS["data_source"] <- "USGS"
seven_day_min_total <- rbind(seven_day_min_model, seven_day_min_USGS)
colnames(seven_day_min_total) <- c("Year", "SevenDayMin", "data_source")
dev.new()
library(ggplot2)
ts <- ggplot(data = seven_day_min_total,
             aes(Year, SevenDayMin, group = data_source, color = data_source)) +
  geom_line(size = 1)
ts

ts <- ts +
  xlab("Year") +
  ylab("Flow (cfs)") +
  ggtitle("USGS and CBP Model 7 Day Minimum Comparison")
ts

# 7 Day Max ------------
seven_day_max_model <- g2_model[,c(1,7)]
seven_day_max_USGS <- g2_USGS[,c(1,7)]
seven_day_max_model["data_source"] <- "Model"
seven_day_max_USGS["data_source"] <- "USGS"
seven_day_max_total <- rbind(seven_day_max_model, seven_day_max_USGS)
colnames(seven_day_max_total) <- c("Year", "SevenDayMax", "data_source")
dev.new()
library(ggplot2)
ts <- ggplot(data = seven_day_max_total,
             aes(Year, SevenDayMax, group = data_source, color = data_source)) +
  geom_line(size = 1)
ts

ts <- ts +
  xlab("Year") +
  ylab("Flow (cfs)") +
  ggtitle("USGS and CBP Model 7 Day Maximum Comparison")
ts


# 30 Day Min ------------
thirty_day_min_model <- g2_model[,c(1,8)]
thirty_day_min_USGS <- g2_USGS[,c(1,8)]
thirty_day_min_model["data_source"] <- "Model"
thirty_day_min_USGS["data_source"] <- "USGS"
thirty_day_min_total <- rbind(thirty_day_min_model, thirty_day_min_USGS)
colnames(thirty_day_min_total) <- c("Year", "ThirtyDayMin", "data_source")
dev.new()
library(ggplot2)
ts <- ggplot(data = thirty_day_min_total,
             aes(Year, ThirtyDayMin, group = data_source, color = data_source)) +
  geom_line(size = 1)
ts

ts <- ts +
  xlab("Year") +
  ylab("Flow (cfs)") +
  ggtitle("USGS and CBP Model 30 Day Minimum Comparison")
ts

# 30 Day Max ------------
thirty_day_max_model <- g2_model[,c(1,9)]
thirty_day_max_USGS <- g2_USGS[,c(1,9)]
thirty_day_max_model["data_source"] <- "Model"
thirty_day_max_USGS["data_source"] <- "USGS"
thirty_day_max_total <- rbind(thirty_day_max_model, thirty_day_max_USGS)
colnames(thirty_day_max_total) <- c("Year", "ThirtyDayMax", "data_source")
dev.new()
library(ggplot2)
ts <- ggplot(data = thirty_day_max_total,
             aes(Year, ThirtyDayMax, group = data_source, color = data_source)) +
  geom_line(size = 1)
ts

ts <- ts +
  xlab("Year") +
  ylab("Flow (cfs)") +
  ggtitle("USGS and CBP Model 30 Day Maximum Comparison")
ts


# 90 Day Min ------------
ninety_day_min_model <- g2_model[,c(1,10)]
ninety_day_min_USGS <- g2_USGS[,c(1,10)]
ninety_day_min_model["data_source"] <- "Model"
ninety_day_min_USGS["data_source"] <- "USGS"
ninety_day_min_total <- rbind(ninety_day_min_model, ninety_day_min_USGS)
colnames(ninety_day_min_total) <- c("Year", "NinetyDayMin", "data_source")
dev.new()
library(ggplot2)
ts <- ggplot(data = ninety_day_min_total,
             aes(Year, NinetyDayMin, group = data_source, color = data_source)) +
  geom_line(size = 1)
ts

ts <- ts +
  xlab("Year") +
  ylab("Flow (cfs)") +
  ggtitle("USGS and CBP Model 30 Day Minimum Comparison")
ts


# 90 Day Max ------------
ninety_day_max_model <- g2_model[,c(1,11)]
ninety_day_max_USGS <- g2_USGS[,c(1,11)]
ninety_day_max_model["data_source"] <- "Model"
ninety_day_max_USGS["data_source"] <- "USGS"
ninety_day_max_total <- rbind(ninety_day_max_model, ninety_day_max_USGS)
colnames(ninety_day_max_total) <- c("Year", "NinetyDayMax", "data_source")
dev.new()
library(ggplot2)
ts <- ggplot(data = ninety_day_max_total,
             aes(Year, NinetyDayMax, group = data_source, color = data_source)) +
  geom_line(size = 1)
ts

ts <- ts +
  xlab("Year") +
  ylab("Flow (cfs)") +
  ggtitle("USGS and CBP Model 90 Day Maximum Comparison")
ts



# Base Index ------------
base_index_model <- g2_model[,c(1,13)]
base_index_USGS <- g2_USGS[,c(1,13)]
base_index_model["data_source"] <- "Model"
base_index_USGS["data_source"] <- "USGS"
base_index_total <- rbind(base_index_model, base_index_USGS)
colnames(base_index_total) <- c("Year", "BaseIndex", "data_source")
dev.new()
library(ggplot2)
ts <- ggplot(data = base_index_total,
             aes(Year, BaseIndex, group = data_source, color = data_source)) +
  geom_line(size = 1)
ts

ts <- ts +
  xlab("Year") +
  ylab("Flow (cfs)") +
  ggtitle("USGS and CBP Model Base Index Comparison")
ts

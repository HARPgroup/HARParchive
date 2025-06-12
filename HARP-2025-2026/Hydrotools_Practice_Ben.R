library(hydrotools)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(sqldf)
#Group 1
 flows <- dataRetrieval::readNWISdv("01633000",parameterCd = "00060")
 flows <- dataRetrieval::renameNWISColumns(flows)
 #Convert flows to zoo
 flows_zoo <- zoo::as.zoo(x = flows$Flow)
 zoo::index(flows_zoo) <- flows$Date
 #Use group 1 to get the minimum monthly flows:
 G1_Mount_Jackson <- hydrotools::group1(flows_zoo,"water",FUN = min)
 
 flows2 <- dataRetrieval::readNWISdv("01632000",parameterCd = "00060")
 flows2 <- dataRetrieval::renameNWISColumns(flows2)
 #Convert flows to zoo
 flows_zoo2 <- zoo::as.zoo(x = flows2$Flow)
 zoo::index(flows_zoo2) <- flows2$Date
 #Use group 1 to get the minimum monthly flows:
 G1_Cootes_Store <- hydrotools::group1(flows_zoo2,"water",FUN = min)
 
 flows3 <- dataRetrieval::readNWISdv("01634000",parameterCd = "00060")
 flows3 <- dataRetrieval::renameNWISColumns(flows3)
 #Convert flows to zoo
 flows_zoo3 <- zoo::as.zoo(x = flows3$Flow)
 zoo::index(flows_zoo3) <- flows3$Date
 #Use group 1 to get the minimum monthly flows:
 G1_Strasburg <- hydrotools::group1(flows_zoo3,"water",FUN = min)
 
#Group 2
  flows4 <- dataRetrieval::readNWISdv("01633000",parameterCd = "00060")
  flows4 <- dataRetrieval::renameNWISColumns(flows4)
  #Convert flows to zoo
 flows_zoo4 <- zoo::as.zoo(x = flows4$Flow)
  zoo::index(flows_zoo4) <- flows4$Date
  #Use group 2 to get critical period flows and stats:
 G2_Mount_Jackson<- hydrotools::group2(flows_zoo4,"water",mimic.tnc = TRUE)
 
 flows5 <- dataRetrieval::readNWISdv("01632000",parameterCd = "00060")
 flows5 <- dataRetrieval::renameNWISColumns(flows5)
 #Convert flows to zoo
 flows_zoo5 <- zoo::as.zoo(x = flows5$Flow)
 zoo::index(flows_zoo5) <- flows5$Date
 #Use group 2 to get critical period flows and stats:
 G2_Cootes_Store<- hydrotools::group2(flows_zoo5,"water",mimic.tnc = TRUE)
 
 flows6 <- dataRetrieval::readNWISdv("01634000",parameterCd = "00060")
 flows6 <- dataRetrieval::renameNWISColumns(flows6)
 #Convert flows to zoo
 flows_zoo6 <- zoo::as.zoo(x = flows6$Flow)
 zoo::index(flows_zoo6) <- flows6$Date
 #Use group 2 to get critical period flows and stats:
 G2_Strasburg<- hydrotools::group2(flows_zoo6,"water",mimic.tnc = TRUE)
 

 max_data <- data.frame(
   Location = c(rep("Cootes Store", nrow(G2_Cootes_Store)),
                rep("Mount Jackson", nrow(G2_Mount_Jackson)),
                rep("Strasburg", nrow(G2_Strasburg))),
   Max_7Day = c(G2_Cootes_Store$"7 Day Max",
                G2_Mount_Jackson$"7 Day Max",
                G2_Strasburg$"7 Day Max"),
   Year    =  c(G2_Cootes_Store$"year",
                G2_Mount_Jackson$"year",
                G2_Strasburg$"year")
   
 )
 
A<- ggplot(max_data, aes(x = Location, y = Max_7Day, fill=Location)) +
   geom_boxplot() +
   labs(title = "7-Day Max Flow (cfs)",
        x = "Location",
        y = "Flow (cfs)") +
   theme_minimal()
 
 
 min_data <- data.frame(
   Location = c(rep("Cootes Store", nrow(G2_Cootes_Store)),
                rep("Mount Jackson", nrow(G2_Mount_Jackson)),
                rep("Strasburg", nrow(G2_Strasburg))),
   Min_7Day = c(G2_Cootes_Store$"7 Day Min",
                G2_Mount_Jackson$"7 Day Min",
                G2_Strasburg$"7 Day Min"),
   Year    =  c(G2_Cootes_Store$"year",
                G2_Mount_Jackson$"year",
                G2_Strasburg$"year")
 )
   
 
 
 B<-ggplot(min_data, aes(x = Location, y = Min_7Day, fill=Location)) +
   geom_boxplot() +
   labs(title = "7-Day Min Flow (cfs)",
        x = "Location",
        y = "Flow (cfs)") +
   theme_minimal()
 
 grid.arrange(A,B, ncol=2, top="North Fork Shenandoah Max and Mins")
 
C<- ggplot() + 
   geom_line(data = min_data, aes(x = Year, y = Min_7Day, color=Location)) +
   geom_line(data = max_data, aes(x = Year, y = Max_7Day, color=Location)) +
   xlab('Year') + 
   ylab('Flow cfs')+
   ggtitle(('Line graph of min and max cfs 1925-2025'))+
   theme_minimal()
C
 
D<- ggplot() + 
   geom_point(data = min_data, aes(x = Year, y = Min_7Day, color=Location)) +
   geom_point(data = max_data, aes(x = Year, y = Max_7Day, color=Location)) +
   xlab('Year') + 
   ylab('Flow cfs')+
   ggtitle('scatter plot of min and max cfs 1925-2025')+
   theme_minimal()
 D
E<- ggplot() + 
   geom_point(data = min_data, aes(x = Year, y = Min_7Day, color=Location)) +
   xlab('Year') + 
   ylab('Flow cfs')+
   ggtitle('Scatter plot of min cfs 1925-2025')+
   theme_minimal()
E

temp1 <- dataRetrieval::readNWISdv("01633000",parameterCd = "00010")
temp1 <- dataRetrieval::renameNWISColumns(temp1)
temp2 <- dataRetrieval::readNWISdv("01632000",parameterCd = "00010")
temp2 <- dataRetrieval::renameNWISColumns(temp2)

flow_temp1 <- merge(flows, temp1, by = "Date")
flow_temp2 <- merge(flows2, temp2, by = "Date")

temp_location <- data.frame(
  Location = c(rep("Cootes Store", nrow(flow_temp1)),
               rep("Mount Jackson", nrow(flow_temp2))),
  Temp = c(flow_temp1$Wtemp,
           flow_temp2$Wtemp),
  Date = c(flow_temp1$Date,
           flow_temp2$Date)
)

f<- ggplot() + 
  geom_line(data =temp_location, aes(x = Date, y = Temp, color=Location)) +
  xlab('Date') + 
  ylab('Water Temperature')+
  ggtitle(('Line graph of water temperature cfs 2007-2009'))+
  theme_minimal()
f


flows$Location <- "Cootes Store"
temp1$Location <- "Cootes Store"

flows2$Location <- "Mount Jackson"
temp2$Location <- "Mount Jackson"

flow_temp1 <- merge(flows, temp1, by = c("Date", "Location"))
flow_temp2 <- merge(flows2, temp2, by = c("Date", "Location"))



flows$Date <- as.Date(flows$Date)
temp1$Date <- as.Date(temp1$Date)

temp_flow_combined <- rbind(flow_temp1, flow_temp2)

ggplot(temp_flow_combined, aes(x = Wtemp, y = Flow, color = Location)) +
  geom_point(alpha = 0.6) +
  xlab("Water Temperature (°C))") +
  ylab("Discharge (CFS)") +
  ggtitle("Water Temperature vs. Discharge (CFS), 2007–2009") +
  theme_minimal()

ggplot(temp_flow_combined, aes(x = Wtemp, y = Flow, color = Location)) +
  ylim(0, 1000)+
  geom_point(alpha = 0.6) +
  xlab("Water Temperature (°C)") +
  ylab("Discharge (CFS))") +
  ggtitle("Water Temperature vs. Discharge (CFS), 2007–2009") +
  theme_minimal()

# Calculate thresholds
threshold_MJ <- quantile(flows$Flow, 0.10, na.rm = TRUE)
threshold_CS <- quantile(flows2$Flow, 0.10, na.rm = TRUE)
threshold_S  <- quantile(flows3$Flow, 0.10, na.rm = TRUE)

# Add drought flags
flows$DroughtDay  <- flows$Flow  < threshold_MJ
flows2$DroughtDay <- flows2$Flow < threshold_CS
flows3$DroughtDay <- flows3$Flow < threshold_S

flows <- flows[!is.na(flows$DroughtDay), ]
flows2 <- flows2[!is.na(flows$DroughtDay), ]
flows3 <- flows3[!is.na(flows$DroughtDay), ]

analyze_drought <- function(df, site_name = "") {
  rle_out <- rle(df$DroughtDay)
  lengths <- rle_out$lengths
  values <- rle_out$values
  ends <- cumsum(lengths)
  starts <- c(1, head(ends, -1) + 1)
  
  drought_starts <- starts[values == TRUE]
  drought_ends <- ends[values == TRUE]
  
  drought_event_df <- data.frame(
    StartDate = df$Date[drought_starts],
    EndDate   = df$Date[drought_ends]
  )
  
  drought_event_df$DaysBetween <- c(NA, as.numeric(difftime(
    drought_event_df$StartDate[-1],
    drought_event_df$EndDate[-nrow(drought_event_df)],
    units = "days"
  )))
  
  max_drought_length <- max(lengths[values == TRUE])
  longest_index <- which(lengths == max_drought_length & values == TRUE)[1]
  longest_start_row <- starts[longest_index]
  longest_end_row <- ends[longest_index]
  
  longest_start <- df$Date[longest_start_row]
  longest_end   <- df$Date[longest_end_row]
  
  cat("\n===== Drought Analysis for", site_name, "=====\n")
  cat("Longest drought lasted", max_drought_length, "days\n")
  cat("From", longest_start, "to", longest_end, "\n")
  
  return(drought_event_df)
}


drought_MJ <- analyze_drought(flows, "Mount Jackson")
drought_CS <- analyze_drought(flows2, "Cootes Store")
drought_S  <- analyze_drought(flows3, "Strasburg")

# Add a gage name column for easy identification
drought_MJ$Site <- "Mount Jackson"
drought_CS$Site <- "Cootes Store"
drought_S$Site  <- "Strasburg"

drought_MJ$Duration <- as.numeric(drought_MJ$EndDate - drought_MJ$StartDate)
drought_CS$Duration <- as.numeric(drought_CS$EndDate - drought_CS$StartDate)
drought_S$Duration <- as.numeric(drought_S$EndDate - drought_S$StartDate)

filtered_MJ_Drought <- sqldf("select * from drought_MJ where Duration > 7")
filtered_CS_Drought <- sqldf("select * from drought_CS where Duration > 7")
filtered_S_Drought <- sqldf("select * from drought_S where Duration > 7")
# Combine all into one dataframe

all_droughts <- rbind(drought_MJ, drought_CS, drought_S)
all_droughts$Duration <- as.numeric(all_droughts$EndDate - all_droughts$StartDate)


# Plot drought periods as horizontal line segments
ggplot(all_droughts) +
  geom_segment(aes(x = StartDate, xend = EndDate, y = Site, yend = Site, color = Site), 
               linewidth = 35, alpha = 0.7) +
  scale_x_date(date_labels = "%Y", date_breaks = "10 years") +
  labs(title = "Drought Periods by Gage",
       x = "Date",
       y = "Site",
       fill = "duaration") +
  theme_minimal()

ggplot(filtered_MJ_Drought, aes(x = Duration)) +
  geom_histogram(bins= 30, fill = "limegreen", color = "black") +
  labs(title = "Time Between Droughts: Mount Jackson",
       x = "Drought Duration",
       y = "Frequency/Count") +
  theme_minimal()

ggplot(filtered_CS_Drought, aes(x = Duration)) +
  geom_histogram(bins = 30, fill = "salmon", color = "black") +
  labs(title = "Time Between Droughts: Cootes Store",
       x = "Drought Duration",
       y = "Frequency/Count") +
  theme_minimal()

ggplot(filtered_S_Drought, aes(x = Duration)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  labs(title = "Time Between Droughts: Strasburg",
       x = "Drought Duration",
       y = "Frequency/Count") +
  theme_minimal()

flows$DroughtPeriod <- NA
for (i in 1:nrow(drought_MJ)) {
  flows$DroughtPeriod[flows$Date >= drought_MJ$StartDate[i] & 
                        flows$Date <= drought_MJ$EndDate[i]] <- "Drought"
}

ggplot(flows, aes(x = Date, y = Flow)) +
  ylim(0, 300)+
  geom_line() +
  geom_line(data = subset(flows, DroughtPeriod == "Drought"), 
            aes(x = Date, y = Flow), color = "red") +
  labs(title = "Flow Time Series with Droughts Highlighted Mount Jackson",
       y = "Flow (CFS)",
       x = "Date") +
  theme_minimal()

flows2$DroughtPeriod <- NA
for (i in 1:nrow(drought_CS)) {
  flows2$DroughtPeriod[flows2$Date >= drought_CS$StartDate[i] & 
                        flows2$Date <= drought_CS$EndDate[i]] <- "Drought"
}

ggplot(flows2, aes(x = Date, y = Flow)) +
  ylim(0, 300)+
  geom_line() +
  geom_line(data = subset(flows2, DroughtPeriod == "Drought"), 
            aes(x = Date, y = Flow), color = "red") +
  labs(title = "Flow Time Series with Droughts Highlighted Cootes Store",
       y = "Flow (CFS)",
       x = "Date") +
  theme_minimal()

flows3$DroughtPeriod <- NA
for (i in 1:nrow(drought_S)) {
  flows3$DroughtPeriod[flows3$Date >= drought_S$StartDate[i] & 
                         flows3$Date <= drought_S$EndDate[i]] <- "Drought"
}

ggplot(flows3, aes(x = Date, y = Flow)) +
  ylim(0, 300)+
  geom_line() +
  geom_line(data = subset(flows3, DroughtPeriod == "Drought"), 
            aes(x = Date, y = Flow), color = "red") +
  labs(title = "Flow Time Series with Droughts Highlighted Strasburg",
       y = "Flow (CFS)",
       x = "Date") +
  theme_minimal()
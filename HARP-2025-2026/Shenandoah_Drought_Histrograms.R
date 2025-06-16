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

###Drought Definition and Analysis

# Calculate thresholds
threshold_MJ <- quantile(flows$Flow, 0.10, na.rm = TRUE)
threshold_CS <- quantile(flows2$Flow, 0.10, na.rm = TRUE)
threshold_S  <- quantile(flows3$Flow, 0.10, na.rm = TRUE)

# Add drought identifiers (TRUE)/(FALSE)
flows$DroughtDay  <- flows$Flow  < threshold_MJ
flows2$DroughtDay <- flows2$Flow < threshold_CS
flows3$DroughtDay <- flows3$Flow < threshold_S

flows <- flows[!is.na(flows$DroughtDay), ]
flows2 <- flows2[!is.na(flows$DroughtDay), ]
flows3 <- flows3[!is.na(flows$DroughtDay), ]

#Create a function to analyze the distance between droughtsS
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

#analyze the data for the 3 gages of interest
drought_MJ <- analyze_drought(flows, "Mount Jackson")
drought_CS <- analyze_drought(flows2, "Cootes Store")
drought_S  <- analyze_drought(flows3, "Strasburg")

# Add a gage name column for easy identification
drought_MJ$Site <- "Mount Jackson"
drought_CS$Site <- "Cootes Store"
drought_S$Site  <- "Strasburg"

#add a duration column
drought_MJ$Duration <- as.numeric(drought_MJ$EndDate - drought_MJ$StartDate)
drought_CS$Duration <- as.numeric(drought_CS$EndDate - drought_CS$StartDate)
drought_S$Duration <- as.numeric(drought_S$EndDate - drought_S$StartDate)

filtered_MJ_Drought <- sqldf("select * from drought_MJ where Duration > 7")
filtered_CS_Drought <- sqldf("select * from drought_CS where Duration > 7")
filtered_S_Drought <- sqldf("select * from drought_S where Duration > 7")

#Create histograms to analyze drought durations and Days between events
MJ_Duration <- ggplot(filtered_MJ_Drought, aes(x = Duration)) +
  geom_histogram(bins= 30, fill = "limegreen", color = "black") +
  labs(title = "Length of Droughts: Mount Jackson",
       x = "Drought Duration (Days)",
       y = "Frequency/Count") +
  theme_minimal()

CS_Duration <- ggplot(filtered_CS_Drought, aes(x = Duration)) +
  geom_histogram(bins = 30, fill = "salmon", color = "black") +
  labs(title = "Length of Droughts: Cootes Store",
       x = "Drought Duration (Days)",
       y = "Frequency/Count") +
  theme_minimal()

S_Duration <- ggplot(filtered_S_Drought, aes(x = Duration)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  labs(title = "Length of Droughts: Strasburg",
       x = "Drought Duration (Days)",
       y = "Frequency/Count") +
  theme_minimal()

MJ_Days <- ggplot(filtered_MJ_Drought, aes(x = DaysBetween)) +
  geom_histogram(bins= 30, fill = "limegreen", color = "black") +
  labs(title = "Time Between Droughts: Mount Jackson",
       x = "Days Between Drought Events",
       y = "Frequency/Count") +
  theme_minimal()

CS_Days <- ggplot(filtered_CS_Drought, aes(x = DaysBetween)) +
  geom_histogram(bins = 30, fill = "salmon", color = "black") +
  labs(title = "Time Between Droughts: Cootes Store",
       x = "Days Between Drought Event",
       y = "Frequency/Count") +
  theme_minimal()

S_Days <- ggplot(filtered_S_Drought, aes(x = DaysBetween)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  labs(title = "Time Between Droughts: Strasburg",
       x = "Days Between Drought Event",
       y = "Frequency/Count") +
  theme_minimal()

#Display plotting histograms in pairs
grid.arrange(MJ_Duration,MJ_Days, ncol=2, top="Mount Jackson Drought Events 10% Threshold CFS")
grid.arrange(CS_Duration,CS_Days, ncol=2, top="Cootes Store Drought Events 10% Threshold CFS")
grid.arrange(S_Duration,S_Days, ncol=2, top="Strasburg Drought Events 10% Threshold CFS")

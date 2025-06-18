library(hydrotools)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(sqldf)
library(ggforce)

flows_MJ <- dataRetrieval::readNWISdv("01633000",parameterCd = "00060")
flows_MJ <- dataRetrieval::renameNWISColumns(flows_MJ)

flows_CS <- dataRetrieval::readNWISdv("01632000",parameterCd = "00060")
flows_CS <- dataRetrieval::renameNWISColumns(flows_CS)

flows_S <- dataRetrieval::readNWISdv("01634000",parameterCd = "00060")
flows_S <- dataRetrieval::renameNWISColumns(flows_S)

###Drought Definition and Analysis
# For Mount Jackson
flows_MJ <- flows_MJ %>%
  mutate(Month = format(Date, "%m")) %>%
  group_by(Month) %>%
  mutate(MonthlyThreshold = quantile(Flow, 0.10, na.rm = TRUE),
         DroughtDay = Flow < MonthlyThreshold) %>%
  ungroup()

# For Cootes Store
flows_CS <- flows_CS %>%
  mutate(Month = format(Date, "%m")) %>%
  group_by(Month) %>%
  mutate(MonthlyThreshold = quantile(Flow, 0.10, na.rm = TRUE),
         DroughtDay = Flow < MonthlyThreshold) %>%
  ungroup()

# For Strasburg
flows_S <- flows_S %>%
  mutate(Month = format(Date, "%m")) %>%
  group_by(Month) %>%
  mutate(MonthlyThreshold = quantile(Flow, 0.10, na.rm = TRUE),
         DroughtDay = Flow < MonthlyThreshold) %>%
  ungroup()

#Create a function to analyze the distance between droughts
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
drought_MJ <- analyze_drought(flows_MJ, "Mount Jackson")
drought_CS <- analyze_drought(flows_CS, "Cootes Store")
drought_S  <- analyze_drought(flows_S, "Strasburg")

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
  geom_vline(aes(xintercept = mean(Duration, na.rm = TRUE)),
             color = "darkgreen", linetype = "dashed", size = 1) +
  labs(title = "Length of Droughts: Mount Jackson",
       x = "Drought Duration (Days)",
       y = "Frequency/Count") +
  theme_minimal()

CS_Duration <- ggplot(filtered_CS_Drought, aes(x = Duration)) +
  geom_histogram(bins = 30, fill = "salmon", color = "black") +
  geom_vline(aes(xintercept = mean(Duration, na.rm = TRUE)),
             color = "red", linetype = "dashed", size = 1)+
  labs(title = "Length of Droughts: Cootes Store",
       x = "Drought Duration (Days)",
       y = "Frequency/Count") +
  theme_minimal()

S_Duration <- ggplot(filtered_S_Drought, aes(x = Duration)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  geom_vline(aes(xintercept = mean(Duration, na.rm = TRUE)),
             color = "darkblue", linetype = "dashed", size = 1)+
  labs(title = "Length of Droughts: Strasburg",
       x = "Drought Duration (Days)",
       y = "Frequency/Count") +
  theme_minimal()

MJ_Days <- ggplot(filtered_MJ_Drought, aes(x = DaysBetween)) +
  geom_histogram(bins= 30, fill = "limegreen", color = "black") +
  geom_vline(aes(xintercept = mean(Duration, na.rm = TRUE)),
             color = "darkgreen", linetype = "dashed", size = 1) +
  labs(title = "Time Between Droughts: Mount Jackson",
       x = "Days Between Drought Events",
       y = "Frequency/Count") +
  theme_minimal()

CS_Days <- ggplot(filtered_CS_Drought, aes(x = DaysBetween)) +
  geom_histogram(bins = 30, fill = "salmon", color = "black") +
  geom_vline(aes(xintercept = mean(Duration, na.rm = TRUE)),
             color = "red", linetype = "dashed", size = 1)+
  labs(title = "Time Between Droughts: Cootes Store",
       x = "Days Between Drought Event",
       y = "Frequency/Count") +
  theme_minimal()

S_Days <- ggplot(filtered_S_Drought, aes(x = DaysBetween)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  geom_vline(aes(xintercept = mean(Duration, na.rm = TRUE)),
             color = "darkblue", linetype = "dashed", size = 1) +
  labs(title = "Time Between Droughts: Strasburg",
       x = "Days Between Drought Event",
       y = "Frequency/Count") +
  theme_minimal()

#Display plotting histograms in pairs
grid.arrange(MJ_Duration,MJ_Days, ncol=2, top="Mount Jackson Drought Events 10% Threshold CFS by month")
grid.arrange(CS_Duration,CS_Days, ncol=2, top="Cootes Store Drought Events 10% Threshold CFS by month")
grid.arrange(S_Duration,S_Days, ncol=2, top="Strasburg Drought Events 10% Threshold CFS by month")


#Hydrographs with drought periods marked in Red
flows_MJ$DroughtPeriod <- "Normal"
for (i in 1:nrow(filtered_MJ_Drought)) {
  flows_MJ$DroughtPeriod[flows_MJ$Date >= filtered_MJ_Drought$StartDate[i] & 
                        flows_MJ$Date <= filtered_MJ_Drought$EndDate[i]] <- "Drought"
}

hydrograph_MJ <- ggplot(flows_MJ, aes(x = Date, y = Flow, color = DroughtPeriod)) +
  geom_line() +
  scale_color_manual(values = c("Drought" = "red", "Normal" = "black")) +
  labs(title = "Original Plot",
       y = "Flow (CFS)",
       x = "Date") + 
  theme(legend.position = "none")

  
flows_CS$DroughtPeriod <- "Normal"
for (i in 1:nrow(filtered_CS_Drought)) {
  flows_CS$DroughtPeriod[flows_CS$Date >= filtered_CS_Drought$StartDate[i] & 
                         flows_CS$Date <= filtered_CS_Drought$EndDate[i]] <- "Drought"
}

hydrograph_CS <- ggplot(flows_CS, aes(x = Date, y = Flow, color = DroughtPeriod)) +
  geom_line() +
  scale_color_manual(values = c("Drought" = "red", "Normal" = "black")) +
  labs(title = "Original Plot",
       y = "Flow (CFS)",
       x = "Date") + 
  theme(legend.position = "none")

flows_S$DroughtPeriod <- "Normal"
for (i in 1:nrow(filtered_S_Drought)){
  flows_S$DroughtPeriod[flows_S$Date >= filtered_S_Drought$StartDate[i] & 
                         flows_S$Date <= filtered_S_Drought$EndDate[i]] <- "Drought"
}

hydrograph_S <- ggplot(flows_S, aes(x = Date, y = Flow, color = DroughtPeriod)) +
  geom_line() +
  scale_color_manual(values = c("Drought" = "red", "Normal" = "black")) +
  labs(title = "Original Plot",
       y = "Flow (CFS)",
       x = "Date") + 
  theme(legend.position = "none")

hydrograph_MJ_zoom <- ggplot(flows_MJ, aes(x = Date, y = Flow, color = DroughtPeriod)) +
  geom_line() +
  scale_color_manual(values = c("Drought" = "red", "Normal" = "black")) +
  ylim(0, 500)+
  labs(title = "Zoomed in",
       y = "Flow (CFS)",
       x = "Date") +
  theme(legend.position = "none")


hydrograph_CS_zoom <- ggplot(flows_CS, aes(x = Date, y = Flow, color = DroughtPeriod)) +
  geom_line() +
  scale_color_manual(values = c("Drought" = "red", "Normal" = "black")) +
  ylim(0, 500)+
  labs(title = "Zoomed in",
       y = "Flow (CFS)",
       x = "Date") +
  theme(legend.position = "none")



hydrograph_S_zoom <- ggplot(flows_S, aes(x = Date, y = Flow, color = DroughtPeriod)) +
  geom_line() +
  scale_color_manual(values = c("Drought" = "red", "Normal" = "black")) +
  ylim(0, 500)+
  labs(title = "Zoomed in",
       y = "Flow (CFS)",
       x = "Date") +
   theme(legend.position = "none")


grid.arrange(hydrograph_MJ,hydrograph_MJ_zoom, nrow=2, top="Mount Jackson Drought Events Over Gage Time with Droughts Marked in Red")
grid.arrange(hydrograph_CS,hydrograph_CS_zoom, nrow=2, top="Cootes Store Drought Events Over Gage Time with Droughts Marked in Red")
grid.arrange(hydrograph_S,hydrograph_S_zoom, nrow=2, top="Strasburg Drought Events Over Gage Time with Droughts Marked in Red")

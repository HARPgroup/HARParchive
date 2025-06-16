Cootes_Store <- read.csv("HARP Data 1/BrocksGap(CootesStore)_PS2_5550_5560_results.csv")
Mount_Jackson <- read.csv("HARP Data 1/MountJackson_PS2_5560_5100_results.csv")
Strasburg <- read.csv("HARP Data 1/Strasburg_PS3_5100_5080_results.csv")

# Calculate thresholds
thresholdMJ <- quantile(Cootes_Store$Qout, 0.10, na.rm = TRUE)
thresholdCS <- quantile(Mount_Jackson$Qout, 0.10, na.rm = TRUE)
thresholdS  <- quantile(Strasburg$Qout, 0.10, na.rm = TRUE)

# Add drought identifiers (TRUE)/(FALSE)
Mount_Jackson$DroughtDay  <- Mount_Jackson$Qout  < thresholdMJ
Cootes_Store$DroughtDay <- Cootes_Store$Qout < thresholdCS
Strasburg$DroughtDay <- Strasburg$Qout < thresholdS

#Add date into one column
Cootes_Store$Date <- as.Date(with(Cootes_Store, paste(year, month, day, sep = "-")))
Mount_Jackson$Date <- as.Date(with(Mount_Jackson, paste(year, month, day, sep = "-")))
Strasburg$Date <- as.Date(with(Strasburg, paste(year, month, day, sep = "-")))



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
droughtMJ <- analyze_drought(Mount_Jackson, "Mount Jackson")
droughtCS <- analyze_drought(Cootes_Store, "Cootes Store")
droughtS  <- analyze_drought(Strasburg, "Strasburg")

# Add a gage name column for easy identification
droughtMJ$Site <- "Mount Jackson"
droughtCS$Site <- "Cootes Store"
droughtS$Site  <- "Strasburg"

#add a duration column
droughtMJ$Duration <- as.numeric(droughtMJ$EndDate - droughtMJ$StartDate)
droughtCS$Duration <- as.numeric(droughtCS$EndDate - droughtCS$StartDate)
droughtS$Duration <- as.numeric(droughtS$EndDate - droughtS$StartDate)

filteredMJ_Drought <- sqldf("select * from droughtMJ where Duration > 7")
filteredCS_Drought <- sqldf("select * from droughtCS where Duration > 7")
filteredS_Drought <- sqldf("select * from droughtS where Duration > 7")

MJDuration <- ggplot(filteredMJ_Drought, aes(x = Duration)) +
  geom_histogram(bins= 30, fill = "limegreen", color = "black") +
  labs(title = "Length of Droughts: Mount Jackson",
       x = "Drought Duration (Days)",
       y = "Frequency/Count") +
  theme_minimal()

CSDuration <- ggplot(filteredCS_Drought, aes(x = Duration)) +
  geom_histogram(bins = 30, fill = "salmon", color = "black") +
  labs(title = "Length of Droughts: Cootes Store",
       x = "Drought Duration (Days)",
       y = "Frequency/Count") +
  theme_minimal()

SDuration <- ggplot(filteredS_Drought, aes(x = Duration)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  labs(title = "Length of Droughts: Strasburg",
       x = "Drought Duration (Days)",
       y = "Frequency/Count") +
  theme_minimal()

MJDays <- ggplot(filteredMJ_Drought, aes(x = DaysBetween)) +
  geom_histogram(bins= 30, fill = "limegreen", color = "black") +
  labs(title = "Time Between Droughts: Mount Jackson",
       x = "Days Between Drought Events",
       y = "Frequency/Count") +
  theme_minimal()

CSDays <- ggplot(filteredCS_Drought, aes(x = DaysBetween)) +
  geom_histogram(bins = 30, fill = "salmon", color = "black") +
  labs(title = "Time Between Droughts: Cootes Store",
       x = "Days Between Drought Event",
       y = "Frequency/Count") +
  theme_minimal()

SDays <- ggplot(filteredS_Drought, aes(x = DaysBetween)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  labs(title = "Time Between Droughts: Strasburg",
       x = "Days Between Drought Event",
       y = "Frequency/Count") +
  theme_minimal()

#Display plotting histograms in pairs
grid.arrange(MJDuration,MJDays, ncol=2, top="Mount Jackson Drought Events 10% threshold Qout")
grid.arrange(CSDuration,CSDays, ncol=2, top="Cootes Store Drought Events 10% threshold Qout")
grid.arrange(SDuration,SDays, ncol=2, top="Strasburg Drought Events 10% threshold Qout")
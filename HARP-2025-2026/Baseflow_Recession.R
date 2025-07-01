#use Q and 1st and 2nd derivative of Q to find period of baseflow

#load in useful packages
library(hydrotools)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(sqldf)
library(grwat)
library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)


#load in stream data from USGS
flows_MJ <- dataRetrieval::readNWISdv("01633000",parameterCd = "00060")
flows_MJ <- dataRetrieval::renameNWISColumns(flows_MJ)

flows_CS <- dataRetrieval::readNWISdv("01632000",parameterCd = "00060")
flows_CS <- dataRetrieval::renameNWISColumns(flows_CS)

flows_S <- dataRetrieval::readNWISdv("01634000",parameterCd = "00060")
flows_S <- dataRetrieval::renameNWISColumns(flows_S)

# Derivative Calculator
finite_diff <- function(x, delta_t = 1, order = 1) {
  if (order == 1) {
    return((x[-1] - x[-length(x)]) / delta_t)
  } else if (order == 2) {
    first_deriv <- finite_diff(x, delta_t, order = 1)
    return(finite_diff(first_deriv, delta_t, order = 1))
  } else {
    stop("Only supports first and second derivatives.")
  }
}

# Calculate derivatives Cootes Store
flows_CS$S <- c(NA, finite_diff(flows_CS$Flow))              
flows_CS$dSdt <- c(NA, NA, finite_diff(flows_CS$Flow, order = 2))  

# Calculate derivatives Mount Jackson
flows_MJ$S <- c(NA, finite_diff(flows_MJ$Flow))              
flows_MJ$dSdt <- c(NA, NA, finite_diff(flows_MJ$Flow, order = 2))  

# Calculate derivatives Strasburg
flows_S$S <- c(NA, finite_diff(flows_S$Flow))              
flows_S$dSdt <- c(NA, NA, finite_diff(flows_S$Flow, order = 2)) 


# Fraction Derivative Calculator
frac_change <- function(x, order = 1) {
  if (order == 1) {
    return((x[-1] - x[-length(x)]) / x[-length(x)])
  } else if (order == 2) {
    first_deriv <- frac_change(x, order = 1)
    return((first_deriv[-1] - first_deriv[-length(first_deriv)]) / first_deriv[-length(first_deriv)])
  } else {
    stop("Only supports order = 1 or 2")
  }
}

# Calculate frac derivatives Cootes Store
flows_CS$S_frac <- c(NA, frac_change(flows_CS$Flow))            
flows_CS$dSdt_frac <- c(NA, NA, frac_change(flows_CS$Flow, order = 2))

# Calculate frac derivatives Mount Jackson
flows_MJ$S_frac <- c(NA, frac_change(flows_MJ$Flow))            
flows_MJ$dSdt_frac <- c(NA, NA, frac_change(flows_MJ$Flow, order = 2))  

# Calculate frac derivatives Strasburg
flows_S$S_frac <- c(NA, frac_change(flows_S$Flow))              
flows_S$dSdt_frac <- c(NA, NA, frac_change(flows_S$Flow, order = 2))  

flows_CS$site <- "Cootes Store"
flows_S$site <- "Strasburg"
flows_MJ$site <- "Mount Jackson"
flows_all <- bind_rows(flows_CS, flows_S, flows_MJ)

ggplot(flows_all, aes(x = Date, y = S, color = site)) +
  geom_line() +
  labs(title = "First Derivative of Flow (S)",
       x = "Date", y = "dQ/dt",
       color = "Gage Site") +
  theme_minimal()

ggplot(flows_all, aes(x = Date, y = dSdt, color = site)) +
  geom_line() +
  labs(title = "Second Derivative of Flow (S)",
       x = "Date", y = "d²Q/dt²",
       color = "Gage Site") +
  theme_minimal()

ggplot(flows_all, aes(x = Date, y = S_frac, color = site)) +
  geom_line() +
  labs(title = "First Fractional Change of Flow (S)",
       x = "Date", y = "Fractional Change",
       color = "Gage Site") +
  theme_minimal()

ggplot(flows_all, aes(x = Date, y = dSdt_frac, color = site)) +
  geom_line() +
  labs(title = "Second Fractional Change of Flow (S)",
       x = "Date", y = "d(S_frac)/dt",
       color = "Gage Site") +
  theme_minimal()

# Add logical column
flows_CS$is_recession <- flows_CS$S < 0 & flows_CS$dSdt < 0 
flows_MJ$is_recession <- flows_MJ$S < 0 & flows_MJ$dSdt < 0 
flows_S$is_recession <- flows_S$S < 0 & flows_S$dSdt < 0 

# Run length encoding
rle_out <- rle(flows_CS$is_recession)
# Convert to data frame with run lengths and values
runs <- data.frame(lengths = rle_out$lengths,
                   values = rle_out$values)
# Find where recession is TRUE and lasts long enough
runs$recession_flag <- with(runs, values & lengths >= 3)
# Create a vector marking whether each row is part of a qualifying recession
recession_rows <- rep(runs$recession_flag, runs$lengths)
# Add to main data frame
flows_CS$in_recession <- recession_rows

# Just recession periods
recessions_only_CS <- flows_CS[flows_CS$in_recession == TRUE, ]

ggplot(flows_CS, aes(x = Date, y = Flow)) +
  geom_line(color = "gray") +
  geom_point(data = recessions_only_CS, aes(x = Date, y = Flow), size=0.5, alpha=0.5, color = "red") +
  labs(title = "Cootes Store Flow with Recession Periods Highlighted")+
  theme_minimal()

# Run length encoding
rle_out <- rle(flows_MJ$is_recession)
# Convert to data frame with run lengths and values
runs <- data.frame(lengths = rle_out$lengths,
                   values = rle_out$values)
# Find where recession is TRUE and lasts long enough
runs$recession_flag <- with(runs, values & lengths >= 3)
# Create a vector marking whether each row is part of a qualifying recession
recession_rows <- rep(runs$recession_flag, runs$lengths)
# Add to main data frame
flows_MJ$in_recession <- recession_rows

# Just recession periods
recessions_only_MJ <- flows_MJ[flows_MJ$in_recession == TRUE, ]

ggplot(flows_MJ, aes(x = Date, y = Flow)) +
  geom_line(color = "gray") +
  geom_point(data = recessions_only_MJ, aes(x = Date, y = Flow), size=0.5, alpha=0.5, color = "red") +
  labs(title = "Mount Jackson Flow  with Recession Periods Highlighted")+
  theme_minimal()

# Run length encoding
rle_out <- rle(flows_S$is_recession)
# Convert to data frame with run lengths and values
runs <- data.frame(lengths = rle_out$lengths,
                   values = rle_out$values)
# Find where recession is TRUE and lasts long enough
runs$recession_flag <- with(runs, values & lengths >= 3)
# Create a vector marking whether each row is part of a qualifying recession
recession_rows <- rep(runs$recession_flag, runs$lengths)
# Add to main data frame
flows_S$in_recession <- recession_rows

# Just recession periods
recessions_only_S <- flows_S[flows_S$in_recession == TRUE, ]

ggplot(flows_CS, aes(x = Date, y = Flow)) +
  geom_line(color = "gray") +
  geom_point(data = recessions_only_S, aes(x = Date, y = Flow), size=0.5, alpha=0.5, color = "red") +
  labs(title = "Strasburg Flow with Recession Periods Highlighted")+
  theme_minimal()

#Stable Recessions:

flows_CS$is_S_negative <- flows_CS$S < 0
flows_MJ$is_S_negative <- flows_MJ$S < 0
flows_S$is_S_negative <- flows_S$S < 0

flows_CS$dSdt_recent_neg <- zoo::rollapplyr(
  flows_CS$dSdt < 0, 
  width = 3, 
  FUN = any, 
  partial = TRUE, 
  fill = NA
)

flows_MJ$dSdt_recent_neg <- zoo::rollapplyr(
  flows_MJ$dSdt < 0, 
  width = 3, 
  FUN = any, 
  partial = TRUE, 
  fill = NA
)

flows_S$dSdt_recent_neg <- zoo::rollapplyr(
  flows_S$dSdt < 0, 
  width = 3, 
  FUN = any, 
  partial = TRUE, 
  fill = NA
)

flows_CS$low_flow <- flows_CS$Flow < quantile(flows_CS$Flow, 0.3, na.rm = TRUE)  # bottom 30%
flows_MJ$low_flow <- flows_MJ$Flow < quantile(flows_MJ$Flow, 0.3, na.rm = TRUE)  # bottom 30%
flows_S$low_flow <- flows_S$Flow < quantile(flows_S$Flow, 0.3, na.rm = TRUE)  # bottom 30%

flows_CS$is_stable_recession <- flows_CS$is_S_negative & flows_CS$dSdt_recent_neg & flows_CS$low_flow
flows_MJ$is_stable_recession <- flows_MJ$is_S_negative & flows_MJ$dSdt_recent_neg & flows_MJ$low_flow
flows_S$is_stable_recession <- flows_S$is_S_negative & flows_S$dSdt_recent_neg & flows_S$low_flow


rle_out_CS <- rle(flows_CS$is_stable_recession)
runs_CS <- data.frame(lengths = rle_out_CS$lengths,
                   values = rle_out_CS$values)
rle_out_MJ <- rle(flows_MJ$is_stable_recession)
runs_MJ <- data.frame(lengths = rle_out_MJ$lengths,
                   values = rle_out_MJ$values)
rle_out_S <- rle(flows_S$is_stable_recession)
runs_S <- data.frame(lengths = rle_out_S$lengths,
                   values = rle_out_S$values)

# Choose duration filter 
runs_CS$flag <- with(runs_CS, values & lengths >= 5)
runs_MJ$flag <- with(runs_MJ, values & lengths >= 5)
runs_S$flag <- with(runs_S, values & lengths >= 5)


flows_CS$recession_event <- rep(runs_CS$flag, runs_CS$lengths)
flows_MJ$recession_event <- rep(runs_MJ$flag, runs_MJ$lengths)
flows_S$recession_event <- rep(runs_S$flag, runs_S$lengths)

stable_recessions_only_CS <- flows_CS[flows_CS$is_stable_recession == TRUE, ]
stable_recessions_only_MJ <- flows_MJ[flows_MJ$is_stable_recession == TRUE, ]
stable_recessions_only_S <- flows_S[flows_MJ$is_stable_recession == TRUE, ]

ggplot(flows_CS, aes(x = Date, y = Flow)) +
  geom_line(color = "gray") +
  geom_point(data = stable_recessions_only_CS, aes(x = Date, y = Flow), size=0.5, alpha=0.5, color = "red") +
  labs(title = "Cootes Store Flow  with Stable Recession Periods Highlighted")+
  ylim(0,1500)+
  theme_minimal()

ggplot(flows_MJ, aes(x = Date, y = Flow)) +
  geom_line(color = "gray") +
  geom_point(data = stable_recessions_only_MJ, aes(x = Date, y = Flow), size=0.5, alpha=0.5, color = "red") +
  labs(title = "Mount Jackson Flow with Stable Recession Periods Highlighted")+
  ylim(0,1500)+
  theme_minimal()

ggplot(flows_S, aes(x = Date, y = Flow)) +
  geom_line(color = "gray") +
  geom_point(data = stable_recessions_only_S, aes(x = Date, y = Flow), size=0.5, alpha=0.5, color = "red") +
  labs(title = "Strasburg Flow  with Stable Recession Periods Highlighted")+
  ylim(0,1500)+
  theme_minimal()

#Add duration information

flows_CS$RecessionDay  <- flows_CS$recession_event
flows_MJ$RecessionDay <- flows_MJ$recession_event
flows_S$RecessionDay <- flows_S$recession_event

flows_CS  <- flows_CS[!is.na(flows_CS$RecessionDay), ]
flows_MJ <- flows_MJ[!is.na(flows_MJ$RecessionDay), ]
flows_S <- flows_S[!is.na(flows_S$RecessionDay), ]

analyze_recession <- function(df, site_name = "") {
  rle_out <- rle(df$RecessionDay)
  lengths <- rle_out$lengths
  values <- rle_out$values
  ends <- cumsum(lengths)
  starts <- c(1, head(ends, -1) + 1)
  
  recession_starts <- starts[values == TRUE]
  recession_ends <- ends[values == TRUE]
  
  recession_event_df <- data.frame(
    StartDate = df$Date[recession_starts],
    EndDate   = df$Date[recession_ends]
  )
  
  recession_event_df$DaysBetween <- c(NA, as.numeric(difftime(
    recession_event_df$StartDate[-1],
    recession_event_df$EndDate[-nrow(recession_event_df)],
    units = "days"
  )))
  
  max_recession_length <- max(lengths[values == TRUE])
  longest_index <- which(lengths == max_recession_length & values == TRUE)[1]
  longest_start_row <- starts[longest_index]
  longest_end_row <- ends[longest_index]
  
  longest_start <- df$Date[longest_start_row]
  longest_end   <- df$Date[longest_end_row]
  
  cat("\n===== Recession Analysis for", site_name, "=====\n")
  cat("Longest recession lasted", max_recession_length, "days\n")
  cat("From", longest_start, "to", longest_end, "\n")
  
  return(recession_event_df)
}

recession_MJ <- analyze_recession(flows_MJ, "Mount Jackson")
recession_CS <- analyze_recession(flows_CS, "Cootes Store")
recession_S  <- analyze_recession(flows_S, "Strasburg")

recession_MJ$Site <- "Mount Jackson"
recession_CS$Site <- "Cootes Store"
recession_S$Site  <- "Strasburg"

recession_MJ$Duration <- as.numeric(recession_MJ$EndDate - recession_MJ$StartDate)
recession_CS$Duration <- as.numeric(recession_CS$EndDate - recession_CS$StartDate)
recession_S$Duration  <- as.numeric(recession_S$EndDate - recession_S$StartDate)

recession_MJ <- analyze_recession(flows_MJ, "Mount Jackson")
recession_CS <- analyze_recession(flows_CS, "Cootes Store")
recession_S  <- analyze_recession(flows_S, "Strasburg")

recession_MJ$Site <- "Mount Jackson"
recession_CS$Site <- "Cootes Store"
recession_S$Site  <- "Strasburg"

recession_MJ$Duration <- as.numeric(recession_MJ$EndDate - recession_MJ$StartDate)
recession_CS$Duration <- as.numeric(recession_CS$EndDate - recession_CS$StartDate)
recession_S$Duration  <- as.numeric(recession_S$EndDate - recession_S$StartDate)


#Create histograms to analyze drought durations and Days between events
MJ_Duration <- ggplot(recession_MJ, aes(x = Duration)) +
  geom_histogram(binwidth =1 , fill = "limegreen", color = "black") +
  geom_vline(aes(xintercept = mean(Duration, na.rm = TRUE)),
             color = "darkgreen", linetype = "dashed", size = 1)+
  labs(title = "Duration of Recession Events: Mount Jackson",
       x = "Recession Duration (Days)",
       y = "Frequency/Count") +
  theme_minimal()

CS_Duration <- ggplot(recession_CS, aes(x = Duration)) +
  geom_histogram(binwidth = 1, fill = "salmon", color = "black") +
  geom_vline(aes(xintercept = mean(Duration, na.rm = TRUE)),
             color = "#E40078", linetype = "dashed", size = 1)+
  labs(title = "Duration of Recession Events: Cootes Store",
       x = "Recession Duration (Days)",
       y = "Frequency/Count") +
  theme_minimal()

S_Duration <- ggplot(recession_S, aes(x = Duration)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  geom_vline(aes(xintercept = mean(Duration, na.rm = TRUE)),
             color = "#00008B", linetype = "dashed", size = 1)+
  labs(title = "Duration of Recession Events: Strasburg",
       x = "Recession Duration (Days)",
       y = "Frequency/Count") +
  theme_minimal()

MJ_Days <- ggplot(recession_MJ, aes(x = DaysBetween)) +
  geom_histogram(binwidth = 50, fill = "limegreen", color = "black") +
  geom_vline(aes(xintercept = mean(DaysBetween, na.rm = TRUE)),
             color = "darkgreen", linetype = "dashed", size = 1)+
  labs(title = "Time Between Recession Events: Mount Jackson",
       x = "Days Between Recession Events",
       y = "Frequency/Count") +
  theme_minimal()

CS_Days <- ggplot(recession_CS, aes(x = DaysBetween)) +
  geom_histogram(binwidth = 50, fill = "salmon", color = "black") +
  geom_vline(aes(xintercept = mean(DaysBetween, na.rm = TRUE)),
             color = "#E40078", linetype = "dashed", size = 1)+
  labs(title = "Time Between Recessions: Cootes Store",
       x = "Days Between Recession Events",
       y = "Frequency/Count") +
  theme_minimal()

S_Days <- ggplot(recession_S, aes(x = DaysBetween)) +
  geom_histogram(binwidth=50, fill = "lightblue", color = "black") +
  geom_vline(aes(xintercept = mean(DaysBetween, na.rm = TRUE)),
             color = "#00008B", linetype = "dashed", size = 1)+
  labs(title = "Time Between Recessions: Strasburg",
       x = "Days Between Recession Events",
       y = "Frequency/Count") +
  theme_minimal()

#Display plotting histograms in pairs
grid.arrange(MJ_Duration,MJ_Days, ncol=2, top="Mount Jackson Recession Events")
grid.arrange(CS_Duration,CS_Days, ncol=2, top="Cootes Store Recession Events")
grid.arrange(S_Duration,S_Days, ncol=2, top="Strasburg Recession Events")

# 5 Days or Longer
stable_recessions_5_days_only_CS <- stable_recessions_only_CS[stable_recessions_only_CS$recession_event== TRUE, ]
stable_recessions_5_days_only_MJ <- stable_recessions_only_MJ[stable_recessions_only_MJ$recession_event== TRUE, ]
stable_recessions_5_days_only_S <- stable_recessions_only_S[stable_recessions_only_S$recession_event== TRUE, ]
                                                              



               
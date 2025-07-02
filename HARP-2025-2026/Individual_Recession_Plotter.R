library(dplyr)
library(tidyr)
library(ggplot2)
library(sqldf)
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

#Stable Recessions:
flows_CS$is_S_negative <- flows_CS$S <= 0
flows_MJ$is_S_negative <- flows_MJ$S <= 0
flows_S$is_S_negative <- flows_S$S <= 0

flows_CS$dSdt_recent_neg <- zoo::rollapplyr(
  flows_CS$dSdt < 0, 
  width = 3, #daily buffer for days since second derivative was negative
  FUN = any, 
  partial = TRUE, 
  fill = NA
)

flows_MJ$dSdt_recent_neg <- zoo::rollapplyr(
  flows_MJ$dSdt < 0, 
  width = 3, #width is daily buffer
  FUN = any, 
  partial = TRUE, 
  fill = NA
)

flows_S$dSdt_recent_neg <- zoo::rollapplyr(
  flows_S$dSdt < 0, 
  width = 3, #daily buffer
  FUN = any, 
  partial = TRUE, 
  fill = NA
)

#To change low flow percentile, change flow quantile
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
# I set to 5 but change the lengths greater than or equal to value
runs_CS$flag <- with(runs_CS, values & lengths >= 5)
runs_MJ$flag <- with(runs_MJ, values & lengths >= 5)
runs_S$flag <- with(runs_S, values & lengths >= 5)


flows_CS$recession_event <- rep(runs_CS$flag, runs_CS$lengths)
flows_MJ$recession_event <- rep(runs_MJ$flag, runs_MJ$lengths)
flows_S$recession_event <- rep(runs_S$flag, runs_S$lengths)

stable_recessions_only_CS <- flows_CS[flows_CS$is_stable_recession == TRUE, ]
stable_recessions_only_MJ <- flows_MJ[flows_MJ$is_stable_recession == TRUE, ]
stable_recessions_only_S <- flows_S[flows_MJ$is_stable_recession == TRUE, ]

#Add duration information

flows_CS$RecessionDay  <- flows_CS$recession_event
flows_MJ$RecessionDay <- flows_MJ$recession_event
flows_S$RecessionDay <- flows_S$recession_event

flows_CS  <- flows_CS[!is.na(flows_CS$RecessionDay), ]
flows_MJ <- flows_MJ[!is.na(flows_MJ$RecessionDay), ]
flows_S <- flows_S[!is.na(flows_S$RecessionDay), ]

analyze_recession <- function(df, site_name = "", min_len = 5, max_len = Inf) {
  rle_out <- rle(df$RecessionDay)
  lengths <- rle_out$lengths
  values <- rle_out$values
  ends <- cumsum(lengths)
  starts <- c(1, head(ends, -1) + 1)
  
  group_id <- rep(NA, nrow(df))
  group_counter <- 1
  
  for (i in seq_along(lengths)) {
    if (values[i] && lengths[i] >= min_len && lengths[i] <= max_len) {
      group_id[starts[i]:ends[i]] <- group_counter
      group_counter <- group_counter + 1
    }
  }
  
  df$GroupID <- group_id
  
  recession_starts <- starts[!is.na(group_id[starts])]
  recession_ends   <- ends[!is.na(group_id[starts])]
  
  recession_event_df <- data.frame(
    GroupID   = unique(na.omit(group_id)),
    StartDate = df$Date[recession_starts],
    EndDate   = df$Date[recession_ends]
  )
  
  recession_event_df$Duration <- as.numeric(recession_event_df$EndDate - recession_event_df$StartDate)+1
  recession_event_df$DaysBetween <- c(NA, as.numeric(
    difftime(
      recession_event_df$StartDate[-1],
      recession_event_df$EndDate[-nrow(recession_event_df)],
      units = "days"
    )
  ))
  
  max_recession_length <- max(recession_event_df$Duration)
  longest <- which.max(recession_event_df$Duration)
  
  cat("\n===== Recession Analysis for", site_name, "=====\n")
  cat("Longest recession lasted", max_recession_length, "days\n")
  cat("From", recession_event_df$StartDate[longest], "to", recession_event_df$EndDate[longest], "\n")
  
  return(list(df = df, summary = recession_event_df))
}

results_MJ <- analyze_recession(flows_MJ, "Mount Jackson")
results_CS <- analyze_recession(flows_CS, "Cootes Store")
results_S  <- analyze_recession(flows_S, "Strasburg")


results_MJ$Site <- "Mount Jackson"
results_CS$Site <- "Cootes Store"
results_S$Site  <- "Strasburg"

flows_CS <- results_CS$df
recession_CS <- results_CS$summary
flows_MJ <- results_MJ$df
recession_MJ <- results_MJ$summary
flows_S <- results_S$df
recession_S <- results_S$summary

results_MJ$Duration <- as.numeric(recession_MJ$EndDate - recession_MJ$StartDate)+1
results_CS$Duration <- as.numeric(recession_CS$EndDate - recession_CS$StartDate)+1
results_S$Duration  <- as.numeric(recession_S$EndDate - recession_S$StartDate)+1

plot_recession_group <- function(flows_df, recession_df, group_id, site_name = "") {
  # Get start and end date for this group
  event <- recession_df %>% filter(GroupID == group_id)
  
  if (nrow(event) == 0) {
    stop("Group ID not found.")
  }
  
  start_date <- event$StartDate
  end_date <- event$EndDate
  
  # Extend window ±3 days
  window_start <- start_date - 3
  window_end   <- end_date + 3
  
  # Subset flow data to this window
  subset_df <- flows_df %>%
    filter(Date >= window_start & Date <= window_end) %>%
    mutate(InGroup = ifelse(RecessionDay & GroupID == group_id, TRUE, FALSE))
  
  # Plot
  ggplot(subset_df, aes(x = Date, y = Flow)) +
    geom_line(color = "gray66") +
    geom_point(data = subset_df %>% filter(InGroup == TRUE),
               aes(x = Date, y = Flow),
               color = "red", size = 1.2) +
    labs(title = paste(site_name, "- Recession Group", group_id),
         subtitle = paste("From", format(start_date), "to", format(end_date)),
         x = "Date", y = "Flow (CFS)") +
    theme_minimal()
}

###Specific Event Plotter
##Change group_id to the event you want to analyze
##Find group ids in recession_site dfs

#Cootes Store
plot_recession_group(flows_CS, recession_CS, group_id = 382, site_name = "Cootes Store")

#Mount Jackson
plot_recession_group(flows_MJ, recession_MJ, group_id = 187, site_name = "Mount Jackson")

#Strasburg
plot_recession_group(flows_S, recession_S, group_id = 63, site_name = "Strasburg")


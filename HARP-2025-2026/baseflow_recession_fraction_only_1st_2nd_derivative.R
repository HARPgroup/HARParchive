library(dplyr)
library(tidyr)
library(ggplot2)

#load in stream data from USGS
flows_MJ <- dataRetrieval::readNWISdv("01633000",parameterCd = "00060")
flows_MJ <- dataRetrieval::renameNWISColumns(flows_MJ)

flows_CS <- dataRetrieval::readNWISdv("01632000",parameterCd = "00060")
flows_CS <- dataRetrieval::renameNWISColumns(flows_CS)

flows_S <- dataRetrieval::readNWISdv("01634000",parameterCd = "00060")
flows_S <- dataRetrieval::renameNWISColumns(flows_S)

normalized_pct_change <- function(x) {
  pct_change <- 100 * (x[-1] - x[-length(x)]) / x[-length(x)]
  pct_change[is.infinite(pct_change) | is.nan(pct_change)] <- 0
  return(pct_change)
}

flows_CS$S_norm <- c(NA, normalized_pct_change(flows_CS$Flow))
flows_CS$dSdt_norm <- c(NA, diff(flows_CS$S_norm))

flows_MJ$S_norm <- c(NA, normalized_pct_change(flows_MJ$Flow))
flows_MJ$dSdt_norm <- c(NA, diff(flows_MJ$S_norm))

flows_S$S_norm <- c(NA, normalized_pct_change(flows_S$Flow))
flows_S$dSdt_norm <- c(NA, diff(flows_S$S_norm))

flows_CS$is_stable_recession <- abs(flows_CS$dSdt_norm) < 0.1 
flows_MJ$is_stable_recession <- abs(flows_MJ$dSdt_norm) < 0.1
flows_S$is_stable_recession <- abs(flows_S$dSdt_norm) < 0.1

#add seasonal information
flows_CS$Month <- format(flows_CS$Date, "%m")

flows_CS$Season <- case_when(
  flows_CS$Month %in% c("12", "01", "02") ~ "Winter",
  flows_CS$Month %in% c("03", "04", "05") ~ "Spring",
  flows_CS$Month %in% c("06", "07", "08") ~ "Summer",
  flows_CS$Month %in% c("09", "10", "11") ~ "Fall"
)

flows_MJ$Month <- format(flows_MJ$Date, "%m")

flows_MJ$Season <- case_when(
  flows_MJ$Month %in% c("12", "01", "02") ~ "Winter",
  flows_MJ$Month %in% c("03", "04", "05") ~ "Spring",
  flows_MJ$Month %in% c("06", "07", "08") ~ "Summer",
  flows_MJ$Month %in% c("09", "10", "11") ~ "Fall"
)

flows_S <- flows_S %>%
  mutate(
    Month = format(Date, "%m"),
    Season = case_when(
      Month %in% c("12", "01", "02") ~ "Winter",
      Month %in% c("03", "04", "05") ~ "Spring",
      Month %in% c("06", "07", "08") ~ "Summer",
      Month %in% c("09", "10", "11") ~ "Fall",
      TRUE ~ NA_character_
    )
  )

rle_out_CS <- rle(flows_CS$is_stable_recession)
runs_CS <- data.frame(lengths = rle_out_CS$lengths,
                      values = rle_out_CS$values)
rle_out_MJ <- rle(flows_MJ$is_stable_recession)
runs_MJ <- data.frame(lengths = rle_out_MJ$lengths,
                      values = rle_out_MJ$values)
rle_out_S <- rle(flows_S$is_stable_recession)
runs_S <- data.frame(lengths = rle_out_S$lengths,
                     values = rle_out_S$values)

# Identify stable recession runs
runs_CS$flag <- with(runs_CS, values & lengths >= 0)
runs_MJ$flag <- with(runs_MJ, values & lengths >= 0)
runs_S$flag  <- with(runs_S,  values & lengths >= 0)

flows_CS$recession_event <- rep(runs_CS$flag, runs_CS$lengths)
flows_MJ$recession_event <- rep(runs_MJ$flag, runs_MJ$lengths)
flows_S$recession_event  <- rep(runs_S$flag,  runs_S$lengths)

flows_CS$RecessionDay  <- as.logical(flows_CS$recession_event)
flows_MJ$RecessionDay <- as.logical(flows_MJ$recession_event)
flows_S$RecessionDay  <- as.logical(flows_S$recession_event)


flows_CS  <- flows_CS[!is.na(flows_CS$RecessionDay), ]
flows_MJ <- flows_MJ[!is.na(flows_MJ$RecessionDay), ]
flows_S <- flows_S[!is.na(flows_S$RecessionDay), ]

analyze_recession <- function(df, site_name = "", min_len = 0, max_len = Inf) {
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
recession_CS_norm <- results_CS$summary
flows_MJ <- results_MJ$df
recession_MJ_norm <- results_MJ$summary
flows_S <- results_S$df
recession_S_norm <- results_S$summary

results_MJ$Duration <- as.numeric(recession_MJ_norm$EndDate - recession_MJ_frac$StartDate)+1
results_CS$Duration <- as.numeric(recession_CS_norm$EndDate - recession_CS_norm$StartDate)+1
results_S$Duration  <- as.numeric(recession_S_norm$EndDate - recession_S_norm$StartDate)+1

plot_recession_group <- function(flows_df, recession_df, group_id, site_name = "") {
  # Get start and end date for this group
  event <- recession_df %>% filter(GroupID == group_id)
  
  if (nrow(event) == 0) {
    stop("Group ID not found.")
  }
  
  start_date <- event$StartDate
  end_date <- event$EndDate
  
  # Extend window ±3 days
  window_start <- start_date - 30
  window_end   <- end_date + 30
  
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
plot_recession_group(flows_CS, recession_CS_norm, group_id = 717, site_name = "Cootes Store")

#Mount Jackson
plot_recession_group(flows_MJ, recession_MJ_norm, group_id = 117, site_name = "Mount Jackson")

#Strasburg
plot_recession_group(flows_S, recession_S_norm, group_id = 301, site_name = "Strasburg")





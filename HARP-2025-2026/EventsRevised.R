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

##FUNCTION TO HANDLE PCT CHANGE##
normalized_pct_change <- function(x) {
  pct_change <- (x[-1] - x[-length(x)]) / x[-length(x)]
  pct_change[is.infinite(pct_change) | is.nan(pct_change)] <- 0
  pct_change[x[-length(x)] == 0] <- 0 
  return(pct_change)
}

#FUNCTION TO HANDLE PCT CHANGE OF PCT CHANGE##
normalized_pct_change_of_change <- function (x) {
  change_pct_change <- (x[-1] - x[-length(x)]) / x[-length(x)]
  change_pct_change[is.infinite(change_pct_change) | is.nan(change_pct_change)] <- 0
  change_pct_change[x[-length(x)] == 0] <- 0
  return(change_pct_change)
}

##calculating AGWR AND change in AGWR using above functions##
flows_CS$AGWR <- c(NA, normalized_pct_change(flows_CS$Flow))
flows_CS$delta_AGWR <- c(NA, normalized_pct_change_of_change(flows_CS$AGWR))

flows_MJ$AGWR <- c(NA, normalized_pct_change(flows_MJ$Flow))
flows_MJ$delta_AGWR <- c(NA, normalized_pct_change_of_change(flows_MJ$AGWR))

flows_S$AGWR <- c(NA, normalized_pct_change(flows_S$Flow))
flows_S$delta_AGWR <- c(NA, normalized_pct_change_of_change(flows_S$AGWR))

#define "stable recession"
flows_CS$is_stable_recession <- abs(flows_CS$delta_AGWR) < 0.1
flows_MJ$is_stable_recession <- abs(flows_MJ$delta_AGWR) < 0.1
flows_S$is_stable_recession <- abs(flows_S$delta_AGWR) < 0.1

##BUILD FUNCTION TO HANDLE SEASONAL INFO##
add_month_season <- function(df) {
  df %>%
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
}
#apply the function to each dataset
flows_CS <- add_month_season(flows_CS)
flows_MJ <- add_month_season(flows_MJ)
flows_S  <- add_month_season(flows_S)

##DEFINE/DISPLAY LENGTH + STATUS OF CONTINUOUS RUN##
get_stable_runs <- function(logical_vec) {
  rle_out <- rle(logical_vec)
  data.frame(lengths = rle_out$lengths, values = rle_out$values)
}
runs_CS <- get_stable_runs(flows_CS$is_stable_recession)
runs_MJ <- get_stable_runs(flows_MJ$is_stable_recession)
runs_S  <- get_stable_runs(flows_S$is_stable_recession)

##FUNCTION FOR GAP FILLING TO FIND SMALL INTERRUPTIONS IN A SEQUENCE OF TRUE##
fill_short_gaps <- function(flag_vec, max_gap = 5) {
  flag_vec[is.na(flag_vec)] <- FALSE
  
  rle_out <- rle(flag_vec)
  lengths <- rle_out$lengths
  values <- rle_out$values
  
  for (i in seq(2, length(values) - 1)) {
    if (!values[i] && lengths[i] <= max_gap && values[i - 1] && values[i + 1]) {
      values[i] <- TRUE
    }
  }
  
  inverse_rle <- inverse.rle(list(lengths = lengths, values = values))
  return(inverse_rle)
}

##FUNCTION THAT FLAGS STABLE BASEFLOW OCCURANCES BASED ON PARAMETERS##
flag_stable_baseflow <- function(df,
                                 flow_col = "Flow",
                                 AGWR_col = "AGWR",
                                 delta_col = "delta_AGWR",
                                 max_flow = 300,
                                 max_AGWR = 0.1,
                                 max_delta = 0.1,
                                 max_gap = 5) {
  
  is_low_flow <- df[[flow_col]] < max_flow
  is_small_AGWR <- abs(df[[AGWR_col]]) < max_AGWR
  is_small_delta <- abs(df[[delta_col]]) < max_delta
  
  initial_flag <- is_low_flow & is_small_AGWR & is_small_delta
  
  final_flag <- fill_short_gaps(initial_flag, max_gap = max_gap)
  df$RecessionDay <- final_flag
  return(df)
}

##REASSIGN##
flows_CS <- flag_stable_baseflow(flows_CS)
flows_MJ <- flag_stable_baseflow(flows_MJ)
flows_S  <- flag_stable_baseflow(flows_S)

flows_CS <- flows_CS[!is.na(flows_CS$RecessionDay), ]
flows_MJ <- flows_MJ[!is.na(flows_MJ$RecessionDay), ]
flows_S  <- flows_S[!is.na(flows_S$RecessionDay), ]

##ANALYZE FLAGGED BASEFLOW RECESSION DAYS BY GROUPING CONSECUTIVE STRETCHES##
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
  
  recession_event_df$Duration <- as.numeric(recession_event_df$EndDate - recession_event_df$StartDate) + 1
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

##RECESSION DETECTION FOR EACH GAGE, EXTRACTS AND RENAMES THE RESULTING COMPONENTS##
library(purrr)
sites <- list(
  MJ = list(data = flows_MJ, name = "Mount Jackson"),
  CS = list(data = flows_CS, name = "Cootes Store"),
  S  = list(data = flows_S,  name = "Strasburg")
)

results <- imap(sites, function(site, abbrev) {
  result <- analyze_recession(site$data, site$name, min_len = 14)
  df <- result$df
  summary_df <- result$summary
  
  analysis_df <- df %>%
    filter(!is.na(GroupID)) %>%
    select(site_no, Date, Flow, AGWR, delta_AGWR, Month, Season, GroupID)
  
  list(
    df = df,
    summary = summary_df,
    analysis = analysis_df,
    name = site$name
  )
})

##AQWRC QUANTILE ANALYSIS##
analysis_CS <- results$CS$analysis
analysis_MJ <- results$MJ$analysis
analysis_S  <- results$S$analysis

AGWR_summary_CS <- analysis_CS %>%
  group_by(GroupID) %>%
  summarise(
    q1  = quantile(AGWR, 0.25, na.rm = TRUE),
    med = quantile(AGWR, 0.50, na.rm = TRUE),
    q3  = quantile(AGWR, 0.75, na.rm = TRUE),
    iqr = q3 - q1,
    duration = n(),
    .groups = "drop"
  ) %>%
  arrange(iqr)

AGWR_summary_MJ <- analysis_MJ %>%
  group_by(GroupID) %>%
  summarise(
    q1  = quantile(AGWR, 0.25, na.rm = TRUE),
    med = quantile(AGWR, 0.50, na.rm = TRUE),
    q3  = quantile(AGWR, 0.75, na.rm = TRUE),
    iqr = q3 - q1,
    duration = n(),
    .groups = "drop"
  ) %>%
  arrange(iqr)

AGWR_summary_S <- analysis_S %>%
  group_by(GroupID) %>%
  summarise(
    q1  = quantile(AGWR, 0.25, na.rm = TRUE),
    med = quantile(AGWR, 0.50, na.rm = TRUE),
    q3  = quantile(AGWR, 0.75, na.rm = TRUE),
    iqr = q3 - q1,
    duration = n(),
    .groups = "drop"
  ) %>%
  arrange(iqr)

analysis_CS <- analysis_CS %>% left_join(AGWR_summary_CS, by = "GroupID")
analysis_MJ <- analysis_MJ %>% left_join(AGWR_summary_MJ, by = "GroupID")
analysis_S  <- analysis_S  %>% left_join(AGWR_summary_S, by = "GroupID")

flows_CS <- results$CS$df
flows_MJ <- results$MJ$df
flows_S  <- results$S$df

flows_CS <- flows_CS %>% left_join(AGWR_summary_CS, by = "GroupID")
flows_MJ <- flows_MJ %>% left_join(AGWR_summary_MJ, by = "GroupID")
flows_S  <- flows_S  %>% left_join(AGWR_summary_S, by = "GroupID")

##FUNCTION TO PLOT INDIVIDUAL EVENTS##
plot_recession_group <- function(flows_df, recession_df, group_id, site_name = "") {
  event <- recession_df %>% filter(GroupID == group_id)
  if (nrow(event) == 0) stop("Group ID not found.")
  
  start_date <- event$StartDate
  end_date   <- event$EndDate
  window_start <- start_date - 30
  window_end   <- end_date + 30
  
  subset_df <- flows_df %>%
    filter(Date >= window_start & Date <= window_end) %>%
    mutate(InGroup = RecessionDay & GroupID == group_id)
  
  iqr_val <- unique(na.omit(subset_df$iqr[subset_df$GroupID == group_id]))[1]
  iqr_label <- if (!is.null(iqr_val) && !is.na(iqr_val)) paste("IQR =", signif(iqr_val, 3)) else "IQR = NA"
  
  ggplot(subset_df, aes(x = Date, y = Flow)) +
    geom_line(color = "gray66") +
    geom_point(data = subset_df %>% filter(InGroup), aes(x = Date, y = Flow), color = "red", size = 0.75) +
    labs(
      title = paste(site_name, "- Recession Group", group_id),
      subtitle = paste("From", format(start_date), "to", format(end_date)),
      caption = iqr_label,
      x = "Date", y = "Flow (CFS)"
    ) +
    theme_minimal()
}

###Specific Event Plotter
##Change group_id to the event you want to analyze
plot_recession_group(flows_CS, results$CS$summary, group_id = 2,  site_name = "Cootes Store")
plot_recession_group(flows_MJ, results$MJ$summary, group_id = 70, site_name = "Mount Jackson")
plot_recession_group(flows_S,  results$S$summary,  group_id = 1,  site_name = "Strasburg")

setwd("C://HARPgeneral//BensWork")
##EXPORT RESULTS##
write.csv(analysis_CS, "Cootes_Store_Recession_Quantile.csv", row.names = FALSE)
write.csv(analysis_MJ, "Mount_Jackson_Recession_Quantile.csv", row.names = FALSE)
write.csv(analysis_S,  "Strasburg_Recession_Quantile.csv", row.names = FALSE)

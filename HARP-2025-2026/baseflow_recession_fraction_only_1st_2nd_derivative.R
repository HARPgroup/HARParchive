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

normalized_pct_change <- function(x) {
  pct_change <- (x[-1] - x[-length(x)]) / x[-length(x)]
  pct_change[is.infinite(pct_change) | is.nan(pct_change)] <- 0
  pct_change[x[-length(x)] == 0] <- 0 
  return(pct_change)
}

normalized_pct_change_of_change <- function (x) {
  change_pct_change <- (x[-1] - x[-length(x)]) / x[-length(x)]
  change_pct_change[is.infinite(change_pct_change) | is.nan(change_pct_change)] <-0
  change_pct_change[x[-length(x)] == 0] <- 0
  return(change_pct_change)
}


flows_CS$AGWR <- c(NA, normalized_pct_change(flows_CS$Flow))
flows_CS$delta_AGWR <- c(NA, normalized_pct_change_of_change(flows_CS$AGWR))

flows_MJ$AGWR <- c(NA, normalized_pct_change(flows_MJ$Flow))
flows_MJ$delta_AGWR <- c(NA, normalized_pct_change_of_change(flows_MJ$AGWR))

flows_S$AGWR <- c(NA, normalized_pct_change(flows_S$Flow))
flows_S$delta_AGWR <- c(NA, normalized_pct_change_of_change(flows_S$AGWR))

flows_CS$is_stable_recession <- abs(flows_CS$delta_AGWR) < 0.1
flows_MJ$is_stable_recession <- abs(flows_MJ$delta_AGWR) < 0.1
flows_S$is_stable_recession <- abs(flows_S$delta_AGWR) < 0.1

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

fill_short_gaps <- function(flag_vec, max_gap = 1) {
  # Replace NA with FALSE
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

flag_stable_baseflow <- function(df,
                                 flow_col = "Flow",
                                 AGWR_col = "AGWR",
                                 delta_col = "delta_AGWR",
                                 max_flow = 300,
                                 max_AGWR = 0.2,
                                 max_delta = 0.2,
                                 max_gap = 1) {   # Add max_gap argument

  
  # Base logic
  is_low_flow <- df[[flow_col]] < max_flow
  is_small_AGWR <- abs(df[[AGWR_col]]) < max_AGWR
  is_small_delta <- abs(df[[delta_col]]) < max_delta
  
  initial_flag <- is_low_flow
  
  # Fill short gaps of FALSE in initial_flag surrounded by TRUE runs
  final_flag <- fill_short_gaps(initial_flag, max_gap = max_gap)
  
  df$RecessionDay <- final_flag
  return(df)
}


flows_CS <- flag_stable_baseflow(flows_CS)
flows_MJ <- flag_stable_baseflow(flows_MJ)
flows_S  <- flag_stable_baseflow(flows_S)

flows_CS <- flows_CS[!is.na(flows_CS$RecessionDay), ]
flows_MJ <- flows_MJ[!is.na(flows_MJ$RecessionDay), ]
flows_S  <- flows_S[!is.na(flows_S$RecessionDay), ]


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

#Change min length of event here
results_MJ <- analyze_recession(flows_MJ, "Mount Jackson", min_len = 14)
results_CS <- analyze_recession(flows_CS, "Cootes Store", min_len = 14)
results_S  <- analyze_recession(flows_S, "Strasburg", min_len = 14)


results_MJ$Site <- "Mount Jackson"
results_CS$Site <- "Cootes Store"
results_S$Site  <- "Strasburg"

flows_CS <- results_CS$df
recession_CS_AGWR <- results_CS$summary
flows_MJ <- results_MJ$df
recession_MJ_AGWR <- results_MJ$summary
flows_S <- results_S$df
recession_S_AGWR <- results_S$summary

results_MJ$Duration <- as.numeric(recession_MJ_AGWR$EndDate - recession_MJ_AGWR$StartDate)+1
results_CS$Duration <- as.numeric(recession_CS_AGWR$EndDate - recession_CS_AGWR$StartDate)+1
results_S$Duration  <- as.numeric(recession_S_AGWR$EndDate - recession_S_AGWR$StartDate)+1


#Derivative Plotter
# ggplot(flows_CS, aes(x = Date)) +
#   geom_line(aes(y = Flow*10), color = "blue"  , size = 0.75) +
#   geom_line(aes(y = AGWR), color = "red"  , size = 0.75) +  
#   scale_y_continuous(
#     name = "Normalized First Derivative",
#     sec.axis = sec_axis(~ . / 10, name = "Flow (CFS) * 10")
#   ) +
#   labs(title = "Cootes Store: Flow and First Derivative",
#        x = "Date") +
#   theme_minimal()
# 
# ggplot(flows_MJ, aes(x = Date)) +
#   geom_line(aes(y = Flow), color = "blue"  , size = 0.75) +
#   geom_line(aes(y = AGWR), color = "red"  , size = 0.75) +  
#   scale_y_continuous(
#     name = "Flow (CFS)",
#     sec.axis = sec_axis(~ . / 10, name = "Normalized First Derivative")
#   ) +
#   labs(title = "Mount Jackson: Flow and First Derivative",
#        x = "Date") +
#   theme_minimal()
# 
# ggplot(flows_S, aes(x = Date)) +
#   geom_line(aes(y = Flow), color = "blue"  , size = 0.75) +
#   geom_line(aes(y = AGWR*10), color = "red"  , size = 0.75) +  
#   scale_y_continuous(
#     name = "Flow (CFS)",
#     sec.axis = sec_axis(~ . / 10, name = "Normalized First Derivative *10")
#   ) +
#   labs(title = "Strasburg: Flow and First Derivative",
#        x = "Date") +
#   theme_minimal()
# 
# 
# ggplot(flows_CS, aes(x = Date)) +
#   geom_line(aes(y = Flow*10), color = "blue"  , size = 0.75) +
#   geom_line(aes(y = delta_AGWR), color = "red"  , size = 0.75) +  
#   scale_y_continuous(
#     name = "Normalized Second Derivative",
#     sec.axis = sec_axis(~ . / 10, name = "Flow (CFS)*10")
#   ) +
#   labs(title = "Cootes Store: Flow and Second Derivative",
#        x = "Date") +
#   theme_minimal()
# 
# ggplot(flows_MJ, aes(x = Date)) +
#   geom_line(aes(y = Flow), color = "blue"  , size = 0.75) +
#   geom_line(aes(y = delta_AGWR), color = "red"  , size = 0.75) +  
#   scale_y_continuous(
#     name = "Flow (CFS)",
#     sec.axis = sec_axis(~ . / 10, name = "Normalized Second Derivative")
#   ) +
#   labs(title = "Mount Jackson: Flow and Second Derivative",
#        x = "Date") +
#   theme_minimal()
# 
# ggplot(flows_S, aes(x = Date)) +
#   geom_line(aes(y = Flow), color = "blue"  , size = 0.75) +
#   geom_line(aes(y = delta_AGWR*10), color = "red"  , size = 0.75) +  
#   scale_y_continuous(
#     name = "Flow (CFS)",
#     sec.axis = sec_axis(~ . / 10, name = "Normalized Second Derivative*10")
#   ) +
#   labs(title = "Strasburg: Flow and Second Derivative",
#        x = "Date") +
#   theme_minimal()
# 

# Cootes Store
analysis_CS <- flows_CS %>%
  filter(!is.na(GroupID)) %>%
  select(site_no, Date, Flow, AGWR, delta_AGWR, Month, Season, GroupID)

# Mount Jackson
analysis_MJ <- flows_MJ %>%
  filter(!is.na(GroupID)) %>%
  select(site_no, Date, Flow, AGWR, delta_AGWR, Month, Season, GroupID)

# Strasburg
analysis_S <- flows_S %>%
  filter(!is.na(GroupID)) %>%
  select(site_no, Date, Flow, AGWR, delta_AGWR, Month, Season, GroupID)


#AQWRC Quantile Analysis
AGWR_summary_CS <- analysis_CS %>%
  filter(!is.na(GroupID)) %>%
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

analysis_CS <- analysis_CS %>%
  left_join(AGWR_summary_CS, by = "GroupID")

AGWR_summary_MJ <- analysis_MJ %>%
  filter(!is.na(GroupID)) %>%
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

analysis_MJ <- analysis_MJ %>%
  left_join(AGWR_summary_MJ, by = "GroupID")

AGWR_summary_S <- analysis_S %>%
  filter(!is.na(GroupID)) %>%
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

#join back with flows
flows_CS <- flows_CS %>%
  left_join(AGWR_summary_S, by = "GroupID")

analysis_S <- analysis_S %>%
  left_join(AGWR_summary_S, by = "GroupID")

analysis_S <- analysis_S %>%
  left_join(AGWR_summary_S, by = "GroupID")

analysis_S <- analysis_S %>%
  left_join(AGWR_summary_S, by = "GroupID")


group_iqr <- flows_CS %>%
  filter(!is.na(GroupID)) %>%
  group_by(GroupID) %>%
  summarise(IQR_AGWR = IQR(AGWR, na.rm = TRUE))  

flows_CS <- flows_CS %>%
  left_join(group_iqr, by = "GroupID")

group_iqr <- flows_MJ %>%
  filter(!is.na(GroupID)) %>%
  group_by(GroupID) %>%
  summarise(IQR_AGWR = IQR(AGWR, na.rm = TRUE))  

flows_MJ <- flows_MJ %>%
  left_join(group_iqr, by = "GroupID")

group_iqr <- flows_S %>%
  filter(!is.na(GroupID)) %>%
  group_by(GroupID) %>%
  summarise(IQR_AGWR = IQR(AGWR, na.rm = TRUE))  

flows_S <- flows_S %>%
  left_join(group_iqr, by = "GroupID")

plot_recession_group <- function(flows_df, recession_df, group_id, site_name = "") {
  # Get start and end date for this group
  event <- recession_df %>% filter(GroupID == group_id)
  
  if (nrow(event) == 0) {
    stop("Group ID not found.")
  }
  
  start_date <- event$StartDate
  end_date <- event$EndDate
  
  # Extend window ±30 days
  window_start <- start_date - 30
  window_end   <- end_date + 30
  
  # Subset flow data to this window
  subset_df <- flows_df %>%
    filter(Date >= window_start & Date <= window_end) %>%
    mutate(InGroup = ifelse(RecessionDay & GroupID == group_id, TRUE, FALSE))
  
  # Grab IQR for this group
  iqr_val <- unique(na.omit(subset_df$IQR_AGWR[subset_df$GroupID == group_id]))
  iqr_val <- iqr_val[1]
  
  if (!is.null(iqr_val) && length(iqr_val) > 0 && !is.na(iqr_val)) {
    iqr_label <- paste("IQR =", signif(iqr_val, 3))
  } else {
    iqr_label <- "IQR = NA"
  }
  
  # Plot
  ggplot(subset_df, aes(x = Date, y = Flow)) +
    geom_line(color = "gray66") +
    geom_point(data = subset_df %>% filter(InGroup == TRUE),
               aes(x = Date, y = Flow),
               color = "red"  , size = 0.75) +
    labs(title = paste(site_name, "- Recession Group", group_id),
         subtitle = paste("From", format(start_date), "to", format(end_date)),
         caption = iqr_label,
         x = "Date", y = "Flow (CFS)") +
    theme_minimal()
}

###Specific Event Plotter
##Change group_id to the event you want to analyze
##Find group ids in recession_site dfs

#Cootes Store
plot_recession_group(flows_CS, recession_CS_AGWR, group_id = 24, site_name = "Cootes Store")

#Mount Jackson
plot_recession_group(flows_MJ, recession_MJ_AGWR, group_id = 212, site_name = "Mount Jackson")

#Strasburg
plot_recession_group(flows_S, recession_S_AGWR, group_id = 32, site_name = "Strasburg")

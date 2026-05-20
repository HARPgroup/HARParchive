# Load required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)
library(dataRetrieval)
library(purrr)

# Load in stream data from USGS
flows_CS <- readNWISdv("01632000", parameterCd = "00060") %>% renameNWISColumns()
flows_MJ <- readNWISdv("01633000", parameterCd = "00060") %>% renameNWISColumns()
flows_S  <- readNWISdv("01634000", parameterCd = "00060") %>% renameNWISColumns()

##CORE CALCULATIONS##
#AGWR = Qt / Qt-1
calc_AGWR <- function(x) {
  c(NA, x[-1] / x[-length(x)])
}
#delta_AGWR = AGWR_t / AGWR_t-1
calc_delta_AGWR <- function(x) {
  c(NA, x[-1] / x[-length(x)])
}
#calculate AGWR and delta_AGWR
flows_CS$AGWR <- calc_AGWR(flows_CS$Flow)
flows_CS$delta_AGWR <- calc_delta_AGWR(flows_CS$AGWR)

flows_MJ$AGWR <- calc_AGWR(flows_MJ$Flow)
flows_MJ$delta_AGWR <- calc_delta_AGWR(flows_MJ$AGWR)

flows_S$AGWR <- calc_AGWR(flows_S$Flow)
flows_S$delta_AGWR <- calc_delta_AGWR(flows_S$AGWR)
#add seasonal info
add_month_season <- function(df) {
  df %>% mutate(
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
flows_CS <- add_month_season(flows_CS)
flows_MJ <- add_month_season(flows_MJ)
flows_S  <- add_month_season(flows_S)

##GAP FILLER##
gap_fill <- function(flag_vec, max_gap = 5) {
  flag_vec[is.na(flag_vec)] <- FALSE
  rle_out <- rle(flag_vec)
  lengths <- rle_out$lengths
  values <- rle_out$values
  
  for (i in seq(2, length(values) - 1)) {
    if (!values[i] && lengths[i] <= max_gap && values[i - 1] && values[i + 1]) {
      values[i] <- TRUE
    }
  }
  # for(i in seq(2, length(values) - 1)) {
  #   # Compare flow at i to overall mean flow
  #   if (flow_vec[i] >= 1.15 * mean(flow_vec, na.rm = TRUE)) {
  #     values[i] <- FALSE
  #   }
  # }
  
  inverse.rle(list(lengths = lengths, values = values))
}


##FLAG STABLE BASEFLOW DAYS BASED ON delta_AGWR ~ 1 and AGWR < 1##
flag_stable_baseflow <- function(df,
                                 flow_col,
                                 AGWR_col = "AGWR",
                                 delta_col = "delta_AGWR",
                                 delta_thresh = 0.03,
                                 max_gap = 3) {
  AGWR <- df[[AGWR_col]]
  delta <- df[[delta_col]]
  
  is_stable <- abs(delta - 1.0) < delta_thresh & AGWR < 1.0
  df$RecessionDay <- gap_fill( is_stable, max_gap)
  
  for(i in 1:length(df$RecessionDay)) {
    # Compare flow at i to overall mean flow
    if (flow_col[i] >= 1.15 * mean(flow_col, na.rm = TRUE)) {
      df$RecessionDay <- FALSE
    }
  return(df)
  }
}

flows_CS <- flag_stable_baseflow(flows_CS, flows_CS$Flow)
flows_MJ <- flag_stable_baseflow(flows_MJ, flows_MJ$Flow)
flows_S  <- flag_stable_baseflow(flows_S, flows_S$Flow)
#remove NAs
flows_CS <- flows_CS[!is.na(flows_CS$RecessionDay), ]
flows_MJ <- flows_MJ[!is.na(flows_MJ$RecessionDay), ]
flows_S  <- flows_S[!is.na(flows_S$RecessionDay), ]

##RECESSION GROUP ANALYZER##

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
  require(tidyr)
  require(purrr)
  require(dplyr)
  df$GroupID <- group_id
  
  recession_starts <- starts[!is.na(group_id[starts])]
  recession_ends   <- ends[!is.na(group_id[starts])]
  
  recession_event_df <- data.frame(
    GroupID   = unique(na.omit(group_id)),
    StartDate = df$Date[recession_starts],
    EndDate   = df$Date[recession_ends]
  )
  recession_event_df$Duration <- as.numeric(recession_event_df$EndDate - recession_event_df$StartDate) + 1
  recession_event_df$DaysBetween <- c(NA, as.numeric(difftime(recession_event_df$StartDate[-1],
                                                              recession_event_df$EndDate[-nrow(recession_event_df)],
                                                              units = "days")))
  
  max_len <- max(recession_event_df$Duration, na.rm = TRUE)
  longest <- which.max(recession_event_df$Duration)
  cat("\n===== Recession Analysis for", site_name, "=====\n")
  cat("Longest recession lasted", max_len, "days\n")
  cat("From", recession_event_df$StartDate[longest], "to", recession_event_df$EndDate[longest], "\n")
  
  list(df = df, summary = recession_event_df)
}
#apply to all three sites
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
  
  list(df = df, summary = summary_df, analysis = analysis_df, name = site$name)
})
#extract
analysis_CS <- results$CS$analysis
analysis_MJ <- results$MJ$analysis
analysis_S  <- results$S$analysis

##OPTIONAL: PLOT A RECESSION GROUP##
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
  
  ggplot(subset_df, aes(x = Date, y = Flow)) +
    geom_line(color = "gray66") +
    geom_point(data = subset_df %>% filter(InGroup), aes(x = Date, y = Flow), color = "red", size = 0.75) +
    labs(
      title = paste(site_name, "- Recession Group", group_id),
      subtitle = paste("From", format(start_date), "to", format(end_date)),
      x = "Date", y = "Flow (CFS)"
    ) +
    theme_minimal()
}
#cootes store, GroupID ___ example
plot_recession_group(
  flows_df    = results$CS$df,
  recession_df = results$CS$summary,
  group_id    = 12,
  site_name   = "Cootes Store"
)

##COMPUTE IQR##
AGWR_summary_CS <- analysis_CS %>%
  group_by(GroupID) %>%
  summarise(
    q1  = quantile(AGWR, 0.25, na.rm = TRUE),
    med = quantile(AGWR, 0.50, na.rm = TRUE),
    q3  = quantile(AGWR, 0.75, na.rm = TRUE),
    iqr = q3 - q1,
    duration = n(),
    .groups = "drop"
  )

AGWR_summary_MJ <- analysis_MJ %>%
  group_by(GroupID) %>%
  summarise(
    q1  = quantile(AGWR, 0.25, na.rm = TRUE),
    med = quantile(AGWR, 0.50, na.rm = TRUE),
    q3  = quantile(AGWR, 0.75, na.rm = TRUE),
    iqr = q3 - q1,
    duration = n(),
    .groups = "drop"
  )

AGWR_summary_S <- analysis_S %>%
  group_by(GroupID) %>%
  summarise(
    q1  = quantile(AGWR, 0.25, na.rm = TRUE),
    med = quantile(AGWR, 0.50, na.rm = TRUE),
    q3  = quantile(AGWR, 0.75, na.rm = TRUE),
    iqr = q3 - q1,
    duration = n(),
    .groups = "drop"
  )
#merge
results$CS$summary <- left_join(results$CS$summary, AGWR_summary_CS, by = "GroupID")
results$MJ$summary <- left_join(results$MJ$summary, AGWR_summary_MJ, by = "GroupID")
results$S$summary  <- left_join(results$S$summary,  AGWR_summary_S,  by = "GroupID")

##TRYING TO DO PLOT-BATCH AUTOMATION##
batch_plot_recessions <- function(flows_df, summary_df, site_name, site_abbr,
                                  iqr_threshold = 0.05, min_duration = 14) {
  # Only keep events that have IQR column (computed during earlier quantile summary)
  if (!"iqr" %in% names(summary_df)) {
    stop("summary_df must include 'iqr' column.")
  }
  
  filtered <- summary_df %>%
    filter(Duration >= min_duration, iqr < iqr_threshold)
  
  message("Found ", nrow(filtered), " events for ", site_name)
  
  for (gid in filtered$GroupID) {
    p <- plot_recession_group(
      flows_df    = flows_df,
      recession_df = summary_df,
      group_id    = gid,
      site_name   = site_name
    )
    
    ggsave(
      filename = paste0("Recession_Plots/", site_abbr, "_Group_", gid, ".jpg"),
      plot     = p,
      width    = 8,
      height   = 5,
      dpi      = 300
    )
  }
}

batch_plot_recessions(results$CS$df, results$CS$summary, "Cootes Store", "CS")
batch_plot_recessions(results$MJ$df, results$MJ$summary, "Mount Jackson", "MJ")
batch_plot_recessions(results$S$df,  results$S$summary,  "Strasburg",     "S")

##COMBINED AGWR SUMMARY##
combined_events <- bind_rows(
  results$CS$analysis %>% mutate(site = "CS"),
  results$MJ$analysis %>% mutate(site = "MJ"),
  results$S$analysis  %>% mutate(site = "S")
  )
summary(combined_events$AGWR)
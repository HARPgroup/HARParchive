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

# Fraction Change Derivative Calculator
frac_change <- function(x, order = 1) {
  if (order == 1) {
    result <- (x[-1] - x[-length(x)]) / x[-length(x)]
    result[is.infinite(result) | is.nan(result)] <- NA  # <-- must come before return
    return(result)
  } else if (order == 2) {
    first_deriv <- frac_change(x, order = 1)
    result <- (first_deriv[-1] - first_deriv[-length(first_deriv)]) / first_deriv[-length(first_deriv)]
    result[is.infinite(result) | is.nan(result)] <- NA  # <-- same fix here
    return(result)
  } else {
    stop("Only supports order = 1 or 2")
  }
}



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

# Calculate frac change derivatives Cootes Store
flows_CS$S_frac <- c(NA, frac_change(flows_CS$Flow))              
flows_CS$dSdt_frac <- c(NA, NA, frac_change(flows_CS$Flow, order = 2))  

# Calculate derivatives Mount Jackson
flows_MJ$S_frac <- c(NA, frac_change(flows_MJ$Flow))              
flows_MJ$dSdt_frac <- c(NA, NA, frac_change(flows_MJ$Flow, order = 2))  

# Calculate derivatives Strasburg
flows_S$S_frac <- c(NA, frac_change(flows_S$Flow))              
flows_S$dSdt_frac <- c(NA, NA, frac_change(flows_S$Flow, order = 2))

#Stable Recessions:
flows_CS$is_S_frac_negative <- flows_CS$S_frac <= 0
flows_MJ$is_S_frac_negative <- flows_MJ$S_frac <= 0
flows_S$is_S_frac_negative <- flows_S$S_frac <= 0

flows_CS$dSdt_frac_recent_neg <- zoo::rollapplyr(
  flows_CS$dSdt_frac < 0, 
  width = 3, #daily buffer for days since second derivative was negative
  FUN = any, 
  partial = TRUE, 
  fill = NA
)

flows_MJ$dSdt_frac_recent_neg <- zoo::rollapplyr(
  flows_MJ$dSdt_frac < 0, 
  width = 3, #width is daily buffer
  FUN = any, 
  partial = TRUE, 
  fill = NA
)

flows_S$dSdt_frac_recent_neg <- zoo::rollapplyr(
  flows_S$dSdt_frac < 0, 
  width = 3, #daily buffer
  FUN = any, 
  partial = TRUE, 
  fill = NA
)

sum(flows_CS$S_frac == 0, na.rm = TRUE)


#To change low flow percentile, change flow quantile
flows_CS <- flows_CS %>%
  group_by(Month) %>%
  mutate(low_flow = Flow < quantile(Flow, 0.15, na.rm = TRUE)) %>%
  ungroup()
flows_MJ <- flows_MJ %>%
  group_by(Month) %>%
  mutate(low_flow = Flow < quantile(Flow, 0.15, na.rm = TRUE)) %>%
  ungroup()
flows_S <- flows_S %>%
  group_by(Month) %>%
  mutate(low_flow = Flow < quantile(Flow, 0.15, na.rm = TRUE)) %>%
  ungroup()


flows_CS$is_stable_recession <- flows_CS$is_S_frac_negative & flows_CS$dSdt_frac_recent_neg & flows_CS$low_flow
flows_MJ$is_stable_recession <- flows_MJ$is_S_frac_negative & flows_MJ$dSdt_frac_recent_neg & flows_MJ$low_flow
flows_S$is_stable_recession <- flows_S$is_S_frac_negative & flows_S$dSdt_frac_recent_neg & flows_S$low_flow


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
stable_recessions_only_S <- flows_S[flows_S$is_stable_recession == TRUE, ]

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
recession_CS_frac <- results_CS$summary
flows_MJ <- results_MJ$df
recession_MJ_frac <- results_MJ$summary
flows_S <- results_S$df
recession_S_frac <- results_S$summary

results_MJ$Duration <- as.numeric(recession_MJ_frac$EndDate - recession_MJ_frac$StartDate)+1
results_CS$Duration <- as.numeric(recession_CS_frac$EndDate - recession_CS_frac$StartDate)+1
results_S$Duration  <- as.numeric(recession_S_frac$EndDate - recession_S_frac$StartDate)+1

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
plot_recession_group(flows_CS, recession_CS_frac, group_id = 268, site_name = "Cootes Store")

#Mount Jackson
plot_recession_group(flows_MJ, recession_MJ_frac, group_id = 163, site_name = "Mount Jackson")

#Strasburg
plot_recession_group(flows_S, recession_S_frac, group_id = 108, site_name = "Strasburg")


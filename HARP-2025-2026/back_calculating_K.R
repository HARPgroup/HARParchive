###work flow
###This close follows the workflow found in this paper:
### https://www.researchgate.net/publication/351589135_Watershed-Scale_Effective_Hydraulic_Properties_of_the_Continental_United_States
##find periods of recessions ~week long
##transform data to a log form
##plot log form and estimate a early
##plug into Brutsaert equation with assumptions from Tashie's Paper
##rearrange and solve for K

#load in useful packages
library(hydrotools)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(sqldf)

#load in stream data from USGS
flows_MJ <- dataRetrieval::readNWISdv("01633000",parameterCd = "00060")
flows_MJ <- dataRetrieval::renameNWISColumns(flows_MJ)

flows_CS <- dataRetrieval::readNWISdv("01632000",parameterCd = "00060")
flows_CS <- dataRetrieval::renameNWISColumns(flows_CS)

flows_S <- dataRetrieval::readNWISdv("01634000",parameterCd = "00060")
flows_S <- dataRetrieval::renameNWISColumns(flows_S)

# Recession event extractor
extract_recession_events <- function(flow_df, min_len = 4, max_len = 7) {
  df <- flow_df %>%
    arrange(Date) %>%
    mutate(
      Flow_next = lead(Flow),
      dQ_dt = -c(NA, diff(Flow)),  # Estimate -dQ/dt
      dQ_dt_next = lead(dQ_dt),
      Q_decreasing = Flow > Flow_next,
      dQ_dt_decreasing = dQ_dt > dQ_dt_next,
      valid_decrease = Q_decreasing & dQ_dt_decreasing
    )
  
  # Use run-length encoding to find consecutive valid decreases
  rle_vals <- rle(df$valid_decrease)
  ends <- cumsum(rle_vals$lengths)
  starts <- ends - rle_vals$lengths + 1
  
  group_id <- rep(NA, nrow(df))
  group_counter <- 1
  for (i in seq_along(rle_vals$lengths)) {
    if (rle_vals$values[i] && rle_vals$lengths[i] >= min_len && rle_vals$lengths[i] <= max_len) {
      group_id[starts[i]:ends[i]] <- group_counter
      group_counter <- group_counter + 1
    }
  }
  
  df$group <- group_id
  df <- df %>% filter(!is.na(group))
  
  return(df)
}

# Apply to each gage for 4–7 day recession periods
decreasing_events_MJ <- extract_recession_events(flows_MJ, min_len = 4, max_len = 7)
decreasing_events_CS <- extract_recession_events(flows_CS, min_len = 4, max_len = 7)
decreasing_events_S  <- extract_recession_events(flows_S,  min_len = 4, max_len = 7)

cat("MJ:", n_distinct(decreasing_events_MJ$group), "\n")
cat("CS:", n_distinct(decreasing_events_CS$group), "\n")
cat("S :", n_distinct(decreasing_events_S$group), "\n")

#apply log transformation
event_MJ <- decreasing_events_MJ %>%
  filter(group == 24) %>%
  arrange(Date) %>%
  mutate(
    dQ_dt = -c(NA, diff(Flow)),  
    log_Q = log10(Flow),
    log_dQ_dt = log10(dQ_dt)
  ) %>%
  filter(!is.na(log_dQ_dt), is.finite(log_dQ_dt)) 
fit_MJ <- lm(log_dQ_dt ~ log_Q, data = event_MJ)
summary(fit_MJ)


event_CS <- decreasing_events_CS %>%
  filter(group == 17) %>%
  arrange(Date) %>%
  mutate(
    dQ_dt = -c(NA, diff(Flow)),  
    log_Q = log10(Flow),
    log_dQ_dt = log10(dQ_dt)
  ) %>%
  filter(!is.na(log_dQ_dt), is.finite(log_dQ_dt))  
fit_CS <- lm(log_dQ_dt ~ log_Q, data = event_CS)
summary(fit_CS)

event_S <- decreasing_events_S %>%
  filter(group == 6) %>%
  arrange(Date) %>%
  mutate(
    dQ_dt = -c(NA, diff(Flow)),  # estimate -dQ/dt
    log_Q = log10(Flow),
    log_dQ_dt = log10(dQ_dt)
  ) %>%
  filter(!is.na(log_dQ_dt), is.finite(log_dQ_dt))  # clean NAs/Infs
fit_S <- lm(log_dQ_dt ~ log_Q, data = event_S)
summary(fit_S)

# Apply to each gage for 4–7 day recession periods
decreasing_events_MJ <- extract_recession_events(flows_MJ, min_len = 4, max_len = 7)
decreasing_events_CS <- extract_recession_events(flows_CS, min_len = 4, max_len = 7)
decreasing_events_S  <- extract_recession_events(flows_S,  min_len = 4, max_len = 7)

cat("MJ:", n_distinct(decreasing_events_MJ$group), "\n")
cat("CS:", n_distinct(decreasing_events_CS$group), "\n")
cat("S :", n_distinct(decreasing_events_S$group), "\n")

# Add a 'gage' column to each event data frame
event_MJ <- event_MJ %>% mutate(gage = "MJ")
event_CS <- event_CS %>% mutate(gage = "CS")
event_S  <- event_S  %>% mutate(gage = "S")

# Combine them into one data frame
combined_events <- bind_rows(event_MJ, event_CS, event_S)
intercept_ref <- mean(combined_events$log_dQ_dt, na.rm = TRUE) - 3 * mean(combined_events$log_Q, na.rm = TRUE)

# Plot all on one graph with colors for each gage
ggplot(combined_events, aes(x = log_Q, y = log_dQ_dt, color = gage)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +  # regression line per gage
  labs(
    title = "Recession Events: log(-dQ/dt) vs log(Q) by Gage",
    x = "log10(Flow, Q)",
    y = "log10(-dQ/dt)"
  ) +
  geom_abline(intercept = intercept_ref, slope = 3, color = "black", 
             size = 1)+
  theme_minimal()

fit_early_MJ <- lm(log_dQ_dt ~ offset(3 * log_Q), data = event_MJ)
a_early_MJ <- coef(fit_early_MJ)[1]
print(a_early_MJ)

fit_early_CS <- lm(log_dQ_dt ~ offset(3 * log_Q), data = event_CS)
a_early_CS <- coef(fit_early_CS)[1]
print(a_early_CS)

fit_early_S <- lm(log_dQ_dt ~ offset(3 * log_Q), data = event_S)
a_early_S <- coef(fit_early_S)[1]
print(a_early_S)

#K: hydro conductivity
#f: drainable porosity (paper averaged 0.1 globally)
#Th: Drainable aquifer thickness
#L: Stream Network Length
#i = slope

# K_MJ <- 1.33 / (a_early_linear_MJ * f_MJ * Th_MJ^3 * L_MJ^2 * cos(i_MJ))
# a_early_MJ_linear <- 10^a_early_MJ
# f_MJ <- 0.1 
# Th_MJ <-
# L_MJ <-
# i_MJ <-
# 
# K_CS <- 1.33 / (a_early_linear_CS * f_CS * Th_CS^3 * L_CS^2 * cos(i_CS))
# a_early_linear_CS <- 10^a_early_CS 
# f_CS <- 0.1
# Th_CS <-
# L_CS <-
# i_CS <-
# 
# K_S <- 1.33 / (a_early_linear_S * f_S * Th_S^3 * L_S^2 * cos(i_S))
# a_early_linear_S <- 10^a_early_S 
# f_S <- 0.1
# Th_S <-
# L_S <-
# i_S <-
#   
# print(K_MJ)
# print(K_CS)
# print(K_S)
# 
# 

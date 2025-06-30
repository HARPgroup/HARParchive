###work flow
###This close follows the workflow found in this paper:
### https://www.researchgate.net/publication/351589135_Watershed-Scale_Effective_Hydraulic_Properties_of_the_Continental_United_States
##find periods of recessions ~week long
##transform data to a log form
##plot log form and estimate a early
##plug into Brutsaert equation with assumptions from Tashie's Paper
##rearrange and solve for K

###NOT FULLY FUNCTIONAL YET 
##Line 151-180 need values for the variables before it is functional

#load in useful packages
library(hydrotools)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(sqldf)
library(grwat)

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


##Plotting Base flow vs Regression Periods

base_MJ <- flows_MJ %>% 
  mutate(Qbase = gr_baseflow(Flow, method = 'lynehollick'))

base_CS <- flows_CS %>% 
  mutate(Qbase = gr_baseflow(Flow, method = 'lynehollick'))

base_S <- flows_S %>% 
  mutate(Qbase = gr_baseflow(Flow, method = 'lynehollick'))

flows_MJ <- flows_MJ %>%
  left_join(base_MJ %>% select(Date, Qbase), by = "Date")

flows_CS <- flows_CS %>%
  left_join(base_CS %>% select(Date, Qbase), by = "Date")

flows_S <- flows_S %>%
  left_join(base_S %>% select(Date, Qbase), by = "Date")

#note only event 24 was used in the regression
#but I thought it would be interesting to graph 2 recession events so close to 
#each other
ggplot(data=flows_MJ, aes(x=Date))+
  geom_line(aes(x=Date, y= Flow), color = "blue")+
  geom_line(aes(x=Date, y = Qbase), color ="red")+
  scale_x_date(limits = as.Date(c("1948-08-04", "1948-08-19"))) +
  ggtitle("Baseflow (Red) vs. Flow (Blue) During Recession Events 24 and 25 Mount Jackson (1948)") +
  ylim(0, 3000) +
  ylab("Discharge (CFS)") +
  theme_minimal()

ggplot(data=flows_CS, aes(x=Date))+
  geom_line(aes(x=Date, y= Flow), color = "blue")+
  geom_line(aes(x=Date, y = Qbase), color ="red")+
  scale_x_date(limits = as.Date(c("1929-10-02", "1929-10-10"))) +
  ggtitle("Baseflow (Red) vs. Flow (Blue) During Recession Event 17 Cootes Store (1929)") +
  ylim(0, 1000) +
  ylab("Discharge (CFS)") +
  theme_minimal()

ggplot(data=flows_S, aes(x=Date))+
  geom_line(aes(x=Date, y= Flow), color = "blue")+
  geom_line(aes(x=Date, y = Qbase), color ="red")+
  scale_x_date(limits = as.Date(c("1928-04-13", "1928-04-20"))) +
  ggtitle("Baseflow (Red) vs. Flow (Blue) During Recession Event 6 Strasburg (1948)") +
  ylim(0, 3000) +
  ylab("Discharge (CFS)") +
  theme_minimal()

#a whole year
ggplot(data=flows_MJ, aes(x=Date))+
  geom_line(aes(x=Date, y= Flow), color = "blue")+
  geom_line(aes(x=Date, y = Qbase), color ="red")+
  scale_x_date(limits = as.Date(c("1948-01-01", "1948-12-31"))) +
  ggtitle("Baseflow (Red) vs. Flow (Blue) Mount Jackson 1948") +
  ylim(0, 5000) +
  ylab("Discharge (CFS)") +
  theme_minimal()

ggplot(data=flows_CS, aes(x=Date))+
  geom_line(aes(x=Date, y= Flow), color = "blue")+
  geom_line(aes(x=Date, y = Qbase), color ="red")+
  scale_x_date(limits = as.Date(c("1929-01-01", "1929-12-31"))) +
  ggtitle("Baseflow (Red) vs. Flow (Blue) Cootes Store 1929") +
  ylim(0, 5000) +
  ylab("Discharge (CFS)") +
  theme_minimal()

ggplot(data=flows_S, aes(x=Date))+
  geom_line(aes(x=Date, y= Flow), color = "blue")+
  geom_line(aes(x=Date, y = Qbase), color ="red")+
  scale_x_date(limits = as.Date(c("1928-01-01", "1928-12-31"))) +
  ggtitle("Baseflow (Red) vs. Flow (Blue) Strasburg 1948") +
  ylim(0, 5000) +
  ylab("Discharge (CFS)") +
  theme_minimal()

#whole gage data just baseflow
ggplot(data=flows_MJ, aes(x=Date))+
  geom_line(aes(x=Date, y = Qbase), color ="red")+
  ggtitle("Baseflow of Mount Jackson Lynehollick Method") +
  ylab("Baseflow Discharge (CFS)") +
  theme_minimal()

ggplot(data=flows_CS, aes(x=Date))+
  geom_line(aes(x=Date, y = Qbase), color ="red")+
  ggtitle("Baseflow  of Cootes Store Lynehollick Method") +
  ylab("Discharge (CFS)") +
  theme_minimal()

ggplot(data=flows_S, aes(x=Date))+
  geom_line(aes(x=Date, y = Qbase), color ="red")+
  ggtitle("Baseflow of Strasburg Lynehollick Method") +
  ylab("Discharge (CFS)") +
  theme_minimal()


#Whole time compared to discharge
ggplot(data=flows_MJ, aes(x=Date))+
  geom_line(aes(x=Date, y= Flow), color = "blue")+
  geom_line(aes(x=Date, y = Qbase), color ="red")+
  ggtitle("Baseflow (Red) vs. Flow (Blue) Mount Jackson") +
  ylab("Discharge (CFS)") +
  theme_minimal()

ggplot(data=flows_CS, aes(x=Date))+
  geom_line(aes(x=Date, y= Flow), color = "blue")+
  geom_line(aes(x=Date, y = Qbase), color ="red")+
  ggtitle("Baseflow (Red) vs. Flow (Blue) Cootes Store") +
  ylab("Discharge (CFS)") +
  theme_minimal()

ggplot(data=flows_S, aes(x=Date))+
  geom_line(aes(x=Date, y= Flow), color = "blue")+
  geom_line(aes(x=Date, y = Qbase), color ="red")+
  ggtitle("Baseflow (Red) vs. Flow (Blue) Strasburg") +
  ylab("Discharge (CFS)") +
  theme_minimal()

# Base Flow Recession event extractor
extract_Base_Flow_recession_events <- function(flow_df, min_len = 4, max_len = 7) {
  df <- flow_df %>%
    arrange(Date) %>%
    mutate(
      Qbase_next = lead(Qbase),
      dQ_dt = -c(NA, diff(Qbase)),  # Estimate -dQ/dt
      dQ_dt_next = lead(dQ_dt),
      Q_decreasing = Qbase > Qbase_next,
      dQ_dt_decreasing = dQ_dt > dQ_dt_next,
      valid_decrease = Q_decreasing
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

# Apply to each gage for 4–10 day recession periods
decreasing_base_events_MJ <- extract_Base_Flow_recession_events(flows_MJ, min_len = 4, max_len = 10)
decreasing_base_events_CS <- extract_Base_Flow_recession_events(flows_CS, min_len = 4, max_len = 10)
decreasing_base_events_S  <- extract_Base_Flow_recession_events(flows_S,  min_len = 4, max_len = 10)

cat("MJ:", n_distinct(decreasing_base_events_MJ$group), "\n")
cat("CS:", n_distinct(decreasing_base_events_CS$group), "\n")
cat("S :", n_distinct(decreasing_base_events_S$group), "\n")

# Add a 'gage' column to each event data frame
decreasing_base_events_MJ <- decreasing_base_events_MJ %>% mutate(gage = "MJ")
decreasing_base_events_CS <- decreasing_base_events_CS %>% mutate(gage = "CS")
decreasing_base_events_S  <- decreasing_base_events_S  %>% mutate(gage = "S")

# Combine them into one data frame
combined_base_events <- bind_rows(decreasing_base_events_MJ, decreasing_base_events_CS, decreasing_base_events_S)

ggplot(combined_base_events, aes(x = Date, y = Qbase, color = gage)) +
  geom_point(size=0.7, alpha = 0.5) +
  labs(
    title = "Baseflow Recession Events:  by Gage",
    x = "Date",
    y = "Baseflow CFS"
  ) +
  scale_x_date(limits = as.Date(c("1984-01-01", "1994-12-31"))) +
  theme_minimal()

# Recession event extractor not for log data
extract_recession_events_not_log <- function(flow_df, min_len = 4, max_len = 10) {
  df <- flow_df %>%
    arrange(Date) %>%
    mutate(
      Flow_next = lead(Flow),
      dQ_dt = -c(NA, diff(Flow)),  # Estimate -dQ/dt
      dQ_dt_next = lead(dQ_dt),
      Q_decreasing = Flow > Flow_next,
      dQ_dt_decreasing = dQ_dt > dQ_dt_next,
      valid_decrease = Q_decreasing
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

decreasing_not_log_events_MJ <- extract_recession_events_not_log(flows_MJ, min_len = 4, max_len = 10)
decreasing_not_log_events_CS <- extract_recession_events_not_log(flows_CS, min_len = 4, max_len = 10)
decreasing_not_log_events_S  <- extract_recession_events_not_log(flows_S,  min_len = 4, max_len = 10)

cat("MJ:", n_distinct(decreasing_not_log_events_MJ$group), "\n")
cat("CS:", n_distinct(decreasing_not_log_events_CS$group), "\n")
cat("S :", n_distinct(decreasing_not_log_events_S$group), "\n")

# Add a 'gage' column to each event data frame
decreasing_not_log_events_MJ <- decreasing_not_log_events_MJ %>% mutate(gage = "MJ")
decreasing_not_log_events_CS <- decreasing_not_log_events_CS %>% mutate(gage = "CS")
decreasing_not_log_events_S  <- decreasing_not_log_events_S  %>% mutate(gage = "S")


# Combine them into one data frame
combined_not_log_events <- bind_rows(decreasing_not_log_events_MJ, decreasing_not_log_events_CS, decreasing_not_log_events_S)

ggplot(combined_not_log_events, aes(x = Date, y = Flow, color = gage)) +
  geom_point(size=0.7, alpha = 0.5) +
  labs(
    title = "Flow Recession Events:  by Gage",
    x = "Date",
    y = "Flow CFS"
  ) +
  scale_x_date(limits = as.Date(c("1984-01-01", "1984-12-31"))) +
  theme_minimal()

tashie_data <- read.csv("C:\\Users\\Ben\\Downloads\\115409dbe8354e78a2c2219d32e2b9de\\Ksat_Storage_for_CONUS_inc_infils.csv")

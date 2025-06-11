##GROUP1 EXAMPLE##
# #Get data for NF Shenandoah _____ (right now set to Cootes Store)
flows1 <- dataRetrieval::readNWISdv("01632000",parameterCd = "00060")
flows1 <- dataRetrieval::renameNWISColumns(flows1)
# #Convert flows to zoo
flows_zoo1 <- zoo::as.zoo(x = flows1$Flow)
zoo::index(flows_zoo1) <- flows1$Date
# #Use group 1 to get the minimum monthly flows:
flows_zoo_min <- data.frame(hydrotools::group1(flows_zoo1,"water",FUN = min))
flows_zoo_min$Year <- rownames(flows_zoo_min)
flows_zoo_min <- flows_zoo_min %>% relocate(Year)
flows_zoo_min$Year <- as.numeric(flows_zoo_min$Year)

##GROUP2 EXAMPLE##
# #Get data for NF Shenandoah _____ (right now set to Strasburg)
flows2 <- dataRetrieval::readNWISdv("01634000",parameterCd = "00060")
flows2 <- dataRetrieval::renameNWISColumns(flows2)
# #Convert flows to zoo
flows_zoo2 <- zoo::as.zoo(x = flows2$Flow)
zoo::index(flows_zoo2) <- flows2$Date
# #Use group 2 to get critical period flows and stats:
flows_zoo_critical <- hydrotools::group2(flows_zoo2,"water",mimic.tnc = TRUE)

##START WORKING ON FLOWS_ZOO_CRITICAL##
df2 <- flows_zoo_critical
##LOW FLOW METRICS OVER ENTIRE TIMEFRAME##
df2_low <- df2 %>%
  select(year, `1 Day Min`, `3 Day Min`, `7 Day Min`, `30 Day Min`, `90 Day Min`) %>%
  pivot_longer(cols = -year, names_to = "LowFlowMetric", values_to = "Flow")

ggplot(df2_low, aes(x = year, y = Flow, color = LowFlowMetric)) +
  geom_line() +
  labs(title = "Low Flow Metrics Over Time (Strasburg)",
       x = "Year", y = "Flow (cfs)", color = "Metric") +
  theme_minimal()


##START WORKING ON FLOWS_ZOO_MIN##
df1 <- flows_zoo_min
# Pivot to long format
flows_long1 <- df1 %>%
  pivot_longer(cols = -Year, names_to = "Month", values_to = "Flow") %>%
  mutate(
    Month = factor(Month, levels = c("January", "February", "March", "April",
                                     "May", "June", "July", "August", "September",
                                     "October", "November", "December"))
  )

##CREATE A PLOT FOR A GIVEN YEAR## #ignore this for now#
#filter
flows_1944 <- flows_long1 %>%
  filter(Year == 1944)
#plot
ggplot(flows_1944, aes(x = Month, y = Flow, group = Year)) +
  geom_line(alpha = 0.3) +
  stat_summary(fun = mean, geom = "line", color = "steelblue", size = 1.2) +
  labs(title = "Seasonal Minimum Flows (1944)", y = "Flow (cfs)", x = "Month") +
  theme_minimal()

##COMPARE WET AND DRY YEARS## #wet years: 2003, 2018; #dry years: 2001, 2002, 2023)
compare_years <- flows_long1 %>% 
  filter(Year %in% c(2001, 2002, 2003, 2018, 2023))

ggplot(compare_years, aes(x = Month, y = Flow, color = factor(Year), group = Year)) +
  geom_line(size = 1.2) +
  labs(title = "Monthly Min Flows in Dry vs Wet Years (Cootes Store)", color = "Year") +
  theme_minimal()

##DROUGHT OF RECORD##
flows_long1 %>% 
  slice_min(Flow, n = 1) #shows month and year of record low

df2 %>% 
  slice_min(`7 Day Min`, n = 1)  #critical-period DOR

##CONSECUTIVE LOW FLOW DAYS## ####THIS NEEDS TO BE FIXED####
flows_daily <- flows1 %>%
  select(Date, Flow) %>%
  mutate(Flow = as.numeric(Flow),
         below_50 = Flow < 50)
rle_below_50 <- rle(flows_daily$below_50)
max(rle_below_50$lengths[rle_below_50$values == TRUE])

##SEASONAL CHANGES OVER DECADES##
flows_long1 %>%
  mutate(Decade = floor(Year / 10) * 10) %>%
  group_by(Decade, Month) %>%
  summarize(mean_flow = mean(Flow, na.rm = TRUE)) %>%
  ggplot(aes(x = Month, y = mean_flow, color = factor(Decade), group = Decade)) +
  geom_line(size = 1.1) +
  labs(title = "Decadal Changes in Monthly Minimum Flows", y = "Mean Flow (cfs)", color = "Decade") +
  theme_minimal()

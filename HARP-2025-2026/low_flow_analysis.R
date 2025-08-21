##GROUP2 SETUP##
# #Get data for NF Shenandoah _____ (right now set to Cootes Store)
flows2 <- dataRetrieval::readNWISdv("01632000",parameterCd = "00060")
flows2 <- dataRetrieval::renameNWISColumns(flows2)
# #Convert flows to zoo
flows_zoo2 <- zoo::as.zoo(x = flows2$Flow)
zoo::index(flows_zoo2) <- flows2$Date
# #Use group 2 to get critical period flows and stats:
flows_zoo_critical <- hydrotools::group2(flows_zoo2,"water",mimic.tnc = TRUE)
df2 <- flows_zoo_critical


##LOW FLOW ANALYSIS##
#30 Day Minimum Flow by Year
ggplot(df2, aes(x = year, y = `30 Day Min`)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "30-Day Minimum Flow by Year", y = "Flow (cfs)", x = "Year") +
  theme_minimal()

# Prepare long format for 30-day min and max
df_30day <- df2 %>%
  select(year, `30 Day Min`, `30 Day Max`) %>%
  pivot_longer(cols = -year, names_to = "Metric", values_to = "Flow")

# Full timeframe plot
ggplot(df_30day, aes(x = year, y = Flow, color = Metric)) +
  geom_line(size = 1.1) +
  labs(title = "30-Day Min vs Max Flow by Year", y = "Flow (cfs)", x = "Year", color = "") +
  theme_minimal()

#do this when you want to narrow to model timespan (1984-2024):
#for min vs max:
vs_30day_recent <- df_30day %>%
  filter(year >= 1984, year <= 2024)
#Narrow 30 Day Minimum vs Maximum Flow by Year
ggplot(vs_30day_recent, aes(x = year, y = Flow, color = Metric)) +
  geom_line(size = 1.1) +
  labs(title = "30-Day Min vs Max Flow (1984–2024)", y = "Flow (cfs)", x = "Year", color = "") +
  theme_minimal()
#for just low flow:
low_30day_recent <- df2 %>%
  filter(year >= 1984, year <= 2024)
#Narrow 30 Day Minimum Flow by Year
ggplot(low_30day_recent, aes(x = year, y = `30 Day Min`)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "30-Day Minimum Flow by Year (1984-2024)", y = "Flow (cfs)", x = "Year") +
  theme_minimal()


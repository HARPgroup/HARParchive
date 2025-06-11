library(zoo)
get_site_30day <- function(site_no) {
  flows <- dataRetrieval::readNWISdv(site_no, parameterCd = "00060") %>%
    dataRetrieval::renameNWISColumns()
  flows_zoo <- zoo(flows$Flow, order.by = flows$Date)
  critical_flows <- hydrotools::group2(flows_zoo, "water", mimic.tnc = TRUE)
  
  critical_flows %>%
    select(year, `30 Day Min`, `30 Day Max`) %>%
    pivot_longer(cols = -year, names_to = "Metric", values_to = "Flow") %>%
    mutate(Site = site_no)
}
#set the ids for all three sites
site_ids <- c("01632000", "01633000", "01634000")
df_all_sites <- bind_rows(lapply(site_ids, get_site_30day))
#assign actual site names (useful for plots)
site_names <- tibble(
  Site = c("01632000", "01633000", "01634000"),
  SiteName = c("Cootes Store", "Mount Jackson", "Strasburg")
)
df_all_sites <- df_all_sites %>%
  left_join(site_names, by = "Site")


#MIN VS MAX PLOT (FULL TIMEFRAME)
ggplot(df_all_sites, aes(x = year, y = Flow, color = SiteName, linetype = Metric)) +
  geom_line(size = 1.1) +
  labs(title = "30-Day Min vs Max Flow by Year (All Sites)", y = "Flow (cfs)", x = "Year",
       color = "USGS Site", linetype = "Flow Metric") +
  theme_minimal()
#MIN VS MAX PLOT (MODEL TIMESPAN (1984-2024))
#filter the data down
df_all_recent <- df_all_sites %>%
  filter(year >= 1984, year <= 2024)
#plot:
ggplot(df_all_recent, aes(x = year, y = Flow, color = SiteName, linetype = Metric)) +
  geom_line(size = 1.1) +
  labs(title = "30-Day Min vs Max Flow by Year (1984–2024)", y = "Flow (cfs)", x = "Year",
       color = "USGS Site", linetype = "Flow Metric") +
  theme_minimal()


#LOW FLOW BY YEAR PLOT (FULL TIMEFRAME)
#filter data down to just min data (previously included max):
df_30day_min <- df_all_sites %>%
  filter(Metric == "30 Day Min")
#plot:
ggplot(df_30day_min, aes(x = year, y = Flow, color = SiteName)) +
  geom_line(size = 1.1) +
  labs(title = "30-Day Minimum Flow by Year (All Sites)",
       x = "Year", y = "Flow (cfs)", color = "USGS Site") +
  theme_minimal()
#LOW FLOW BY YEAR PLOT (MODEL TIMESPAN (1984-2024))
#filter df_30day_min by year:
df_30day_min_recent <- df_30day_min %>%
  filter(year >= 1984, year <= 2024)
#plot:
ggplot(df_30day_min_recent, aes(x = year, y = Flow, color = SiteName)) +
  geom_line(size = 1.1) +
  labs(title = "30-Day Minimum Flow by Year (1984–2024)", 
       x = "Year", y = "Flow (cfs)", color = "USGS Site") +
  theme_minimal()



#PLAYING AROUND WITH SOME NUMBERS (very much a work in progress)
#read in group2 data:
flows2 <- dataRetrieval::readNWISdv("01632000",parameterCd = "00060")
flows2 <- dataRetrieval::renameNWISColumns(flows2)
# #Convert flows to zoo
flows_zoo2 <- zoo::as.zoo(x = flows2$Flow)
zoo::index(flows_zoo2) <- flows2$Date
# #Use group 2 to get critical period flows and stats:
flows_zoo_critical <- hydrotools::group2(flows_zoo2,"water",mimic.tnc = TRUE)
df2 <- flows_zoo_critical


DOR_flow <- df2 %>%
  filter(`30 Day Min` == min(`30 Day Min`, na.rm = TRUE)) %>%
  pull(`30 Day Min`)
duration_days <- 30 #this number can be adjusted, just a placeholder for now
volume_cuft <- DOR_flow * 86400 * duration_days  #cfs × sec/day × days
watershed_area_sqft <- 210 * 640 * 43560 #210 is the drainage area pertaining to Cootes Store
storage_in_inches <- (volume_cuft / watershed_area_sqft) * 12

#DOR expressed in watershed inches per day
DOR_cfs <- DOR_flow
# Convert to cu ft per day
DOR_volume_daily_cuft <- DOR_cfs * 86400
# Convert to inches over watershed
inches_per_day <- (DOR_volume_daily_cuft / watershed_area_sqft) * 12
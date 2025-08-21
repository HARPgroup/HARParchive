library(dataRetrieval)
library(hydrotools)
library(tidyverse)
library(zoo)

#function to calculate storage and DOR inches/day
calculate_storage_needs <- function(site_no, duration_days = 15) {
  #get flow data
  flows <- readNWISdv(site_no, parameterCd = "00060") %>%
    renameNWISColumns()
  flows_zoo <- zoo(flows$Flow, order.by = flows$Date)
  df2 <- group2(flows_zoo, "water", mimic.tnc = TRUE)
  
  #filter to model period (comment these lines out when necessary)
  df2_model <- df2 %>%
    filter(year >= 1984 & year <= 2024)
  
  #get DOR (min of ___-day minimum)
  DOR_flow <- min(df2_model$`7 Day Min`, na.rm = TRUE) #change to grab different column. also change dataframe when needed (switching to model timeframe)
  
  #get drainage area from NWIS (sq mi)
  site_info <- readNWISsite(site_no)
  drainage_area_sqmi <- site_info$drain_area_va
  
  #skip site if drainage area is missing
  if (is.na(drainage_area_sqmi)) return(NULL)
  
  #convert to sq ft
  watershed_area_sqft <- drainage_area_sqmi * 640 * 43560
  
  #total volume over drought duration
  volume_cuft <- DOR_flow * 86400 * duration_days
  
  #inches of storage needed
  storage_in_inches <- (volume_cuft / watershed_area_sqft) * 12
  
  #daily inches needed to maintain DOR flow
  DOR_daily_in_inches <- (DOR_flow * 86400 / watershed_area_sqft) * 12
  
  #output as a tibble row
  tibble(
    Site = site_no,
    SiteName = site_info$station_nm,
    DrainageArea_sqmi = drainage_area_sqmi,
    DOR_cfs = DOR_flow,
    Duration_days = duration_days,
    Storage_inches = storage_in_inches,
    DailyInchesNeeded = DOR_daily_in_inches
  )
}

site_ids <- c("01632000", "01633000", "01634000")  #Cootes Store, Mount Jackson, Strasburg

storage_summary <- purrr::map_dfr(site_ids, calculate_storage_needs, duration_days = 15)

print(storage_summary)

#lookup table (to make plot cleaner) + simple plot to show storage:
site_labels <- tibble(
  Site = c("01632000", "01633000", "01634000"),
  SiteLabel = c("Cootes Store", "Mount Jackson", "Strasburg")
)

storage_summary <- purrr::map_dfr(site_ids, calculate_storage_needs, duration_days = 15) %>%
  left_join(site_labels, by = "Site")

ggplot(storage_summary, aes(x = SiteLabel, y = Storage_inches, fill = SiteLabel)) +
  geom_col() +
  labs(title = "Storage Needed to Sustain DOR for Gage Record (7-Day Minimum)",
       y = "Storage (inches over watershed)", x = "Site") +
  theme_minimal() +
  theme(legend.position = "none")


library(writexl)

write_xlsx(storage_summary, 'C://HARPgeneral//gage_record.xlsx')
write_xlsx(storage_summary, 'C://HARPgeneral//model_time.xlsx')




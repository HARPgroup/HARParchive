library(purrr)
calculate_storage_needs_flex <- function(site_no, 
                                         day_metric = "7 Day Min", 
                                         flow_summary_func = min, 
                                         duration_days = 15, 
                                         start_year = 1984, 
                                         end_year = 2024, 
                                         ...) {
  #get flow data and convert to zoo
  flows <- dataRetrieval::readNWISdv(site_no, parameterCd = "00060") %>%
    dataRetrieval::renameNWISColumns()
  flows_zoo <- zoo::zoo(flows$Flow, order.by = flows$Date)
  
  #compute daily flow summaries with hydrotools::group2
  df2 <- hydrotools::group2(flows_zoo, "water", mimic.tnc = TRUE)
  
  #filter to model timeframe
  df2_model <- df2 %>%
    dplyr::filter(year >= start_year & year <= end_year)
  
  #get summary value (e.g., DOR or a threshold value)
  summary_flow <- flow_summary_func(df2_model[[day_metric]], ...)
  
  #get drainage area (sq mi) from USGS
  site_info <- dataRetrieval::readNWISsite(site_no)
  drainage_area_sqmi <- site_info$drain_area_va
  
  #handle missing drainage area
  if (is.na(drainage_area_sqmi)) return(NULL)
  
  #convert drainage area to square feet
  watershed_area_sqft <- drainage_area_sqmi * 640 * 43560
  
  #compute total volume needed (cu ft)
  total_volume_cuft <- summary_flow * 86400 * duration_days
  
  #compute required watershed inches of storage
  storage_in_inches <- (total_volume_cuft / watershed_area_sqft) * 12
  
  #compute required daily inches to sustain flow
  daily_inches_needed <- (summary_flow * 86400 / watershed_area_sqft) * 12
  
  #return as a tidy tibble row
  tibble::tibble(
    Site = site_no,
    SiteName = site_info$station_nm,
    DrainageArea_sqmi = drainage_area_sqmi,
    FlowMetric = day_metric,
    SummaryFlow_cfs = summary_flow,
    Duration_days = duration_days,
    Storage_inches = storage_in_inches,
    DailyInchesNeeded = daily_inches_needed
  )
}

#DOR with 7-day min example:
# DOR_table <- calculate_storage_needs_flex("01633000")

#30-day min with 10th percentile example:
# min_table <- calculate_storage_needs_flex(
#   site_no = "01633000",
#   day_metric = "30 Day Min",
#   flow_summary_func = function(x) quantile(x, 0.10, na.rm = TRUE)
# )

#max, max, 7, 30, 90, 10th percentile max 7 30 90

site_ids <- c("01632000", "01633000", "01634000")
#mean 7-day min between 2000-2020:
max_table_90_10th <- purrr::map_dfr(
  site_ids,
  ~calculate_storage_needs_flex(
  site_no =.x,
  day_metric = "90 Day Max",
  duration_days = 30,
  flow_summary_func = function(x) quantile(x, 0.10, na.rm = TRUE),
  start_year = 1925,
  end_year = 2024
 )
)

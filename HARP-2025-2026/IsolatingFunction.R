#function:
calculate_storage_needs_flex <- function(site_no,
                                         grouped_flows,     #flow data output of hydrotools::group2() (in this case)
                                         site_info,         #list or tibble with drain_area_va and station_nm
                                         day_metric = "7 Day Min",
                                         flow_summary_func = min,
                                         duration_days = 15,
                                         start_year = 1984,
                                         end_year = 2024,
                                         ...) {
  #filter to desired year range
  df_model <- dplyr::filter(grouped_flows, year >= start_year & year <= end_year)
  
  #compute summary flow (e.g., min, quantile, etc.)
  summary_flow <- flow_summary_func(df_model[[day_metric]], ...)
  
  #extract drainage area
  drainage_area_sqmi <- site_info$drain_area_va
  if (is.na(drainage_area_sqmi)) return(NULL)
  
  #convert drainage area to square feet
  watershed_area_sqft <- drainage_area_sqmi * 640 * 43560
  
  #total volume needed in cubic feet
  total_volume_cuft <- summary_flow * 86400 * duration_days
  
  #compute required storage (in inches)
  storage_in_inches <- (total_volume_cuft / watershed_area_sqft) * 12
  
  #daily inches needed
  daily_inches_needed <- (summary_flow * 86400 / watershed_area_sqft) * 12
  
  #return output
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

#get and prepare data externally
flows_raw <- dataRetrieval::readNWISdv("01633000", parameterCd = "00060") %>%
  dataRetrieval::renameNWISColumns()
flows_zoo <- zoo::zoo(flows_raw$Flow, order.by = flows_raw$Date)
grouped <- hydrotools::group2(flows_zoo, "water", mimic.tnc = TRUE)
site_info <- dataRetrieval::readNWISsite("01633000")
#define sites
site_ids <- c("01632000", "01633000", "01634000")

#prepare a list of grouped flows and site info for each site
site_inputs <- map(site_ids, function(site_no) {
  flows_raw <- readNWISdv(site_no, parameterCd = "00060") %>%
    renameNWISColumns()
  flows_zoo <- zoo(flows_raw$Flow, order.by = flows_raw$Date)
  grouped <- hydrotools::group2(flows_zoo, "water", mimic.tnc = TRUE)
  site_info <- readNWISsite(site_no)
  
  list(
    site_no = site_no,
    grouped_flows = grouped,
    site_info = site_info
  )
})

#map over these inputs
model_quantile_90 <- map_dfr(
  site_inputs,
  ~calculate_storage_needs_flex(
    site_no = .x$site_no,
    grouped_flows = .x$grouped_flows,
    site_info = .x$site_info,
    day_metric = "90 Day Min",
    duration_days = 30,
    flow_summary_func = function(x) quantile(x, 0.10, na.rm = TRUE),
    start_year = 1984,
    end_year = 2024
  )
)

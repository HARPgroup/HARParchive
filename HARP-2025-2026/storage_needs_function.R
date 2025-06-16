calculate_storage_needs_flex <- function(site_no, 
                                         duration_days = 15,
                                         day_metric = "7 Day Min",  #can be "1 Day Min", "30 Day Min", etc.
                                         start_year = 1984,
                                         end_year = 2024,
                                         flow_summary_func = min,
                                         ...) {
  #pull daily flow and get drought summary metrics
  flows <- dataRetrieval::readNWISdv(site_no, parameterCd = "00060") %>%
    dataRetrieval::renameNWISColumns()
  flows_zoo <- zoo::zoo(flows$Flow, order.by = flows$Date)
  df2 <- hydrotools::group2(flows_zoo, "water", mimic.tnc = TRUE)
  #filter to target period (for model range: 1984–2024)
  df2_model <- df2 %>%
    filter(year >= start_year & year <= end_year)
  #check that the metric exists and apply the summary function with any extra arguments
  if (!day_metric %in% colnames(df2_model)) stop("day_metric not found in data.")
  DOR_flow <- flow_summary_func(df2_model[[day_metric]], ...)
  #apply the user-specified summary function to the selected drought metric
  if (!day_metric %in% colnames(df2_model)) stop("day_metric not found in data.")
  DOR_flow <- flow_summary_func(df2_model[[day_metric]], na.rm = TRUE)
  #get drainage area
  site_info <- dataRetrieval::readNWISsite(site_no)
  drainage_area_sqmi <- site_info$drain_area_va
  if (is.na(drainage_area_sqmi)) return(NULL)
  
  watershed_area_sqft <- drainage_area_sqmi * 640 * 43560
  #total volume needed to sustain flow over the drought duration
  volume_cuft <- DOR_flow * 86400 * duration_days
  storage_in_inches <- (volume_cuft / watershed_area_sqft) * 12
  #daily requirement in inches
  DOR_daily_in_inches <- (DOR_flow * 86400 / watershed_area_sqft) * 12
  
  tibble(
    Site = site_no,
    SiteName = site_info$station_nm,
    DrainageArea_sqmi = drainage_area_sqmi,
    Flow_cfs = DOR_flow,
    Metric = day_metric,
    SummaryFunction = deparse(substitute(flow_summary_func)),
    Duration_days = duration_days,
    Storage_inches = storage_in_inches,
    DailyInchesNeeded = DOR_daily_in_inches
  )
}

#example uses for new function at Mount Jackson:
#quantile:
quantile_example <- calculate_storage_needs_flex(
  "01633000",
  day_metric = "30 Day Min",
  flow_summary_func = quantile,
  probs = 0.10,
  na.rm = TRUE
)
#mean:
mean_example <- calculate_storage_needs_flex(
  "01633000",
  day_metric = "7 Day Min",
  flow_summary_func = mean,
  na.rm = TRUE
)
#min:
min_example <- calculate_storage_needs_flex(
  "01633000",
  day_metric = "7 Day Min",
  flow_summary_func = min,
  na.rm = TRUE
)


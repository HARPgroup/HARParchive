#Load hydrotools and the config file
library(hydrotools)
basepath='/var/www/R'
source(paste(basepath,'config.R',sep='/'))
#function (mostly unchanged, any changes made were due to trying to handle both datasets at once):
calculate_storage_needs_flex <- function(site_no,
                                         flow_data,
                                         site_info,
                                         flow_summary_func = min,
                                         duration_days = 15,
                                         start_year = 1984,
                                         end_year = 2014,
                                         ...) {
  #ensure Date and Year columns
  flow_data <- flow_data %>%
    dplyr::mutate(
      Date = as.Date(Date),
      year = lubridate::year(Date)
    )
  
  print(paste("Year range for", site_no, ":", min(flow_data$year, na.rm = TRUE), "-", max(flow_data$year, na.rm = TRUE)))
  
  df_model <- dplyr::filter(flow_data, year >= start_year & year <= end_year)
  
  if (nrow(df_model) == 0) {
    warning(paste("No flow data for", site_no, "between", start_year, "and", end_year))
    return(NULL)
  }
  
  summary_flow <- flow_summary_func(df_model$Flow, ...)
  
  drainage_area_sqmi <- site_info$drain_area_va
  if (is.na(drainage_area_sqmi)) return(NULL)
  
  watershed_area_sqft <- drainage_area_sqmi * 640 * 43560
  total_volume_cuft <- summary_flow * 86400 * duration_days
  storage_in_inches <- (total_volume_cuft / watershed_area_sqft) * 12
  daily_inches_needed <- (summary_flow * 86400 / watershed_area_sqft) * 12
  
  tibble::tibble(
    Site = site_no,
    SiteName = site_info$station_nm,
    DrainageArea_sqmi = drainage_area_sqmi,
    SummaryFlow_cfs = summary_flow,
    Duration_days = duration_days,
    Storage_inches = storage_in_inches,
    DailyInchesNeeded = daily_inches_needed
  )
}




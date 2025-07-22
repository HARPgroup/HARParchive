library(purrr)
library(tidyverse)
library(dplyr)
library(dataRetrieval)
#NWIS DATA LOAD
site_ids <- c("01632000", "01633000", "01634000")

site_inputs_nwis <- map(site_ids, function(site_no) {
  flows_raw <- readNWISdv(site_no, parameterCd = "00060") %>%
    renameNWISColumns()
  
  site_info <- readNWISsite(site_no)
  
  list(
    site_no = site_no,
    flow_data = flows_raw[, c("Date", "Flow")],
    site_info = site_info
  )
})

results_nwis <- map_dfr(
  site_inputs_nwis,
  ~calculate_storage_needs_flex(
    site_no = .x$site_no,
    flow_data = .x$flow_data,
    site_info = .x$site_info,
    flow_summary_func = function(x) quantile(x, 0.10, na.rm = TRUE),
    duration_days = 30,
    start_year = 1985,
    end_year = 2013
  )
)



#function(x) quantile(x, 0.10, na.rm = TRUE)
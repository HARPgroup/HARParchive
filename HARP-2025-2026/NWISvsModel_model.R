#MODEL DATA LOAD
hydrocodes <- c(
  'vahydrosw_wshed_PS2_5550_5560',
  'vahydrosw_wshed_PS2_5560_5100',
  'vahydrosw_wshed_PS3_5100_5080'
)

modelResultsList <- list()

for (hc in hydrocodes) {
  wshdFeat <- RomFeature$new(ds, config = list(hydrocode = hc), TRUE)
  wshdModel <- wshdFeat$get_prop(propcode = 'vahydro-1.0')
  
  if (any(grepl("runid_600", wshdModel$propvalues()[, "propname"]))) {
    results <- as.data.frame(
      om_get_rundata(
        wshdModel$propvalues("om_element_connection")$propvalue,
        600,
        site = omsite
      )
    )
    modelResultsList[[hc]] <- results
  }
}

#standardize the model data
standardize_model_data <- function(df) {
  print("Converting rownames to Date column...")
  df <- df %>%
    tibble::rownames_to_column(var = "Date")
  
  df_clean <- df %>%
    dplyr::rename(Flow = Qout) %>%
    dplyr::select(Date, Flow) %>%
    dplyr::mutate(
      Date = as.Date(Date),  
      Flow = as.numeric(Flow)
    ) %>%
    dplyr::filter(!is.na(Date), !is.na(Flow))
  
  return(df_clean)
}

#model site mapping and function
model_site_map <- list(
  "vahydrosw_wshed_PS2_5550_5560" = "01632000",
  "vahydrosw_wshed_PS2_5560_5100" = "01633000",
  "vahydrosw_wshed_PS3_5100_5080" = "01634000"
)

get_site_info_from_nwis <- function(site_no, site_inputs_nwis) {
  match <- purrr::keep(site_inputs_nwis, ~ .x$site_no == site_no)
  if (length(match) == 1) return(match[[1]]$site_info)
  warning(paste("Missing NWIS metadata for site", site_no))
  return(NULL)
}

results_model <- map_dfr(
  names(modelResultsList),
  function(hc) {
    raw_df <- modelResultsList[[hc]]
    clean_df <- standardize_model_data(raw_df)
    
    site_no <- model_site_map[[hc]]
    site_info <- get_site_info_from_nwis(site_no, site_inputs_nwis)
    if (is.null(site_info)) return(NULL)
    
    calculate_storage_needs_flex(
      site_no = site_no,
      flow_data = clean_df,
      site_info = site_info,
      flow_summary_func = function(x) quantile(x, 0.10, na.rm = TRUE),
      duration_days = 30,
      start_year = 1985,
      end_year = 2013
    )
  }
)



#function(x) quantile(x, 0.10, na.rm = TRUE)
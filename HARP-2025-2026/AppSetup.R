args <- commandArgs(trailingOnly = T)

flow_csv <- read.csv(paste0(args[1]))
flow_csv$Date <- as.Date(flow_csv$Date)
flow_col <- paste0(args[2])
gage_name <- as.character(args[3])
manual_opt <- as.logical(args[4])
end_path <- paste0(args[5])

source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/main/MainAnalysisFunctionsPt1.R")
source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/analyze_recession.R")
source("https://raw.githubusercontent.com/HARPgroup/baseflow_storage/refs/heads/main/attach_event_stats.R")
# Load in stream data from USGS
#flow_csv <- readNWISdv("01634000", parameterCd = "00060") %>% renameNWISColumns()

suppressPackageStartupMessages(library(purrr))


#robust attach_event_stats() that tolerates upstream name drift and avoids suffix collisions
attach_event_stats <- function(analysis_data, r_lim = 0) {
  suppressPackageStartupMessages({ library(dplyr) })
  
  es <- summarize_event(analysis_data)  # all events
  if (is.null(es) || nrow(es) == 0) return(analysis_data)
  
  #1) normalize the event id column to `i`
  i_col <- if ("i" %in% names(es)) es$i else
    if ("event_num" %in% names(es)) es$event_num else
      if ("Event" %in% names(es)) es$Event else {
        warning("summarize_event() returned no event id column. Names: ", paste(names(es), collapse = ", "))
        return(analysis_data)
      }
  
  #2) pick the best-available AGWR and R^2 columns from known variants
  pick_col <- function(df, cand) {
    for (nm in cand) if (nm %in% names(df)) return(df[[nm]])
    return(rep(NA_real_, nrow(df)))
  }
  AGWR_std <- pick_col(es, c("AGWR", "calc.AGWR", "calc_AGWR", "AGWR_calc"))
  R2_std   <- pick_col(es, c("R_squared", "R.squared", "R2", "r2"))
  
  # 3) Build a clean summary with unique names so we avoid suffixes on join
  es_std <- tibble(
    i = i_col,
    AGWR_event = as.numeric(AGWR_std),
    R_squared_event = as.numeric(R2_std)
  )
  
  #4) join and expose consistent fields keeping time-series AGWR intact
  out <- analysis_data %>%
    left_join(es_std, by = c("GroupID" = "i")) %>%
    mutate(
      calc_AGWR = AGWR_event,
      R_squared = R_squared_event
    )
  
  #5)filter by R^2 if requested, then optionally drop helper columns
  out %>%
    filter(is.na(R_squared) | R_squared > r_lim) %>%
    select(-AGWR_event, -R_squared_event)
}


#calculate AGWR and delta_AGWR
flow_csv$AGWR <- calc_AGWR(flow_csv[[flow_col]])
flow_csv$delta_AGWR <- calc_delta_AGWR(flow_csv$AGWR)

flow_csv <- add_month_season(flow_csv)

if(manual_opt == TRUE){
  flow_csv$GroupID <- 1
  flow_csv$RecessionDay <- TRUE
}else{
  flow_csv <- flag_stable_baseflow(flow_csv, flow_csv[[flow_col]])
}

#remove NAs
flow_csv <- flow_csv[!is.na(flow_csv$RecessionDay), ]

#apply to gage of interest
sites <- list(
  gage = list(data = flow_csv, name = paste0(gage_name))
)

results <- imap(sites, function(site, abbrev) {
  result <- analyze_recession(site$data, site$name, min_len = 14)
  df <- result$df
  summary_df <- result$summary
  
  analysis_df <- df %>%
    filter(!is.na(GroupID)) %>%
    select(site_no, Date, Flow, AGWR, delta_AGWR, Month, Season, GroupID)
  
  list(df = df, summary = summary_df, analysis = analysis_df, name = site$name)
})

#extract
analysis_df <- results$gage$analysis

analysis_df <- attach_event_stats(analysis_df, r_lim = 0)

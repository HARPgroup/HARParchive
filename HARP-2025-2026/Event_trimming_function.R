trim_event_buffer <- function(event_df, tol = 0.03) {
  n <- nrow(event_df)
  
  # find first "stable" index
  i_start <- which(abs(event_df$calc_AGWR - 1) <= tol)[1]
  # find last "stable" index
  i_end <- tail(which(abs(event_df$calc_AGWR - 1) <= tol), 1)
  
  # guard against no matches or invalid indices
  if (is.null(i_start) || is.null(i_end) ||
      length(i_start) == 0 || length(i_end) == 0 ||
      is.na(i_start) || is.na(i_end) ||
      i_start >= i_end) {
    return(tibble())  # always return a df
  }
  
  trimmed <- event_df[i_start:i_end, ]
  
  # run Mann-Kendall test on trimmed series
  mk <- tryCatch(
    Kendall::MannKendall(trimmed$calc_agwrc),
    error = function(e) NULL
  )
  
  pval <- if (!is.null(mk)) mk$sl else NA
  
  trimmed <- trimmed %>%
    mutate(mk_pval = pval)
  
  return(trimmed)
}


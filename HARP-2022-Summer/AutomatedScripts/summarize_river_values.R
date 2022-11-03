# The calculated values necessary for the figure generation in 
# summarize_river_figures.R
# The value calculations are from script summarize_river.R, and it pushes
# the values to VAHydro

summarize_river_values <- function (hydr) {
  
  #hydr <- as.data.frame(hydr)
  
  ### ANALYSIS
  ## water year:
  
  syear = as.integer(min(hydr$year), na.rm = TRUE)
  eyear = as.integer(max(hydr$year), na.rm = TRUE)
  model_run_start <- min(hydr$date)   
  model_run_end <- max(hydr$date)
  years <- seq(syear,eyear)
  
  if (syear < (eyear - 2)) {
    sdate <- as.Date(paste0(syear,"-10-01"))
    edate <- as.Date(paste0((eyear-1),"-09-30")) 
    flow_year_type <- 'water'
  } else {
    sdate <- as.Date(paste0(syear,"-02-01"))
    edate <- as.Date(paste0(eyear,"-12-31"))
    flow_year_type <- 'calendar'
  }
  
  hydr <- with(hydr, hydr[(date >= sdate & date <= edate),]) #replaced filter()
  
  #Assumptions and placeholders columns 
  imp_off = 1
  hydr$imp_off = 1 # set to 1 meaning there will be no impoundment 
  
  hydr$wd_imp_child_mgd = 0 #child vars used in hspf 
  hydr$wd_cumulative_mgd = hydr$wd_mgd  
  hydr$ps_cumulative_mgd = hydr$ps_mgd
  hydr$ps_nextdown_mgd = 0 

  wd_mgd <- mean(as.numeric(hydr$wd_mgd))
  
  wd_imp_child_mgd <- mean(as.numeric(hydr$wd_imp_child_mgd) )
  if (is.na(wd_imp_child_mgd)) {   # setting this to zero since it doesn't exist
    wd_imp_child_mgd = 0.0
  }
  
  wd_mgd <- wd_mgd + wd_imp_child_mgd   # the official wd_mgd
  
  wd_cumulative_mgd <- mean(as.numeric(hydr$wd_cumulative_mgd) )
  if (is.na(wd_cumulative_mgd)) {   # setting this to zero since it doesn't exist
    wd_cumulative_mgd = 0.0
  }
  
  # ps
  ps_mgd <- mean(as.numeric(hydr$ps_mgd) )
  #ps_mgd to be added to the hydr table using the conversion script,
  # after ps_afd is added to hydr using the join_col script 
  
  ps_cumulative_mgd <- mean(as.numeric(hydr$ps_cumulative_mgd) )
  if (is.na(ps_cumulative_mgd)) {   # setting this to zero since it doesn't exist
    ps_cumulative_mgd = 0.0
  }
  
  ps_nextdown_mgd <- mean(as.numeric(hydr$ps_nextdown_mgd) )
  if (is.na(ps_nextdown_mgd)) {   # setting this to zero since it doesn't exist
    ps_nextdown_mgd = 0.0
  }
  
  # net consumption
  net_consumption_mgd <- wd_cumulative_mgd - ps_cumulative_mgd
  if (is.na(net_consumption_mgd)) {
    net_consumption_mgd = 0.0
  }
  
  # Qout, Q baseline
  Qout <- mean(as.numeric(hydr$Qout))

  hydr$Qbaseline <- hydr$Qout +
    (hydr$wd_cumulative_mgd - hydr$ps_cumulative_mgd ) * 1.547
  # alter calculation to account for pump store
  if (imp_off == 0) {
    if("impoundment_Qin" %in% cols) {
      if (!("ps_cumulative_mgd" %in% cols)) {
        hydr$ps_cumulative_mgd <- 0.0
      }
      hydr$Qbaseline <- hydr$impoundment_Qin +
        (hydr$wd_cumulative_mgd - hydr$ps_cumulative_mgd) * 1.547
    }
  }
  
  # L90 and l30
  Qout_zoo <- zoo(hydr$Qout, order.by = hydr$index)
  Qout_g2 <- data.frame(group2(Qout_zoo))
  
  Qout_zoo <- zoo(hydr$Qout, order.by = hydr$index)
  Qout_g2 <- data.frame(group2(Qout_zoo));
  l90 <- Qout_g2["X90.Day.Min"];
  ndx = which.min(as.numeric(l90[,"X90.Day.Min"]));
  l90_Qout = round(Qout_g2[ndx,]$"X90.Day.Min",6);
  l90_year = Qout_g2[ndx,]$"year";
  
  if (is.na(l90)) {
    l90_Runit = 0.0
    l90_year = 0
  }
  
  l30 <- Qout_g2["X30.Day.Min"];
  ndx = which.min(as.numeric(l30[,"X30.Day.Min"]));
  l30_Qout = round(Qout_g2[ndx,]$"X30.Day.Min",6);
  l30_year = Qout_g2[ndx,]$"year";
  
  if (is.na(l30)) {
    l30_Runit = 0.0
    l30_year = 0
  }
  
  # Unmet demand
  unmet_demand_mgd <- mean(as.numeric(hydr$unmet_demand_mgd) )
  if (is.na(unmet_demand_mgd)) {
    unmet_demand_mgd = 0.0
  }
  
  values <- list(hydr$index, hydr$date, hydr$hour, hydr$day, hydr$month, hydr$year, 
                 hydr$Qout, hydr$Qbaseline, hydr$wd_mgd, hydr$wd_cumulative_mgd, 
                 hydr$ps_mgd, hydr$ps_cumulative_mgd, hydr$ps_nextdown_mgd)
  names(values) <- c("index", "date", "hour", "day", "month", 'year', "Qout", 
                     "Qbaseline", "wd_mgd", "wd_cumulative_mgd", "ps_mgd", "ps_cumulative_mgd", 
                     "ps_nextdown_mg")
  
  values2 <- list(l90_Qout, l90_year, l30_Qout, l30_year, imp_off, net_consumption_mgd, unmet_demand_mgd)
  names(values2) <- c("l90_Qout", "l90_year", "l30_Qout", "l30_year","imp_off", 
                      "net_consumption_mgd", "unmet_demand_mgd")
  
  values3 <- list(values, values2)
  
  return(values3)
 }
  

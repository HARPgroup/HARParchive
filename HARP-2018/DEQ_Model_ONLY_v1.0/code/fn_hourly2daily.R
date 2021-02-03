fn_mod.hourly2daily <- function(model_hourly) {
  # Converting hourly to daily data
  model_hourly <- model_hourly[-1,]
  model_hourly$V1 <- trimws(model_hourly$V1, which = "both")
  colnames(model_hourly) <- c("year","month","day","hour","ovol")
  model_hourly$date <- as.Date(paste0(model_hourly$year,"-",model_hourly$month,"-",model_hourly$day))
  model_daily <- aggregate(model_hourly$ovol, list(model_hourly$date), FUN = sum)
  colnames(model_daily) <- c("date","mod.flow")
  return(model_daily)
}
library(dataRetrieval)
#shenandoah (01634000)
#rappahannock (01668000)
#potomac (01646500)
#vail (09066325)

get_discharge_plot <- function(site, year, title) {
  start <- paste0(year, "-01-01")
  end <- paste0(year, "-12-31")
  
  #retrieve data
  data <- readNWISdata(
    sites = site,
    parameterCd = "00060",
    startDate = start,
    endDate = end
  )
  
  #clean with SQL
  clean_data <- sqldf("SELECT dateTime, x_00060_00003 AS Discharge_cfs FROM data")
  
  #make plot
  p <- ggplot(clean_data, aes(dateTime, Discharge_cfs)) +
    geom_line(color = "steelblue", size = 1) +
    theme_minimal(base_size = 14) +
    xlab("Date") +
    ylab("Discharge (cfs)") +
    ggtitle(title)
  
  return(p)
}

va_sites <- list(
  "01634000" = "N F Shenandoah River Near Strasburg, VA",
  "01668000" = "Rappahannock River Near Fredericksburg, VA",
  "01646500" = "Potomac River Near Little Falls Pump Station"
)

#VA years to loop over
years <- c(1999, 2018)

#store all plots for VA sites
va_plots <- list()
for (site in names(va_sites)) {
  for (yr in years) {
    key <- paste(site, yr, sep = "_")
    va_plots[[key]] <- get_discharge_plot(site, yr, paste(va_sites[[site]], "(", yr, ")", sep = ""))
  }
}

vail_2003 <- get_discharge_plot("09066325", 2003, "Gore Creek Discharge at Vail, CO (2003)")
vail_2007 <- get_discharge_plot("09066325", 2007, "Gore Creek Discharge at Vail, CO (2007)")

#VA plots
library(patchwork)
for (site in names(va_sites)) {
  p1999 <- va_plots[[paste(site, 1999, sep = "_")]]
  p2018 <- va_plots[[paste(site, 2018, sep = "_")]]
  
  combined_plot <- p1999 + p2018 + plot_layout(ncol = 2)
  print(combined_plot)
}

#vail plots
vail_2003 + vail_2007 + plot_layout(ncol = 2)

library(dataRetrieval)
#shenandoah (01634000)
#rappahannock (01668000)
#potomac (01646500)
#vail (09066325)

ymax_lookup <- list(
  "01634000" = 25000,   #Shenandoah
  "01668000" = 75000,   #Rappahannock
  "01646500" = 150000   #Potomac
)

get_discharge_plot <- function(site, year, title, ymax = NULL) {
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
  if (!is.null(ymax)) {
    p <- p + coord_cartesian(ylim = c(0, ymax))
  }
  
  return(p)
}

#plot maximums
ymax_va <- 150000
ymax_vail <- 1800

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
    title <- paste(va_sites[[site]], "(", yr, ")", sep = "")
    ymax_val <- ymax_lookup[[site]] #site specific
    va_plots[[key]] <- get_discharge_plot(site, yr, title, ymax = ymax_val)
  }
}

vail_2003 <- get_discharge_plot("09066325", 2003, "Gore Creek Discharge at Vail, CO (2003)", ymax = ymax_vail)
vail_2007 <- get_discharge_plot("09066325", 2007, "Gore Creek Discharge at Vail, CO (2007)", ymax = ymax_vail)

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

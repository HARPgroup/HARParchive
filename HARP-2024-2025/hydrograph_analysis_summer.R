# Download Necessary Libraries
suppressPackageStartupMessages(library(dataRetrieval))
suppressPackageStartupMessages(library(sqldf))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(lubridate))

# Shenandoah at Strasburg - 01634000
# New River at Radford - 03171000
# James River at Richmond - 02037500
# Verde River near Camp Verde, AZ - 09506000
# Discharge in cfs (00060)


plot.gage <- function(gage_id, gage_name, start_date, end_date, y_max=0){
  # Download gage data from USGS
  gage_data <- readNWISdata(sites = gage_id,
                            parameterCD = "00060",
                            startDate = start_date,
                            endDate = end_date)
  # Select relevant columns from data
  gage_data$obs_date <- gage_data$dateTime
  gage_data <- sqldf(
    "Select obs_date, X_00060_00003 as flow_cfs
  from gage_data
  "
  )
  
 if(y_max == 0){
   y_max <- max(gage_data$flow_cfs)
 }
  # Create Desired Plot
  gage_plot <- ggplot(data = gage_data, mapping = aes(x = obs_date, y = flow_cfs))+
    geom_line(size = 0.5, color = "navy")+
    theme_bw()+
    ggtitle(paste("Streamflow at ",gage_name))+
    theme(plot.title = element_text(hjust = 0.5))+
    xlab("Date")+
    ylab("Flow (cfs)")+
    coord_cartesian(ylim=c(0,y_max))
  
  # Return plot
  return(gage_plot)
}


# Wet years: 2018 and 1992

plot.gage("01634000", "Strasburg (North Fork Shenandoah River)", "2018-01-01", "2018-12-31", 17500)

plot.gage("03171000", "Radford (New River)", "2018-01-01", "2018-12-31", 65000)

plot.gage("02037500", "Richmond (James River)", "2018-01-01", "2018-12-31", 70000)

plot.gage("09506000", "Camp Verde (Verde River)", "1992-01-01", "1992-12-31", 9000)

# Dry years: 2023 and 2021

plot.gage("01634000", "Strasburg (North Fork Shenandoah River)", "2023-01-01", "2023-12-31", 17500)

plot.gage("03171000", "Radford (New River)", "2023-01-01", "2023-12-31", 65000)

plot.gage("02037500", "Richmond (James River)", "2023-01-01", "2023-12-31", 70000)

plot.gage("09506000", "Camp Verde (Verde River)", "2002-01-01", "2002-12-31", 9000)

# Summer 2024

plot.gage("01634000", "Strasburg (North Fork Shenandoah River)", "2024-05-01", "2024-08-31", 17500)

plot.gage("03171000", "Radford (New River)", "2024-05-01", "2024-08-31", 65000)

plot.gage("02037500", "Richmond (James River)", "2024-05-01", "2024-08-31", 70000)

plot.gage("09506000", "Camp Verde (Verde River)", "2024-05-01", "2024-08-31", 9000)



# Function to produce plots and summary statistics
summarize.event <- function(analysis_data,
         event_number,
         dAGWR_range = 0.03){
  data <- analysis_data
  eventnum <- event_number
  dAGWRmax <- 1 + dAGWR_range
  dAGWRmin <- 1 - dAGWR_range
  
  if(eventnum > max(data$GroupID)){
    stop("Data does not contain enough events, choose a lower number")
  }
  
  event_data <- fn$sqldf(
    "select * from data 
    where GroupID = $eventnum and
    AGWR < 1 and
    delta_AGWR < $dAGWRmax and
    delta_AGWR > $dAGWRmin
    ")

  
  logFlow_lm <-lm(log(event_data$Flow) ~ event_data$Date)
  event_sum <- summary(logFlow_lm)
  
  AGWR <- event_sum$coefficients[[2,1]]
  R_squared <- event_sum$r.squared
  
  print(paste0("calculated AGWR = ", round(exp(AGWR), 4)))
  print(paste0("R-Squared = ", round(R_squared, 4)))
  
  return(list(event_data, event_sum))
}

# Example of how to run function
# event_115 <- summarize.event(analysis_CS, 115)[[1]]
# event_115_summary <- summarize.event(analysis_CS, 115)[[2]]
# print(event_115_summary)

plot.event.values <- function(analysis_data,
                             event_number,
                             dAGWR_range = 0.03){
  data <- analysis_data
  eventnum <- event_number
  
  event <- summarize.event(data, event_number)[[1]]
  
  a <- ggplot(data = event, mapping = aes(x = Date, y = log(Flow)))+
    geom_point(color = "dodgerblue4")+
    geom_line(color = "dodgerblue3")+
    theme_bw()
  
  b <- ggplot(data = event, mapping = aes(x = Date, y = AGWR))+
    geom_point(color = "firebrick4")+
    geom_line(color = "firebrick")+
    theme_bw()
  
  c <- ggplot(data = event, mapping = aes(x = Date, y = delta_AGWR))+
    geom_point(color = "darkolivegreen")+
    geom_line(color = "darkolivegreen4")+
    theme_bw()
  
  d <- gridExtra::grid.arrange(a, b, c, ncol=1, top=paste0("Event ", event_number, " Selected Dates"))
  
  return(d)
 
}

# Example of how to run function
# plot.event.values(analysis_CS, 40)

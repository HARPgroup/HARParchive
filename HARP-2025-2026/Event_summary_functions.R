# Function to produce plots and summary statistics
# analysis_data = dataframe produced by MainAnalysis.R script (l. 161-163)
# event_number = desired event for summary, or leave blank to get summary data for all events
# dAGWR_range = range, in either direction from 1, that is a valid dAGWR
summarize.event <- function(analysis_data,
         event_number = 0,
         dAGWR_range = 0.03){
  
  require(sqldf)
  
  data <- analysis_data
  eventnum <- event_number
  dAGWRmax <- 1 + dAGWR_range
  dAGWRmin <- 1 - dAGWR_range
  
  #new (for integration with app)
  max_gid <- suppressWarnings(max(data$GroupID, na.rm = TRUE))
  #slight change (for integration with app)
  if (is.na(eventnum) || is.na(max_gid) || eventnum > max_gid) {
    return(NULL)
  }
  
  # Warning for event number not existing in data
  if(eventnum > max(data$GroupID)){
    stop("Data does not contain enough events, choose a lower number")
  }
  
  # COndition to produce rsquared and agwr for all events
  if(eventnum == 0){
    # Create empty dataframe
    event_df <- data.frame(Event = numeric(), calc.AGWR = numeric(), R.squared = numeric(), stringsAsFactors = FALSE)
    
    for(i in (1:max(data$GroupID))){
      
    # Set sqldf query for valid days since fn$ wasnt working
      sqldf_query <- paste0(
      "select * from data 
    where GroupID = ", i, " and
    AGWR < 1 and
    delta_AGWR < ", dAGWRmax," and
    delta_AGWR > ", dAGWRmin,"
    ")
      
      event_data <- sqldf(sqldf_query)
      
    # Create lm of event selected dates
      logFlow_lm <-lm(log(event_data$Flow) ~ event_data$Date)
      event_sum <- summary(logFlow_lm)
      
    # Assign AWGR and R-squared
      AGWR <- exp(event_sum$coefficients[[2,1]])
      R_squared <- event_sum$r.squared
      
      new_row <- data.frame(i, AGWR, R_squared)
      
      event_df <- rbind(event_df, new_row)
      
    }
    
    return(event_df)
    
  } else {
  
  event_data <- fn$sqldf(
    "select * from data 
    where GroupID = $eventnum
    ")
  
  # and
  #AGWR < 1 and
  #delta_AGWR < $dAGWRmax and
  #delta_AGWR > $dAGWRmin

  #new (for integration with app)
  if (nrow(event_data) == 0) {
    return(list(data = NULL, AGWR = NA_real_, R2 = NA_real_))
  }
  #new/changes made (for integration with app) (applies to rest of function)
  logFlow_lm <- tryCatch(
    lm(log(event_data$Flow) ~ event_data$Date),
    error = function(e) NULL
  )
  
  if (is.null(logFlow_lm)) {
    return(list(data = event_data, AGWR = NA_real_, R2 = NA_real_))
  }
  
  event_sum <- summary(logFlow_lm)
  
  return(list(
    data = event_data,
    summary = event_sum,
    AGWR = suppressWarnings(round(exp(coef(logFlow_lm)[[2]]), 4)),
    R2   = suppressWarnings(round(event_sum$r.squared, 4))
  ))
  }
  
}

# Example of how to run function
# ... To create list object with all relevant data for one event
# event_3_list <- summarize.event(analysis_S, 3)

# ... To get dataframe with valid data from event number 8
# event_8_data <- summarize.event(analysis_S, 8)[[1]]

# ... To print lm summary for event 130
# print(summarize.event(analysis_S, 130)[[2]])

# ... To view the AGWR and R-squared for all events
# View(summarize.event(analysis_S))


# Function to produce plots of data from selected days from desired event
# analysis_data = dataframe produced by MainAnalysis.R script (l. 161-163)
# event_number = desired event for plots
# dAGWR_range = range, in either direction from 1, that is a valid dAGWR
plot.event.values <- function(analysis_data,
                             event_number,
                             dAGWR_range = 0.03){
  data <- analysis_data
  eventnum <- event_number
  
  # Get event data using previous function
  event <- summarize.event(data, event_number)[[1]]
  
  # log flow plot
  a <- ggplot(data = event, mapping = aes(x = Date, y = log(Flow)))+
    geom_point(color = "dodgerblue4")+
    geom_line(color = "dodgerblue3")+
    theme_bw()
  
  # AGWR valid plot
  b <- ggplot(data = event, mapping = aes(x = Date, y = AGWR))+
    geom_point(color = "firebrick4")+
    geom_line(color = "firebrick")+
    theme_bw()
  
  # dAGWR valid plot
  c <- ggplot(data = event, mapping = aes(x = Date, y = delta_AGWR))+
    geom_point(color = "darkolivegreen")+
    geom_line(color = "darkolivegreen4")+
    theme_bw()
  
  d <- gridExtra::grid.arrange(a, b, c, ncol=1, top=paste0("Event ", event_number, " Selected Dates"))
  
  return(d)
 
}

# Example of how to run function
# plot.event.values(analysis_S, 99)
 
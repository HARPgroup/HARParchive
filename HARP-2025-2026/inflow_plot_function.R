# Packages
library(ggplot2)

add.season <- function(data, date_col){
  require(lubridate)
  
  data$month <- month(data[[date_col]])
  
  data$Season <- ifelse(data$month>11,'Winter',
                        ifelse(data$month>8,'Fall',
                               ifelse(data$month>5,'Summer',
                                      ifelse(data$month>2,'Spring','Winter'))))
  data$Season <- factor(data$Season, levels=c('Fall','Winter','Spring','Summer'))
  
  return(data)
}


plot.zero.inflows <- function(gw_data, landseg, landtype, 
                              zone = c("AGW", "LZ", "UZ"), 
                              second_axis = c("ET", "O", "S")) {
  
  require(ggplot2)
  
  # Get column names from inputs
  inflow_col <- paste0(zone, "I")
  second_col <- paste0(zone, second_axis)
  
  # Subset to zero inflow
  zero_inflow <- gw_data[gw_data[[inflow_col]] == 0, ]
  
  # Daily sum for second variable
  if(second_axis == "S"){
    daily_sc <- aggregate(gw_data[[second_col]], by = list(Date = gw_data$date), FUN = mean)
  }else{
    daily_sc <- aggregate(gw_data[[second_col]], by = list(Date = gw_data$date), FUN = sum)
  }
  
  daily_sc$month <- month(daily_sc$Date)
  #Monthly average for second varial
  
  monthly_sc <- aggregate(daily_sc$x, by = list(Month = daily_sc$month), FUN = mean)
  
  
  # Scale factor for second axis matching
  bar_counts <- table(zero_inflow$month)
  max_bar <- max(bar_counts)
  max_second <- max(monthly_sc$x, na.rm = TRUE)
  scale_factor <- max_bar / max_second
  
  zero_inflow[["Month"]] <- factor(zero_inflow[["month"]], levels = 1:12, labels = month.abb)
  monthly_sc$Month <- factor(monthly_sc$Month, levels = 1:12, labels = month.abb)
  
  
  # Fmake plot
  plot <- ggplot(zero_inflow, aes(x = Month, fill = Season)) +
    geom_bar(alpha = 0.65) +
    geom_line(data = monthly_sc, 
              aes(x = Month, y = monthly_sc$x * scale_factor),
              inherit.aes = FALSE, color = "firebrick4", group = 1, size = 1.05) +
    scale_y_continuous(
      name = "Number of 0 in/day Inflows",
      sec.axis = sec_axis(~ . / scale_factor,
                          name = paste("Avg. Monthly", second_col, "(in/day)"))) +
    scale_fill_manual(values = c("orangered3", "midnightblue", "darkolivegreen3", "maroon3")) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(paste("Seasonal Zero Inflows for", landtype, "in", landseg))
  
  return(plot)
}


H51165 <- add.season(H51165, "index")
N51165 <- add.season(N51165, "index")
N51171 <- add.season(N51171, "index")
N54031 <- add.season(N54031, "index")


plot.zero.inflows(N51165, 
                  landseg = "N51165", 
                  landtype = "Forested", 
                  zone = "AGW", 
                  second_axis = "O")

plot.zero.inflows(H51165, 
                  landseg = "H51165", 
                  landtype = "Forested", 
                  zone = "LZ", 
                  second_axis = "ET")

plot.zero.inflows(N51171, 
                  landseg = "N51171", 
                  landtype = "Forested", 
                  zone = "UZ", 
                  second_axis = "S")


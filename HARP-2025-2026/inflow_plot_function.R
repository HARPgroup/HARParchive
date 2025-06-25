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

H51165 <- add.season(H51165, "date")

max_bar <- max(table(H51165$month[H51165$AGWI == 0]))
max_ET <- max(monthly_ET$AGWET)
scale_factor <- max_bar / max_ET

# Monthly ET just for plot
monthly_ET <- aggregate(H51165$AGWET, by = list(Date = H51165$date), FUN = sum)

monthly_ET$month <- month(monthly_ET$Date)

monthly_ET <- aggregate(monthly_ET$x, by = list(Month = monthly_ET$month), FUN = mean)

monthly_ET$AGWET <- monthly_ET$x

H51165$Month <- factor(H51165$month, levels = 1:12, labels = month.abb)
monthly_ET$Month <- factor(monthly_ET$Month, levels = 1:12, labels = month.abb)
  

ggplot(H51165[H51165$AGWI == 0, ], aes(x = Month, fill = Season)) +
  geom_bar(alpha = 0.65) +
  geom_line(data = monthly_ET, aes(x = Month, y = AGWET*scale_factor, group = 1), 
             inherit.aes = FALSE, color = "firebrick4", size = 0.75) +
  scale_fill_manual(values = c("darkorange2", "lightblue2", "olivedrab3", "orchid3")) +
  theme_bw() +
  ggtitle("Seasonal Zero Inflows (H51165, for)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(
    name = "Number of 0 in/day Inflows",
    sec.axis = sec_axis(~ . / scale_factor, name = "Avg. Monthly AGW ET (in/day)"))



ggplot(data=monthly_ET, mapping = aes(x = Month))+
  geom_line(mapping = aes(y=x))

plot.zero.inflows <- function(gw_data, landseg, landtype, 
                              zone=c("AGW","LZ","UZ"), 
                              second_axis=c("ET","O","S")){
  # Packages
    require(ggplot2)
  
  
  
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
  daily_sc <- aggregate(gw_data[[sec_col]], by = list(Date = gw_data$date), FUN = sum)
  daily_sc$month <- month(daily_sc$Date)
  #Monthly average for second varial
  monthly_sc <- aggregate(daily_sc$x, by = list(Month = daily_sc$month), FUN = mean)
  colnames(monthly_sc)[2] <- "second_col_avg"
  
  # Scale factor for second axis matching
  bar_counts <- table(zero_inflow$month)
  max_bar <- max(bar_heights)
  max_second <- max(monthly_sc$second_col_avg, na.rm = TRUE)
  scale_factor <- max_bar / max_sec
  
  
  # Fmake plot
  plot <- ggplot(zero_inflows, aes(x = factor(month, levels = 1:12), fill = Season)) +
    geom_bar(alpha = 0.65) +
    geom_point(data = monthly_ET, 
               aes(x = factor(Month, levels = 1:12), y = second_col_avg * scale_factor),
               inherit.aes = FALSE, color = "firebrick4") +
    scale_y_continuous(
      name = "Number of 0 in/day Inflows",
      sec.axis = sec_axis(~ . / scale_factor,
                          name = paste("Avg. Monthly", sec_col, "(in/day)"))) +
    scale_fill_manual(values = c("orangered3", "midnightblue", "darkolivegreen3", "maroon3")) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(paste("Seasonal Zero Inflows for", landtype, "in", landseg))
  
  return(plot)
}

plot.zero.inflows(H51165, landseg = "H51165", landtype = "Forested", zone = "AGW", second_axis = "S")

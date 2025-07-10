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
  
  # Set correct units
  if(second_axis == "S"){
    uts <- "(in)"
  } else {
    uts <- "(in/day)"
  }
  
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
                          name = paste("Average", second_col, "", uts))) +
    scale_fill_manual(values = c("orangered3", "midnightblue", "darkolivegreen3", "maroon3")) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(paste("Seasonal Zero Inflows for", landtype, "in", landseg))
  
  return(plot)
}

# H51165 <- add.season(H51165, "index")
# N51165 <- add.season(N51165, "index")
# N51171 <- add.season(N51171, "index")
# N54031 <- add.season(N54031, "index")
# H51165_daily <- add.season(H51165_daily, "Date")
# N51165_daily <- add.season(N51165_daily, "Date")
# N51171_daily <- add.season(N51171_daily, "Date")
# N54031_daily <- add.season(N54031_daily, "Date")

# Examples
# plot.zero.inflows(N51165, 
#                   landseg = "N51165", 
#                   landtype = "Forested", 
#                   zone = "AGW", 
#                   second_axis = "O")
# 
# plot.zero.inflows(H51165, 
#                   landseg = "H51165", 
#                   landtype = "Forested", 
#                   zone = "LZ", 
#                   second_axis = "ET")
# 
# plot.zero.inflows(N51171, 
#                   landseg = "N51171", 
#                   landtype = "Forested", 
#                   zone = "UZ", 
#                   second_axis = "S")

plot.inflows <- function(gw_data, landseg, landtype, 
                              zone = c("AGW", "LZ", "UZ"), 
                              second_axis = c("ET", "O", "S"),
                         plot_type = c("bar", "box")) {
  require(ggplot2)
  require(patchwork)
  
  # Get column names from inputs
  inflow_col <- paste0(zone, "I")
  second_col <- paste0(zone, second_axis)
  
  inflow <- gw_data

  # Set correct units
  if(second_axis == "S"){
    uts <- "(in)"
  } else {
    uts <- "(in/day)"
  }
  
  # Daily sum for second variable if needed
  if(second_axis == "S"){
    daily_sc <- aggregate(gw_data[[second_col]], by = list(Date = gw_data$date), FUN = mean)
  }else{
    daily_sc <- aggregate(gw_data[[second_col]], by = list(Date = gw_data$date), FUN = sum)
  }
    
    
  daily_sc$month <- month(daily_sc$Date)
  #Monthly average for second varial
  
  monthly_sc <- aggregate(daily_sc$x, by = list(Month = daily_sc$month), FUN = mean)

  
  inflow[["Month"]] <- factor(inflow[["month"]], levels = 1:12, labels = month.abb)
  monthly_sc$Month <- factor(monthly_sc$Month, levels = 1:12, labels = month.abb)
  

  if(plot_type == "box"){
  inflow <- add.season(inflow, "index")  
  
  p1 <- ggplot(inflow, aes(x = Month, fill = Season, color = Season)) +
    geom_boxplot(coef = 100,
                 aes(y = inflow[[inflow_col]]),
                 size = 0.5, alpha = 0.65) +
    theme_bw()+
    coord_cartesian(ylim = c(0, quantile(inflow[[inflow_col]], 0.99)))+
    scale_fill_manual(values = c("orangered3", "midnightblue", "darkolivegreen3", "maroon3"))+
    scale_color_manual(values = c("orangered3", "midnightblue", "darkolivegreen3", "maroon3"))+
    ylab(paste0(inflow_col, " (in/day)"))
  
  }else{
    
    # Create sums and averages
    monthly_totals <- aggregate(inflow[[inflow_col]] ~ year + month, data = inflow, FUN = sum)
    names(monthly_totals)[3] <- "monthly_volume"
    df_bar <- aggregate(monthly_volume ~ month, data = monthly_totals, FUN = mean)
    names(df_bar)[2] <- "mean_in"
    df_bar <- add.season(df_bar, "month")
    
  
    p1 <- ggplot(df_bar, aes(x = month, y = mean_in, fill = Season, color = Season)) +
      geom_bar(stat = "identity", size = 0.5, alpha = 0.65) +
      theme_bw() +
      coord_cartesian(ylim = c(0, quantile(df_bar$mean_inflow, 0.8))) +
      scale_fill_manual(values = c("orangered3", "midnightblue", "darkolivegreen3", "maroon3")) +
      scale_color_manual(values = c("orangered3", "midnightblue", "darkolivegreen3", "maroon3")) +
      ylab(paste0(inflow_col, " (in/month)"))
  }
  
  p2 <- ggplot(monthly_sc, aes(x = Month, y = x)) +
    geom_line(color = "firebrick4", size = 1.05, group = 1) +
    theme_bw()+
    ylab(paste0(second_col, " ",uts))
  
  # add plots together
  plot <- p1 / p2
  plot <- plot + 
    plot_annotation(title = paste0("Seasonal Trends in Water of ", landtype, " Land in ", landseg )) & 
    theme(plot.title = element_text(hjust = 0.5))
  
  return(plot)
}

# Examples
# plot.inflows(H51165, 
#              landseg = "H51165", 
#              landtype = "Forested", 
#              zone = "AGW", 
#              second_axis = "O",
#              plot_type = "bar")
# 
# 
# plot.inflows(N51165_daily, 
#               landseg = "N51165", 
#               landtype = "Forested", 
#               zone = "AGW", 
#               second_axis = "O",
#              plot_type = "bar")
# 
# 
# 
# plot.inflows(N51171_daily, 
#               landseg = "N51171", 
#               landtype = "Forested", 
#               zone = "UZ", 
#               second_axis = "S")
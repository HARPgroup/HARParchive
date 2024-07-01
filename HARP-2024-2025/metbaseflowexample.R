site_id <- "01660400"
parameterCd <- "00060"
startDate <- "2022-10-01"
endDate <- "2023-10-01"
library(tidyverse)
library(grwat)

#This function takes in our common USGS variables and downloads data for that site and 
#puts it into a csv in a spot you decide on. it has default values for start and end date
#becuase you can choose to grab all the data or only some of the data available at the site
usgs_data <- function(site_id, parameterCd, startDate = '', endDate = '', file.path){
  #Uses default values (all data), unless you put a start and/or end date
      usgs_dat <- dataRetrieval::readNWISdv(site_id, parameterCd, startDate, endDate)
      usgs_dat <- usgs_dat %>%
        rename(Q = X_00060_00003, Datetime = Date)
  #Returns your created dataframe to the file path selected earlier.
  return(write.csv(usgs_dat, file.path))
}

usgs_data(site_id,parameterCd, file.path =
           "~/HarpData/HARParchive/HARP-2024-2025/test_data.csv")

spas <- dataRetrieval::readNWISdv(site_id, parameterCd, startDate, endDate)
spas <- spas %>%
  rename(Q = X_00060_00003, Datetime = Date)



gageid = '01660400'
prism_data <- read.csv(pase"http://deq1.bse.vt.edu:81/met/PRISM/out/usgs_ws_01660400-PRISM-all.csv")
prism_data[,c('yr', 'mo', 'da', 'wk')] <- cbind(year(as.Date(prism_data$obs_date)),
                                                month(as.Date(prism_data$obs_date)),
                                                day(as.Date(prism_data$obs_date)),
                                                week(as.Date(prism_data$obs_date)))


prism_data$Datetime <- as.Date(paste(prism_data$da, prism_data$mo, prism_data$yr), "%d %m %Y")
hdata = spas %>%
  mutate(Qbase = gr_baseflow(Q, method = method))

joined_data <- hdata |> 
  left_join(prism_data, by = "Datetime")

#^all this is just reading in data, which is something we'd already have I would assume
#######################

         

ggplot(hdata) +
  geom_area(aes(Datetime, Q), fill = 'steelblue', color = 'black') +
  geom_area(aes(Datetime, Qbase), fill = 'orangered', color = 'black')

#^adds baseflow to our flow data, and then plots it.
##############

p1 <- joined_data |> ggplot() +
  geom_area(aes(as.Date(Datetime), Q), fill = 'steelblue', color = 'black') +
  geom_area(aes(as.Date(Datetime), Qbase), fill = 'orangered', color = 'black') + 
  scale_y_continuous(position = "left",
                     limits = c(0, max(joined_data$Q)),
                     expand = c(0,0)) + 
  labs(y = "Discharge [cfs]",
       x = "Date") +
  theme_minimal() +
  theme(axis.title.y.left = element_text(hjust = 0),
        legend.position = "bottom",
        legend.justification = c(0.25, 0.5),
        legend.title = element_blank())


p2 <- ggplot(joined_data) +
  geom_line(aes(as.Date(Datetime), precip_in, color = "Precip")) +
  scale_y_reverse(position = "right",
                  limits = c(5,0),
                  breaks = c(0,0.25,0.5, 1.0),
                  labels = c(0,0.25,0.5, 1.0),
                  expand = c(0,0)) +
  scale_color_manual(values = c("sienna1")) +
  labs(y = "Precipitation [inches]", x = "") +
  theme_minimal() +
  theme(axis.title.y.right = element_text(hjust = 0),
        legend.position = "bottom",
        legend.justification = c(0.75, 0.5),
        legend.title = element_blank())
aligned_plots <- align_plots(p1, p2, align = "hv", axis = "tblr")
out <- ggdraw(aligned_plots[[1]]) + draw_plot(aligned_plots[[2]])
out


#This takes in precip and flow data and joins them to make plots
#Can also choose what method you want to select, default is lynehollick
create_data <- function(precip_data, flow_data, method = "lynehollick"){
  #makes sure the precip data is considered a date
  precip_data$Datetime <- as.Date(paste(precip_data$da, 
                                       precip_data$mo, precip_data$yr), "%d %m %Y")
  #sets up the Qbase variable with the a selected method, default is lynehollick
  hdata <- flow_data %>%
    mutate(Qbase = gr_baseflow(Q, method))
  #joins the two datasets using datetime, this works well because even if 
  #your precip data is over a larger timespan it will only join the dates that match
  joined_data <- hdata |> 
    left_join(precip_data, by = "Datetime")
  return(joined_data)
}


#takes in the joined data from the previous function
baseflow_graphs <- function(joined_data){
  #creates the first plot, which is the baseflow and total flow
  p1 <- joined_data |> ggplot() +
    geom_area(aes(as.Date(Datetime), Q), fill = 'steelblue', color = 'steelblue') +
    geom_area(aes(as.Date(Datetime), Qbase), fill = 'orangered', color = 'orangered') + 
    scale_y_continuous(position = "left",
                       #sets the limit as the maximum flow for the data
                       limits = c(0, max(joined_data$Q)),
                       expand = c(0,0)) + 
    labs(y = "Discharge [cfs]",
         x = "Date") +
    theme_minimal() +
    theme(axis.title.y.left = element_text(hjust = 0),
          legend.position = "bottom",
          legend.justification = c(0.25, 0.5),
          legend.title = element_blank())
  
  #creates the second plot which is the precipitation
  p2 <- ggplot(joined_data) +
    geom_line(aes(as.Date(Datetime), precip_in, color = "Precip")) +
    scale_y_reverse(position = "right",
                    limits = c(7,0),
                    breaks = c(0,0.25,0.5, 1.0),
                    labels = c(0,0.25,0.5, 1.0),
                    expand = c(0,0)) +
    scale_color_manual(values = c("sienna1")) +
    labs(y = "Precipitation [inches]", x = "") +
    theme_minimal() +
    theme(axis.title.y.right = element_text(hjust = 0),
          legend.position = "bottom",
          legend.justification = c(0.75, 0.5),
          legend.title = element_blank())
  #combines the two plots into one plot and then assigns it to a variable
  aligned_plots <- align_plots(p1, p2, align = "hv", axis = "tblr")
  out <- ggdraw(aligned_plots[[1]]) + draw_plot(aligned_plots[[2]])
  #returns the plot
  return(out)
  
}




baseflow_graphs(create_data(prism_data, get_data(site_id, parameterCd), method = "lynehollick"))


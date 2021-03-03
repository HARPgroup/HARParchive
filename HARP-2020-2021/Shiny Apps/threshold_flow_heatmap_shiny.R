##Flow Threshold Grid Function Shiny

#Load Libraries
library(dataRetrieval)
library(sqldf)
library(ggplot2)
library(dplyr)
library(ggnewscale)
library(shiny)

#Fluid Page
ui <- fluidPage(
  titlePanel("Streamflow Threshold Heatmap"),
  sidebarLayout(
    sidebarPanel(
      textInput("gage_id", "USGS Gage ID", value = "01646502"),
      dateInput("start_date", "Start Date", value = "1984-01-01"),
      dateInput("end_date", "End Date", value = "2014-12-31"),
      numericInput("threshold_flow", 'Threshold Flow', value = 5000)
    ),
    mainPanel(
      plotOutput(outputId = "flow_heatmap"),
    )
  )
)

#Server function
server <- function(input, output) {
  output$flow_heatmap <- renderPlot({
    start_date <- input$start_date
    end_date <- input$end_date
    gage_id <- input$gage_id
    threshold_flow <- input$threshold_flow
#Loading data
dat <- readNWISdv(gage_id,'00060',start_date,end_date)

##Data manipulation
Date = dat$Date
#Adding year column to data frame
dat$year = format(as.Date(Date, format="%d%m%Y"), "%Y")
#Adding month column to data frame
dat$month = format(as.Date(Date, format="%d%m%Y"), "%m")
#Adding threshold flow column
dat$threshold_flow <- threshold_flow

#Creating count table
modat <- sqldf(" select month months, year years, count(*) count 
                    from dat where X_00060_00003 < threshold_flow
                    group by month, year")
#Adding NA data points for months with no value
modat <- sqldf("SELECT * FROM dat LEFT JOIN modat ON modat.years = dat.year 
                    AND modat.months = dat.month group by month, year")
#Retrimming table to only include month, year, and count
modat <- sqldf('SELECT month, year, count FROM modat GROUP BY month, year')
#Turning the NA data points to 0
modat[is.na(modat)] = 0

# #start and end year
# syear = min(dat$year)
# eyear = max(dat$year)
# num_syear <- as.numeric(syear)
# num_eyear <- as.numeric(eyear)
# 
# #monthly totals via sqldf
# mosum <- sqldf("SELECT  month, sum(count) count FROM modat GROUP BY month")
# mosum$year <- rep(num_eyear+1,12)
# 
# #yearly sum
# yesum <-  sqldf("SELECT year, sum(count) count FROM modat GROUP BY year")
# yesum$month <- rep(13,length(yesum$year))
# 
# #create monthly averages 
# moavg<- sqldf('SELECT * FROM mosum')
# moavg$year <- moavg$year + 1
# moavg$avg <- round(moavg$count/((num_eyear-num_syear)+1),1)
# 
# #create yearly averages
# yeavg<- sqldf('SELECT * FROM yesum')
# yeavg$month <- yeavg$month + 1
# yeavg$avg <- round(yeavg$count/12,1)
# 
# #create x and y axis breaks
# y_breaks <- seq(syear,num_eyear+2,1)
# x_breaks <- seq(1,14,1)
# 
# #create x and y labels
# y_labs <- c(seq(syear,eyear,1),'Totals', 'Avg')
# x_labs <- c(month.abb,'Totals','Avg')

#Graphing
# count_grid <- ggplot() +
#   geom_tile(data=modat, color='black',aes(x = month, y = year, fill = count)) +
#   geom_text(aes(label=modat$count, x=modat$month, y= modat$year), size = 3.5, colour = "black") +
#   scale_fill_gradient2(low = "#00cc00", high = "red",mid ='yellow',
#                        midpoint = 15, guide = "colourbar",
#                        name= 'Unmet Days') +
#   theme(panel.background = element_rect(fill = "transparent"))+
#   theme() + labs(title = 'Threshold Flow Heatmap', y=NULL, x=NULL) +
#   scale_x_continuous(expand=c(0,0), breaks= x_breaks, labels=x_labs, position='top') +
#   scale_y_reverse(expand=c(0,0), breaks=y_breaks, labels= y_labs) +
#   theme(axis.ticks= element_blank()) +
#   theme(plot.title = element_text(size = 12, face = "bold",  hjust = 0.5)) +
#   theme(legend.title.align = 0.5)
# 
# flow <- count_grid + new_scale_fill() +
#   geom_tile(data = yesum, color='black', aes(x = month, y = year, fill = count)) +
#   geom_tile(data = mosum, color='black', aes(x = month, y = year, fill = count)) +
#   geom_text(data = yesum, size = 3.5, color='black', aes(x = month, y = year, label = count)) +
#   geom_text(data = mosum, size = 3.5, color='black', aes(x = month, y = year, label = count)) +
#   scale_fill_gradient2(low = "#63D1F4", high = "#8A2BE2", mid='#CAB8FF',
#                        midpoint = mean(mosum$count), name= 'Total Unmet Days')
# 
# 
# flow_avg <- flow + new_scale_fill()+
#   geom_tile(data = yeavg, color='black', aes(x = month, y = year, fill = avg)) +
#   geom_tile(data = moavg, color='black', aes(x = month, y = year, fill = avg)) +
#   geom_text(data = yeavg, size = 3.5, color='black', aes(x = month, y = year, label = avg)) +
#   geom_text(data = moavg, size = 3.5, color='black', aes(x = month, y = year, label = avg))+
#   scale_fill_gradient2(low = "#FFF8DC", mid = "#FFDEAD", high ="#DEB887",
#                        name= 'Average Unmet Days', midpoint = mean(yeavg$avg))
# flow_avg

threshold_grid <- ggplot(modat)+
  geom_tile(aes(y=year, x=month, fill=count)) + 
  geom_text(aes(y=year, x=month, label = round(count, 1))) +
  theme_bw()+
  labs( title = 'Streamflow Threshold Heatmap', subtitle = 'Number of days that the streamflow (cfs) is less than the threshold flow', x= NULL, y = NULL) +
  theme(plot.title = element_text(face = 'bold',hjust = 0.5)) +
  scale_x_discrete(position='top',
                   labels = c('Jan','Feb','Mar','Apr',
                              'May','Jun','July','Aug','Sep',
                              'Oct','Nov','Dec'))+
  scale_y_discrete(position = 'right')+
  scale_fill_gradient2(low = "#00cc00", mid = "yellow", high ="red",
                       name= 'Count', midpoint = 15)

threshold_grid
  })
}

#Running the app
shinyApp(ui=ui, server=server)

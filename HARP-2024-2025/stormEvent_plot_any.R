#Shows precip and real flow data for a storm event. 
## Examples located at bottom for gage and date notation. 
## Date will locate the closest storm event and plot it.
## Buffer days is the number of days before and after the storm event to include
### Ex: bufferDays = 10 would include 20 days.


##Next Steps:
### Include model predicted flows to compare to real streamflow
### Include storm event start and end points to mark different storm events.


library(sqldf)
library(ggplot2)
#Create a plot that shows the baseflow, storm hydrograph, and precip hyetographs
#for a point
#
# Example gage:
# gageNo <- '01634000'

storm_plot <- function(gageNo,bufferDays = 3,date = "2020-02-22"){
  #Get precip for the gage
  nldasPrecip <- read.csv(paste0('http://deq1.bse.vt.edu:81/met/nldas2/precip/usgs_ws_',gageNo,'_precip_daily.csv'))
  daymetPrecip <- read.csv(paste0('http://deq1.bse.vt.edu:81/met/daymet/precip/usgs_ws_',gageNo,'_precip_daily.csv'))
  prismPrecip <- read.csv(paste0('http://deq1.bse.vt.edu:81/met/PRISM/precip/usgs_ws_',gageNo,'_precip_daily.csv'))
  nldasPrecip$obs_date <- as.Date(nldasPrecip$obs_date)
  daymetPrecip$obs_date <- as.Date(daymetPrecip$obs_date)
  prismPrecip$obs_date <- as.Date(prismPrecip$obs_date)
  
  #Get the statistics and streamsflow associated with the storm
  stormFlow <- read.csv(paste0('http://deq1.bse.vt.edu:81/met/stormVol_nldas2_tiled/flow/usgs_ws_',gageNo,'-stormevent-flow.csv'))
  stormStats <- read.csv(paste0('http://deq1.bse.vt.edu:81/met/stormVol_nldas2_tiled/flow/usgs_ws_',gageNo,'-stormevent-stats.csv'))
  #Find the closest storm to plot
  stormToPlot <- stormStats[which.min(abs(as.Date(date)-as.Date(stormStats$maxDate))),]
  #Get the flow data associated with the closest storm to date input from above
  #and include a buffer of x days
  bufferDays <- bufferDays
  flowToPlot <- stormFlow[as.Date(stormFlow$timestamp) >= (as.Date(stormToPlot$startDate) - bufferDays) &
                            as.Date(stormFlow$timestamp) <= (as.Date(stormToPlot$endDate) + bufferDays),]
  flowToPlot$timestamp <- as.Date(flowToPlot$timestamp)
  #Union precip data together in one long style data frame and limit to dates in
  #flowToPlot
  combinePrecip <- sqldf("
SELECT np.*, 'nldas' as precipDataSource
FROM nldasPrecip as np
INNER JOIN flowToPlot as fp
ON (fp.timestamp = np.obs_date)
UNION ALL
SELECT dp.*, 'daymet' as precipDataSource
FROM daymetPrecip as dp
INNER JOIN flowToPlot as fp
ON fp.timestamp = dp.obs_date
UNION ALL
SELECT pp.*, 'prism' as precipDataSource
FROM prismPrecip as pp
INNER JOIN flowToPlot as fp
ON fp.timestamp = pp.obs_date
ORDER BY obs_date,precipDataSource
")
  
  ## Plotting in base R
  # png('stormMethod.png',width = 6,height = 4,units = 'in',res = 300)
  # par(mar = c(2,4,1,4))
  # barplot(combinePrecip$precip_in,
  #         col = c('firebrick4','steelblue4','grey70'),
  #         axes = FALSE, ann = FALSE)
  # axis(4)
  # mtext('Precipitation (in)',4,line = 2)
  # par(new = TRUE)
  # plot(flowToPlot$timestamp,flowToPlot$flow,col = 'black',lwd = 3,
  #      type = "l",ann = FALSE, axes = FALSE,
  #      ylim = c(0,max(flowToPlot$flow*1.2)))
  # lines(flowToPlot$timestamp,flowToPlot$baseQ,col = 'black',lwd = 2,lty = 3)
  # axis(2)
  # mtext('Flow (CFS)',2,line = 2)
  # axis.Date(1,at = seq(as.Date(min(flowToPlot$timestamp)),
  #                      as.Date(max(flowToPlot$timestamp)), by = "3 days"))
  # legend('topright',legend = c("Streamflow","Baseflow","NLDAS","PRISM","daymet"),
  #        pch=c(NA,NA,15,15,15),col = c('black','black','firebrick4','steelblue4','grey70'),
  #        lty = c(1, 3, NA, NA, NA))
  # box()
  # dev.off()
  
  ##Plotting in ggplot
  scale_factor <- mean(max(combinePrecip$obs_flow) / max(combinePrecip$precip_in,na.rm = TRUE))
  
  # Create a baseflow data frame that is of equal length to the combinePrecip df
  
  baseQ <- sqldf("
WITH nums AS (
    SELECT 1 AS n
    UNION ALL
    SELECT 2
    UNION ALL
    SELECT 3
)
SELECT
    timestamp,
    baseQ
FROM flowToPlot
CROSS JOIN nums;
")


ggplot(data = combinePrecip,
       aes(x = obs_date))+
  geom_bar(mapping = aes(y = precip_in, 
                         fill = precipDataSource), 
           stat = "identity", 
           position = "dodge",
           color = "black")+
  geom_line(mapping = aes(y = obs_flow / scale_factor, color = "Streamflow"), group = 1,
            size = 1)+
  geom_line(mapping = aes(y = baseQ$baseQ / scale_factor, color = "Baseflow"),
            linetype = "dashed")+
  scale_y_continuous(name = "Precip (in)", 
                     sec.axis = sec_axis(~.*scale_factor, name = "Flow (CFS)"))+
  scale_fill_manual(values = c("nldas" = "firebrick4",
                               "prism" = "steelblue4",
                               "daymet" = "grey70"),
                    labels = c("nldas" = "NLDAS",
                               "prism" = "PRISM",
                               "daymet" = "Daymet"))+
  scale_color_manual(values = c("Baseflow" = "black",
                                "Streamflow" = "black"))+
  labs(x = NULL, fill = "Data Source", color = NULL)+
  theme_bw()+
  theme(legend.position = c(0.85,0.75))

}

# Example storm_plot function uses
storm_plot("01613900")
storm_plot("01629500")
storm_plot("01646000", date = "2007-08-10")
storm_plot("01646000", date = "2003-01-01", bufferDays = 60)



#Shows precip and real flow data for a storm event. 
## Examples located at bottom for gage and date notation. 
## Date will locate the closest storm event and plot it.
## Buffer days is the number of days before and after the storm event to include
### Ex: bufferDays = 10 would include 20 days.
#Can include modelScenarios as an input, which takes a vector of model scenarios
#to include model Qout (often lubsheds, mubsheds, pubsheds)
#riverseg is necessary if modelScenarios is populated, although if left blank
#the function will try to find the river model via spatial overlap

library(sqldf)
library(ggplot2)
#Create a plot that shows the baseflow, storm hydrograph, and precip hyetographs
#for a point
#
# Example gage:
# hydrocode <- 'usgs_ws_01646000'

storm_plot <- function(hydrocode, bufferDays = 3, date = "2020-02-22",
                       modelScenarios = NULL, riverseg = NULL){
  #Get precip for the gage
  nldasPrecip <- read.csv(paste0('http://deq1.bse.vt.edu:81/met/nldas2/precip/',hydrocode,'_precip_daily.csv'))
  daymetPrecip <- read.csv(paste0('http://deq1.bse.vt.edu:81/met/daymet/precip/',hydrocode,'_precip_daily.csv'))
  prismPrecip <- read.csv(paste0('http://deq1.bse.vt.edu:81/met/PRISM/precip/',hydrocode,'_precip_daily.csv'))
  nldasPrecip$obs_date <- as.Date(nldasPrecip$obs_date)
  daymetPrecip$obs_date <- as.Date(daymetPrecip$obs_date)
  prismPrecip$obs_date <- as.Date(prismPrecip$obs_date)
  
  #Get the statistics and streamsflow associated with the storm
  stormFlow <- read.csv(paste0('http://deq1.bse.vt.edu:81/met/stormVol_nldas2_tiled/flow/',hydrocode,'-stormevent-flow.csv'))
  stormStats <- read.csv(paste0('http://deq1.bse.vt.edu:81/met/stormVol_nldas2_tiled/flow/',hydrocode,'-stormevent-stats.csv'))
  
  #Add the flow at each start and end date of the storm, for reference
  stormStats$startFlow <- stormFlow$flow[match(stormStats$startDate,stormFlow$timestamp)]
  stormStats$endFlow <- stormFlow$flow[match(stormStats$endDate,stormFlow$timestamp)]
  
  #Find the closest storm to plot
  stormToPlot <- stormStats[which.min(abs(as.Date(date) - as.Date(stormStats$maxDate))),]
  
  #Get the flow data associated with the closest storm to date input from above
  #and include a buffer of x days
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
  
  #Get all stats of storms included on plot:
  stormStatsForPlot <- stormStats[stormStats$startDate >= min(combinePrecip$obs_date) & 
                                    stormStats$endDate <= max(combinePrecip$obs_date),]
  
  #Check if user has requested any model data be included on plot:
  model_out <- NULL
  if(!is.null(modelScenarios)){
    #If user did not provide the river segment, try to guess it with spatial
    #contain
    if(is.null(riverseg)){
      #Find the river segment that contains this USGS gage
      gageFeat <- RomFeature$new(ds,config = list(hydrocode = hydrocode,
                                                  bundle = 'watershed',
                                                  ftype = 'usgs_full_drainage'),
                                 TRUE)
      riverModel <- gageFeat$find_spatial_relations(operator = 'overlaps',
                                                    inputs = list(bundle = 'watershed',
                                                                  ftype = 'vahydro')
      )
      #If more than one segment is found, return an error
      if(nrow(riverModel) > 1){
        message("Cannot find river model that contains gage. Please provide.")
      }else{
        message(paste0("Riversegment model with hydrocode ",riverModel$hydrocode,
                       " assumed to be representative of area based on spatial overlap"))
        #If river segment was found, get just the CB ID
        riverseg <- gsub("vahydrosw_wshed_","",riverModel$hydrocode)
      }
    }
    #For each runid provided, get the model data and add the run id as a column.
    #Then combine into one dataframe
    if(!is.null(riverseg)){
      #Create an empty list to populate with model data
      model_out <- list()
      for(i in modelScenarios){
        # riverseg <- 'PM7_4581_4580'
        #Compile URL to get model data
        model_output_file <- paste0('http://deq1.bse.vt.edu:81/p6/out/river/',
                                    i, '/hydr/', riverseg, '_hydrd_wy.csv')
        message(paste0("Retrieving data for ",i," for ",riverseg," from: ",model_output_file))
        model_data <- read.table(model_output_file, header = TRUE, sep = ",")
        #Set the date column and add the run id as a column
        model_data$date <- as.Date(model_data$date)
        model_data$runid <- i
        #Add data to any exisitng model data
        model_out <- c(model_out,
                       list(model_data[model_data$date <= max(combinePrecip$obs_date) & 
                                         model_data$date >= min(combinePrecip$obs_date),]))
      }
      #Bind data into a single data frame (in case of multiple modelScenarios)
      model_out <- do.call(rbind,model_out)
    }
    
  }
  
  
  p <- ggplot(data = combinePrecip,
         aes(x = obs_date))+
    geom_bar(mapping = aes(y = precip_in, 
                           fill = precipDataSource), 
             stat = "identity", 
             position = "dodge",
             color = "black")+
    geom_line(mapping = aes(y = obs_flow / scale_factor, color = "Streamflow"), 
              group = 1, linewidth = 1)+
    geom_line(mapping = aes(y = baseQ$baseQ / scale_factor, color = "Baseflow"),
              linetype = "dashed")+
    annotate(
      geom = 'point',
      x = as.Date(stormStatsForPlot$startDate),
      y = stormStatsForPlot$startFlow / scale_factor,
      color = 'red'
    ) + 
    annotate(
      geom = 'point',
      x = as.Date(stormStatsForPlot$endDate),
      y = stormStatsForPlot$endFlow / scale_factor,
      color = 'red'
    )
  
  if(is.null(model_out)){
    p <- p + scale_y_continuous(name = "Precip (in)", 
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
      theme(legend.position = 'inside',
            legend.position.inside = c(0.85,0.75))
  }else{
    #Assign colors to each model output flow from terrain.colors()
    numColors <- length(modelScenarios) + 1
    manualColors <- c(rep("black",2),
      terrain.colors(numColors)[-numColors])
    names(manualColors) <- c("Baseflow","Streamflow",modelScenarios)
    
    p <- p + 
      geom_line(data = model_out,
        mapping = aes(x = date, 
                      y = Qout / scale_factor,
                      color = runid),linewidth = 1) + 
    scale_y_continuous(name = "Precip (in)", 
                       sec.axis = sec_axis(~.*scale_factor, name = "Flow (CFS)"))+
      scale_fill_manual(values = c("nldas" = "firebrick4",
                                   "prism" = "steelblue4",
                                   "daymet" = "grey70"),
                        labels = c("nldas" = "NLDAS",
                                   "prism" = "PRISM",
                                   "daymet" = "Daymet"))+
      scale_color_manual(values = manualColors)+
      labs(x = NULL, fill = "Data Source", color = NULL)+
      theme_bw()+
      theme(legend.position = 'inside',
            legend.position.inside = c(0.85,0.75))
  }
    
  return(p)
}

# Example storm_plot function uses
storm_plot("usgs_ws_01613900")
storm_plot("usgs_ws_01629500")
storm_plot("usgs_ws_01646000", date = "2007-08-10")
storm_plot("usgs_ws_01646000", date = "2003-01-01", bufferDays = 60)
#Incorporate model data:
storm_plot("usgs_ws_01646000", date = "2003-01-01", bufferDays = 30,
           modelScenarios = c("pubsheds","mubsheds","lubsheds"),
           riverseg = "PM7_4581_4580")


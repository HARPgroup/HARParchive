###### Testing to see if I can get all upstream segments
## Last Updated 4/28/2021
library(ggplot2)
library(stringr)
library(plotly)
library(sqldf)
library('hydrotools')

site <- "http://deq2.bse.vt.edu/d.dh"  #Specify the site of interest, either d.bet OR d.dh
basepath <- '/var/www/R';
source(paste(basepath,'config.R',sep='/'))
source(paste(hydro_tools, "R", "fn_extract_basin.R", sep = "/"))

# Download and Import csv with all river segments
localpath <- tempdir()
filename <- paste("vahydro_riversegs_export.csv",sep="")
destfile <- paste(localpath,filename,sep="\\")
download.file(paste(site,"/vahydro_riversegs_export",sep=""), destfile = destfile, method = "libcurl")
RSeg.csv <- read.csv(file=paste(localpath , filename,sep="\\"), header=TRUE, sep=",")

####################################
# Inputs
flow_metric <-'7q10' # input flow metric vahydro name as a string
runid1 <- 11 # inputs for the two runids to compare
runid2 <- 18 #6011 #6012 #401
riv_seg <- 'PS5_4380_4370' #'PS3_5990_6161' #'OR2_7900_7740' #'PS3_6161_6280' #'PS5_4380_4370' #'OR7_8470_8490' #'TU3_8880_9230' #'OR3_7740_8271_carvins'

AllSegList <- RSeg.csv$hydrocode
AllSegList <- substring(AllSegList, 17)


# CIA_data Function: Grabs data from vahydro and returns data frame 
#data frame contains flow and percent change data for 2 runids of all upstream and downstream river segments
CIA_data <- function(riv_seg, runid1, runid2, flow_metric, AllSegList){
  downstream <- data.frame(fn_ALL.downstream(riv_seg, AllSegList))
  names(downstream)[names(downstream) == colnames(downstream)[1]] <- "riv_seg"
  riv_seg <- as.data.frame(riv_seg)
  
  # Calculates Upstream River Segments
  upstream <- data.frame((fn_ALL.upstream(riv_seg, AllSegList)))
  names(upstream)[names(upstream) == colnames(upstream)[1]] <- "riv_seg"
  
  if(upstream == 'NA'){
    river <- rbind(riv_seg, downstream)
  }else {
    river <- rbind(upstream,riv_seg, downstream)
  }
  
  #setting up dataframe for om_vahydro_metric_grid
  df <- data.frame(
    'model_version' = c('vahydro-1.0'),
    'runid' = c(paste0('runid_', runid1), paste0('runid_', runid2), paste0('runid_', runid1), paste0('runid_', runid2), paste0('runid_', runid1), paste0('runid_', runid2), paste0('runid_', runid1), paste0('runid_', runid2), paste0('runid_', runid1), paste0('runid_', runid2), paste0('runid_', runid1), paste0('runid_', runid2), paste0('runid_', runid1), paste0('runid_', runid2), '0.%20River%20Channel', 'local_channel'),
    'runlabel' = c('Qbaseline_1', 'Qbaseline_2', 'L90_1', 'L90_2', 'WD_1', 'WD_2', 'PS_1', 'PS_2', 'PSNX_1', 'PSNX_2', 'Qout_1', 'Qout_2', 'Metric_1', 'Metric_2', 'length', 'sub_length'),
    'metric' = c('Qbaseline', 'Qbaseline','l90_Qout','l90_Qout','wd_cumulative_mgd','wd_cumulative_mgd','ps_cumulative_mgd','ps_cumulative_mgd','ps_nextdown_mgd','ps_nextdown_mgd', 'Qout', 'Qout', flow_metric, flow_metric, 'length', 'length')
  )
  
  #importing dataframe of river segment metrics
  wshed_data <- om_vahydro_metric_grid(metric, df)
  
  #triming dataframe to just river segments on river of interest
  cia_data <- sqldf("SELECT * FROM river join wshed_data
                    WHERE riverseg like riv_seg")
  
  #pull the values that exist from length and subcomp_da (One of the two will be NA for each segment)
  da_data <- sqldf("SELECT pid, length, sub_length,
                    CASE
                    WHEN length is null then sub_length
                    ELSE length
                    END as da
                    from cia_data")
  
  #selecting the values that exist and adding them to cia_data (convert to miles)
  cia_data$length <- da_data$da/5280
  
  #deleting sub_length column bc all values are not in length
  cia_data$sub_length <- NULL
  #deleting riv_seg column bc it is a duplicate
  cia_data$riv_seg <- NULL
  
  #creating percent change columns
  #calculating Percent change values for mean annual flow and inputed metric flow
  cia_data$Qout_pc <- ((abs(cia_data$Qout_2 - cia_data$Qout_1))/cia_data$Qout_1)*100
  cia_data$metric_pc <- ((abs(cia_data$Metric_2 - cia_data$Metric_1))/cia_data$Metric_1)*100
  
  #creating column describing - vs + change
  cia_data <- sqldf("select *, CASE
                     WHEN Metric_1 > Metric_2 
                     THEN '-' 
                     WHEN Metric_2 < Metric_1
                     THEN '+'
                     ELSE '0' 
                     END as Metric_change 
                     FROM cia_data")
  
  return(cia_data)
}

#Running function to get river data
cdat <- CIA_data(riv_seg = riv_seg, runid1 = runid1, runid2 = runid2, flow_metric = flow_metric, AllSegList = AllSegList)


# fn_river_network()
# @inputs: riv_seg, AllSegList, and cia_data_frame (output of CIA_data function)
# calculates river mile and returns data trimmed frame to be used in fn_plot_cia_dend()


# outdated function
#fn_river_network <- function(riv_seg, AllSegList, cia_data_frame){

#Calculates Upstream River Segments
upstream <- data.frame((fn_ALL.upstream(riv_seg, AllSegList)))
names(upstream)[names(upstream) == colnames(upstream)[1]] <- "riv_seg"

#While loop that runs the function for every upstream segment
a <- 1
cia_data <- data.frame()
while(a <= nrow(upstream)){
  if(upstream == 'NA'){
    riv_seg <- riv_seg
  }else{
    riv_seg <- upstream[a,]
  }
  #only runs code if river segment is headwater
  if(fn_ALL.upstream(riv_seg,AllSegList) == 'NA'){
    #determines all downstream segments
    downstream <- data.frame(fn_ALL.downstream(riv_seg, AllSegList))
    names(downstream)[names(downstream) == colnames(downstream)[1]] <- "riv_seg"
    riv_seg <- as.data.frame(riv_seg)
    #creates dataframe of river segment and all downstream segments
    river <- rbind(riv_seg, downstream)
    names(river)[names(river) == colnames(river)[1]] <- "riv_seg"
    
    #pulls river data from river segments that match headwater and its downstream segs
    cia_data_loop <- sqldf("SELECT * FROM river join cia_data_frame
                        WHERE riv_seg like riverseg")
    
    #Adding length segments together to form river mile (distance from headwater) column
    i <- 1
    while (i <= nrow(cia_data_loop)) {
      
      river_length <- c()
      
      #Loop creates vector of current segment and upstream segment lengths
      for (n in 1:i) {
        n_length <- as.numeric(cia_data_loop$length[n])
        river_length <- c(river_length, n_length)
      }
      #Makes length column to total length to segment from start of river
      cia_data_loop$mile[i] <- sum(river_length)
      
      i <- i + 1
    }
    
    #Creating a river mile column
    for (i in 1:(length(cia_data_loop$mile))){
      if(i == 1){
        cia_data_loop$rmile[i] <- cia_data_loop$mile[length(cia_data_loop$mile)]
      }
      else{
        cia_data_loop$rmile[i] <- cia_data_loop$mile[length(cia_data_loop$mile)] - cia_data_loop$mile[i-1]
      }
    }
    
    #combine current data frame with new data frame
    cia_data <- rbind(cia_data_loop, cia_data)
    
  }
  
  a <- a + 1
}

#Creating data frame with segment ID numbers
cia_data <- cia_data[!duplicated(cia_data$riv_seg),]
#Makes numbers ordered by river mile (is this whats best? should we make it based on tributary?)
cia_data <- cia_data[order(cia_data$rmile, decreasing = TRUE),]
cia_data$seglist <- 1:nrow(cia_data)
#Triming to only solumns needed in fn_plot_cia_dend
cia_data <- sqldf("SELECT seglist, riverseg, propname, rmile, Metric_1, Metric_2, metric_pc, Metric_change
                   FROM cia_data")

return(cia_data)
}

#Running fn_river_network()
cia_data_frame <- fn_river_network(riv_seg = riv_seg, AllSegList = AllSegList, cia_data_frame = cdat)

# fn_plot_cia_dend Function: Takes CIA_data out put and returns plots
#As of latest update to code 
#we did not figure out a way to separate this into multiple functions because of the way the graphing is done
fn_plot_cia_dend <- function(riv_seg, AllSegList, runid1, runid2, cia_data_frame){
  
  #Declaring initial inputed river segment for graphing dot
  riv_seg_i <- riv_seg
  
  #Calculates Upstream River Segments
  upstream <- data.frame((fn_ALL.upstream(riv_seg, AllSegList)))
  names(upstream)[names(upstream) == colnames(upstream)[1]] <- "riv_seg"
  
  #While loop that runs the function for every upstream segment
  a <- 1
  cia_data <- data.frame()
  p <- ggplot(NULL)
  while(a <= nrow(upstream)){
    if(upstream == 'NA'){
      riv_seg <- riv_seg
    }else{
      riv_seg <- upstream[a,]
    }
    #only runs code if river segment is headwater
    if(fn_ALL.upstream(riv_seg,AllSegList) == 'NA'){
      #determines all downstream segments
      downstream <- data.frame(fn_ALL.downstream(riv_seg, AllSegList))
      names(downstream)[names(downstream) == colnames(downstream)[1]] <- "riv_seg"
      riv_seg <- as.data.frame(riv_seg)
      #creates dataframe of river segment and all downstream segments
      river <- rbind(riv_seg, downstream)
      names(river)[names(river) == colnames(river)[1]] <- "riv_seg"
      
      #pulls river data from river segments that match headwater and its downstream segs
      cia_data_loop <- sqldf("SELECT * FROM river join cia_data_frame
                        WHERE riv_seg like riverseg")
      
      #Adding length segments together to form river mile (distance from headwater) column
      i <- 1
      while (i <= nrow(cia_data_loop)) {
        
        river_length <- c()
        
        #Loop creates vector of current segment and upstream segment lengths
        for (n in 1:i) {
          n_length <- as.numeric(cia_data_loop$length[n])
          river_length <- c(river_length, n_length)
        }
        #Makes length column to total length to segment from start of river
        cia_data_loop$mile[i] <- sum(river_length)
        
        i <- i + 1
      }
      
      #Creating a river mile column
      for (i in 1:(length(cia_data_loop$mile))){
        if(i == 1){
          cia_data_loop$rmile[i] <- cia_data_loop$mile[length(cia_data_loop$mile)]
        }
        else{
          cia_data_loop$rmile[i] <- cia_data_loop$mile[length(cia_data_loop$mile)] - cia_data_loop$mile[i-1]
        }
      }
      
      #combine current data frame with new data frame
      cia_data <- rbind(cia_data_loop, cia_data)
      
      #plot graph
      p <- p +
        geom_line(data = cia_data_loop, aes(x = rmile, y = Metric_1, colour = Metric_change, size = metric_pc))
      
      
    }
    
    a <- a + 1
  }
  
  # Reversing scale for correct river mile orientation
  p <- p + scale_x_reverse()
  
  #Creating data frame with segment ID numbers
  cia_data <- cia_data[!duplicated(cia_data$riv_seg),]
  #Makes numbers ordered by river mile (is this whats best? should we make it based on tributary?)
  cia_data <- cia_data[order(cia_data$rmile, decreasing = TRUE),]
  cia_data$seglist <- 1:nrow(cia_data)
  
  #Creating data frame with just original inputed river segment to graph point
  riv_seg_og <- cia_data[cia_data$rivseg == riv_seg_i,]
  
  p <- p +
    geom_point(data = riv_seg_og, aes(x = rmile, y = Metric_1)) +
    geom_text(data = cia_data, aes(x = rmile, y = Metric_1, label = seglist, vjust = 1.0)) + 
    scale_size_continuous(range = c(0.2, 3), name = "Percent Change") +
    scale_colour_manual(values = c("blue", "brown"), name = "Percent Change") +
    theme_bw() +
    ggtitle(paste0("Percent Change in ", flow_metric, " Flow between runid", runid1, " and runid", runid2)) +
    xlab('River Mile [Mi]') +
    ylab('Flow [cfs]')
  
  return(p)
}

fn_plot_cia_dend <- function(riv_seg, AllSegList, runid1, runid2, flow_metric, cia_data_frame){
  
  #Declaring initial inputed river segment for graphing dot
  riv_seg_i <- riv_seg
  
  #Calculates Upstream River Segments
  upstream <- data.frame((fn_ALL.upstream(riv_seg, AllSegList)))
  names(upstream)[names(upstream) == colnames(upstream)[1]] <- "riv_seg"
  
  #While loop that runs the function for every upstream segment
  a <- 1
  p <- ggplot(NULL)
  while(a <= nrow(upstream)){
    if(upstream == 'NA'){
      riv_seg <- riv_seg
    }else{
      riv_seg <- upstream[a,]
    }
    #only runs code if river segment is headwater
    if(fn_ALL.upstream(riv_seg,AllSegList) == 'NA'){
      #determines all downstream segments
      downstream <- data.frame(fn_ALL.downstream(riv_seg, AllSegList))
      names(downstream)[names(downstream) == colnames(downstream)[1]] <- "riv_seg"
      riv_seg <- as.data.frame(riv_seg)
      #creates dataframe of river segment and all downstream segments
      river <- rbind(riv_seg, downstream)
      names(river)[names(river) == colnames(river)[1]] <- "riv_seg"
      
      #pulls river data from river segments that match headwater and its downstream segs
      cia_data_loop <- sqldf("SELECT * FROM river join cia_data_frame
                        WHERE riv_seg like riverseg")
      
      #plot graph
      p <- p +
        geom_line(data = cia_data_loop, aes(x = rmile, y = Metric_1, colour = Metric_change, size = metric_pc))
      
    }
    
    a <- a + 1
  }
  
  # Reversing scale for correct river mile orientation
  p <- p + scale_x_reverse()
  
  
  #Creating data frame with just original inputed river segment to graph point
  riv_seg_og <- cia_data_frame[cia_data_frame$riverseg == riv_seg_i,]
  
  p <- p +
    geom_point(data = riv_seg_og, aes(x = rmile, y = Metric_1)) +
    geom_text(data = cia_data_frame, aes(x = rmile, y = Metric_1, label = seglist, vjust = 1.0)) + 
    scale_size_continuous(range = c(0.2, 3), name = "Percent Change") +
    scale_colour_manual(values = c("blue", "grey", "brown"), name = "Percent Change") +
    theme_bw() +
    ggtitle(paste0("Percent Change in ", flow_metric, " Flow between runid", runid1, " and runid", runid2)) +
    xlab('River Mile [Mi]') +
    ylab('Flow [cfs]')
  
  return(p)
}

#running fn_plot_cia_dend
cia_plot <- fn_plot_cia_dend(riv_seg = riv_seg, AllSegList = AllSegList, runid1 = runid1, runid2 = runid2, flow_metric = flow_metric, cia_data_frame = cia_data_frame)

#for testing purposes
cia_plot

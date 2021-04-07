###### Testing to see if I can get all upstream segments
## Last Updated 4/7/2021
library(ggplot2)
library(stringr)
library(plotly)
library(sqldf)
library('hydrotools')

site <- "http://deq2.bse.vt.edu/d.dh"  #Specify the site of interest, either d.bet OR d.dh
basepath <- '/var/www/R';
source(paste(basepath,'config.R',sep='/'))

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
runid2 <- 18
riv_seg <- 'PS5_4380_4370' #'PS3_5990_6161' #'OR2_7900_7740' #'PS3_6161_6280' #'PS5_4380_4370' #'OR7_8470_8490'

AllSegList <- RSeg.csv$hydrocode
AllSegList <- substring(AllSegList, 17)

#Function  
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
  
  # #Adding length segments together to form river mile (distance from headwater) column
  # i <- 1
  # while (i <= nrow(cia_data)) {
  #   
  #   river_length <- c()
  #   
  #   # Loop creates vector of current segment and upstream segment lengths
  #   for (n in 1:i) {
  #     n_length <- as.numeric(cia_data$length[n])
  #     river_length <- c(river_length, n_length)
  #   }
  #   # Makes length column to total length to segment from start of river
  #   cia_data$mile[i] <- sum(river_length)
  #   
  #   i <- i + 1
  # }
  # 
  # # Creating a river mile column
  # for (i in 1:(length(cia_data$mile))){
  #   if(i == 1){
  #     cia_data$rmile[i] <- cia_data$mile[length(cia_data$mile)]
  #   }
  #   else{
  #     cia_data$rmile[i] <- cia_data$mile[length(cia_data$mile)] - cia_data$mile[i-1]
  #   }
  # }
  # 
  # # Calculating Percent change values for mean annual flow and inputed metric flow
  # cia_data$Qout_pc <- ((cia_data$Qout_2 - cia_data$Qout_1)/cia_data$Qout_1)*100
  # cia_data$metric_pc <- ((cia_data$Metric_2 - cia_data$Metric_1)/cia_data$Metric_1)*100
  # 
  # # Must make seg list numbered for x axis on graphs with bars, could be used in table companion
  # cia_data$seglist = as.numeric(c(1:nrow(cia_data)))
  # 
  # # Now lets graph using mean baseflow
  # # Pre setting y axes max
  # if (max(cia_data$Qout_1 >= cia_data$Qout_2)) {
  #   y_prim <- c(0,max(cia_data$Qout_1) + 100)
  # } else {
  #   y_prim <- c(0, max(cia_data$Qout_2) + 100)
  # }
  # y_sec <-  c(min(cia_data$Qout_pc) - 2, max(cia_data$Qout_pc) + 2)
  # 
  # coeff <- max(y_sec) / max(y_prim)
  # cia_data$Qout_pc_graph <- cia_data$Qout_pc/coeff
  # 
  # # Pre setting y axes max (Do Not Need If No More ggplot)
  # if (max(cia_data$Metric_1 >= cia_data$Metric_2)) {
  #   y_prim2 <- c(0,max(cia_data$Metric_1) + 100)
  # } else {
  #   y_prim2 <- c(0, max(cia_data$Metric_2) + 100)
  # }
  # y_sec2 <-  c(min(cia_data$metric_pc) - 2, max(cia_data$metric_pc) + 2)
  # 
  # coeff2 <- max(y_sec2) / max(y_prim2)
  # cia_data$metric_pc_graph <- cia_data$metric_pc/coeff2
  
  
  
  #return(list(cia_data,
   #           y_prim,
    #          y_prim2,
     #         coeff,
      #        coeff2))
  
  return(cia_data)
}

#running function to get river data
cdat <- CIA_data(riv_seg = riv_seg, runid1 = runid1, runid2 = runid2, flow_metric = flow_metric, AllSegList = AllSegList)

# Calculates Upstream River Segments
upstream <- data.frame((fn_ALL.upstream(riv_seg, AllSegList)))
names(upstream)[names(upstream) == colnames(upstream)[1]] <- "riv_seg"

# While loop that runs the function for every upstream segment
a <- 1
cia_data <- data.frame()
p <- ggplot(NULL)
p1 <- ggplot(NULL)
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
    names(river)[names(river) == colnames(river)[1]] <- "rivseg"
    
    #pulls river data from river segments that match headwater and its downstream segs
    cia_data_loop <- sqldf("SELECT * FROM river join cdat
                      WHERE rivseg like riverseg")
    
      #Adding length segments together to form river mile (distance from headwater) column
      i <- 1
      while (i <= nrow(cia_data_loop)) {
        
        river_length <- c()
        
        # Loop creates vector of current segment and upstream segment lengths
        for (n in 1:i) {
          n_length <- as.numeric(cia_data_loop$length[n])
          river_length <- c(river_length, n_length)
        }
        # Makes length column to total length to segment from start of river
        cia_data_loop$mile[i] <- sum(river_length)
        
        i <- i + 1
      }
    
      # Creating a river mile column
      for (i in 1:(length(cia_data_loop$mile))){
        if(i == 1){
          cia_data_loop$rmile[i] <- cia_data_loop$mile[length(cia_data_loop$mile)]
        }
        else{
          cia_data_loop$rmile[i] <- cia_data_loop$mile[length(cia_data_loop$mile)] - cia_data_loop$mile[i-1]
        }
      }
    
      # Calculating Percent change values for mean annual flow and inputed metric flow
      cia_data_loop$Qout_pc <- ((cia_data_loop$Qout_2 - cia_data_loop$Qout_1)/cia_data_loop$Qout_1)*100
      cia_data_loop$metric_pc <- ((cia_data_loop$Metric_2 - cia_data_loop$Metric_1)/cia_data_loop$Metric_1)*100
      
      # Must make seg list numbered for x axis on graphs with bars, could be used in table companion
      cia_data_loop$seglist = as.numeric(c(1:nrow(cia_data_loop)))
      
      # Now lets graph using mean baseflow
      # Pre setting y axes max
      if (max(cia_data_loop$Qout_1 >= cia_data_loop$Qout_2)) {
        y_prim <- c(0,max(cia_data_loop$Qout_1) + 100)
      } else {
        y_prim <- c(0, max(cia_data_loop$Qout_2) + 100)
      }
      y_sec <-  c(min(cia_data_loop$Qout_pc) - 2, max(cia_data_loop$Qout_pc) + 2)
      
      coeff <- max(y_sec) / max(y_prim)
      cia_data_loop$Qout_pc_graph <- cia_data_loop$Qout_pc/coeff
      
      # Pre setting y axes max (Do Not Need If No More ggplot)
      if (max(cia_data_loop$Metric_1 >= cia_data_loop$Metric_2)) {
        y_prim2 <- c(0,max(cia_data_loop$Metric_1) + 100)
      } else {
        y_prim2 <- c(0, max(cia_data_loop$Metric_2) + 100)
      }
      y_sec2 <-  c(min(cia_data_loop$metric_pc) - 2, max(cia_data_loop$metric_pc) + 2)
      
      coeff2 <- max(y_sec2) / max(y_prim2)
      cia_data_loop$metric_pc_graph <- cia_data_loop$metric_pc/coeff2
    
    #combine current data frame with new data frame
    cia_data <- rbind(cia_data_loop, cia_data)
    
    #plot graph
    p <- p +
      geom_line(data = cia_data_loop, aes(x = rmile, y = Qout_1, colour = Qout_pc, size = Qout_pc)) +
      #geom_line(data = cia_data_loop, aes(x = rmile, y = Qout_2)) + 
      scale_color_gradient2(low = "Red", high = "Blue", mid = "Green", midpoint = 0, name = "Percent Change") +
      theme_bw() +
      ggtitle(paste0("Percent Change in Mean Annual Flow between runid", runid1, " and runid", runid2)) +
      xlab('River Mile [Mi]') +
      ylab('Flow [cfs]')
      #labs(size = 'Percent Change') +
      #geom_vline(data = cia_data_loop, (aes(xintercept = rmile)),linetype=8, colour = "grey") +
      #geom_text(data = cia_data_loop, aes(x = rmile, label = paste(propname),
      #                                    y=(max(Qout_1)/2)), colour="grey", angle=90,
      #          vjust=-0.4, size=3)
    
    p1 <- p1 +
      geom_line(data = cia_data_loop, aes(x = rmile, y = Metric_1, colour = metric_pc, size = metric_pc)) +
      #geom_line(data = cia_data_loop, aes(x = rmile, y = Qout_2)) + 
      scale_color_gradient2(low = "Red", high = "Blue", mid = "Green", midpoint = 0, name = "Percent Change") +
      theme_bw() +
      ggtitle(paste0("Percent Change in ", flow_metric, " Flow between runid", runid1, " and runid", runid2)) +
      xlab('River Mile [Mi]') +
      ylab('Flow [cfs]') +
      scale_x_reverse()
    }
  
  a <- a + 1
}

p <- p + scale_x_reverse()

##############################################################################testing grouping by river segment
test <- cia_data[!duplicated(cia_data$riv_seg),]
test <- test[order(test$rmile, decreasing = TRUE),]
test$seglist <- 1:nrow(test)

p <- p +
  geom_point(data = cia_data_loop, aes(x = rmile, y = Qout_1, size = Qout_pc)) +
  geom_text(data = test, aes(x = rmile, y = Qout_1, label = seglist, vjust = 1.0)) + 
  scale_size_continuous(range = c(3, 0.2), name = "Percent Change")

p1 <- p1 +
  geom_point(data = cia_data_loop, aes(x = rmile, y = Metric_1, size = metric_pc)) +
  geom_text(data = test, aes(x = rmile, y = Metric_1, label = seglist, vjust = 1.0)) + 
  scale_size_continuous(range = c(3, 0.2), name = "Percent Change")

##############################################################################trying to import our most up to date function (does not work as of 3/24/2021)
par(mar = c(5, 5, 8, 5)) #ADJUST PLOTTING MARGINS TO ACCOMMODATE RSEG NAMES
plot(cia_data$seglist, cia_data$Qout_1, type = "l", col = "green", xaxt = 'n', xlab = "", ylab = "", las=1,ylim=c(0,y_prim[2])) #CREATE FIRST PLOT (ON PRIMARY Y-AXIS)
lines(cia_data$seglist, cia_data$Qout_2, type = "l", col = "blue")  #ADD ADDITIONAL PLOTTING DATAPOINTS
abline(v=1:length(cia_data$propname), col="azure2") #ADD VERTICAL GRID LINES FOR EACH RSEG
text(x = cia_data$seglist,y = y_prim[2]+(0.06*y_prim[2]),labels=cia_data$propname,xpd = NA, srt = 45, offset = -0.2, pos = 4,cex=0.7) #ADD RSEG NAMES ABOVE PLOT
par(new = TRUE) #ADD SECOND PLOT (ON SECONDARY Y-AXIS)                                                
plot(cia_data$seglist, cia_data$Qout_pc, type = "h", col = "red",axes = FALSE, xlab = "River Mile", ylab = "Flow (cfs)",ylim=c(-15,15)) #NOTE: SWITCH BETWEEN type = "h" (FOR 'histogram' LIKE VERTICAL LINES) and "l" (FOR LINES)
axis(side = 4, las=1,cex.axis=0.8) # ADD SECOND AXIS
mtext("Percent Difference in Flow between runids", side = 4, line = 3) #ADD SECOND AXIS LABEL     
abline(h=0, col="grey", lty=2) #ADD HORIZONTAL DOTTED LINE AT Y=0
legend("topleft", c("Qout_1", "Qout_2", "Qout_pc"),col = c("green", "blue", "red"), lty = c(1,1,1), bg='white') #ADD LEGEND
axis(side = 1, at = c(1:nrow(cia_data)), labels = as.character(round(cia_data$rmile, 0)))
p8 <- recordPlot()
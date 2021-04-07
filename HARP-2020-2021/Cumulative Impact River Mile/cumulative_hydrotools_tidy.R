#CIA Analyis using the hydrotools funcitons

library(ggplot2)
library(stringr)
library(plotly)
library(sqldf)
library(rapportools)
library('hydrotools')

site <- "http://deq2.bse.vt.edu/d.dh"  #Specify the site of interest, either d.bet OR d.dh
basepath <- '/var/www/R';
source(paste(basepath,'config.R',sep='/'))

# Pulling Vahydro River Segment List
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
riv_seg <- 'OR3_7740_8271' #'PS3_5990_6161' #'OR2_7900_7740' #'OR3_7740_8271'

CIA_data <- function(riv_seg, runid1, runid2, flow_metric){
  
  AllSegList <- RSeg.csv$hydrocode
  AllSegList <- substring(AllSegList, 17)
  
  #Finding headwater and all downstring river segments
  upstream <- data.frame((fn_ALL.upstream(riv_seg, AllSegList)))
  upstream <- upstream[nrow(upstream):1,]
  upstream <- data.frame(upstream)
  names(upstream)[names(upstream) == colnames(upstream)[1]] <- "riv_seg"
  
  upstream <- as.character(upstream[[1,1]])
  
  if(upstream == 'NA'){
    riv_seg <- riv_seg
  }else {
    riv_seg <- upstream
  }
  
  downstream <- data.frame(fn_ALL.downstream(riv_seg, AllSegList))
  names(downstream)[names(downstream) == colnames(downstream)[1]] <- "riv_seg"
  riv_seg <- as.data.frame(riv_seg)
  river <- rbind(riv_seg, downstream)
  
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
  
  #Adding length segments together to form river mile (distance from headwater) column
  i <- 1
  while (i <= nrow(cia_data)) {
    
    river_length <- c()
    
    # Loop creates vector of current segment and upstream segment lengths
    for (n in 1:i) {
      n_length <- as.numeric(cia_data$length[n])
      river_length <- c(river_length, n_length)
    }
    # Makes length column to total length to segment from start of river
    cia_data$mile[i] <- sum(river_length)
    
    i <- i + 1
  }
  
  # Creating a river mile column
  for (i in 1:(length(cia_data$mile))){
    if(i == 1){
      cia_data$rmile[i] <- cia_data$mile[length(cia_data$mile)]
    }
    else{
    cia_data$rmile[i] <- cia_data$mile[length(cia_data$mile)] - cia_data$mile[i-1]
    }
  }
  
  # Calculating Percent change values for mean annual flow and inputed metric flow
  cia_data$Qout_pc <- ((cia_data$Qout_2 - cia_data$Qout_1)/cia_data$Qout_1)*100
  cia_data$metric_pc <- ((cia_data$Metric_2 - cia_data$Metric_1)/cia_data$Metric_1)*100
  
  # Must make seg list numbered for x axis on graphs with bars, could be used in table companion
  cia_data$seglist = as.numeric(c(1:nrow(cia_data)))
  
  # Now lets graph using mean baseflow
  # Pre setting y axes max
  if (max(cia_data$Qout_1 >= cia_data$Qout_2)) {
    y_prim <- c(0,max(cia_data$Qout_1) + 100)
  } else {
    y_prim <- c(0, max(cia_data$Qout_2) + 100)
  }
  y_sec <-  c(min(cia_data$Qout_pc) - 2, max(cia_data$Qout_pc) + 2)
  
  coeff <- max(y_sec) / max(y_prim)
  cia_data$Qout_pc_graph <- cia_data$Qout_pc/coeff
  
  # Pre setting y axes max (Do Not Need If No More ggplot)
  if (max(cia_data$Metric_1 >= cia_data$Metric_2)) {
    y_prim2 <- c(0,max(cia_data$Metric_1) + 100)
  } else {
    y_prim2 <- c(0, max(cia_data$Metric_2) + 100)
  }
  y_sec2 <-  c(min(cia_data$metric_pc) - 2, max(cia_data$metric_pc) + 2)
  
  coeff2 <- max(y_sec2) / max(y_prim2)
  cia_data$metric_pc_graph <- cia_data$metric_pc/coeff2
  
  
  
  return(list(cia_data,
              y_prim,
              y_prim2,
              coeff,
              coeff2))
}

#testing function
cdat <- CIA_data(riv_seg = riv_seg, runid1 = runid1, runid2 = runid2, flow_metric = flow_metric)
cia_data <- cdat[[1]]
y_prim <- cdat[[2]]
y_prim2 <- cdat[[3]]
coeff <- cdat[[4]]
coeff2 <- cdat[[5]]


#calculating flow changes (Do Not Need if using equally spaced graph)
rivseg_flow_change <- function(cia_data, pct_change_in_flow){
  riv_changes <- c()
  for (i in 1:(length(cia_data$Qout_1)-1)){
    val1 <- cia_data$Qout_1[i]
    val2 <- cia_data$Qout_1[i+1]
    pct_change <- val2/val1 *100
    if (pct_change >= 100+pct_change_in_flow | pct_change <= 100-pct_change_in_flow) {
      riv_changes <- append(riv_changes,i+1)
    }
  }
  #then pull full name/info, by pulling row for each value in df
  # add for loop to iterate through each value of
  changes_df <- data.frame()
  for (val in riv_changes) {
    change <- cia_data[val,]
    changes_df <- rbind(changes_df,change)
  }
  
  return(changes_df)
}

#testing function
changes_df <- rivseg_flow_change(cia_data = cia_data, 40)


CIA_plots <- function(cia_data){
# Pre setting y axes max
# if (max(cia_data$Qout_1 >= cia_data$Qout_2)) {
#    y_prim <- c(0,max(cia_data$Qout_1) + 100)
# } else {
#   y_prim <- c(0, max(cia_data$Qout_2) + 100)
# }
# 
# y_sec <-  c(min(cia_data$Qout_pc) - 2, max(cia_data$Qout_pc) + 2)
# coeff <- max(y_sec) / max(y_prim)
#   
# ################################################################################# Graph
# p1 <- ggplot(cia_data, aes(x = seglist)) +
#           geom_col(aes(y = Qout_pc_graph), size = 1, color = "blue", fill = 'lightblue') +
#           geom_point(aes(y = Qout_1, colour = paste0('runid_',runid1)), size = 2.0) +
#           geom_line(aes(y = Qout_1, colour = paste0('runid_',runid1)), size = 1.25) +
#           geom_point(aes(y = Qout_2, colour = paste0('runid_',runid2)), size = 2.0) +
#           geom_line(aes(y = Qout_2, colour = paste0('runid_',runid2)), size = 1.25) +
#           labs(colour = 'Legend') +
#           ggtitle(paste0('Comparison of Mean Annual Flow for ', runid1, ' and ', runid2)) +
#           xlab('Segment List (1 = headwater)') +
#           ylab('Flow (cfs)') +
#           scale_y_continuous(name = "Flow (cfs)", limits = y_prim,
#                              sec.axis = sec_axis(~.*coeff, name = 'Percent Difference in Flow between runids')) +
#           geom_vline(data = cia_data, (aes(xintercept = seglist)),linetype=8) +
#           theme_bw() +
#           geom_text(data = cia_data, aes(x = seglist, label = paste(propname),
#                                          y=(max(Qout_1)/2)), colour="black", angle=90,
#                                          vjust=-0.4, size=3)
#   
#   
# ############################################################# Graph with logarithmic flow, no percentages
# p2 <- ggplot(cia_data, aes(x = seglist)) +
#           geom_point(aes(y = Qout_1, colour = paste0('runid_',runid1)), size = 2.0) +
#           geom_line(aes(y = Qout_1, colour = paste0('runid_',runid1)), size = 1.25) +
#           geom_point(aes(y = Qout_2, colour = paste0('runid_',runid2)), size = 2.0) +
#           geom_line(aes(y = Qout_2, colour = paste0('runid_',runid2)), size = 1.25) +
#           labs(colour = 'Legend') +
#           ggtitle(paste0('Comparison of Flow for ', runid1, ' and ', runid2)) +
#           xlab('Segment List (1 = headwater)') +
#           ylab('Flow (cfs)') +
#           scale_y_log10() +
#           theme_bw()
#   
#   
# ################################################### Graph with flow, percentages as a line
# p3 <- ggplot(cia_data, aes(x = seglist)) +
#           geom_point(aes(y = Qout_pc_graph, colour = paste0('% difference')), size = 1.5) +
#           geom_line(aes(y = Qout_pc_graph, colour = paste0('% difference')), size = 0.75) +
#           geom_point(aes(y = Qout_1, colour = paste0('runid_',runid1)), size = 2.0) +
#           geom_line(aes(y = Qout_1, colour = paste0('runid_',runid1)), size = 1.25) +
#           geom_point(aes(y = Qout_2, colour = paste0('runid_',runid2)), size = 2.0) +
#           geom_line(aes(y = Qout_2, colour = paste0('runid_',runid2)), size = 1.25) +
#           labs(colour = 'Legend') +
#           ggtitle(paste0('Comparison of Flow for ', runid1, ' and ', runid2)) +
#           xlab('Segment List (1 = headwater)') +
#           ylab('Flow (cfs)') +
#           scale_y_continuous(name = "Flow (cfs)", limits = y_prim,
#                              sec.axis = sec_axis(~.*coeff, name = 'Percent Diff in Flow between runids (%)')) +
#           theme_bw()+
#           xlab('Segment List (1 = headwater)') +
#           ylab('Flow (cfs)') +
#           scale_y_continuous(name = "Flow (cfs)", limits = y_prim,
#                              sec.axis = sec_axis(~.*coeff, name = 'Percent Difference in Flow between runids')) +
#           geom_vline(data = cia_data, (aes(xintercept = seglist)),linetype=8) +
#           theme_bw() +
#           geom_text(data = cia_data, aes(x = seglist, label = paste(propname),
#                                          y=(max(Qout_1)/2)), colour="black", angle=90,
#                     vjust=-0.4, size=3)
#   
# 
#  Pre setting y axes max (Do Not Need If No More ggplot)
# if (max(cia_data$Metric_1 >= cia_data$Metric_2)) {
#   y_prim <- c(0,max(cia_data$Metric_1) + 100)
# } else {
#   y_prim <- c(0, max(cia_data$Metric_2) + 100)
# }
# 
# y_sec <-  c(min(cia_data$metric_pc) - 2, max(cia_data$metric_pc) + 2)
# coeff <- max(y_sec) / max(y_prim)
# 
# ###################################################### Bar Graph
# p4 <- ggplot(cia_data, aes(x = seglist)) +
#           geom_col(aes(y = metric_pc_graph), size = 1, color = "blue", fill = 'lightblue') +
#           geom_point(aes(y = Metric_1, colour = paste0('runid_',runid1)), size = 2.0) +
#           geom_line(aes(y = Metric_1, colour = paste0('runid_',runid1)), size = 1.25) +
#           geom_point(aes(y = Metric_2, colour = paste0('runid_',runid2)), size = 2.0) +
#           geom_line(aes(y = Metric_2, colour = paste0('runid_',runid2)), size = 1.25) +
#           labs(colour = 'Legend') +
#           ggtitle(paste0('Comparison of ', flow_metric,' Flow for ', runid1, ' and ', runid2)) +
#           xlab('Segment List (1 = headwater)') +
#           ylab('Flow (cfs)') +
#           scale_y_continuous(name = "Flow (cfs)", limits = y_prim2,
#                              sec.axis = sec_axis(~.*coeff2, name = 'Percent Difference in Flow between runids')) +
#           geom_vline(data = cia_data, (aes(xintercept = seglist)),linetype=8, colour = "grey") +
#           theme_light() +
#           geom_text(data = cia_data, aes(x = seglist, label = paste(propname),
#                                          y=(max(Metric_1)/2)), colour="black", angle=90,
#                     vjust=-0.4, size=3)
#   
# ###################################################### River Mile with markers
# p5 <- ggplot(cia_data, aes(x = rmile)) +
#           geom_point(aes(y = Qout_1, colour = 'runid11' )) +
#           geom_line(aes(y = Qout_1, colour = 'runid11' )) +
#           geom_point(aes(y = Qout_2, colour = 'runid18' )) +
#           geom_line(aes(y = Qout_2, colour = 'runid18' )) +
#           labs(colour = 'Legend') +
#           ggtitle(paste0('Comparison of Flow')) +
#           xlab('River Mile [mi]') +
#           ylab(paste0('Flow [cfs]')) +
#           geom_vline(data=changes_df,(aes(xintercept = changes_df$rmile)),linetype=8, colour = "grey") + 
#           theme_bw() +
#           geom_text(data= changes_df, aes(x=changes_df$rmile, label=paste(changes_df$propname),
#                                           y=(max(cia_data$Qout_1)/2)), colour="grey", angle=90,
#                                           vjust=-0.4, size=4) +
#           theme(panel.grid = element_blank())+
#           scale_x_reverse()
#   
# 
# # Pre setting y axes max
# if (max(cia_data$Qout_1 >= cia_data$Qout_2)) {
#   y_prim <- c(0,max(cia_data$Qout_1) + 100)
# } else {
#   y_prim <- c(0, max(cia_data$Qout_2) + 100)
# }
# 
# y_sec <-  c(min(cia_data$Qout_pc) - 2, max(cia_data$Qout_pc) + 2)
# coeff <- max(y_sec) / max(y_prim)
# 
# ###################################################### River Mile with flow and percent change
# p6 <- ggplot(cia_data, aes(x = rmile)) +
#           geom_point(aes(y = Qout_pc_graph, colour = paste0('% difference')), size = 1.5) +
#           geom_line(aes(y = Qout_pc_graph, colour = paste0('% difference')), size = 0.75) +
#           geom_point(aes(y = Qout_1, colour = 'runid11' )) +
#           geom_line(aes(y = Qout_1, colour = 'runid11' )) +
#           geom_point(aes(y = Qout_2, colour = 'runid18' )) +
#           geom_line(aes(y = Qout_2, colour = 'runid18' )) +
#           labs(colour = 'Legend') +
#           ggtitle(paste0('Comparison of Flow')) +
#           xlab('River Mile [mi]') +
#           ylab(paste0('Flow [cfs]')) +
#           scale_y_continuous(name = "Flow (cfs)", limits = y_prim,
#                              sec.axis = sec_axis(~.*coeff, name = 'Percent Diff in Flow between runids (%)')) +
#           theme_bw()+
#           xlab('River Mile [Mi]') +
#           ylab('Flow [cfs]') +
#           scale_y_continuous(name = "Flow [cfs]", limits = y_prim,
#                              sec.axis = sec_axis(~.*coeff, name = 'Percent Difference in Flow between runids')) +
#           geom_vline(data=changes_df,(aes(xintercept = changes_df$rmile)),linetype=8, colour = "grey") + 
#           theme_bw() +
#           geom_text(data= changes_df, aes(x=changes_df$rmile, label=paste(changes_df$propname),
#                                           y=(max(cia_data$Qout_1)/2)), colour="grey", angle=90,
#                     vjust=-0.4, size=4) +
#           theme(panel.grid = element_blank(), text = element_text(size=15))+
#           scale_x_reverse()

###################################################### Base R plot
par(mar = c(5, 5, 8, 5)) #ADJUST PLOTTING MARGINS TO ACCOMMODATE RSEG NAMES
plot(cia_data$seglist, cia_data$Qout_1, type = "l", col = "green", xlab = "", ylab = "", las=1,cex.axis=0.8,ylim=c(0,y_prim[2])) #CREATE FIRST PLOT (ON PRIMARY Y-AXIS)
lines(cia_data$seglist, cia_data$Qout_2, type = "l", col = "blue")  #ADD ADDITIONAL PLOTTING DATAPOINTS
abline(v=1:length(cia_data$propname), col="azure2") #ADD VERTICAL GRID LINES FOR EACH RSEG
text(x = cia_data$seglist,y = y_prim[2]+(0.06*y_prim[2]),labels=cia_data$propname,xpd = NA, srt = 45, offset = -0.2, pos = 4,cex=0.7) #ADD RSEG NAMES ABOVE PLOT
par(new = TRUE) #ADD SECOND PLOT (ON SECONDARY Y-AXIS)                                                
plot(cia_data$seglist, cia_data$Qout_pc, type = "h", col = "red",axes = FALSE, xlab = "Segment List (1 = headwater)", ylab = "Flow (cfs)",ylim=c(-15,15)) #NOTE: SWITCH BETWEEN type = "h" (FOR 'histogram' LIKE VERTICAL LINES) and "l" (FOR LINES)
axis(side = 4, las=1,cex.axis=0.8) # ADD SECOND AXIS
mtext("Percent Difference in Flow between runids", side = 4, line = 3) #ADD SECOND AXIS LABEL     
abline(h=0, col="grey", lty=2) #ADD HORIZONTAL DOTTED LINE AT Y=0
legend("topleft", c("Qout_1", "Qout_2", "Qout_pc"),col = c("green", "blue", "red"), lty = c(1,1,1), bg='white') #ADD LEGEND
p7 <- recordPlot()

###################################################### Base R plot with river mile
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

return(list(p7,
            p8))

}

# Testing Plotting Function
cia_plots <- CIA_plots(cia_data = cia_data)
plot1 <- cia_plots[[1]]
plot2 <- cia_plots[[2]]


###################################################################
# MAP #############################################################
###################################################################
source(paste(github_location,"/HARParchive/HARP-2020-2021/Cumulative Impact River Mile/CIA_maps.R",sep = '/'))
map_layers <- load_MapLayers(site = site) #WARNING - DO NOT ATTEMPT TO OUTPUT map_layers DIRECTLY TO YOUR CONSOLE, ITS A LIST OF MANY LARGE MAPPING LAYERS
cia_map <- CIA_maps(cia_data = cia_data, map_layers = map_layers)
ggsave(paste0(export_path,riv_seg,"_cia_map.png",sep = ""), width=5.5, height=5)



#A function to plot individual storms. Expects storm to be a data frame in the
#style output by stormSep_USGS, which is to say that it should have the
#following columns IN ORDER:
#timestamp (date objects)
#Flow (cfs)
#Baseflow (cfs)
#Baseline flow (cfs)
#Also take in flowDataAll, a data frame of a longer time period that has flow,
#baseflow, and precip
plotStorm <- function(pathOut,storm,flowDataAll){
  #Set colors for barplot:
  PRISMCol <- rgb(31/255,119/255,171/255,alpha = 0.5)
  daymetCol <- rgb(171/255,40/255,31/255,alpha = 0.5)
  NLDASCol <- rgb(90/255,133/255,33/255,alpha = 0.5)
  
  png(pathOut, width=1820,height=760)
  #Set plot margines
  par(mar=c(5,6,2,4))
  #Show precip as barplot
  #Base R barplot requires matrix input:
  stormPrecip <- t(as.matrix(flowDataAll[,c("prism_p_in","daymet_p_in","nldas2_p_in")]))
  barplot(stormPrecip,
          # names.arg = flowDataAll$timestamp,
          beside = TRUE,
          axes = FALSE,
          col = c(PRISMCol, daymetCol, NLDASCol)
  )
  axis(4,line = -3,lwd = 2,cex.axis = 2)
  mtext("Precip (in)",side = 4, line = 2,cex = 2)
  #Add new plot for flow and storm:
  par(new = TRUE)
  #Plot the storm, making the labels a little thicker and the lines of the
  #plot and labeling the axes
  plot(flowDataAll$timestamp, flowDataAll$flow, type='l',
       xlab='Date', ylab='Flow (cfs)',frame.plot = FALSE,
       lwd=3, cex.axis=2, cex.lab=2)
  #Plot the baseflow
  lines(flowDataAll$timestamp, flowDataAll$baseQ,
        col = "darkgreen", lwd = 3)
  #Plot the baseline flow brk as a dashed line via lty = 3
  lines(flowDataAll$timestamp, flowDataAll$baselineQ,lty = 3,lwd = 3)
  # lines(flowDataAll$timestamp,flowDataAll$prism_p_cfs,col = "blue",lty = 2)
  # lines(flowDataAll$timestamp,flowDataAll$daymet_p_cfs,col = "red",lty = 2)
  # lines(flowDataAll$timestamp,flowDataAll$nldas2_p_cfs,col = "orange",lty = 2)
  lines(stormi$timestamp,stormi$flow,lwd = 4)
  lines(stormi$timestamp,stormi$baseflow,lwd = 4,col = "darkgreen")
  
  #Put a small legend on the plot
  legend("topleft",c("Flow","Baseflow","Baseline","PRISM","daymet","NLDAS2"),
         col = c("black","darkgreen","black",
                 PRISMCol, daymetCol, NLDASCol),
         lty = c(1,1,3,NA,NA,NA),
         pch = c(NA,NA,NA,15,15,15),
         cex = 2,
         bty = "n")
  #Close the plot PNG and output the file
  dev.off()
}

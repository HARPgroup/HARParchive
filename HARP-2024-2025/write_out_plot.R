


plotBin <- R6Class(
  "plotBin", 
  public = list(
    plot = NULL, data=list(), atts=list(),
    initialize = function(plot = NULL, data = list()){ 
      self.plot = plot; self.data=data; 
    }
  )
)



mon_lm <- function(read_data, y_var, x_var, mo_var, data_name){
  sample_data <- read.csv(read_data)
  plot_out <- plotBin$new(data = sample_data)
  nwd_stats <- data.frame(row.names=c('month', 'rsquared_a'))
  for (i in 1:12) {
    mo_data=sample_data[which((sample_data[,mo_var] == i)),]
    weekmo_data <- lm(mo_data[,y_var] ~ mo_data[,x_var])
    dsum <- summary(weekmo_data)
    nwd_stats <- rbind(nwd_stats, data.frame(i, dsum$adj.r.squared))
  }
  plot_out$atts[['stats']] <- nwd_stats
  barplot(
    nwd_stats$dsum.adj.r.squared ~ nwd_stats$i,
    ylim=c(0,1.0),
    main=paste("lm(Q ~ P), monthly,", data_name)
  )
  plot_out$plot <- recordPlot()
  return(plot_out)
}

write_out_stats <- function(plot_out, write_file){
  
  write.csv(plot_out$atts, write_file)
}

write_out_plot <- function(plot_out, write_file){
  png(filename=write_file)
  plot_out$plot
  dev.off()
}

write_out_stats(mon_lm("~/HarpData/HARParchive/HARP-2024-2025/attempt.csv", "precip_cfs", "usgs_cfs", "mo", "PRISM"), 
                write_file = "~/HarpData/HARParchive/HARP-2024-2025/stats.csv")





# across all months of the year
# do all months and assemble a barplot of R^2
plotBin <- R6Class(
  "plotBin", 
  public = list(
    plot = NULL, data=list(), atts=list(), r_col='',
    initialize = function(plot = NULL, data = list()){ 
      self.plot = plot; self.data=data; 
    }
  )
)
# Week
mon_lm_stats <- function(sample_data, y_var, x_var, mo_var){
  plot_out <- plotBin$new(data = sample_data)
  plot_out$atts$lms <- list()
  nwd_stats <- data.frame(row.names=c('month', 'rsquared_a'))
  for (i in 1:12) {
    mo_data=sample_data[which((sample_data[,mo_var] == i)),]
    weekmo_data <- lm(mo_data[,y_var] ~ mo_data[,x_var])
    plot_out$atts$lms[[i]] <- weekmo_data
    dsum <- summary(weekmo_data)
    print("test")
    nwd_stats <- rbind(nwd_stats, data.frame(i, dsum$adj.r.squared))
  }
      print("test")
     plot_out$atts$stats <- nwd_stats
     plot_out$r_col <- paste0('r_squared')
     names(plot_out$atts$stats) <- c('mo', plot_out$r_col)
     return(plot_out)
}

mon_lm_plot <- function(stats,data_name,label_name){
  bp <- barplot(
    stats$r_squared ~ stats$mo,
    ylim=c(0,1.0),
    main=paste("lm(Q ~ P), monthly,",data_name,label_name))
}
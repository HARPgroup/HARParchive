# across all months of the year
# do all months and assemble a barplot of R^2
plotBin <- R6Class(
  "plotBin", 
  public = list(
    data=list(), atts=list(), r_col='',
    initialize = function(data = list(), data_is_json = FALSE){ 
      if (!data_is_json) {
        self.data=data; 
      } else {
        self$fromJSON(data)
      }
      
    },
    asJSON = function() {
      self.toJSON()
    },
    toJSON = function() {
      json_out <- jsonlite::serializeJSON(self)
      json_out[['data']] <- jsonlite::serializeJSON(self$data, pretty=TRUE)
      json_out[['atts']] <- jsonlite::serializeJSON(self$atts, pretty=TRUE)
      json_out[['r_col']] <- jsonlite::serializeJSON(self$r_col, pretty=TRUE)
      return(json_out)
    },
    fromJSON = function(json_out) {
      self$data <- jsonlite::unserializeJSON(json_out[['data']])
      self$atts <- jsonlite::unserializeJSON(json_out[['atts']])
      self$r_col <- jsonlite::unserializeJSON(json_out[['r_col']])
    }
  )
)



# Week
#This takes in sample data, y_var, x_var, and mo_var and outputs an environment of
#lm stats, residuals, and our r_squared stats we use
mon_lm_stats <- function(sample_data, y_var, x_var, mo_var){
  plot_out <- plotBin$new(data = sample_data)
  plot_out$atts$lms <- list()
  nwd_stats <- data.frame(row.names=c('month', 'rsquared_a'))
  for (i in 1:12) {
    mo_data=sample_data[which((sample_data[,mo_var] == i)),]
    if(nrow(mo_data) >= 3){
      weekmo_data <- lm(mo_data[,y_var] ~ mo_data[,x_var])
      plot_out$atts$lms[[i]] <- weekmo_data
      dsum <- summary(weekmo_data)
      nwd_stats <- rbind(nwd_stats, data.frame(month = i, rsquared_a = dsum$adj.r.squared))
    }else{
      print(paste0("Less than 3 data points for month ",i,". Cannot conduct linear regression. Skipping."))
      plot_out$atts$lms[[i]] <- NA
      nwd_stats <- rbind(nwd_stats, data.frame(month = i, rsquared_a = NA))
    }
  }
  plot_out$atts$stats <- nwd_stats
  names(plot_out$atts$stats) <- c('mo', 'r_squared')
  return(plot_out)
}

#Takes in the stats that are output from mon_lm_stats and uses them to generate out barplots
#This also uses data_name and label_name in order to put them on the plot
#Generally we use the precipitation dataset and gageide as a way to generally show these plots
mon_lm_plot <- function(stats,data_name,label_name){
  bp <- barplot(
    stats$r_squared ~ stats$mo,
    ylim=c(0,1.0),
    main=paste("lm(Q ~ P), monthly,",data_name,label_name))
}


# Week
mon_lm <- function(sample_data, y_var, x_var, mo_var, data_name, label_name){
  # deprecated.  
  message("Notice: mon_lm() is deprecated, and now returns only the output pf mon_lm_stats().  PLots can be done by calling mon_lm_plot()")
  return(mon_lm_stats(sample_data, y_var, x_var, mo_var))
}
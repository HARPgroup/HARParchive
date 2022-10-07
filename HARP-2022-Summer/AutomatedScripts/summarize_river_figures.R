### This is copied from waterSupplyModelNode.R and is all the code needed for plotting figures
### and pushing them to VAHydro





message("Plotting critical flow periods")
# does this have an active impoundment sub-comp
if (imp_off == 0) {
  
  if("impoundment" %in% cols) {
    # Plot and analyze impoundment sub-comps
    dat$storage_pct <- dat$impoundment_use_remain_mg * 3.07 / dat$impoundment_max_usable
    #
    storage_pct <- mean(as.numeric(dat$storage_pct) )
    if (is.na(storage_pct)) {
      usable_pct_p0 <- 0
      usable_pct_p10 <- 0
      usable_pct_p50 <- 0
    } else {
      usable_pcts = quantile(as.numeric(dat$storage_pct), c(0,0.1,0.5) )
      usable_pct_p0 <- usable_pcts["0%"]
      usable_pct_p10 <- usable_pcts["10%"]
      usable_pct_p50 <- usable_pcts["50%"]
    }
    impoundment_days_remaining <- mean(as.numeric(dat$impoundment_days_remaining) )
    if (is.na(impoundment_days_remaining)) {
      remaining_days_p0 <- 0
      remaining_days_p10 <- 0
      remaining_days_p50 <- 0
    } else {
      remaining_days = quantile(as.numeric(dat$impoundment_days_remaining), c(0,0.1,0.5) )
      remaining_days_p0 <- remaining_days["0%"]
      remaining_days_p10 <- remaining_days["10%"]
      remaining_days_p50 <- remaining_days["50%"]
    }
    
    # post em up
    vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'usable_pct_p0', usable_pct_p0, ds)
    vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'usable_pct_p10', usable_pct_p10, ds)
    vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'usable_pct_p50', usable_pct_p50, ds)
    
    vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'remaining_days_p0', remaining_days_p0, ds)
    vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'remaining_days_p10', remaining_days_p10, ds)
    vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'remaining_days_p50', remaining_days_p50, ds)
    
    # this has an impoundment.  Plot it up.
    # Now zoom in on critical drought period
    pdstart = as.Date(paste0(l90_year,"-06-01") )
    pdend = as.Date(paste0(l90_year, "-11-15") )
    datpd <- window(
      dat,
      start = pdstart,
      end = pdend
    );
    fname <- paste(
      save_directory,
      paste0(
        'l90_imp_storage.',
        elid, '.', runid, '.png'
      ),
      sep = '/'
    )
    furl <- paste(
      save_url,
      paste0(
        'l90_imp_storage.',
        elid, '.', runid, '.png'
      ),
      sep = '/'
    )
    png(fname)
    ymn <- 1
    ymx <- 100
    par(mar = c(8.8,5,0.5,5))
    plot(
      datpd$storage_pct * 100.0,
      ylim=c(ymn,ymx),
      ylab="Reservoir Storage (%)",
      xlab=paste("Lowest 90 Day Flow Period",pdstart,"to",pdend),
      legend=c('Storage', 'Qin', 'Qout', 'Demand (mgd)')
    )
    par(new = TRUE)
    plot(datpd$impoundment_Qin,col='blue', axes=FALSE, xlab="", ylab="")
    lines(datpd$impoundment_Qout,col='green')
    lines(datpd$impoundment_demand * 1.547,col='red')
    axis(side = 4)
    mtext(side = 4, line = 3, 'Flow/Demand (cfs)')
    legend("bottom",inset=-0.36, xpd=TRUE, c("Reservoir Storage","Inflow","Outflow","Demand"),
           col = c("black", "blue", "green","red"),
           lty = c(1,1,1,1),
           bg='white',cex=0.8) #ADD LEGEND
    dev.off()
    print(paste("Saved file: ", fname, "with URL", furl))
    vahydro_post_metric_to_scenprop(scenprop$pid, 'dh_image_file', furl, 'fig.l90_imp_storage', 0.0, ds)
    
    # l90 2 year
    # this has an impoundment.  Plot it up.
    # Now zoom in on critical drought period
    pdstart = as.Date(paste0( (as.integer(l90_year) - 1),"-01-01") )
    pdend = as.Date(paste0(l90_year, "-12-31") )
    datpd <- window(
      dat,
      start = pdstart,
      end = pdend
    );
    fname <- paste(
      save_directory,
      paste0(
        'l90_imp_storage.2yr.',
        elid, '.', runid, '.png'
      ),
      sep = '/'
    )
    furl <- paste(
      save_url,
      paste0(
        'l90_imp_storage.2yr.',
        elid, '.', runid, '.png'
      ),
      sep = '/'
    )
    png(fname)
    ymn <- 1
    ymx <- 100
    par(mar = c(8.8,5,0.5,5))
    plot(
      datpd$storage_pct * 100.0,
      ylim=c(ymn,ymx),
      ylab="Reservoir Storage (%)",
      xlab=paste("Lowest 90 Day Flow Period",pdstart,"to",pdend)
    )
    par(new = TRUE)
    plot(datpd$impoundment_Qin,col='blue', axes=FALSE, xlab="", ylab="")
    lines(datpd$impoundment_Qout,col='green')
    lines(datpd$wd_mgd * 1.547,col='red')
    axis(side = 4)
    mtext(side = 4, line = 3, 'Flow/Demand (cfs)')
    legend("bottom",inset=-0.36, xpd=TRUE, c("Reservoir Storage","Inflow","Outflow","Demand"),
           col = c("black", "blue", "green","red"),
           lty = c(1,1,1,1),
           bg='white',cex=0.8) #ADD LEGEND
    dev.off()
    print(paste("Saved file: ", fname, "with URL", furl))
    vahydro_post_metric_to_scenprop(scenprop$pid, 'dh_image_file', furl, 'fig.l90_imp_storage.2yr', 0.0, ds)
    
    # All Periods
    # this has an impoundment.  Plot it up.
    
    # Full period Flow duration curve
    datpd <- dat
    fname <- paste(
      save_directory,
      paste0(
        'fig.fdc.all.',
        elid, '.', runid, '.png'
      ),
      sep = '/'
    )
    furl <- paste(
      save_url,
      paste0(
        'fig.fdc.all.',
        elid, '.', runid, '.png'
      ),
      sep = '/'
    )
    png(fname)
    hydroTSM::fdc(cbind(datpd$impoundment_Qin, datpd$impoundment_Qout),ylab="Q (cfs)")
    dev.off()
    print(paste("Saved file: ", fname, "with URL", furl))
    vahydro_post_metric_to_scenprop(scenprop$pid, 'dh_image_file', furl, 'fig.fdc.all.', 0.0, ds)
    
    
    # Full period inflow/outflow, res level
    fname <- paste(
      save_directory,
      paste0(
        'fig.imp_storage.all.',
        elid, '.', runid, '.png'
      ),
      sep = '/'
    )
    furl <- paste(
      save_url,
      paste0(
        'fig.imp_storage.all.',
        elid, '.', runid, '.png'
      ),
      sep = '/'
    )
    png(fname)
    ymn <- 1
    ymx <- 100
    par(mar = c(8.8,5,0.5,5))
    plot(
      datpd$storage_pct * 100.0,
      ylim=c(ymn,ymx),
      ylab="Reservoir Storage (%)",
      xlab=paste("Storage and Flows",sdate,"to",edate)
    )
    par(new = TRUE)
    plot(datpd$impoundment_Qin,col='blue', axes=FALSE, xlab="", ylab="")
    lines(datpd$impoundment_Qout,col='green')
    lines(datpd$wd_mgd * 1.547,col='red')
    axis(side = 4)
    mtext(side = 4, line = 3, 'Flow/Demand (cfs)')
    legend("bottom",inset=-0.36, xpd=TRUE, c("Reservoir Storage","Inflow","Outflow","Demand"),
           col = c("black", "blue", "green","red"),
           lty = c(1,1,1,1),
           bg='white',cex=0.8) #ADD LEGEND
    dev.off()
    print(paste("Saved file: ", fname, "with URL", furl))
    vahydro_post_metric_to_scenprop(scenprop$pid, 'dh_image_file', furl, 'fig.imp_storage.all', 0.0, ds)
    
    # Low Elevation Period
    # Dat for Critical Period
    elevs <- zoo(dat$storage_pct, order.by = index(dat));
    loelevs <- group2(elevs, flow_year_type);
    l90 <- loelevs["90 Day Min"];
    ndx = which.min(as.numeric(l90[,"90 Day Min"]));
    l90_elev = round(loelevs[ndx,]$"90 Day Min",6);
    l90_elevyear = loelevs[ndx,]$"year";
    l90_elev_start = as.Date(paste0(l90_elevyear - 2,"-01-01"))
    l90_elev_end = as.Date(paste0(l90_elevyear,"-12-31"))
    elevdatpd <- window(
      dat,
      start = l90_elev_start,
      end = l90_elev_end
    );
    datpd <- elevdatpd
    fname <- paste(
      save_directory,
      paste0(
        'elev90_imp_storage.all.',
        elid, '.', runid, '.png'
      ),
      sep = '/'
    )
    furl <- paste(
      save_url,
      paste0(
        'elev90_imp_storage.all.',
        elid, '.', runid, '.png'
      ),
      sep = '/'
    )
    png(fname)
    ymn <- 1
    ymx <- 100
    par(mar = c(8.8,5,01,5))
    plot(
      datpd$storage_pct * 100.0,cex.main=1,
      ylim=c(ymn,ymx),
      main="Minimum Modeled Reservoir Storage Period",
      ylab="Reservoir Storage (%)",
      xlab=paste("Model Time Period",l90_elev_start,"to",l90_elev_end)
    )
    par(new = TRUE)
    plot(datpd$impoundment_Qin,col='blue', axes=FALSE, xlab="", ylab="")
    lines(datpd$Qout,col='green')
    lines(datpd$wd_mgd * 1.547,col='red')
    axis(side = 4)
    mtext(side = 4, line = 3, 'Flow/Demand (cfs)')
    legend("bottom",inset=-0.36, xpd=TRUE, c("Reservoir Storage","Inflow","Outflow","Demand"),
           col = c("black", "blue", "green","red"),
           lty = c(1,1,1,1),
           bg='white',cex=0.8) #ADD LEGEND
    dev.off()
    print(paste("Saved file: ", fname, "with URL", furl))
    vahydro_post_metric_to_scenprop(scenprop$pid, 'dh_image_file', furl, 'elev90_imp_storage.all', 0.0, ds)
    
  }
} else {
  
  
  # plot Qin, Qout of mainstem, and wd_mgd, and wd_cumulative_mgd
  # TBD
  # l90 2 year
  # this has an impoundment.  Plot it up.
  # Now zoom in on critical drought period
  pdstart = as.Date(paste0(l90_year,"-06-01") )
  pdend = as.Date(paste0(l90_year, "-11-15") )
  datpd <- window(
    dat,
    start = pdstart,
    end = pdend
  );
  fname <- paste(
    save_directory,
    paste0(
      'l90_flows.2yr.',
      elid, '.', runid, '.png'
    ),
    sep = '/'
  )
  furl <- paste(
    save_url,
    paste0(
      'l90_flows.2yr.',
      elid, '.', runid, '.png'
    ),
    sep = '/'
  )
  png(fname)
  # Because these are zoo timeseries, they will throw an error if you use a normal DF
  # max() syntax which is OK with max(c(df1, df2))
  # instead, we cbind them instead of the default which is an implicit rbind
  # ymx <- max(datpd$Qbaseline, datpd$Qout)
  ymx <- max(cbind(datpd$Qbaseline, datpd$Qout))
  plot(
    datpd$Qbaseline, ylim = c(0,ymx),
    ylab="Flow/WD/PS (cfs)",
    xlab=paste("Lowest 90 Day Flow Period",pdstart,"to",pdend)
  )
  lines(datpd$Qout,col='blue')
  par(new = TRUE)
  # Because these are zoo timeseries, they will throw an error if you use a normal DF
  # max() syntax which is OK with max(c(df1, df2))
  # instead, we cbind them instead of the default which is an implicit rbind
  # ymx <- max(cbind(datpd$wd_cumulative_mgd * 1.547, datpd$ps_cumulative_mgd * 1.547))
  ymx <- max(cbind(datpd$wd_cumulative_mgd * 1.547, datpd$ps_cumulative_mgd * 1.547))
  plot(
    datpd$wd_cumulative_mgd * 1.547,col='red',
    axes=FALSE, xlab="", ylab="", ylim=c(0,ymx)
  )
  lines(datpd$ps_cumulative_mgd * 1.547,col='green')
  axis(side = 4)
  mtext(side = 4, line = 3, 'Flow/Demand (cfs)')
  dev.off()
  print(paste("Saved file: ", fname, "with URL", furl))
  vahydro_post_metric_to_scenprop(scenprop$pid, 'dh_image_file', furl, 'fig.l90_flows.2yr', 0.0, ds)
  
  datpd <- dat
  fname <- paste(
    save_directory,
    paste0(
      'flows.all.',
      elid, '.', runid, '.png'
    ),
    sep = '/'
  )
  furl <- paste(
    save_url,
    paste0(
      'flows.all.',
      elid, '.', runid, '.png'
    ),
    sep = '/'
  )
  png(fname)
  ymx <- max(cbind(max(datpd$Qbaseline), max(datpd$Qout)))
  plot(
    datpd$Qbaseline, ylim = c(0,ymx),
    ylab="Flow/WD/PS (cfs)",
    xlab=paste("Model Flow Period",sdate,"to",edate)
  )
  lines(datpd$Qout,col='blue')
  par(new = TRUE)
  ymx <- max(cbind(datpd$wd_cumulative_mgd * 1.547, datpd$ps_cumulative_mgd * 1.547))
  plot(
    datpd$wd_cumulative_mgd * 1.547,col='red',
    axes=FALSE, xlab="", ylab="", ylim=c(0,ymx)
  )
  lines(datpd$ps_cumulative_mgd * 1.547,col='green')
  axis(side = 4)
  mtext(side = 4, line = 3, 'Flow/Demand (cfs)')
  dev.off()
  print(paste("Saved file: ", fname, "with URL", furl))
  vahydro_post_metric_to_scenprop(scenprop$pid, 'dh_image_file', furl, 'fig.flows.all', 0.0, ds)
  
}


###############################################
# RSEG FDC
###############################################
base_var <- "Qbaseline" #BASE VARIABLE USED IN FDCs AND HYDROGRAPHS
comp_var <- "Qout" #VARIABLE TO COMPARE AGAINST BASE VARIABLE, DEFAULT Qout

# FOR TESTING 
# save_directory <- 'C:/Users/nrf46657/Desktop/GitHub/om/R/summarize'
datpd <- datdf
fname <- paste(
  save_directory,
  paste0(
    'fdc.',
    elid, '.', runid, '.png'
  ),
  sep = '/'
)
# FOR TESTING 
# save_url <- save_directory
furl <- paste(
  save_url,
  paste0(
    'fdc.',
    elid, '.', runid, '.png'
  ),
  sep = '/'
)


png(fname, width = 700, height = 700)
legend_text = c("Baseline Flow","Scenario Flow")
fdc_plot <- hydroTSM::fdc(
  cbind(datpd[names(datpd)== base_var], datpd[names(datpd)== comp_var]),
  # yat = c(0.10,1,5,10,25,100,400),
  # yat = c(round(min(datpd),0),500,1000,5000,10000),
  yat = seq(round(min(datpd),0),round(max(datpd),0), by = 500),
  leg.txt = legend_text,
  main=paste("Flow Duration Curve","\n","(Model Flow Period ",sdate," to ",edate,")",sep=""),
  ylab = "Flow (cfs)",
  # ylim=c(1.0, 5000),
  ylim=c(min(datpd), max(datpd)),
  cex.main=1.75,
  cex.axis=1.50,
  leg.cex=2,
  cex.sub = 1.2
)
dev.off()

print(paste("Saved file: ", fname, "with URL", furl))
vahydro_post_metric_to_scenprop(scenprop$pid, 'dh_image_file', furl, 'fig.fdc', 0.0, ds)
###############################################
###############################################


###############################################
# RSEG Hydrograph (Drought Period)
###############################################
# Zoom in on critical drought period
pdstart = as.Date(paste0(l90_year,"-06-01") )
pdend = as.Date(paste0(l90_year, "-11-15") )
datpd <- window(
  dat,
  start = pdstart,
  end = pdend
);
datpd <- data.frame(datpd)
datpd$date <- rownames(datpd)

fname <- paste(
  save_directory,
  paste0(
    'hydrograph_dry.',
    elid, '.', runid, '.png'
  ),
  sep = '/'
)
furl <- paste(
  save_url,
  paste0(
    'hydrograph_dry.',
    elid, '.', runid, '.png'
  ),
  sep = '/'
)

png(fname, width = 900, height = 700)
legend_text = c("Baseline Flow","Scenario Flow")
xmn <- as.Date(pdstart)
xmx <- as.Date(pdend)
ymn <- 0
#ymx <- 1000
ymx <- max(cbind(as.numeric(unlist(datpd[names(datpd)== base_var])),
                 as.numeric(unlist(datpd[names(datpd)== comp_var]))))
par(mar = c(5,5,2,5))
hydrograph_dry <- plot(as.numeric(unlist(datpd[names(datpd)== base_var]))~as.Date(datpd$date),
                       type = "l", lty=2, lwd = 1,ylim=c(ymn,ymx),xlim=c(xmn,xmx),
                       ylab="Flow (cfs)",xlab=paste("Lowest 90 Day Flow Period",pdstart,"to",pdend),
                       main = "Hydrograph: Dry Period",
                       cex.main=1.75,
                       cex.axis=1.50,
                       cex.lab=1.50
)
par(new = TRUE)
plot(as.numeric(unlist(datpd[names(datpd)== comp_var]))~as.Date(datpd$date),
     type = "l",col='brown3', lwd = 2, 
     axes=FALSE,ylim=c(ymn,ymx),xlim=c(xmn,xmx),ylab="",xlab="")
legend("topright",legend=legend_text,col=c("black","brown3"), 
       lty=c(2,1), lwd=c(1,2), cex=1.5)
dev.off()

print(paste("Saved file: ", fname, "with URL", furl))
vahydro_post_metric_to_scenprop(scenprop$pid, 'dh_image_file', furl, 'fig.hydrograph_dry', 0.0, ds)
###############################################
###############################################


###############################################
# RSEG ELFGEN
###############################################
#GET RSEG HYDROID FROM RSEG MODEL PID
#rseg <-getProperty(list(pid=pid), site)
rseg <- RomProperty$new( ds, list(pid=pid), TRUE)
rseg_hydroid<-rseg$featureid

huc_level <- 'huc8'
dataset <- 'VAHydro-EDAS'

elfgen_huc(runid, rseg_hydroid, huc_level, dataset, scenprop, ds, save_directory, save_url, site)
    
# # From hsp_hydr_analysis.R:
#     
# #Exporting graph of Qout to Vahydro
# save_url = paste(omsite, '/', path_string_m2, sep ='')
# fname <- paste(
#   image_directory_path,paste0( river_segment_name, '.','fig.Qout','.png'), # building file name
#   sep = '/'
# )
# furl <- paste(
#   save_url,paste0( river_segment_name,'.','fig.Qout', '.png'),
#   sep = '/'
# )
# png(fname) 
# plot(monthlyQout$Qout, type = 'l', col = 'blue', ylab = 'Qout (cfs)', xaxt = 'n', xlab = NA,)
# title(main = 'Outflow from the River Segment', sub = 'Monthly average values are plotted')
# axis(1, at = seq(0,len_Qmon,12), labels = years)
# dev.off()
# print(paste("Saved file: ", fname, "with URL", furl))
# model_graph1 <- RomProperty$new(
#   ds, list(
#     varkey="dh_image_file",
#     featureid=model_scenario$pid,
#     entity_type='dh_properties',
#     propcode = furl,
#     propname = 'fig.Qout'
#   ),
#   TRUE
# )
# model_graph1$save(TRUE)

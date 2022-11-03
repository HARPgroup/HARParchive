### This is copied from waterSupplyModelNode.R and is all the code needed for plotting figures
### and pushing them to VAHydro

basepath='/var/www/R';
source("/var/www/R/config.R")
#save_directory <- "/var/www/html/data/proj3/out" 
# ^want this to correspond to our image directory path

suppressPackageStartupMessages(library(data.table)) 
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(zoo))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(caTools))
suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(IHA))
suppressPackageStartupMessages(library(PearsonDS))
#suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(R.utils))
suppressPackageStartupMessages(library(hydroTSM))
# establishing location on server for storing images
omsite = "http://deq1.bse.vt.edu:81"


# setwd("/Users/glenncampagna/Desktop/HARPteam22/Data") # for testing only 
# setwd("/Users/VT_SA/Documents/HARP") # for testing only
# hydr <- fread("OR1_7700_7980_hydr.csv")
# river_seg <- 'OR1_7700_7980'
# hydr_file_path <- '/media/model/p532/out/river/hsp2_2022/hydr/OR1_7700_7980_hydr.csv'
# scenario_name <- 'hsp_2022'
# model_version <- 'cbp-5.3.2'
# source("/Users/VT_SA/Documents/GitHUB/HARParchive/HARP-2022-Summer/AutomatedScripts/summarize_river_values.R") #testing

# Accepting command arguments:
argst <- commandArgs(trailingOnly = T)
river_seg <- argst[1]
scenario_name <- argst[2]
hydr_file_path <- argst[3]
model_version <- argst[4]
image_dir <- argst[5]
#image_dir <- '/media/model/p532/out/river/hsp2_2022/images' #for testing

split <- strsplit(image_dir, split = "/")
path_list_m2 <- as.list(split[[1]][-c(1,2,3)])
path_string_m2 <- paste(path_list_m2, collapse = "/")
save_url <- paste0('http://deq1.bse.vt.edu:81/', path_string_m2)

hydr <- fread("hydr_file_path")
hydr <- as.data.frame(hydr)
source("summarize_river_values.R")
  #need to run the source above
  #have to check if it will find the Rscript in HARParhive automatically or not
  #NOTE: when making changes to the function, have to run the source again to activate the changes
hydr <- summarize_river_values(hydr)

#setting the values out of the list
l90_Qout <- hydr[[2]][["l90_Qout"]]
l90_year <- hydr[[2]][["l90_year"]]
l30_Qout <- hydr[[2]][["l30_Qout"]]
l30_year <- hydr[[2]][["l30_Qout"]]
imp_off <- hydr[[2]][["imp_off"]]
net_consumption_mgd <- hydr[[2]][["net_consumption_mgd"]]
unmet_demand_mgd <- hydr[[2]][["unmet_demand_mgd"]]

#transforming hydr to a data frame again
hydr <- as.data.frame(hydr[[1]])


# create a place to save an image if it does not exist
# note: we do NOT create a path for the hydr_file because it MUST exist, otherwise,
#       we would have nothing to analyze
if (!file.exists(image_dir)) {
  dir.create(file.path(image_dir)) #creates directory if one does not yet exists
}


### Exporting to VAHydro
## Set up currently to output all the Qout values & the Qout

# Set up our hydra source
ds <- RomDataSource$new(site, rest_uname = rest_uname)
ds$get_token(rest_pw)

rseg_name=river_seg
rseg_ftype='vahydro'

riverseg<- RomFeature$new(
  ds,
  list(
    hydrocode=paste('vahydrosw_wshed_',rseg_name, sep = ''),
    ftype=rseg_ftype,
    bundle='watershed'
  ),
  TRUE
)

model <- RomProperty$new(
  ds,
  list(
    varkey="om_model_element", 
    propname=riverseg$name,
    featureid=riverseg$hydroid, 
    entity_type="dh_feature", 
    propcode=model_version
  ), 
  TRUE
)
model$save(TRUE)

model_scenario <- RomProperty$new( 
  ds,
  list(
    varkey="om_scenario", 
    featureid=model$pid, 
    entity_type="dh_properties", 
    propname=scenario_name,
    propcode=scenario_name
  ), 
  TRUE
)
model_scenario$save(TRUE)


# Uploading constants to VaHydro:
# entity-type specifies what we are attaching the constant to 
# Edit to more compact version???

model_constant_hydr_path <- RomProperty$new(
  ds, list(
    varkey="om_class_textField", 
    featureid=model_scenario$pid,
    entity_type='dh_properties',
    propname = 'hydr_file_path'
  ),
  TRUE
)
model_constant_hydr_path$propcode <- as.character(input_file_path)
model_constant_hydr_path$save(TRUE)


message("Plotting critical flow periods")
# does this have an active impoundment sub-comp
if (imp_off == 0) {
  
  if("impoundment" %in% cols) {
    # Plot and analyze impoundment sub-comps
    hydr$storage_pct <- hydr$impoundment_use_remain_mg * 3.07 / hydr$impoundment_max_usable
    #
    storage_pct <- mean(as.numeric(hydr$storage_pct) )
    if (is.na(storage_pct)) {
      usable_pct_p0 <- 0
      usable_pct_p10 <- 0
      usable_pct_p50 <- 0
    } else {
      usable_pcts = quantile(as.numeric(hydr$storage_pct), c(0,0.1,0.5) )
      usable_pct_p0 <- usable_pcts["0%"]
      usable_pct_p10 <- usable_pcts["10%"]
      usable_pct_p50 <- usable_pcts["50%"]
    }
    impoundment_days_remaining <- mean(as.numeric(hydr$impoundment_days_remaining) )
    if (is.na(impoundment_days_remaining)) {
      remaining_days_p0 <- 0
      remaining_days_p10 <- 0
      remaining_days_p50 <- 0
    } else {
      remaining_days = quantile(as.numeric(hydr$impoundment_days_remaining), c(0,0.1,0.5) )
      remaining_days_p0 <- remaining_days["0%"]
      remaining_days_p10 <- remaining_days["10%"]
      remaining_days_p50 <- remaining_days["50%"]
    }
    
    # post em up
    # vahydro_post_metric_to_scenprop(model_scenario$pid, 'om_class_Constant', NULL, 'usable_pct_p0', usable_pct_p0, ds)
    # vahydro_post_metric_to_scenprop(model_scenario$pid, 'om_class_Constant', NULL, 'usable_pct_p10', usable_pct_p10, ds)
    # vahydro_post_metric_to_scenprop(model_scenario$pid, 'om_class_Constant', NULL, 'usable_pct_p50', usable_pct_p50, ds)
    # 
    # vahydro_post_metric_to_scenprop(model_scenario$pid, 'om_class_Constant', NULL, 'remaining_days_p0', remaining_days_p0, ds)
    # vahydro_post_metric_to_scenprop(model_scenario$pid, 'om_class_Constant', NULL, 'remaining_days_p10', remaining_days_p10, ds)
    # vahydro_post_metric_to_scenprop(model_scenario$pid, 'om_class_Constant', NULL, 'remaining_days_p50', remaining_days_p50, ds)
    # 
    # this has an impoundment.  Plot it up.
    # Now zoom in on critical drought period
    pdstart = as.Date(paste0(l90_year,"-06-01") )
    pdend = as.Date(paste0(l90_year, "-11-15") )

    hydrpd <- window(
      hydr,
      start = pdstart,
      end = pdend
    );                   # setting the fname and furl for pushing graphs to vahydro
    fname <- paste(
      image_dir,
      paste0(
        'l90_imp_storage.',
        river_seg, '.', scenario_name, '.png'
      ),
      sep = '/'
    )
    furl <- paste(
      save_url,
      paste0(
        'l90_imp_storage.',
        river_seg, '.', scenario_name, '.png'
      ),
      sep = '/'
    )
    png(fname)
    ymn <- 1
    ymx <- 100
    par(mar = c(8.8,5,0.5,5))
    plot(
      hydrpd$storage_pct * 100.0,
      ylim=c(ymn,ymx),
      ylab="Reservoir Storage (%)",
      xlab=paste("Lowest 90 Day Flow Period",pdstart,"to",pdend),
      legend=c('Storage', 'Qin', 'Qout', 'Demand (mgd)')
    )
    par(new = TRUE)
    plot(hydrpd$impoundment_Qin,col='blue', axes=FALSE, xlab="", ylab="")
    lines(hydrpd$impoundment_Qout,col='green')
    lines(hydrpd$impoundment_demand * 1.547,col='red')
    axis(side = 4)
    mtext(side = 4, line = 3, 'Flow/Demand (cfs)')
    legend("bottom",inset=-0.36, xpd=TRUE, c("Reservoir Storage","Inflow","Outflow","Demand"),
           col = c("black", "blue", "green","red"),
           lty = c(1,1,1,1),
           bg='white',cex=0.8) #ADD LEGEND
    dev.off()
    print(paste("Saved file: ", fname, "with URL", furl))
    # vahydro_post_metric_to_scenprop(model_scenario$pid, 'dh_image_file', furl, 'fig.l90_imp_storage', 0.0, ds)
    
    # l90 2 year
    # this has an impoundment.  Plot it up.
    # Now zoom in on critical drought period
    pdstart = as.Date(paste0( (as.integer(l90_year) - 1),"-01-01") )
    pdend = as.Date(paste0(l90_year, "-12-31") )
    hydrpd <- window(
      hydr,
      start = pdstart,
      end = pdend
    );
    fname <- paste(
      image_dir,
      paste0(
        'l90_imp_storage.2yr.',
        river_seg, '.', scenario_name, '.png'
      ),
      sep = '/'
    )
    furl <- paste(
      save_url,
      paste0(
        'l90_imp_storage.2yr.',
        river_seg, '.', scenario_name, '.png'
      ),
      sep = '/'
    )
    png(fname)
    ymn <- 1
    ymx <- 100
    par(mar = c(8.8,5,0.5,5))
    plot(
      hydrpd$storage_pct * 100.0,
      ylim=c(ymn,ymx),
      ylab="Reservoir Storage (%)",
      xlab=paste("Lowest 90 Day Flow Period",pdstart,"to",pdend)
    )
    par(new = TRUE)
    plot(hydrpd$impoundment_Qin,col='blue', axes=FALSE, xlab="", ylab="")
    lines(hydrpd$impoundment_Qout,col='green')
    lines(hydrpd$wd_mgd * 1.547,col='red')
    axis(side = 4)
    mtext(side = 4, line = 3, 'Flow/Demand (cfs)')
    legend("bottom",inset=-0.36, xpd=TRUE, c("Reservoir Storage","Inflow","Outflow","Demand"),
           col = c("black", "blue", "green","red"),
           lty = c(1,1,1,1),
           bg='white',cex=0.8) #ADD LEGEND
    dev.off()
    print(paste("Saved file: ", fname, "with URL", furl))
    # vahydro_post_metric_to_scenprop(model_scenario$pid, 'dh_image_file', furl, 'fig.l90_imp_storage.2yr', 0.0, ds)
    
    # All Periods
    # this has an impoundment.  Plot it up.
    
    # Full period Flow duration curve
    hydrpd <- hydr
    fname <- paste(
      image_dir,
      paste0(
        'fig.fdc.all.',
        river_seg, '.', scenario_name, '.png'
      ),
      sep = '/'
    )
    furl <- paste(
      save_url,
      paste0(
        'fig.fdc.all.',
        river_seg, '.', scenario_name, '.png'
      ),
      sep = '/'
    )
    png(fname)
    hydroTSM::fdc(cbind(hydrpd$impoundment_Qin, hydrpd$impoundment_Qout),ylab="Q (cfs)")
    dev.off()
    print(paste("Saved file: ", fname, "with URL", furl))
    # vahydro_post_metric_to_scenprop(model_scenario$pid, 'dh_image_file', furl, 'fig.fdc.all.', 0.0, ds)
    
    
    # Full period inflow/outflow, res level
    fname <- paste(
      image_dir,
      paste0(
        'fig.imp_storage.all.',
        river_seg, '.', scenario_name, '.png'
      ),
      sep = '/'
    )
    furl <- paste(
      save_url,
      paste0(
        'fig.imp_storage.all.',
        river_seg, '.', scenario_name, '.png'
      ),
      sep = '/'
    )
    png(fname)
    ymn <- 1
    ymx <- 100
    par(mar = c(8.8,5,0.5,5))
    plot(
      hydrpd$storage_pct * 100.0,
      ylim=c(ymn,ymx),
      ylab="Reservoir Storage (%)",
      xlab=paste("Storage and Flows",sdate,"to",edate)
    )
    par(new = TRUE)
    plot(hydrpd$impoundment_Qin,col='blue', axes=FALSE, xlab="", ylab="")
    lines(hydrpd$impoundment_Qout,col='green')
    lines(hydrpd$wd_mgd * 1.547,col='red')
    axis(side = 4)
    mtext(side = 4, line = 3, 'Flow/Demand (cfs)')
    legend("bottom",inset=-0.36, xpd=TRUE, c("Reservoir Storage","Inflow","Outflow","Demand"),
           col = c("black", "blue", "green","red"),
           lty = c(1,1,1,1),
           bg='white',cex=0.8) #ADD LEGEND
    dev.off()
    print(paste("Saved file: ", fname, "with URL", furl))
    # vahydro_post_metric_to_scenprop(model_scenario$pid, 'dh_image_file', furl, 'fig.imp_storage.all', 0.0, ds)
    
    # Low Elevation Period
    # hydr for Critical Period
    elevs <- zoo(hydr$storage_pct, order.by = index(hydr));
    loelevs <- group2(elevs, flow_year_type);
    l90 <- loelevs["90 Day Min"];
    ndx = which.min(as.numeric(l90[,"90 Day Min"]));
    l90_elev = round(loelevs[ndx,]$"90 Day Min",6);
    l90_elevyear = loelevs[ndx,]$"year";
    l90_elev_start = as.Date(paste0(l90_elevyear - 2,"-01-01"))
    l90_elev_end = as.Date(paste0(l90_elevyear,"-12-31"))
    elevhydrpd <- window(
      hydr,
      start = l90_elev_start,
      end = l90_elev_end
    );
    hydrpd <- elevhydrpd
    fname <- paste(
      image_dir,
      paste0(
        'elev90_imp_storage.all.',
        river_seg, '.', scenario_name, '.png'
      ),
      sep = '/'
    )
    furl <- paste(
      save_url,
      paste0(
        'elev90_imp_storage.all.',
        river_seg, '.', scenario_name, '.png'
      ),
      sep = '/'
    )
    png(fname)
    ymn <- 1
    ymx <- 100
    par(mar = c(8.8,5,01,5))
    plot(
      hydrpd$storage_pct * 100.0,cex.main=1,
      ylim=c(ymn,ymx),
      main="Minimum Modeled Reservoir Storage Period",
      ylab="Reservoir Storage (%)",
      xlab=paste("Model Time Period",l90_elev_start,"to",l90_elev_end)
    )
    par(new = TRUE)
    plot(hydrpd$impoundment_Qin,col='blue', axes=FALSE, xlab="", ylab="")
    lines(hydrpd$Qout,col='green')
    lines(hydrpd$wd_mgd * 1.547,col='red')
    axis(side = 4)
    mtext(side = 4, line = 3, 'Flow/Demand (cfs)')
    legend("bottom",inset=-0.36, xpd=TRUE, c("Reservoir Storage","Inflow","Outflow","Demand"),
           col = c("black", "blue", "green","red"),
           lty = c(1,1,1,1),
           bg='white',cex=0.8) #ADD LEGEND
    dev.off()
    print(paste("Saved file: ", fname, "with URL", furl))
    # vahydro_post_metric_to_scenprop(model_scenario$pid, 'dh_image_file', furl, 'elev90_imp_storage.all', 0.0, ds)
    
  }
} else {

## End of the if-loop; this is what we will plot since imp_off is 1!!!!!!!!!!  
  
  # plot Qin, Qout of mainstem, and wd_mgd, and wd_cumulative_mgd
  # TBD
  # l90 2 year
  # this has an impoundment.  Plot it up.
  # Now zoom in on critical drought period
  pdstart = as.Date(paste0(l90_year,"-06-01") )
  pdend = as.Date(paste0(l90_year, "-11-15") )
  
  #Replaced filter()
  # hydrpd <- hydr %>% filter(date > pdstart) %>% filter(date < pdend)
  hydrpd <- with(hydr, hydr[(date >= pdstart & date <= pdend)])
  
  fname <- paste(
    image_dir,
    paste0(
      'l90_flows.2yr.',
      river_seg, '.', scenario_name, '.png'
    ),
    sep = '/'
  )
  furl <- paste(
    save_url,
    paste0(
      'l90_flows.2yr.',
      river_seg, '.', scenario_name, '.png'
    ),
    sep = '/'
  )
  png(fname)
  # Because these are zoo timeseries, they will throw an error if you use a normal DF
  # max() syntax which is OK with max(c(df1, df2))
  # instead, we cbind them instead of the default which is an implicit rbind
  # ymx <- max(hydrpd$Qbaseline, hydrpd$Qout)
  ymx <- max(cbind(hydrpd$Qbaseline, hydrpd$Qout))
  plot(
    hydrpd$Qbaseline, ylim = c(0,ymx), xlim = c(0, 4100),  #Placeholders for xlim, come back to this and create xlim based on hydrpd
    ylab="Flow/WD/PS (cfs)",
    xlab=paste("Lowest 90 Day Flow Period",pdstart,"to",pdend)
  )
  lines(hydrpd$Qout,col='blue')
  par(new = TRUE)
  # Because these are zoo timeseries, they will throw an error if you use a normal DF
  # max() syntax which is OK with max(c(df1, df2))
  # instead, we cbind them instead of the default which is an implicit rbind
  
  
  

  ymx <- max(cbind(hydrpd$wd_cumulative_mgd * 1.547, hydrpd$ps_cumulative_mgd * 1.547))
  if (ymx == 0) {
    print('No withdrawal or point source for this segment')
  } else {
    plot(
      hydrpd$wd_cumulative_mgd * 1.547,col='red',
      axes=FALSE, xlab="", ylab="", ylim=c(0,ymx)
    )
    lines(hydrpd$ps_cumulative_mgd * 1.547,col='green')
    axis(side = 4)
    mtext(side = 4, line = 3, 'Flow/Demand (cfs)')
    dev.off()
    print(paste("Saved file: ", fname, "with URL", furl))
    # vahydro_post_metric_to_scenprop(model_scenario$pid, 'dh_image_file', furl, 'fig.l90_flows.2yr', 0.0, ds)
  }
 
  hydrpd <- hydr
  fname <- paste(
    image_dir,
    paste0(
      'flows.all.',
      river_seg, '.', scenario_name, '.png'
    ),
    sep = '/'
  )
  furl <- paste(
    save_url,
    paste0(
      'flows.all.',
      river_seg, '.', scenario_name, '.png'
    ),
    sep = '/'
  )
  png(fname)
  ymx <- max(cbind(max(hydrpd$Qbaseline), max(hydrpd$Qout)))
  plot(
    hydrpd$Qbaseline, ylim = c(0,ymx),
    ylab="Flow/WD/PS (cfs)",
    xlab=paste("Model Flow Period",sdate,"to",edate)
  )
  lines(hydrpd$Qout,col='blue')
  par(new = TRUE)
  
  #Revert these changes (loop), the graphic could be expected even if 'meaningless'
  #Have message be a part of the figure (main title of plot)
  ymx <- max(cbind(hydrpd$wd_cumulative_mgd * 1.547, hydrpd$ps_cumulative_mgd * 1.547))
  if (ymx == 0) {
    print('No withdrawal or point source for this segment')
  } else {  
  plot(
    hydrpd$wd_cumulative_mgd * 1.547,col='red',
    axes=FALSE, xlab="", ylab="", ylim=c(0,ymx)
  )
  lines(hydrpd$ps_cumulative_mgd * 1.547,col='green')
  axis(side = 4)
  mtext(side = 4, line = 3, 'Flow/Demand (cfs)')
  dev.off()
  print(paste("Saved file: ", fname, "with URL", furl))
  # vahydro_post_metric_to_scenprop(model_scenario$pid, 'dh_image_file', furl, 'fig.flows.all', 0.0, ds)
  }
}


###############################################
# RSEG FDC
###############################################
base_var <- "Qbaseline" #BASE VARIABLE USED IN FDCs AND HYDROGRAPHS
comp_var <- "Qout" #VARIABLE TO COMPARE AGAINST BASE VARIABLE, DEFAULT Qout
## ^^ Currently equal so comparison is pointless
# FOR TESTING 
# save_directory <- 'C:/Users/nrf46657/Desktop/GitHub/om/R/summarize'
#hydrpd <- hydrdf
fname <- paste(
  image_dir,
  paste0(
    'fdc.',
    river_seg, '.', scenario_name, '.png'
  ),
  sep = '/'
)
# FOR TESTING 
# save_url <- save_directory
furl <- paste(
  save_url,
  paste0(
    'fdc.',
    river_seg, '.', scenario_name, '.png'
  ),
  sep = '/'
)

# Glenn and Julia tested up to here 10/21/22

#var_df <- as.data.frame(hydrpd[base_var], row.names = NULL)
#var_df$comp_var <- hydrpd[comp_var]
#colnames(var_df) <- c(base_var, comp_var)
#^This was created to replace the cbind - Glenn

png(fname, width = 700, height = 700)
legend_text = c("Baseline Flow","Scenario Flow")
#ncol_comp <- which(colnames(hydrpd)==comp_var)
#ncol_base <- which(colnames(hydrpd)==base_var)
#Temporary alternative to cbind (unsuccessful):
#df_comp <- as.data.frame(cbind(hydrpd$Qbaseline, hydrpd$Qout)) #For testing only 
fdc_plot <- hydroTSM::fdc(
  cbind(hydrpd[names(hydrpd)== base_var], hydrpd[names(hydrpd)== comp_var]), #this line is giving the first error, test with zoo
  #Otherwise, may need to summarize data first, or sqldf
  # yat = c(0.10,1,5,10,25,100,400),
  # yat = c(round(min(hydrpd),0),500,1000,5000,10000),
  yat = seq(round(min(hydrpd),0),round(max(hydrpd),0), by = 500),
  leg.txt = legend_text,
  main=paste("Flow Duration Curve","\n","(Model Flow Period ",sdate," to ",edate,")",sep=""),
  ylab = "Flow (cfs)",
  # ylim=c(1.0, 5000),
  ylim=c(min(hydrpd), max(hydrpd)),
  cex.main=1.75,
  cex.axis=1.50,
  leg.cex=2,
  cex.sub = 1.2
)
dev.off()

print(paste("Saved file: ", fname, "with URL", furl))
# vahydro_post_metric_to_scenprop(model_scenario$pid, 'dh_image_file', furl, 'fig.fdc', 0.0, ds)

# RSEG Hydrograph (Drought Period)
# Zoom in on critical drought period
pdstart = as.Date(paste0(l90_year,"-06-01") )
pdend = as.Date(paste0(l90_year, "-11-15") )

#Replace filter()
#hydrpd <- hydr %>% filter(date > pdstart) %>% filter(date < pdend)
hydrpd <- with(hydr, hydr[(date >= pdstart & date <= pdend)])

hydrpd <- data.frame(hydrpd)
#hydrpd$Date <- rownames(hydrpd)


fname <- paste(
  image_dir,
  paste0(
    'hydrograph_dry.',
    river_seg, '.', scenario_name, '.png'
  ),
  sep = '/'
)
furl <- paste(
  save_url,
  paste0(
    'hydrograph_dry.',
    river_seg, '.', scenario_name, '.png'
  ),
  sep = '/'
)

png(fname, width = 900, height = 700)
legend_text = c("Baseline Flow","Scenario Flow")
xmn <- as.Date(pdstart)
xmx <- as.Date(pdend)
ymn <- 0
#ymx <- 1000
ymx <- max(cbind(as.numeric(unlist(hydrpd[names(hydrpd)== base_var])),
                 as.numeric(unlist(hydrpd[names(hydrpd)== comp_var]))))
par(mar = c(5,5,2,5))

hydrograph_dry <- plot(as.numeric(unlist(hydrpd[names(hydrpd)== base_var]))~as.Date(hydrpd$Date),
                       type = "l", lty=2, lwd = 1,ylim=c(ymn,ymx),xlim=c(xmn,xmx),
                       ylab="Flow (cfs)",xlab=paste("Lowest 90 Day Flow Period",pdstart,"to",pdend),
                       main = "Hydrograph: Dry Period",
                       cex.main=1.75,
                       cex.axis=1.50,
                       cex.lab=1.50
)
#par(new = TRUE)
#plot(as.numeric(unlist(hydrpd[names(hydrpd)== comp_var]))~as.Date(hydrpd$Date),
#     type = "l",col='brown3', lwd = 2, 
#    axes=FALSE,ylim=c(ymn,ymx),xlim=c(xmn,xmx),ylab="",xlab="")
#legend("topright",legend=legend_text,col=c("black","brown3"), 
#       lty=c(2,1), lwd=c(1,2), cex=1.5)
#dev.off()

print(paste("Saved file: ", fname, "with URL", furl))
# vahydro_post_metric_to_scenprop(model_scenario$pid, 'dh_image_file', furl, 'fig.hydrograph_dry', 0.0, ds)
###############################################
###############################################


###############################################
# RSEG ELFGEN
###############################################
#GET RSEG HYDROID FROM RSEG MODEL PID
#rseg <-getProperty(list(pid=pid), site)
rseg <- RomProperty$new(ds, list(pid=pid), TRUE)
rseg_hydroid<-rseg$featureid

huc_level <- 'huc8'
Dataset <- 'VAHydro-EDAS'

#elfgen_huc(scenario_name, rseg_hydroid, huc_level, hydraset, scenprop, ds, image_dir, save_url, site)
    
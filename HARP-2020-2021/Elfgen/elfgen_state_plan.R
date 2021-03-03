#----------------------------------------------
site <- "http://deq2.bse.vt.edu/d.dh"    #Specify the site of interest, either d.bet OR d.dh
#----------------------------------------------
# Load Libraries
basepath='/var/www/R';

source(paste(basepath,'config.R', sep='/'))
library(stringr)
library(sqldf)
library(elfgen)

# dirs/URLs
save_directory <- "/var/www/html/data/proj3/out"
save_url <- paste(str_remove(site, 'd.dh'), "data/proj3/out", sep='');

#Add in all riv seg list


#DOWNLOAD RSEG LAYER DIRECT FROM VAHYDROlocalpath <- tempdir()
filename <- paste("vahydro_riversegs_export.csv",sep="")
destfile <- paste(localpath,filename,sep="\\")
download.file(paste(site,"/vahydro_riversegs_export",sep=""), destfile = destfile, method = "libcurl")
RSeg.csv <- read.csv(file=paste(localpath , filename,sep="\\"), header=TRUE, sep=",")
hydroid_all<-RSeg.csv$hydroid



# riv_seg <- 'PS3_5990_6161'  # TU3_9040_9180' random example for practice
runid<-11
# pid <- get.overall.vahydro.prop(riv_seg, site = site, token = token)
# 
# inputs <- list(
# hydrocode = paste0('vahydrosw_wshed_', riv_seg))
# feature <- getFeature(inputs, token, site)
# 
# hydroid<-as.character(feature$hydroid)

# Read Args
# argst <- commandArgs(trailingOnly=T)
# pid <- as.integer(argst[1])
# elid <- as.integer(argst[2])
# runid <- as.integer(argst[3])

elfgen_huc8 <- function(runid, hydroid_all){

  for(i in 1:length(hydroid_all)) {
  
  hydroid <- as.character(hydroid_all[i])
  scen.propname<-paste0('runid_', runid)
  
  # GETTING SCENARIO PROPERTY FROM VA HYDRO
  sceninfo <- list(
    varkey = 'om_scenario',
    propname = scen.propname,
    featureid = pid,
    entity_type = "dh_properties"
  )
  scenprop <- getProperty(sceninfo, site, scenprop)
  # POST PROPERTY IF IT IS NOT YET CREATED
  if (identical(scenprop, FALSE)) {
    # create
    sceninfo$pid = NULL
  } else {
    sceninfo$pid = scenprop$pid
  }
  #scenprop = postProperty(inputs=sceninfo,base_url=base_url,prop)
  scenprop <- getProperty(sceninfo, site, scenprop)
  
  # Create an algorithm that finds the outlet point for the watershed 
  
  nhdplus_views <- paste(site,'dh-feature-containing-export', hydroid, 'watershed/nhdplus/nhdp_drainage_sqmi',  sep = '/')
  nhdplus_df <- read.csv(file=nhdplus_views, header=TRUE, sep=",")
  
  hydroid_out <-sqldf("SELECT hydroid, max(propvalue)
                    FROM nhdplus_df ")
  
  inputs <- list(
    varkey = 'om_class_Constant',
    propname = 'consumptive_use_frac',
    entity_type = 'dh_properties',
    featureid = scenprop$pid
  )
  prop <- getProperty(inputs, site)
  cuf <- prop$propvalue
  
  
  inputs <- list(
    varkey = 'om_class_Constant',
    propname = 'Qbaseline',
    entity_type = 'dh_properties',
    featureid = scenprop$pid
  )
  prop <- getProperty(inputs, site)
  outlet_flow <- prop$propvalue #outlet flow as a baseline flow
  
  
  site_comparison <- paste(site,'dh-feature-contained-within-export', hydroid_out$hydroid, 'watershed', sep = '/')
  containing_watersheds <- read.csv(file=site_comparison, header=TRUE, sep=",")
  
  
  nhd_huc8_code <- sqldf("SELECT hydrocode 
               FROM containing_watersheds 
               WHERE ftype = 'nhd_huc8'")
  
  # Load the model result data for this scenario for "consumptive_use_frac" property
  
  
  # Example input parameters for retrieving data from VAHydro
  
  #HUC 8 Section---------------------------------------------------------------------------
  watershed.code <- as.character(nhd_huc8_code$hydrocode)
  watershed.bundle <- 'watershed'
  watershed.ftype <- 'nhd_huc8' #watershed.ftpe[i] when creating function
  x.metric <- 'erom_q0001e_mean'
  y.metric <- 'aqbio_nt_total'
  y.sampres <- 'species'
  datasite <- site
  
  # elfdata_vahydro() function for retrieving data from VAHydro
  watershed.df <- elfdata_vahydro(watershed.code,watershed.bundle,watershed.ftype,x.metric,y.metric,y.sampres,datasite)
  # clean_vahydro() function for cleaning data by removing any stations where the ratio of DA:Q is greater than 1000, also aggregates to the maximum richness value at each flow value
  watershed.df <- clean_vahydro(watershed.df)
  
  elf_quantile <- 0.80
  
  #breakpt <- bkpt_pwit("watershed.df" = watershed.df, "quantile" = elf_quantile, "blo" = 100, "bhi" = 1000)  
  breakpt<-530
  
  elf <- elfgen("watershed.df" = watershed.df,
                "quantile" = elf_quantile,
                "breakpt" = breakpt,
                "yaxis_thresh" = 53, 
                "xlabel" = "Mean Annual Flow (ft3/s)",
                "ylabel" = "Fish Species Richness")
  
  #Confidence Interval information
  uq <- elf$plot$plot_env$upper.quant
  
  upper.lm <- lm(y_var ~ log(x_var), data = uq)
  
  predict <- as.data.frame(predict(upper.lm, newdata = data.frame(x_var = outlet_flow), interval = 'confidence'))
  
  species_richness<-elf$stats$m*log(outlet_flow)+elf$stats$b
  
  xmin <- min(uq$x_var)
  xmax <- max(uq$x_var)
  
  yval1 <- predict(upper.lm, newdata = data.frame(x_var = xmin), interval = 'confidence')
  yval2 <- predict(upper.lm, newdata = data.frame(x_var = xmax), interval = 'confidence')
  
  ymin1 <- yval1[2] # bottom left point, line 1
  ymax1 <- yval2[3] # top right point, line 1
  
  ymin2 <- yval1[3] # top left point, line 2
  ymax2 <- yval2[2] # bottom right point, line 2
  
  m <- elf$stats$m
  b <- elf$stats$b
  int <- round((m*log(outlet_flow) + b),2)      # solving for outlet_flow y-value
  
  m1 <- (ymax1-ymin1)/(log(xmax)-log(xmin)) # line 1
  b1 <- ymax1-(m1*log(xmax))
  
  m2 <- (ymax2-ymin2)/(log(xmax)-log(xmin)) # line 2
  b2 <- ymax2 - (m2*log(xmax))
  
  # Calculating y max value based on greatest point value or intake y val
  if (int > max(watershed.df$NT.TOTAL.UNIQUE)) {
    ymax <- int + 2
  } else {
    ymax <- as.numeric(max(watershed.df$NT.TOTAL.UNIQUE)) + 2
  }
  
  
  #### Calculating median percent/absolute richness change
  
  pct_change <- richness_change(elf$stats, "pctchg" = cuf*100, "xval" = outlet_flow)
  abs_change <- richness_change(elf$stats, "pctchg" = cuf*100)
  
  #### Using confidence interval lines to find percent/absolute richness bounds
  
  elf$bound1stats$m <- m1
  elf$bound1stats$b <- b1
  
  percent_richness_change_bound1 <- round(richness_change(elf$bound1stats, "pctchg" = cuf*100, "xval" = outlet_flow),2)
  abs_richness_change_bound1 <- round(richness_change(elf$bound1stats, "pctchg" = cuf*100),2)
  
  elf$bound2stats$m <- m2
  elf$bound2stats$b <- b2
  
  percent_richness_change_bound2 <- round(richness_change(elf$bound2stats, "pctchg" = cuf*100, "xval" = outlet_flow),2)
  abs_richness_change_bound2 <- round(richness_change(elf$bound2stats, "pctchg" = cuf*100),2)
  
  # checking diffs in pct richness
  pct_d1 <- round((pct_change - percent_richness_change_bound1),2)
  pct_d2 <- round((pct_change - percent_richness_change_bound2),2)
  
  #checking diffs in abs richness
  abs_d1 <- round((abs_change - abs_richness_change_bound1),2)
  abs_d2 <- round((abs_change - abs_richness_change_bound2),2)
  
  #Scenario Property posts
  vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'elfgen_richness_change_huc8', pct_change, site, token)
  
  
  #Abs change branch
  inputs <- list(
    varkey = 'om_class_Constant',
    propname = 'elfgen_richness_change_huc8',
    entity_type = 'dh_properties',
    featureid = scenprop$pid)
  
  prop<-getProperty(inputs, site)
  
  vahydro_post_metric_to_scenprop(prop$pid, 'om_class_Constant', NULL, 'richness_change_abs', abs_change, site, token)
  
  inputs <- list(
    varkey = 'om_class_Constant',
    propname = 'richness_change_abs',
    entity_type = 'dh_properties',
    featureid = prop$pid)
  
  prop_abs<-getProperty(inputs, site)
  
  vahydro_post_metric_to_scenprop(prop_abs$pid, 'om_class_Constant', NULL, 'upper_confidence', abs_d2, site, token)
  vahydro_post_metric_to_scenprop(prop_abs$pid, 'om_class_Constant', NULL, 'lower_confidence', abs_d1, site, token)
  
  #Pct Change branch
  vahydro_post_metric_to_scenprop(prop$pid, 'om_class_Constant', NULL, 'richness_change_pct', pct_change, site, token)
  
  inputs <- list(
    varkey = 'om_class_Constant',
    propname = 'richness_change_pct',
    entity_type = 'dh_properties',
    featureid = prop$pid)
  
  prop_pct<-getProperty(inputs, site)
  
  vahydro_post_metric_to_scenprop(prop_pct$pid, 'om_class_Constant', NULL, 'upper_confidence', pct_d2, site, token)
  vahydro_post_metric_to_scenprop(prop_pct$pid, 'om_class_Constant', NULL, 'lower_confidence', pct_d1, site, token)
  
  ####Elf stats posts---------------------------
  # vahydro_post_metric_to_scenprop(prop$pid, 'om_class_Constant', NULL, 'stat_quantreg_bkpt', elf$stats$bkpt, site, token)
  # vahydro_post_metric_to_scenprop(prop$pid, 'om_class_Constant', NULL, 'stat_quantreg_qu', elf$stats$quantile, site, token)
  # vahydro_post_metric_to_scenprop(prop$pid, 'om_class_Constant', NULL, 'stat_quantreg_m', elf$stats$m, site, token)
  # vahydro_post_metric_to_scenprop(prop$pid, 'om_class_Constant', NULL, 'stat_quantreg_b', elf$stats$b, site, token)
  # vahydro_post_metric_to_scenprop(prop$pid, 'om_class_Constant', NULL, 'stat_quantreg_rsq', elf$stats$rsquared, site, token)
  # vahydro_post_metric_to_scenprop(prop$pid, 'om_class_Constant', NULL, 'stat_quantreg_adj_rsq', elf$stats$rsquared_adj, site, token)
  # vahydro_post_metric_to_scenprop(prop$pid, 'om_class_Constant', NULL, 'stat_quantreg_p', elf$stats$p, site, token)
  # vahydro_post_metric_to_scenprop(prop$pid, 'om_class_Constant', NULL, 'stat_quantreg_n_tot', elf$stats$n_total, site, token)
  # vahydro_post_metric_to_scenprop(prop$pid, 'om_class_Constant', NULL, 'stat_quantreg_n_sub', elf$stats$n_subset, site, token)
  # vahydro_post_metric_to_scenprop(prop$pid, 'om_class_Constant', NULL, 'stat_quantreg_n', elf$stats$n_subset_upper, site, token)
  }
}
elfgen_huc10 <- function(runid, hydroid_all){
  
  for(i in 1:length(hydroid_all)) {
    
    hydroid <- as.character(hydroid_all[i])
    scen.propname<-paste0('runid_', runid)
    
    # GETTING SCENARIO PROPERTY FROM VA HYDRO
    sceninfo <- list(
      varkey = 'om_scenario',
      propname = scen.propname,
      featureid = pid,
      entity_type = "dh_properties"
    )
    scenprop <- getProperty(sceninfo, site, scenprop)
    # POST PROPERTY IF IT IS NOT YET CREATED
    if (identical(scenprop, FALSE)) {
      # create
      sceninfo$pid = NULL
    } else {
      sceninfo$pid = scenprop$pid
    }
    #scenprop = postProperty(inputs=sceninfo,base_url=base_url,prop)
    scenprop <- getProperty(sceninfo, site, scenprop)
    
    # Create an algorithm that finds the outlet point for the watershed 
    
    nhdplus_views <- paste(site,'dh-feature-containing-export', hydroid, 'watershed/nhdplus/nhdp_drainage_sqmi',  sep = '/')
    nhdplus_df <- read.csv(file=nhdplus_views, header=TRUE, sep=",")
    
    hydroid_out <-sqldf("SELECT hydroid, max(propvalue)
                    FROM nhdplus_df ")
    
    inputs <- list(
      varkey = 'om_class_Constant',
      propname = 'consumptive_use_frac',
      entity_type = 'dh_properties',
      featureid = scenprop$pid
    )
    prop <- getProperty(inputs, site)
    cuf <- prop$propvalue
    
    
    inputs <- list(
      varkey = 'om_class_Constant',
      propname = 'Qbaseline',
      entity_type = 'dh_properties',
      featureid = scenprop$pid
    )
    prop <- getProperty(inputs, site)
    outlet_flow <- prop$propvalue #outlet flow as a baseline flow
    
    
    site_comparison <- paste(site,'dh-feature-contained-within-export', hydroid_out$hydroid, 'watershed', sep = '/')
    containing_watersheds <- read.csv(file=site_comparison, header=TRUE, sep=",")
    
    nhd_huc10_code <- sqldf("SELECT hydrocode 
               FROM containing_watersheds 
               WHERE ftype = 'nhd_huc10'")
    
    
    # Load the model result data for this scenario for "consumptive_use_frac" property
    
    
    # Example input parameters for retrieving data from VAHydro
    
    #HUC 10 Section---------------------------------------------------------------------------
    watershed.code <- as.character(nhd_huc10_code$hydrocode)
    watershed.bundle <- 'watershed'
    watershed.ftype <- 'nhd_huc10' #watershed.ftpe[i] when creating function
    x.metric <- 'erom_q0001e_mean'
    y.metric <- 'aqbio_nt_total'
    y.sampres <- 'species'
    datasite <- site
    
    # elfdata_vahydro() function for retrieving data from VAHydro
    watershed.df <- elfdata_vahydro(watershed.code,watershed.bundle,watershed.ftype,x.metric,y.metric,y.sampres,datasite)
    # clean_vahydro() function for cleaning data by removing any stations where the ratio of DA:Q is greater than 1000, also aggregates to the maximum richness value at each flow value
    watershed.df <- clean_vahydro(watershed.df)
    
    elf_quantile <- 0.80
    
    #breakpt <- bkpt_pwit("watershed.df" = watershed.df, "quantile" = elf_quantile, "blo" = 100, "bhi" = 1000)  
    breakpt<-530
    
    elf <- elfgen("watershed.df" = watershed.df,
                  "quantile" = elf_quantile,
                  "breakpt" = breakpt,
                  "yaxis_thresh" = 53, 
                  "xlabel" = "Mean Annual Flow (ft3/s)",
                  "ylabel" = "Fish Species Richness")
    
    #Confidence Interval information
    uq <- elf$plot$plot_env$upper.quant
    
    upper.lm <- lm(y_var ~ log(x_var), data = uq)
    
    predict <- as.data.frame(predict(upper.lm, newdata = data.frame(x_var = outlet_flow), interval = 'confidence'))
    
    species_richness<-elf$stats$m*log(outlet_flow)+elf$stats$b
    
    xmin <- min(uq$x_var)
    xmax <- max(uq$x_var)
    
    yval1 <- predict(upper.lm, newdata = data.frame(x_var = xmin), interval = 'confidence')
    yval2 <- predict(upper.lm, newdata = data.frame(x_var = xmax), interval = 'confidence')
    
    ymin1 <- yval1[2] # bottom left point, line 1
    ymax1 <- yval2[3] # top right point, line 1
    
    ymin2 <- yval1[3] # top left point, line 2
    ymax2 <- yval2[2] # bottom right point, line 2
    
    m <- elf$stats$m
    b <- elf$stats$b
    int <- round((m*log(outlet_flow) + b),2)      # solving for outlet_flow y-value
    
    m1 <- (ymax1-ymin1)/(log(xmax)-log(xmin)) # line 1
    b1 <- ymax1-(m1*log(xmax))
    
    m2 <- (ymax2-ymin2)/(log(xmax)-log(xmin)) # line 2
    b2 <- ymax2 - (m2*log(xmax))
    
    # Calculating y max value based on greatest point value or intake y val
    if (int > max(watershed.df$NT.TOTAL.UNIQUE)) {
      ymax <- int + 2
    } else {
      ymax <- as.numeric(max(watershed.df$NT.TOTAL.UNIQUE)) + 2
    }
    
    
    #### Calculating median percent/absolute richness change
    
    pct_change <- richness_change(elf$stats, "pctchg" = cuf*100, "xval" = outlet_flow)
    abs_change <- richness_change(elf$stats, "pctchg" = cuf*100)
    
    #### Using confidence interval lines to find percent/absolute richness bounds
    
    elf$bound1stats$m <- m1
    elf$bound1stats$b <- b1
    
    percent_richness_change_bound1 <- round(richness_change(elf$bound1stats, "pctchg" = cuf*100, "xval" = outlet_flow),2)
    abs_richness_change_bound1 <- round(richness_change(elf$bound1stats, "pctchg" = cuf*100),2)
    
    elf$bound2stats$m <- m2
    elf$bound2stats$b <- b2
    
    percent_richness_change_bound2 <- round(richness_change(elf$bound2stats, "pctchg" = cuf*100, "xval" = outlet_flow),2)
    abs_richness_change_bound2 <- round(richness_change(elf$bound2stats, "pctchg" = cuf*100),2)
    
    # checking diffs in pct richness
    pct_d1 <- round((pct_change - percent_richness_change_bound1),2)
    pct_d2 <- round((pct_change - percent_richness_change_bound2),2)
    
    #checking diffs in abs richness
    abs_d1 <- round((abs_change - abs_richness_change_bound1),2)
    abs_d2 <- round((abs_change - abs_richness_change_bound2),2)
    
    #Scenario Property posts
    vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'elfgen_richness_change_huc10', pct_change, site, token)
    
    
    #Abs change branch
    inputs <- list(
      varkey = 'om_class_Constant',
      propname = 'elfgen_richness_change_huc10',
      entity_type = 'dh_properties',
      featureid = scenprop$pid)
    
    prop<-getProperty(inputs, site)
    
    vahydro_post_metric_to_scenprop(prop$pid, 'om_class_Constant', NULL, 'richness_change_abs', abs_change, site, token)
    
    inputs <- list(
      varkey = 'om_class_Constant',
      propname = 'richness_change_abs',
      entity_type = 'dh_properties',
      featureid = prop$pid)
    
    prop_abs<-getProperty(inputs, site)
    
    vahydro_post_metric_to_scenprop(prop_abs$pid, 'om_class_Constant', NULL, 'upper_confidence', abs_d2, site, token)
    vahydro_post_metric_to_scenprop(prop_abs$pid, 'om_class_Constant', NULL, 'lower_confidence', abs_d1, site, token)
    
    #Pct Change branch
    vahydro_post_metric_to_scenprop(prop$pid, 'om_class_Constant', NULL, 'richness_change_pct', pct_change, site, token)
    
    inputs <- list(
      varkey = 'om_class_Constant',
      propname = 'richness_change_pct',
      entity_type = 'dh_properties',
      featureid = prop$pid)
    
    prop_pct<-getProperty(inputs, site)
    
    vahydro_post_metric_to_scenprop(prop_pct$pid, 'om_class_Constant', NULL, 'upper_confidence', pct_d2, site, token)
    vahydro_post_metric_to_scenprop(prop_pct$pid, 'om_class_Constant', NULL, 'lower_confidence', pct_d1, site, token)
    
    ####Elf stats posts---------------------------
    # vahydro_post_metric_to_scenprop(prop$pid, 'om_class_Constant', NULL, 'stat_quantreg_bkpt', elf$stats$bkpt, site, token)
    # vahydro_post_metric_to_scenprop(prop$pid, 'om_class_Constant', NULL, 'stat_quantreg_qu', elf$stats$quantile, site, token)
    # vahydro_post_metric_to_scenprop(prop$pid, 'om_class_Constant', NULL, 'stat_quantreg_m', elf$stats$m, site, token)
    # vahydro_post_metric_to_scenprop(prop$pid, 'om_class_Constant', NULL, 'stat_quantreg_b', elf$stats$b, site, token)
    # vahydro_post_metric_to_scenprop(prop$pid, 'om_class_Constant', NULL, 'stat_quantreg_rsq', elf$stats$rsquared, site, token)
    # vahydro_post_metric_to_scenprop(prop$pid, 'om_class_Constant', NULL, 'stat_quantreg_adj_rsq', elf$stats$rsquared_adj, site, token)
    # vahydro_post_metric_to_scenprop(prop$pid, 'om_class_Constant', NULL, 'stat_quantreg_p', elf$stats$p, site, token)
    # vahydro_post_metric_to_scenprop(prop$pid, 'om_class_Constant', NULL, 'stat_quantreg_n_tot', elf$stats$n_total, site, token)
    # vahydro_post_metric_to_scenprop(prop$pid, 'om_class_Constant', NULL, 'stat_quantreg_n_sub', elf$stats$n_subset, site, token)
    # vahydro_post_metric_to_scenprop(prop$pid, 'om_class_Constant', NULL, 'stat_quantreg_n', elf$stats$n_subset_upper, site, token)
  }
}

elfgen_huc8(runid, hydroid_all)
elfgen_huc10(runid, hydroid_all)


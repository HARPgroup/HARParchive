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

#------------------------------------------------
#Inputs

riv_seg <- 'PS3_5990_6161' #PS3_5990_6161' #'TU4_8680_8810' 'TU3_9040_9180' random examples for practice
runid<-11

pid <- get.overall.vahydro.prop(riv_seg, site = site, token = token)

huc_level<- 'huc8'

dataset <- 'VAHydro-EDAS' #'VAHydro-EDAS' or 'IchthyMaps'

# Read Args
# argst <- commandArgs(trailingOnly=T)
# pid <- as.integer(argst[1])
# runid <- as.integer(argst[2])
# huc_level <- as.integer(argst[3])
# dataset <- as.integer(argst[4])

inputs<-list(pid=pid)
property<-getProperty(inputs, site)
hydroid<-property$featureid

#future inputs of x and y metrics 

elfgen_confidence <- function(elf){
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
  
  #Calculating median percent/absolute richness change
  pct_change <- round(richness_change(elf$stats, "pctchg" = cuf*100, "xval" = outlet_flow),2)
  abs_change <- round(richness_change(elf$stats, "pctchg" = cuf*100),2)
  
  #Using confidence interval lines to find percent/absolute richness bounds
  elf$bound1stats$m <- m1
  elf$bound1stats$b <- b1
  
  percent_richness_change_bound1 <- round(richness_change(elf$bound1stats, "pctchg" = cuf*100, "xval" = outlet_flow),2)
  abs_richness_change_bound1 <- round(richness_change(elf$bound1stats, "pctchg" = cuf*100),2)
  
  elf$bound2stats$m <- m2
  elf$bound2stats$b <- b2
  
  percent_richness_change_bound2 <- round(richness_change(elf$bound2stats, "pctchg" = cuf*100, "xval" = outlet_flow),2)
  abs_richness_change_bound2 <- round(richness_change(elf$bound2stats, "pctchg" = cuf*100),2)
  
  #checking diffs in pct richness
  pct_d1 <- round((pct_change - percent_richness_change_bound1),2)
  pct_d2 <- round((pct_change - percent_richness_change_bound2),2)
  
  #checking diffs in abs richness
  abs_d1 <- round((abs_change - abs_richness_change_bound1),2)
  abs_d2 <- round((abs_change - abs_richness_change_bound2),2)
  
  plt <- elf$plot +
    geom_segment(aes(x = outlet_flow, y = -Inf, xend = outlet_flow, yend = int), color = 'red', linetype = 'dashed') +
    geom_segment(aes(x = 0, xend = outlet_flow, y = int, yend = int), color = 'red', linetype = 'dashed') +
    geom_point(aes(x = outlet_flow, y = int, fill = 'Outlet Flow'), color = 'red', shape = 'triangle', size = 2) +
    geom_segment(aes(x = xmin, y = (m1 * log(xmin) + b1), xend = xmax, yend = (m1 * log(xmax) + b1)), color = 'blue', linetype = 'dashed') +
    geom_segment(aes(x = xmin, y = (m2 * log(xmin) + b2), xend = xmax, yend = (m2 * log(xmax) + b2)), color = 'blue', linetype = 'dashed') +
    labs(fill = 'Outlet Legend', 
         x=paste(elf$plot$labels$x, '  Breakpt:',elf$stats$breakpt,sep=' ')) + 
    theme(plot.title = element_text(face = 'bold', vjust = -5)) +
    ylim(0,ymax)
  
  confidence<-list(plot = plt, df = data.frame(pct_change,pct_d1,pct_d2, abs_change,abs_d1,abs_d2))
  return(confidence)
  
}

elfgen_huc <- function(runid, hydroid, huc_level, dataset){
  
  
  breakpt <- 530
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
  
  #Determines watershed outlet nhd+ segment and hydroid
  nhdplus_views <- paste(site,'dh-feature-containing-export', hydroid, 'watershed/nhdplus/nhdp_drainage_sqmi',  sep = '/')
  nhdplus_df <- read.csv(file=nhdplus_views, header=TRUE, sep=",")
  
  hydroid_out <-sqldf("select hydroid from nhdplus_df where propvalue in (select max(propvalue) from nhdplus_df)")

  #Determines cumulative consumptive use fraction for the river segment
  inputs <- list(
    varkey = 'om_class_Constant',
    propname = 'consumptive_use_frac',
    entity_type = 'dh_properties',
    featureid = scenprop$pid
  )
  prop_cuf <- getProperty(inputs, site)
  cuf <- prop_cuf$propvalue
  
  #Pulls mean annual outlet flow
  inputs <- list(
    varkey = 'erom_q0001e_mean',
    featureid = as.numeric(hydroid_out$hydroid),
    entity_type = "dh_feature"
  )
  
  prop_flow <- getProperty(inputs, site)
  outlet_flow <- prop_flow$propvalue #outlet flow as erom_q0001e_mean of nhdplus segment
  
  #Determines huc of interest for outlet nhd+ segment
  site_comparison <- paste(site,'dh-feature-contained-within-export', hydroid_out$hydroid, 'watershed', sep = '/')
  containing_watersheds <- read.csv(file=site_comparison, header=TRUE, sep=",")
  
  nhd_code <- sqldf(paste("SELECT hydrocode 
             FROM containing_watersheds 
             WHERE ftype = 'nhd_", huc_level,"'", sep = "" ))

  #HUC Section---------------------------------------------------------------------------
  watershed.code <- as.character(nhd_code$hydrocode)
  watershed.bundle <- 'watershed'
  watershed.ftype <- paste("nhd_", huc_level, sep = "") #watershed.ftpe[i] when creating function
  x.metric <- 'erom_q0001e_mean'
  y.metric <- 'aqbio_nt_total'
  y.sampres <- 'species'
  datasite <- site
 
  if (dataset == 'IchthyMaps'){
    #if loop below works only for huc:6,8,10 due to naming convention in containing_watershed
    if(huc_level == 'huc8'){ 
      watershed.code <- str_sub(watershed.code, -8,-1)
      }
    watershed.df <- elfdata(watershed.code)
  }else{
    # elfdata_vahydro() function for retrieving data from VAHydro
    watershed.df <- elfdata_vahydro(watershed.code,watershed.bundle,watershed.ftype,x.metric,y.metric,y.sampres,datasite)
    # clean_vahydro() function for cleaning data by removing any stations where the ratio of DA:Q is greater than 1000, also aggregates to the maximum richness value at each flow value
    watershed.df <- clean_vahydro(watershed.df)
  }
  
  elf <- elfgen("watershed.df" = watershed.df,
                "quantile" = 0.8,
                "breakpt" = breakpt,
                "yaxis_thresh" = 53, 
                "xlabel" = "Mean Annual Flow (ft3/s)",
                "ylabel" = "Fish Species Richness")
  
  
  confidence <- elfgen_confidence(elf)
  
  #Scenario Property posts
  
  if (dataset == 'IchthyMaps'){  
    dataname='Ichthy'
  }else{
    dataname='EDAS'
  }
  
  inputs <- list(
    varkey = 'om_class_Constant',
    propname = paste('elfgen_', dataname,'_', huc_level, sep=''),
    entity_type = 'dh_properties',
    propcode = nhd_code$hydrocode,
    featureid = scenprop$pid,
    proptext = dataset, #figure out how to make this part changeable
    propvalue = NULL)
  
  postProperty(inputs, site)
  
  #Absolute change branch - posted underneath elfgen_richness_change_huc_level scenario property
  inputs <- list(
    varkey = 'om_class_Constant',
    propname = paste('elfgen_', dataname,'_', huc_level, sep=''),
    entity_type = 'dh_properties',
    propcode = nhd_code$hydrocode,
    featureid = scenprop$pid)
  
  prop_huc<-getProperty(inputs, site)
  
  vahydro_post_metric_to_scenprop(prop_huc$pid, 'om_class_Constant', NULL, 'richness_change_abs', confidence$df$abs_change, site, token)
  
  #Absolute change confidence interval bounds - posted underneath richness_change_abs property 
  inputs <- list(
    varkey = 'om_class_Constant',
    propname = 'richness_change_abs',
    entity_type = 'dh_properties',
    featureid = prop_huc$pid)
  
  prop_abs<-getProperty(inputs, site)
  
  vahydro_post_metric_to_scenprop(prop_abs$pid, 'om_class_Constant', NULL, 'upper_confidence', confidence$df$abs_d1, site, token) #flipped and negated to match negative richness change value
  vahydro_post_metric_to_scenprop(prop_abs$pid, 'om_class_Constant', NULL, 'lower_confidence', confidence$df$abs_d2, site, token)
  
  #Percent change branch - posted underneath elfgen_richness_change_huc_level scenario property
  vahydro_post_metric_to_scenprop(prop_huc$pid, 'om_class_Constant', NULL, 'richness_change_pct', confidence$df$pct_change, site, token)
  
  #Percent change confidence interval bounds - posted underneath richness_change_pct property 
  inputs <- list(
    varkey = 'om_class_Constant',
    propname = 'richness_change_pct',
    entity_type = 'dh_properties',
    featureid = prop_huc$pid)
  
  prop_pct<-getProperty(inputs, site)
  
  vahydro_post_metric_to_scenprop(prop_pct$pid, 'om_class_Constant', NULL, 'upper_confidence', confidence$df$pct_d1, site, token) #flipped similar to vahydro
  vahydro_post_metric_to_scenprop(prop_pct$pid, 'om_class_Constant', NULL, 'lower_confidence', confidence$df$pct_d2, site, token)
  
  #Elf$stats posts - posted underneath elfgen_richness_change_huc_level scenario property-----------------------
  vahydro_post_metric_to_scenprop(prop_huc$pid, 'stat_quantreg_bkpt', NULL, 'breakpt', elf$stats$breakpt, site, token)
  vahydro_post_metric_to_scenprop(prop_huc$pid, 'stat_quantreg_qu', NULL, 'quantile', elf$stats$quantile, site, token)
  vahydro_post_metric_to_scenprop(prop_huc$pid, 'stat_quantreg_m', NULL, 'm', elf$stats$m, site, token)
  vahydro_post_metric_to_scenprop(prop_huc$pid, 'stat_quantreg_b', NULL, 'b', elf$stats$b, site, token)
  vahydro_post_metric_to_scenprop(prop_huc$pid, 'stat_quantreg_rsq', NULL, 'rsquared', elf$stats$rsquared, site, token)
  vahydro_post_metric_to_scenprop(prop_huc$pid, 'stat_quantreg_adj_rsq', NULL, 'rsquared_adj', elf$stats$rsquared_adj, site, token)
  vahydro_post_metric_to_scenprop(prop_huc$pid, 'stat_quantreg_p', NULL, 'p', elf$stats$p, site, token)
  vahydro_post_metric_to_scenprop(prop_huc$pid, 'stat_quantreg_n_tot', NULL, 'n_total', elf$stats$n_total, site, token)
  vahydro_post_metric_to_scenprop(prop_huc$pid, 'stat_quantreg_n_sub', NULL, 'n_subset', elf$stats$n_subset, site, token)
  vahydro_post_metric_to_scenprop(prop_huc$pid, 'stat_quantreg_n', NULL, 'n_subset_upper', elf$stats$n_subset_upper, site, token)
  
  #Elf$plot post - posted underneath elfgen_richness_change_huc_level scenario property------------
  

  #Elf$plot saving functions
  

#Image saving & naming
  
  fname <- paste(
    save_directory,
    paste0(
      'fig.elfgen.',
      prop_huc$pid,'.png'
    ),
    sep = '/'
  )
  
  furl <- paste(
    save_url,paste0(
    'fig.elfgen.',
    prop_huc$pid,'.png'
  ),
    sep = '/'
  )
  
  print(fname)
  ggsave(fname, plot = confidence$plot, width = 7, height = 5.5)
  
  print(paste("Saved file: ", fname, "with URL", furl))
  
  vahydro_post_metric_to_scenprop(prop_huc$pid, 'dh_image_file', furl, 'fig.elfgen', 0.0, site, token) 
  
  print('DONE')
}

elfgen_huc(runid, hydroid, huc_level, dataset)

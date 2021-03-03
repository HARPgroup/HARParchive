# working on creating unmet demand grids

#site
site <- "http://deq2.bse.vt.edu/d.dh"    #Specify the site of interest, either d.bet OR d.dh
# librarys
library(ggplot2)
library(sqldf)
library(ggnewscale)
library(dplyr)
library(ggpubr)

# source config.r
basepath='/var/www/R';
source(paste(basepath,'config.R',sep='/'))

#test run inputs
elid <- 299330 
runid <- 18
pid <- 4964892
save_directory = "/var/www/html/data/proj3/out"


# Turning all of these steps into a function which will out put the grid of unmet demand
# if dates are entered into the start and end date parameters the grid will be shortened 
# to that time frame otherwise it will run from 01-01-1984 to 12-31-2014
# saves an image called fig.unmet_grid_elid.runid.png to provided save_directory
# Enter at least one year of time between 1984 and 2014
# uses a function that is sourced in the above config.R file
# inputted dates should be in format Y-M-D

unmet_grid <- function(elid,pid,runid,start_date,end_date,save_directory) {
  dat <- fn_get_runfile(elid, runid, site= omsite,  cached = FALSE) # get data
  # set start and end dates based on inputs
  if (missing(start_date)) {
    syear <- as.numeric(min(dat$year)) + 1
    sdate <- as.Date(paste0(syear,"-01-01"))
  } else {
    sdate <- start_date
    syear <- format(as.Date(start_date, format="%Y-%m-%d"),"%Y")
  }
  if (missing(end_date)) {
    eyear <- max(dat$year)
    edate <- as.Date(paste0(eyear,"-12-31"))
  } else {
    edate <- end_date
    eyear <- format(as.Date(end_date, format="%Y-%m-%d"),"%Y")
  }   
  dat <- window(dat, start = sdate, end = edate);
  mode(dat) <- 'numeric'
  
  #######################################
  # section that would get scen prop and info and post to va hydro/ not in use now/yet
  # scen.propname<-paste0('runid_', runid)
  # 
  # # GETTING SCENARIO PROPERTY FROM VA HYDRO
  # sceninfo <- list(
  #   varkey = 'om_scenario',
  #   propname = scen.propname,
  #   featureid = pid,
  #   entity_type = "dh_properties"
  # )
  # scenprop <- getProperty(sceninfo, site, scenprop)
  # # POST PROPERTY IF IT IS NOT YET CREATED
  # if (identical(scenprop, FALSE)) {
  #   # create
  #   sceninfo$pid = NULL
  # } else {
  #   sceninfo$pid = scenprop$pid
  # }
  # scenprop = postProperty(inputs=sceninfo,base_url=base_url,prop)
  # scenprop <- getProperty(sceninfo, site, scenprop)
  # sceninfo <- list(
  #   varkey = 'om_scenario',
  #   propname = scen.propname,
  #   featureid = pid,
  #   entity_type = "dh_properties"
  # )
  ###################################
  
  # change runfile to data frame
  datdf <- as.data.frame(dat)
  
  modat <- sqldf(" select month months, year years, count(*) count from datdf where unmet_demand_mgd > 0
  group by month, year") #Counts sum of unmet_days by month and year
  
  #Join counts with original data frame to get missing month and year combos then selects just count month and year
  modat <- sqldf("SELECT * FROM datdf LEFT JOIN modat ON modat.years = datdf.year AND modat.months = datdf.month group by month, year")
  modat <- sqldf('SELECT month, year, count count_unmet_days FROM modat GROUP BY month, year')
  
  #Replace NA for count with 0s
  modat[is.na(modat)] = 0
  
  ########################################################### Calculating Totals
  # numeric versions of eyear and syear
  num_syear <- as.numeric(syear)  
  num_eyear <- as.numeric(eyear)
  
  # monthly totals via sqldf
  mosum <- sqldf("SELECT  month, sum(count_unmet_days) count_unmet_days FROM modat GROUP BY month")
  mosum$year <- rep(num_eyear+1,12)
  
  #yearly sum
  yesum <-  sqldf("SELECT year, sum(count_unmet_days) count_unmet_days FROM modat GROUP BY year")
  yesum$month <- rep(13,length(yesum$year))
  
  # create monthly averages 
  moavg<- sqldf('SELECT * FROM mosum')
  moavg$year <- moavg$year + 1
  moavg$avg <- round(moavg$count_unmet_days/((num_eyear-num_syear)+1),1)
  
  # create yearly averages
  yeavg<- sqldf('SELECT * FROM yesum')
  yeavg$month <- yeavg$month + 1
  yeavg$avg <- round(yeavg$count_unmet_days/12,1)
  
  # create x and y axis breaks
  y_breaks <- seq(syear,num_eyear+2,1)
  x_breaks <- seq(1,14,1)
  
  # create x and y labels
  y_labs <- c(seq(syear,eyear,1),'Totals', 'Avg')
  x_labs <- c(month.abb,'Totals','Avg')
  
  
  ############################################################### Plot and Save
  if (sum(mosum$count_unmet_days) == 0) {
    count_grid <- ggplot() +
      geom_tile(data=modat, color='black',aes(x = month, y = year, fill = count_unmet_days)) +
      geom_text(aes(label=modat$count_unmet_days, x=modat$month, y= modat$year), size = 3.5, colour = "black") +
      scale_fill_gradient2(low = "#00cc00", mid= "#00cc00", high = "#00cc00", guide = "colourbar", 
                           name= 'Unmet Days') +
      theme(panel.background = element_rect(fill = "transparent"))+
      theme() + labs(title = 'Unmet Demand Heatmap', y=NULL, x=NULL) +
      scale_x_continuous(expand=c(0,0), breaks= x_breaks, labels=x_labs, position='top') + 
      scale_y_reverse(expand=c(0,0), breaks=y_breaks, labels= y_labs) +
      theme(axis.ticks= element_blank()) +
      theme(plot.title = element_text(size = 12, face = "bold",  hjust = 0.5)) +
      theme(legend.title.align = 0.5) 
    
    unmet <- count_grid + new_scale_fill() +
      geom_tile(data = yesum, color='black', aes(x = month, y = year, fill = count_unmet_days)) +
      geom_tile(data = mosum, color='black', aes(x = month, y = year, fill = count_unmet_days)) +
      geom_text(data = yesum, size = 3.5, color='black', aes(x = month, y = year, label = count_unmet_days)) +
      geom_text(data = mosum, size = 3.5, color='black', aes(x = month, y = year, label = count_unmet_days)) +
      scale_fill_gradient2(low = "#63D1F4", high = "#8A2BE2", mid="#63D1F4",
                           midpoint = mean(mosum$count_unmet_days), name= 'Total Unmet Days')
    
    
    unmet_avg <- unmet + new_scale_fill()+
      geom_tile(data = yeavg, color='black', aes(x = month, y = year, fill = avg)) +
      geom_tile(data = moavg, color='black', aes(x = month, y = year, fill = avg)) +
      geom_text(data = yeavg, size = 3.5, color='black', aes(x = month, y = year, label = avg)) +
      geom_text(data = moavg, size = 3.5, color='black', aes(x = month, y = year, label = avg))+
      scale_fill_gradient2(low = "#FFF8DC", mid = "#FFF8DC", high ="#FFF8DC",
                           name= 'Average Unmet Days', midpoint = mean(yeavg$avg))
  } else{
    count_grid <- ggplot() +
      geom_tile(data=modat, color='black',aes(x = month, y = year, fill = count_unmet_days)) +
      geom_text(aes(label=modat$count_unmet_days, x=modat$month, y= modat$year), size = 3.5, colour = "black") +
      scale_fill_gradient2(low = "#00cc00", high = "red",mid ='yellow',
                           midpoint = 15, guide = "colourbar", 
                           name= 'Unmet Days') +
      theme(panel.background = element_rect(fill = "transparent"))+
      theme() + labs( y=NULL, x=NULL) +
      scale_x_continuous(expand=c(0,0), breaks= x_breaks, labels=x_labs, position='top') + 
      scale_y_reverse(expand=c(0,0), breaks=y_breaks, labels= y_labs) +
      theme(axis.ticks= element_blank()) +
      theme(plot.title = element_text(size = 12, face = "bold",  hjust = 0.5)) +
      theme(legend.title.align = 0.5) 
    
    unmet <- count_grid + new_scale_fill() +
      geom_tile(data = yesum, color='black', aes(x = month, y = year, fill = count_unmet_days)) +
      geom_tile(data = mosum, color='black', aes(x = month, y = year, fill = count_unmet_days)) +
      geom_text(data = yesum, size = 3.5, color='black', aes(x = month, y = year, label = count_unmet_days)) +
      geom_text(data = mosum, size = 3.5, color='black', aes(x = month, y = year, label = count_unmet_days)) +
      scale_fill_gradient2(low = "#63D1F4", high = "#8A2BE2", mid='#CAB8FF',
                           midpoint = mean(mosum$count_unmet_days), name= 'Total Unmet Days')
    
    
    unmet_avg <- unmet + new_scale_fill()+
      geom_tile(data = yeavg, color='black', aes(x = month, y = year, fill = avg)) +
      geom_tile(data = moavg, color='black', aes(x = month, y = year, fill = avg)) +
      geom_text(data = yeavg, size = 3.5, color='black', aes(x = month, y = year, label = avg)) +
      geom_text(data = moavg, size = 3.5, color='black', aes(x = month, y = year, label = avg))+
      scale_fill_gradient2(low = "#FFF8DC", mid = "#FFDEAD", high ="#DEB887",
                           name= 'Average Unmet Days', midpoint = mean(yeavg$avg))
    
  }
  
  
  fname <- paste(save_directory,paste0('fig.unmet_heatmap_',elid, '.', runid, '.png'),sep = '/')
  
  ggsave(fname,plot = unmet_avg, width= 7, height=7)
  
  print('File saved to save_directory')
  
  return(unmet_avg)
  #here we would add vahydro posting eventually if needed
}

counts <- unmet_grid(elid,pid,runid,save_directory=save_directory)

# heatmap unmet count and value option returns the ggplot
unmet_grid_vals <- function(elid,pid,runid,start_date,end_date,save_directory) {
  dat <- fn_get_runfile(elid, runid, site= omsite,  cached = FALSE) # get data
  # set start and end dates based on inputs
  if (missing(start_date)) {
    syear <- as.numeric(min(dat$year)) + 1
    sdate <- as.Date(paste0(syear,"-01-01"))
  } else {
    sdate <- start_date
    syear <- format(as.Date(start_date, format="%Y-%m-%d"),"%Y")
  }
  if (missing(end_date)) {
    eyear <- max(dat$year)
    edate <- as.Date(paste0(eyear,"-12-31"))
  } else {
    edate <- end_date
    eyear <- format(as.Date(end_date, format="%Y-%m-%d"),"%Y")
  }   
  dat <- window(dat, start = sdate, end = edate);
  mode(dat) <- 'numeric'
  
  #######################################
  # section that would get scen prop and info and post to va hydro/ not in use now/yet
  # scen.propname<-paste0('runid_', runid)
  # 
  # # GETTING SCENARIO PROPERTY FROM VA HYDRO
  # sceninfo <- list(
  #   varkey = 'om_scenario',
  #   propname = scen.propname,
  #   featureid = pid,
  #   entity_type = "dh_properties"
  # )
  # scenprop <- getProperty(sceninfo, site, scenprop)
  # # POST PROPERTY IF IT IS NOT YET CREATED
  # if (identical(scenprop, FALSE)) {
  #   # create
  #   sceninfo$pid = NULL
  # } else {
  #   sceninfo$pid = scenprop$pid
  # }
  # scenprop = postProperty(inputs=sceninfo,base_url=base_url,prop)
  # scenprop <- getProperty(sceninfo, site, scenprop)
  # sceninfo <- list(
  #   varkey = 'om_scenario',
  #   propname = scen.propname,
  #   featureid = pid,
  #   entity_type = "dh_properties"
  # )
  ###################################
  
  # change runfile to data frame
  datdf <- as.data.frame(dat)
  
  modat <- sqldf(" SELECT month months, year years,sum(unmet_demand_mgd) sum_unmet, count(*) count FROM datdf WHERE unmet_demand_mgd > 0
  GROUP BY month, year") #Counts sum of unmet_days by month and year, and calculates sum of unmet demand
  modat$avg_unmet <- modat$sum_unmet / modat$count # truns unmet sum into the avg
  #Join counts with original data frame to get missing month and year combos then selects just count month and year
  modat <- sqldf("SELECT * FROM datdf LEFT JOIN modat ON modat.years = datdf.year AND modat.months = datdf.month group by month, year")
  modat <- sqldf('SELECT month, year, avg_unmet, count count_unmet_days FROM modat GROUP BY month, year')
  
  #Replace NA for count with 0s
  modat[is.na(modat)] = 0
  
  ########################################################### Calculating Totals
  # numeric versions of eyear and syear
  num_syear <- as.numeric(syear)  
  num_eyear <- as.numeric(eyear)
  
  # monthly totals via sqldf
  mosum <- sqldf("SELECT  month, sum(count_unmet_days) count_unmet_days, sum(avg_unmet) avg_unmet FROM modat GROUP BY month")
  mosum$year <- rep(num_eyear+1,12)
  
  #yearly sum
  yesum <-  sqldf("SELECT year, sum(count_unmet_days) count_unmet_days,sum(avg_unmet) avg_unmet FROM modat GROUP BY year")
  yesum$month <- rep(13,length(yesum$year))
  
  # create monthly averages 
  moavg<- sqldf('SELECT * FROM mosum')
  moavg$year <- moavg$year + 1
  moavg$avg <- round(moavg$count_unmet_days/((num_eyear-num_syear)+1),1)
  moavg$avg_unmet <- round(moavg$avg_unmet/((num_eyear-num_syear)+1),1)
  
  # create yearly averages
  yeavg<- sqldf('SELECT * FROM yesum')
  yeavg$month <- yeavg$month + 1
  yeavg$avg <- round(yeavg$count_unmet_days/12,1)
  yeavg$avg <- round(yeavg$count_unmet_days/12,1)
  
  # create x and y axis breaks
  y_breaks <- seq(syear,num_eyear+2,1)
  x_breaks <- seq(1,14,1)
  
  # create x and y labels
  y_labs <- c(seq(syear,eyear,1),'Totals', 'Avg')
  x_labs <- c(month.abb,'Totals','Avg')
  
  
  ############################################################### Plot and Save
  if (sum(mosum$count_unmet_days) == 0) {
    count_grid <- ggplot() +
      geom_tile(data=modat, color='black',aes(x = month, y = year, fill = count_unmet_days)) +
      geom_text(aes(label=paste(modat$count_unmet_days,' / ',round(modat$avg_unmet,1), sep=''), 
                    x=modat$month, y= modat$year), size = 3.5, colour = "black") +
      scale_fill_gradient2(low = "#00cc00", mid= "#00cc00", high = "#00cc00", guide = "colourbar", 
                           name= 'Unmet Days') +
      theme(panel.background = element_rect(fill = "transparent"))+
      theme() + labs(title = 'Unmet Demand Heatmap', y=NULL, x=NULL) +
      scale_x_continuous(expand=c(0,0), breaks= x_breaks, labels=x_labs, position='top') + 
      scale_y_reverse(expand=c(0,0), breaks=y_breaks, labels= y_labs) +
      theme(axis.ticks= element_blank()) +
      theme(plot.title = element_text(size = 12, face = "bold",  hjust = 0.5)) +
      theme(legend.title.align = 0.5) 
    
    unmet <- count_grid + new_scale_fill() +
      geom_tile(data = yesum, color='black', aes(x = month, y = year, fill = count_unmet_days)) +
      geom_tile(data = mosum, color='black', aes(x = month, y = year, fill = count_unmet_days)) +
      geom_text(data = yesum, size = 3.5, color='black', aes(x = month, y = year, label = count_unmet_days)) +
      geom_text(data = mosum, size = 3.5, color='black', aes(x = month, y = year, label = count_unmet_days)) +
      scale_fill_gradient2(low = "#63D1F4", high = "#8A2BE2", mid="#63D1F4",
                           midpoint = mean(mosum$count_unmet_days), name= 'Total Unmet Days')
    
    
    unmet_avg <- unmet + new_scale_fill()+
      geom_tile(data = yeavg, color='black', aes(x = month, y = year, fill = avg)) +
      geom_tile(data = moavg, color='black', aes(x = month, y = year, fill = avg)) +
      geom_text(data = yeavg, size = 3.5, color='black', aes(x = month, y = year, label = avg)) +
      geom_text(data = moavg, size = 3.5, color='black', aes(x = month, y = year, label = avg))+
      scale_fill_gradient2(low = "#FFF8DC", mid = "#FFF8DC", high ="#FFF8DC",
                           name= 'Average Unmet Days', midpoint = mean(yeavg$avg))
  } else{
    count_grid <- ggplot() +
      geom_tile(data=modat, color='black',aes(x = month, y = year, fill = count_unmet_days)) +
      geom_text(aes(label=paste(modat$count_unmet_days,' / ',round(modat$avg_unmet,1), sep=''), 
                    x=modat$month, y= modat$year), size = 3, colour = "black") +
      scale_fill_gradient2(low = "#00cc00", high = "red",mid ='yellow',
                           midpoint = 15, guide = "colourbar", 
                           name= 'Unmet Days') +
      theme(panel.background = element_rect(fill = "transparent"))+
      theme() + labs(title = 'Unmet Demand Heatmap', y=NULL, x=NULL) +
      scale_x_continuous(expand=c(0,0), breaks= x_breaks, labels=x_labs, position='top') + 
      scale_y_reverse(expand=c(0,0), breaks=y_breaks, labels= y_labs) +
      theme(axis.ticks= element_blank()) +
      theme(plot.title = element_text(size = 12, face = "bold",  hjust = 0.5)) +
      theme(legend.title.align = 0.5) 
    
    unmet <- count_grid + new_scale_fill() +
      geom_tile(data = yesum, color='black', aes(x = month, y = year, fill = count_unmet_days)) +
      geom_tile(data = mosum, color='black', aes(x = month, y = year, fill = count_unmet_days)) +
      geom_text(data = yesum, size = 3.5, color='black', aes(x = month, y = year, label = count_unmet_days)) +
      geom_text(data = mosum, size = 3.5, color='black', aes(x = month, y = year, label = count_unmet_days)) +
      scale_fill_gradient2(low = "#63D1F4", high = "#8A2BE2", mid='#CAB8FF',
                           midpoint = mean(mosum$count_unmet_days), name= 'Total Unmet Days')
    
    
    unmet_avg <- unmet + new_scale_fill()+
      geom_tile(data = yeavg, color='black', aes(x = month, y = year, fill = avg)) +
      geom_tile(data = moavg, color='black', aes(x = month, y = year, fill = avg)) +
      geom_text(data = yeavg, size = 3.5, color='black', aes(x = month, y = year, label = avg)) +
      geom_text(data = moavg, size = 3.5, color='black', aes(x = month, y = year, label = avg))+
      scale_fill_gradient2(low = "#FFF8DC", mid = "#FFDEAD", high ="#DEB887",
                           name= 'Average Unmet Days', midpoint = mean(yeavg$avg))
    
  }
  
  unmet_avg
  
  
  fname <- paste(save_directory,paste0('fig.unmet_grid_vals_', '.png'),sep = '/')
  
  ggsave(fname,plot = unmet_avg, width= 9.5, height=6)
  
  print('File saved to save_directory')
  
  return(unmet_avg)
}

vals_count <- unmet_grid_vals(elid,pid,runid,save_directory=save_directory)


#Second heatmap version, with unmet values instead of count, returns the ggplot
unmet_values <- function(elid,pid,runid,start_date,end_date,save_directory) {
  dat <- fn_get_runfile(elid, runid, site= omsite,  cached = FALSE) # get data
  # set start and end dates based on inputs
  if (missing(start_date)) {
    syear <- as.numeric(min(dat$year)) + 1
    sdate <- as.Date(paste0(syear,"-01-01"))
  } else {
    sdate <- start_date
    syear <- format(as.Date(start_date, format="%Y-%m-%d"),"%Y")
  }
  if (missing(end_date)) {
    eyear <- max(dat$year)
    edate <- as.Date(paste0(eyear,"-12-31"))
  } else {
    edate <- end_date
    eyear <- format(as.Date(end_date, format="%Y-%m-%d"),"%Y")
  }   
  dat <- window(dat, start = sdate, end = edate);
  mode(dat) <- 'numeric'
  
  #######################################
  # section that would get scen prop and info and post to va hydro/ not in use now/yet
  # scen.propname<-paste0('runid_', runid)
  # 
  # # GETTING SCENARIO PROPERTY FROM VA HYDRO
  # sceninfo <- list(
  #   varkey = 'om_scenario',
  #   propname = scen.propname,
  #   featureid = pid,
  #   entity_type = "dh_properties"
  # )
  # scenprop <- getProperty(sceninfo, site, scenprop)
  # # POST PROPERTY IF IT IS NOT YET CREATED
  # if (identical(scenprop, FALSE)) {
  #   # create
  #   sceninfo$pid = NULL
  # } else {
  #   sceninfo$pid = scenprop$pid
  # }
  # scenprop = postProperty(inputs=sceninfo,base_url=base_url,prop)
  # scenprop <- getProperty(sceninfo, site, scenprop)
  # sceninfo <- list(
  #   varkey = 'om_scenario',
  #   propname = scen.propname,
  #   featureid = pid,
  #   entity_type = "dh_properties"
  # )
  ###################################
  
  # change runfile to data frame
  datdf <- as.data.frame(dat)
  
  modat <- sqldf(" SELECT month months, year years,sum(unmet_demand_mgd) sum_unmet, count(*) count FROM datdf WHERE unmet_demand_mgd > 0
  GROUP BY month, year") #Counts sum of unmet_days by month and year, and calculates sum of unmet demand
  modat$avg_unmet <- round(modat$sum_unmet / modat$count,1) # truns unmet sum into the avg
  #Join counts with original data frame to get missing month and year combos then selects just count month and year
  modat <- sqldf("SELECT * FROM datdf LEFT JOIN modat ON modat.years = datdf.year AND modat.months = datdf.month group by month, year")
  modat <- sqldf('SELECT month, year, avg_unmet, count count_unmet_days FROM modat GROUP BY month, year')
  
  #Replace NA for count with 0s
  modat[is.na(modat)] = 0
  
  ########################################################### Calculating Totals
  # numeric versions of eyear and syear
  num_syear <- as.numeric(syear)  
  num_eyear <- as.numeric(eyear)
  
  # monthly totals via sqldf
  mosum <- sqldf("SELECT  month, sum(count_unmet_days) count_unmet_days, sum(avg_unmet) avg_unmet FROM modat GROUP BY month")
  mosum$year <- rep(num_eyear+1,12)
  
  #yearly sum
  yesum <-  sqldf("SELECT year, sum(count_unmet_days) count_unmet_days,sum(avg_unmet) avg_unmet FROM modat GROUP BY year")
  yesum$month <- rep(13,length(yesum$year))
  
  # create monthly averages 
  moavg<- sqldf('SELECT * FROM mosum')
  moavg$avg <- round(moavg$count_unmet_days/((num_eyear-num_syear)+1),1)
  moavg$avg_unmet <- round(moavg$avg_unmet/((num_eyear-num_syear)+1),1)
  
  # create yearly averages
  yeavg<- sqldf('SELECT * FROM yesum')
  yeavg$avg <- round(yeavg$count_unmet_days/12,1)
  yeavg$avg_unmet <- round(yeavg$avg_unmet/12,1)
  
  # create x and y axis breaks
  y_breaks <- seq(syear,num_eyear+1,1)
  x_breaks <- seq(1,13,1)
  
  # create x and y labels
  y_labs <- c(seq(syear,eyear,1), 'Avg')
  x_labs <- c(month.abb,'Avg')
  
  
  ############################################################### Plot and Save
  if (sum(mosum$count_unmet_days) == 0) {
    count_grid <- ggplot() +
      geom_tile(data=modat, color='black',aes(x = month, y = year, fill = count_unmet_days)) +
      geom_text(aes(label=paste(modat$count_unmet_days,' / ',round(modat$avg_unmet,1), sep=''), 
                    x=modat$month, y= modat$year), size = 3.5, colour = "black") +
      scale_fill_gradient2(low = "#00cc00", mid= "#00cc00", high = "#00cc00", guide = "colourbar", 
                           name= 'Average Unmet Demand [mgd]') +
      theme(panel.background = element_rect(fill = "transparent"))+
      theme() + labs(title = 'Unmet Demand Heatmap', y=NULL, x=NULL) +
      scale_x_continuous(expand=c(0,0), breaks= x_breaks, labels=x_labs, position='top') + 
      scale_y_reverse(expand=c(0,0), breaks=y_breaks, labels= y_labs) +
      theme(axis.ticks= element_blank()) +
      theme(plot.title = element_text(size = 12, face = "bold",  hjust = 0.5)) +
      theme(legend.title.align = 0.5) 
    
    unmet_avg <- count_grid + new_scale_fill()+
      geom_tile(data = yeavg, color='black', aes(x = month, y = year, fill = avg)) +
      geom_tile(data = moavg, color='black', aes(x = month, y = year, fill = avg)) +
      geom_text(data = yeavg, size = 3.5, color='black', aes(x = month, y = year, label = avg)) +
      geom_text(data = moavg, size = 3.5, color='black', aes(x = month, y = year, label = avg))+
      scale_fill_gradient2(low = "#FFF8DC", mid = "#FFF8DC", high ="#FFF8DC",
                           name= ' ', midpoint = mean(yeavg$avg))
  } else{
    count_grid <- ggplot() +
      geom_tile(data=modat, color='black',aes(x = month, y = year, fill = avg_unmet)) +
      geom_text(aes(label=modat$avg_unmet, x=modat$month, y= modat$year), size = 3.5, colour = "black") +
      scale_fill_gradient2(low = "#00cc00", high = "red",mid ='yellow',
                           midpoint = max(modat$avg_unmet)/2.1, guide = "colourbar", 
                           name= 'Average Unmet Demand [mgd]') +
      theme(panel.background = element_rect(fill = "transparent"))+
      theme() + labs(title = 'Unmet Demand Heatmap', y=NULL, x=NULL) +
      scale_x_continuous(expand=c(0,0), breaks= x_breaks, labels=x_labs, position='top') + 
      scale_y_reverse(expand=c(0,0), breaks=y_breaks, labels= y_labs) +
      theme(axis.ticks= element_blank()) +
      theme(plot.title = element_text(size = 12, face = "bold",  hjust = 0.5)) +
      theme(legend.title.align = 0.5) 
    
    unmet_avg <- count_grid + new_scale_fill()+
      geom_tile(data = yeavg, color='black', aes(x = month, y = year, fill = avg)) +
      geom_tile(data = moavg, color='black', aes(x = month, y = year, fill = avg)) +
      geom_text(data = yeavg, size = 3.5, color='black', aes(x = month, y = year, label = avg_unmet)) +
      geom_text(data = moavg, size = 3.5, color='black', aes(x = month, y = year, label = avg_unmet))+
      scale_fill_gradient2(low = "#FFF8DC", mid = "#FFDEAD", high ="#DEB887",
                           name= 'Overall Average Unmet Demand [mgd]', midpoint = mean(yeavg$avg_unmet))
    
  }
  
  
  unmet_avg
  
  
  fname <- paste(save_directory,paste0('fig.unmet_heatmap_',elid, '.', runid, '.png'),sep = '/')
  
  ggsave(fname,plot = unmet_avg, width= 8, height=6)
  
  print('File saved to save_directory')
  return(unmet_avg)
}

vals <- unmet_values(elid,pid,runid,save_directory=save_directory)
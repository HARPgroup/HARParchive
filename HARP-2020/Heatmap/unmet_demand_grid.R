################################
#### Unmet demand grid work
#### *** July 14th, 2020
################################

## Urls/sourcing
#----------------------------------------------
site <- "http://deq2.bse.vt.edu/d.dh"    #Specify the site of interest, either d.bet OR d.dh
save_url <- paste(str_remove(site, 'd.dh'), "data/proj3/out", sep='');
#----------------------------------------------
# Load Libraries
library(stringr)
library(ggplot2)
library(dplyr)

basepath ='/var/www/R';
source(paste(basepath,'config.R',sep='/'))
save_directory <-  "/var/www/html/data/proj3/out"

## Define location and scenarios
pid <- '4964892'
elid <- '299330'                       
Name <- 'SHENANDOAH (TOWN) WTP:SF Shenandoah River @ Luray'
runid <- '18'

## Getting raw data as zoo and manipulating start and end date
dat <- fn_get_runfile(elid, runid = runid, site= omsite,  cached = FALSE)
syear = min(dat$year)
eyear = max(dat$year)
if (syear != eyear) {
  sdate <- as.Date(paste0(syear,"-10-01"))
  edate <- as.Date(paste0(eyear,"-09-30"))
} else {
  # special case to handle 1 year model runs
  # just omit January in order to provide a short warmup period.
  sdate <- as.Date(paste0(syear,"-02-01"))
  edate <- as.Date(paste0(eyear,"-12-31"))
}
#dat <- window(dat, start = sdate, end = edate);
#mode(dat) <- 'numeric'
#scen.propname<-paste0('runid_', runid)

## Converting zoo to data frame and creating new data frame with unmet days count
dat <- data.frame(dat)
modat <- sqldf(" select month months, year years, count(*) count from dat where unmet_demand_mgd > 0 group by month, year")
modat_0 <- sqldf("SELECT * FROM dat LEFT JOIN modat ON modat.years = dat.year AND modat.months = dat.month group by month, year")
modat_0 <- sqldf('SELECT month, year, count count_unmet_days FROM modat_0 GROUP BY month, year')
modat_0[is.na(modat_0)] = 0

## Creating a heatmap for grid
grid <- ggplot()+ 
        geom_tile(data = modat_0, aes(y = year, x = month, fill = count_unmet_days)) +
        geom_text(data = modat_0, aes(y = year, x = month, label = round(count_unmet_days, 1))) +
        scale_fill_gradient(low = "white", high = "red") +
        theme_bw() +
        labs(title = 'Number of Days When There is Unmet Demand For Given Facility', x = 'Month', y = 'Year') +
        theme(plot.title = element_text(face = 'bold',hjust = 0.5))

## Creating monthly and yearly totals
h_total <- modat_0 %>%
  group_by(year) %>%
  summarise(count_unmet_days = sum(count_unmet_days)) %>%
  mutate(month = 'Total')

v_total <- modat_0 %>%
  group_by(month) %>%
  summarise(count_unmet_days = sum(count_unmet_days)) %>%
  mutate(year = 'Total')

## Adding column totals to grid heatmap
grid <- grid +
        geom_point(data = h_total, aes(x = month, y = year, color = count_unmet_days), size = 10, shape = 16) +
        geom_point(data = v_total, aes(x = month, y = year, color = count_unmet_days), size = 10, shape = 16) +
        geom_text(data = h_total, aes(x = month, y = year, label = round(count_unmet_days, 1))) +
        geom_text(data = v_total, aes(x = month, y = year, label = round(count_unmet_days, 1))) +
        scale_color_gradient(low = "yellow", high = "green") +
        labs(col = "Total Count", fill = "Unmet Days Count")
grid



## Creating a function that plots for you given inputs: facilities elid, and the scenario runid
unmet_demand_grid_plot <- function(elid, runid){
  # Loading urls/ sourcing
  site <- "http://deq2.bse.vt.edu/d.dh"    #Specify the site of interest, either d.bet OR d.dh
  #----------------------------------------------
  # Load Libraries
  library(stringr)
  basepath <- '/var/www/R'
  source(paste(basepath,'config.R',sep='/'))
  # Creating data table
  dat <- fn_get_runfile(elid, runid, site= omsite,  cached = FALSE)
  dat <- data.frame(dat)
  modat <- sqldf(" select month months, year years, count(*) count from dat where unmet_demand_mgd > 0 group by month, year")
  modat_0 <- sqldf("SELECT * FROM dat LEFT JOIN modat ON modat.years = dat.year AND modat.months = dat.month group by month, year")
  modat_0 <- sqldf('SELECT month, year, count count_unmet_days FROM modat_0 GROUP BY month, year')
  modat_0[is.na(modat_0)] = 0
  # Creating heatmap grid using ggplot
  ggplot(modat_0, aes(y=year, x=month, fill=count_unmet_days))+ 
    geom_tile() +
    geom_text(aes(label = round(count_unmet_days, 1))) +
    scale_fill_gradient(low = "white", high = "red") +
    theme_bw() +
    labs( title = 'Number of days when there is unmet demand', x = 'Month', y = 'Year') +
    theme(plot.title = element_text(face = 'bold',hjust = 0.5))
}

plot <- unmet_demand_grid_plot(elid, runid)

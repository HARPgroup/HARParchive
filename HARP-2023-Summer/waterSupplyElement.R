################################
#### *** Water Supply Element
################################
# dirs/URLs

#----------------------------------------------
site <- "http://deq1.bse.vt.edu/d.dh"    #Specify the site of interest, either d.bet OR d.dh
#----------------------------------------------
# Load Libraries
basepath='/var/www/R';
source(paste(basepath,'config.R',sep='/'))
save_directory <-  "/var/www/html/data/proj3/out"
library(hydrotools)
# authenticate
ds <- RomDataSource$new(site, rest_uname)
ds$get_token(rest_pw)

# Load Local libs
library(stringr)
library(ggplot2)
library(sqldf)
library(ggnewscale)
library(dplyr)

#Load Smin_CPL function
source(paste0(github_location,"/HARParchive/HARP-2023-Summer/fn_get_pd_min.R"),local = TRUE)

# Read Args
argst <- commandArgs(trailingOnly=T)
pid <- as.integer(argst[1])
elid <- as.integer(argst[2])
runid <- as.integer(argst[3])

finfo <- fn_get_runfile_info(elid, runid,37, site= omsite)
remote_url <- as.character(finfo$remote_url)
dat <- fn_get_runfile(elid, runid, site= omsite,  cached = FALSE)
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
cols <- names(dat)
# does this have an impoundment sub-comp and is imp_off = 0?
# check for local_impoundment, and if so, rename to impoundment for processing
if("local_impoundment" %in% cols) {
  dat$impoundment_use_remain_mg <- dat$local_impoundment_use_remain_mg
  dat$impoundment_max_usable <- dat$local_impoundment_max_usable
  dat$impoundment_Qin <- dat$local_impoundment_Qin
  dat$impoundment_Qout <- dat$local_impoundment_Qout
  dat$impoundment_demand <- dat$local_impoundment_demand
  dat$impoundment <- dat$local_impoundment
  cols <- names(dat)
}
imp_enabled = FALSE
if("impoundment" %in% cols) {
  imp_enabled = TRUE
}
pump_store = FALSE
# rename ps_refill_pump_mgd to refill_pump_mgd
if (!("refill_pump_mgd" %in% cols)) {
  if ("ps_refill_pump_mgd" %in% cols) {
    dat$refill_pump_mgd <- dat$ps_refill_pump_mgd
  }
}
if ("refill_pump_mgd" %in% cols) {
  max_pump <- max(dat$refill_pump_mgd)
  if (max_pump > 0) {
    # this is a pump store
    pump_store = TRUE
  }
}

# yrdat will be used for generating the heatmap with calendar years
yrdat <- dat

yr_sdate <- as.Date(paste0((as.numeric(syear) + 1),"-01-01"))
yr_edate <- as.Date(paste0(eyear,"-12-31"))

yrdat <- window(yrdat, start = yr_sdate, end = yr_edate);

# water year data frame
dat <- window(dat, start = sdate, end = edate);
mode(dat) <- 'numeric'
scen.propname<-paste0('runid_', runid)

# GETTING SCENARIO PROPERTY FROM VA HYDRO
sceninfo <- list(
  varkey = 'om_scenario',
  propname = scen.propname,
  featureid = pid,
  entity_type = "dh_properties",
  bundle = "dh_properties"
)
# newschool
#scenprop <- getProperty(sceninfo, site, scenprop)
scenprop <- RomProperty$new( ds, sceninfo, TRUE)

# POST PROPERTY IF IT IS NOT YET CREATED
if (is.na(scenprop$pid) | is.null(scenprop$pid) ) {
  # create
  scenprop$save(TRUE)
}
vahydro_post_metric_to_scenprop(scenprop$pid, 'external_file', remote_url, 'logfile', NULL, ds)

#omsite = site <- "http://deq2.bse.vt.edu"
#dat <- fn_get_runfile(elid, runid, site= omsite,  cached = FALSE);
#amn <- 10.0 * mean(as.numeric(dat$Qreach))

#dat <- window(dat, start = as.Date("1984-10-01"), end = as.Date("2014-09-30"));
#boxplot(as.numeric(dat$Qreach) ~ dat$year, ylim=c(0,amn))

datdf <- as.data.frame(dat)
modat <- sqldf("select month, avg(base_demand_mgd) as base_demand_mgd from datdf group by month")
#barplot(wd_mgd ~ month, data=modat)
fname <- paste(
  save_directory,paste0('fig.monthly_demand.', elid, '.', runid, '.png'),
  sep = '/'
)
furl <- paste(
  save_url,paste0('fig.monthly_demand.',elid, '.', runid, '.png'),
  sep = '/'
)
png(fname)
barplot(modat$base_demand_mgd ~ modat$month, xlab="Month", ylab="Base Demand (mgd)")
dev.off()
print(paste("Saved file: ", fname, "with URL", furl))
vahydro_post_metric_to_scenprop(scenprop$pid, 'dh_image_file', furl, 'fig.monthly_demand', 0.0, ds)

# Calculate
base_demand_mgd <- mean(as.numeric(dat$base_demand_mgd) )
if (is.na(base_demand_mgd)) {
  base_demand_mgd = 0.0
}
wd_mgd <- mean(as.numeric(dat$wd_mgd) )
if (is.na(wd_mgd)) {
  wd_mgd = 0.0
}
gw_demand_mgd <- mean(as.numeric(dat$gw_demand_mgd) )
if (is.na(gw_demand_mgd)) {
  gw_demand_mgd = 0.0
}
unmet_demand_mgd <- mean(as.numeric(dat$unmet_demand_mgd) )
if (is.na(unmet_demand_mgd)) {
  unmet_demand_mgd = 0.0
}
ps_mgd <- mean(as.numeric(dat$discharge_mgd) )
if (is.na(ps_mgd)) {
  ps_mgd = 0.0
}

# Analyze unmet demands
uds <- zoo(as.numeric(dat$unmet_demand_mgd), order.by = index(dat));
udflows <- group2(uds, 'calendar');

unmet90 <- udflows["90 Day Max"];
ndx = which.max(as.numeric(unmet90[,"90 Day Max"]));
unmet90 = round(udflows[ndx,]$"90 Day Max",6);
unmet30 <- udflows["30 Day Max"];
ndx1 = which.max(as.numeric(unmet30[,"30 Day Max"]));
unmet30 = round(udflows[ndx,]$"30 Day Max",6);
unmet7 <- udflows["7 Day Max"];
ndx = which.max(as.numeric(unmet7[,"7 Day Max"]));
unmet7 = round(udflows[ndx,]$"7 Day Max",6);
unmet1 <- udflows["1 Day Max"];
ndx = which.max(as.numeric(unmet1[,"1 Day Max"]));
unmet1 = round(udflows[ndx,]$"1 Day Max",6);


# post em up'
vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'base_demand_mgd', base_demand_mgd, ds)
vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'base_demand_mgy', base_demand_mgd * 365.0, ds)
vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'wd_mgd', wd_mgd, ds)
vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'wd_mgy', wd_mgd * 365.0, ds)
vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'gw_demand_mgd', gw_demand_mgd, ds)
vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'unmet_demand_mgd', unmet_demand_mgd, ds)
vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'unmet_demand_mgy', unmet_demand_mgd * 365.0, ds)
vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'ps_mgd', ps_mgd, ds)
vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'unmet90_mgd', unmet90, ds)
vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'unmet30_mgd', unmet30, ds)
vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'unmet7_mgd', unmet7, ds)
vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'unmet1_mgd', unmet1, ds)

# Intake Flows
iflows <- zoo(as.numeric(dat$Qintake), order.by = index(dat));
uiflows <- group2(iflows, 'calendar')
Qin30 <- uiflows["30 Day Min"];
l30_Qintake <- min(Qin30["30 Day Min"]);
vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'l30_Qintake', l30_Qintake, ds)

# Define year at which highest 30 Day Max occurs (Lal's code, line 405)
#defines critical period based on Qintake if there is no unmet demand
if (sum(datdf$unmet_demand_mgd)==0) {
  # base it on flow since we have no unmet demand.
  ndx1 = which.min(as.numeric(Qin30[,"30 Day Min"]))
  u30_year2 = uiflows[ndx1,]$"year";
} else {
  u30_year2 = udflows[ndx1,]$"year";
}

# Metrics that need Zoo (IHA)
flows <- zoo(as.numeric(as.character( dat$Qintake )), order.by = index(dat));
loflows <- group2(flows);
l90 <- loflows["90 Day Min"];
ndx = which.min(as.numeric(l90[,"90 Day Min"]));
l90_Qout = round(loflows[ndx,]$"90 Day Min",6);
l90_year = loflows[ndx,]$"year";

# Find l30_year for Smin_L30
l30 <- loflows["30 Day Min"];
ndx = which.min(as.numeric(l30[,"30 Day Min"]));
l30_Qout = round(loflows[ndx,]$"30 Day Min",6);
l30_year = loflows[ndx,]$"year";

# Smin_CPL metrics

# Prep for Smin_CPL function
start_date_30 <- paste0(l30_year,"-01-01") # Dates for l90_year
end_date_30 <- paste0(l30_year,"-12-31")

start_date_90 <- paste0(l90_year,"-01-01") # Dates for l30_year
end_date_90 <- paste0(l90_year,"-12-31")

# Storage col could be a couple different names 
if("local_impoundment_Storage" %in% cols) {
  storagecol <- "local_impoundment_Storage"
}
if("impoundment_Storage" %in% cols) {
  storagecol <- "impoundment_Storage"
}

# Calculate Smin_CPLs using function
Smin_L30_acft <- fn_get_pd_min(ts_data = dat, critical_pd_length = 30,
                               start_date = start_date_30, end_date = end_date_30,
                               colname = storagecol)

Smin_L90_acft <- fn_get_pd_min(ts_data = dat, critical_pd_length = 90,
                               start_date = start_date_90, end_date = end_date_90,
                               colname = storagecol)

# Convert from from ac-ft to mg: 1 mg = 3.069 acre-feet
Smin_L30_mg <- round(Smin_L30_acft/3.069, digits = 3)
Smin_L90_mg <- round(Smin_L90_acft/3.069, digits = 3)

# Set Smin metrics to 0 if impoundment is not active
#imp_enabled used in this script instead of imp_off, and set to either TRUE or FALSE
if (imp_enabled == FALSE) { 
  Smin_L30_mg <- 0
  Smin_L90_mg <- 0
}

# Post Smin metrics to vahydro
vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'Smin_L30_mg', Smin_L30_mg, ds)
vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'Smin_L90_mg', Smin_L90_mg, ds)

##### Define fname before graphing
# hydroImpoundment lines 144-151

fname <- paste(
  save_directory,
  paste0(
    'fig.30daymax_unmet.',
    elid, '.', runid, '.png'
  ),
  sep = '/'
)

furl <- paste(
  save_url,
  paste0(
    'fig.30daymax_unmet.',
    elid, '.', runid, '.png'
  ),
  sep = '/'
)

#png(fname)

##### Define data for graph, just within that defined year, and graph it
# Lal's code, lines 410-446 (412 commented out)
if (sum(datdf$unmet_demand_mgd)==0) {
  # base it on flow since we have no unmet demand.
  dsql <- paste(
    "select min(month) as dsmo, max(month) as demo
     from datdf
     where Qintake <= ", l30_Qintake,
    " and year = ",
    u30_year2
  )
} else {
  dsql <- paste(
    "select min(month) as dsmo, max(month) as demo
     from datdf
     where unmet_demand_mgd > 0
     and year = ",
    u30_year2
  )
}
drange <- sqldf(dsql)
# Drought range dates
dsy <- u30_year2
dey <- u30_year2
dsmo <- as.integer(drange$dsmo) - 1
demo <- as.integer(drange$demo) + 1
if (dsmo < 1) {
  dsmo <- 12 + dsmo
  dsy <- dsy - 1
}
if (demo > 12) {
  demo <- demo - 12
  dey <- dey + 1
}
dsmo <- sprintf('%02i',dsmo)
demo <- sprintf('%02i',demo)
ddat2 <- window(
  dat,
  start = as.Date(paste0(dsy, "-", dsmo, "-01")),
  end = as.Date(paste0(dey,"-", demo, "-28") )
);

#dmx2 = max(ddat2$Qintake)
if (pump_store || !imp_enabled) {
  flow_ts <- ddat2$Qintake
  flow_ts_name = "Source Stream"
} else {
  flow_ts <- ddat2$impoundment_Qin
  flow_ts_name = "Inflow"
}

png(fname)
par(mar = c(5,5,2,5))
plot(
  flow_ts,
  xlab=paste0("Critical Period: ",u30_year2),
  ylim=c(0,max(flow_ts)),
  col="blue"
)
par(new = TRUE)
plot(
  ddat2$base_demand_mgd,col='green',
  xlab="",
  ylab="",
  axes=FALSE,
  ylim=c(0,max(ddat2$base_demand_mgd))
)
lines(ddat2$unmet_demand_mgd,col='red')
axis(side = 4)
mtext(side = 4, line = 3, 'Base/Unmet Demand (mgd)')
legend("topleft", c(flow_ts_name,"Base Demand","Unmet"),
       col = c("blue", "green","red"),
       lty = c(1,1,1,1),
       bg='white',cex=0.8) #ADD LEGEND
dev.off()

# map2<-as.data.frame(ddat2$Qintake + (ddat2$discharge_mgd - ddat2$wd_mgd) * 1.547)
# colnames(map2)<-"flow"
# map2$date <- rownames(map2)
# map2$base_demand_mgd<-ddat2$base_demand_mgd * 1.547
# map2$unmetdemand<-ddat2$unmet_demand_mgd * 1.547
# df <- data.frame(as.Date(map2$date), map2$flow, map2$base_demand_mgd,map2$unmetdemand);
# colnames(df)<-c("date","flow","base_demand_mgd","unmetdemand")
# #options(scipen=5, width = 1400, height = 950)
# ggplot(df, aes(x=date)) +
#   geom_line(aes(y=flow, color="Flow"), size=0.5) +
#   geom_line(aes(y=base_demand_mgd, colour="Base demand"), size=0.5)+
#   geom_line(aes(y=unmetdemand, colour="Unmet demand"), size=0.5)+
#   theme_bw()+
#   theme(legend.position="top",
#         legend.title=element_blank(),
#         legend.box = "horizontal",
#         legend.background = element_rect(fill="white",
#                                          size=0.5, linetype="solid",
#                                          colour ="white"),
#         legend.text=element_text(size=12),
#         axis.text=element_text(size=12, color = "black"),
#         axis.title=element_text(size=14, color="black"),
#         axis.line = element_line(color = "black",
#                                  size = 0.5, linetype = "solid"),
#         axis.ticks = element_line(color="black"),
#         panel.grid.major=element_line(color = "light grey"),
#         panel.grid.minor=element_blank())+
#   scale_colour_manual(values=c("purple","black","blue"))+
#   guides(colour = guide_legend(override.aes = list(size=5)))+
#   labs(y = "Flow (cfs)", x= paste("Critical Period:",u30_year2, sep=' '))
# #dev.off()
# print(fname)
# ggsave(fname,width=7,height=4.75)

##### Naming for saving and posting to VAHydro

print(paste("Saved file: ", fname, "with URL", furl))

vahydro_post_metric_to_scenprop(scenprop$pid, 'dh_image_file', furl, 'fig.30daymax_unmet', 0.0, ds)


##### HEATMAP
# includes code needed for both the heatmap with counts and heatmap with counts and averages

# Uses dat2 for heatmap calendar years
# make numeric versions of syear and eyear
num_syear <- as.numeric(syear) + 1
num_eyear <- as.numeric(eyear)

mode(yrdat) <- 'numeric'

yrdatdf <- as.data.frame(yrdat)

yrmodat <- sqldf("SELECT month months, year years,sum(unmet_demand_mgd) sum_unmet, count(*) count from yrdatdf where unmet_demand_mgd > 0
group by month, year") #Counts sum of unmet_days by month and year
#converts unmet_mgd sums to averages for cells
yrmodat$avg_unmet <- yrmodat$sum_unmet / yrmodat$count

#Join counts with original data frame to get missing month and year combos then selects just count month and year
yrmodat <- sqldf("SELECT * FROM yrdatdf LEFT JOIN yrmodat ON yrmodat.years = yrdatdf.year AND yrmodat.months = yrdatdf.month group by month, year")
yrmodat <- sqldf('SELECT month, year, avg_unmet, count count_unmet_days FROM yrmodat GROUP BY month, year')

#Replace NA for count with 0s
yrmodat[is.na(yrmodat)] = 0

########################################################### Calculating Totals
# monthly totals via sqldf
mosum <- sqldf("SELECT  month, sum(count_unmet_days) count_unmet_days FROM yrmodat GROUP BY month")
mosum$year <- rep(num_eyear+1,12)

#JK addition 3/25/22: Cell of total days unmet in simulation period
total_unmet_days <- sum(yrmodat$count_unmet_days)
total_unmet_days_cell <- data.frame("month" = 13,
                                    "count_unmet_days" = as.numeric(total_unmet_days),
                                    "year" = num_eyear+1)

#yearly sum
yesum <-  sqldf("SELECT year, sum(count_unmet_days) count_unmet_days FROM yrmodat GROUP BY year")
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


############################################################### Plot and Save count heatmap
# If loop makes sure plots are green if there is no unmet demand
if (sum(mosum$count_unmet_days) == 0) {
  count_grid <- ggplot() +
    geom_tile(data=yrmodat, color='black',aes(x = month, y = year, fill = count_unmet_days)) +
    geom_text(aes(label=yrmodat$count_unmet_days, x=yrmodat$month, y= yrmodat$year), size = 3.5, colour = "black") +
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
  
  total <- unmet + new_scale_fill() +
    geom_tile(data = total_unmet_days_cell, color='black',fill="grey",aes(x = month, y = year, fill = count_unmet_days)) +
    geom_text(data = total_unmet_days_cell, size = 3.5, color='black', aes(x = month, y = year, label = count_unmet_days))
  
  #unmet_avg <- unmet + new_scale_fill()+
  unmet_avg <- total + new_scale_fill()+
    geom_tile(data = yeavg, color='black', aes(x = month, y = year, fill = avg)) +
    geom_tile(data = moavg, color='black', aes(x = month, y = year, fill = avg)) +
    geom_text(data = yeavg, size = 3.5, color='black', aes(x = month, y = year, label = avg)) +
    geom_text(data = moavg, size = 3.5, color='black', aes(x = month, y = year, label = avg))+
    scale_fill_gradient2(low = "#FFF8DC", mid = "#FFF8DC", high ="#FFF8DC",
                         name= 'Average Unmet Days', midpoint = mean(yeavg$avg))
} else{
  count_grid <- ggplot() +
    geom_tile(data=yrmodat, color='black',aes(x = month, y = year, fill = count_unmet_days)) +
    geom_text(aes(label=yrmodat$count_unmet_days, x=yrmodat$month, y= yrmodat$year), size = 3.5, colour = "black") +
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
  
  total <- unmet + new_scale_fill() +
    geom_tile(data = total_unmet_days_cell, color='black',fill="grey",aes(x = month, y = year, fill = count_unmet_days)) +
    geom_text(data = total_unmet_days_cell, size = 3.5, color='black', aes(x = month, y = year, label = count_unmet_days))
  
  #unmet_avg <- unmet + new_scale_fill()+
  unmet_avg <- total + new_scale_fill()+  
    geom_tile(data = yeavg, color='black', aes(x = month, y = year, fill = avg)) +
    geom_tile(data = moavg, color='black', aes(x = month, y = year, fill = avg)) +
    geom_text(data = yeavg, size = 3.5, color='black', aes(x = month, y = year, label = avg)) +
    geom_text(data = moavg, size = 3.5, color='black', aes(x = month, y = year, label = avg))+
    scale_fill_gradient2(low = "#FFF8DC", mid = "#FFDEAD", high ="#DEB887",
                         name= 'Average Unmet Days', midpoint = mean(yeavg$avg))
  
}


fname2 <- paste(save_directory,paste0('fig.unmet_heatmap.',elid, '.', runid, '.png'),sep = '/')

furl2 <- paste(save_url, paste0('fig.unmet_heatmap.',elid, '.', runid, '.png'),sep = '/')

ggsave(fname2,plot = unmet_avg, width= 7, height=7)

print(paste('File saved to save_directory:', fname2))

vahydro_post_metric_to_scenprop(scenprop$pid, 'dh_image_file', furl2, 'fig.unmet_heatmap', 0.0, ds)

###################################### Plot and save Second unmet Demand Grid
# contains count/ Avg unmet demand mgd
if (sum(mosum$count_unmet_days) == 0) {
  count_grid <- ggplot() +
    geom_tile(data=yrmodat, color='black',aes(x = month, y = year, fill = count_unmet_days)) +
    geom_text(aes(label=paste(yrmodat$count_unmet_days,' / ',round(yrmodat$avg_unmet,1), sep=''),
                  x=yrmodat$month, y= yrmodat$year), size = 3.5, colour = "black") +
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
  
  total <- unmet + new_scale_fill() +
    geom_tile(data = total_unmet_days_cell, color='black',fill="grey",aes(x = month, y = year, fill = count_unmet_days)) +
    geom_text(data = total_unmet_days_cell, size = 3.5, color='black', aes(x = month, y = year, label = count_unmet_days))
  
  #unmet_avg <- unmet + new_scale_fill()+
  unmet_avg <- total + new_scale_fill()+
    geom_tile(data = yeavg, color='black', aes(x = month, y = year, fill = avg)) +
    geom_tile(data = moavg, color='black', aes(x = month, y = year, fill = avg)) +
    geom_text(data = yeavg, size = 3.5, color='black', aes(x = month, y = year, label = avg)) +
    geom_text(data = moavg, size = 3.5, color='black', aes(x = month, y = year, label = avg))+
    scale_fill_gradient2(low = "#FFF8DC", mid = "#FFF8DC", high ="#FFF8DC",
                         name= 'Average Unmet Days', midpoint = mean(yeavg$avg))
} else{
  count_grid <- ggplot() +
    geom_tile(data=yrmodat, color='black',aes(x = month, y = year, fill = count_unmet_days)) +
    geom_text(aes(label=paste(yrmodat$count_unmet_days,' / ',signif(yrmodat$avg_unmet,digits=1), sep=''),
                  x=yrmodat$month, y= yrmodat$year), size = 3, colour = "black") +
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
  
  total <- unmet + new_scale_fill() +
    geom_tile(data = total_unmet_days_cell, color='black',fill="grey",aes(x = month, y = year, fill = count_unmet_days)) +
    geom_text(data = total_unmet_days_cell, size = 3.5, color='black', aes(x = month, y = year, label = count_unmet_days))
  
  #unmet_avg <- unmet + new_scale_fill()+
  unmet_avg <- total + new_scale_fill()+ 
    geom_tile(data = yeavg, color='black', aes(x = month, y = year, fill = avg)) +
    geom_tile(data = moavg, color='black', aes(x = month, y = year, fill = avg)) +
    geom_text(data = yeavg, size = 3.5, color='black', aes(x = month, y = year, label = avg)) +
    geom_text(data = moavg, size = 3.5, color='black', aes(x = month, y = year, label = avg))+
    scale_fill_gradient2(low = "#FFF8DC", mid = "#FFDEAD", high ="#DEB887",
                         name= 'Average Unmet Days', midpoint = mean(yeavg$avg))
  
}

fname3 <- paste(save_directory,paste0('fig.unmet_heatmap_amt.',elid,'.',runid ,'.png'),sep = '/')

furl3 <- paste(save_url, paste0('fig.unmet_heatmap_amt.',elid, '.', runid, '.png'),sep = '/')

ggsave(fname3,plot = unmet_avg, width= 9.5, height=6)

print('File saved to save_directory')

vahydro_post_metric_to_scenprop(scenprop$pid, 'dh_image_file', furl3, 'fig.unmet_heatmap_amt', 0.0, ds)

if("impoundment" %in% cols) {
  # Plot and analyze impoundment sub-comps
  dat$storage_pct <- as.numeric(dat$impoundment_use_remain_mg) * 3.07 / as.numeric(dat$impoundment_max_usable)
  #set the storage percent
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
  # post em up
  vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'usable_pct_p0', usable_pct_p0, ds)
  vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'usable_pct_p10', usable_pct_p10, ds)
  vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'usable_pct_p50', usable_pct_p50, ds)
  
  
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
  ymn <- 0
  ymx <- 100
  
  par(mar = c(5,5,2,5))
  plot(
    datpd$storage_pct * 100.0,
    ylim=c(ymn,ymx),
    main="Minimum Modeled Reservoir Storage Period",
    ylab="Reservoir Storage (%)",
    xlab=paste("Model Time Period",pdstart,"to",pdend)
  )
  par(new = TRUE)
  if (pump_store) {
    flow_ts <- datpd$Qreach
  } else {
    flow_ts <- datpd$impoundment_Qin
  }
  plot(flow_ts,col='blue', axes=FALSE, xlab="", ylab="")
  lines(datpd$Qout,col='green')
  lines(datpd$wd_mgd * 1.547,col='red')
  axis(side = 4)
  mtext(side = 4, line = 3, 'Flow/Demand (cfs)')
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
  par(mar = c(5,5,2,5))
  par(mar = c(1,5,2,5),mfrow = c(2,1))
  plot(
    datpd$storage_pct * 100.0,
    ylim=c(0,100),
    ylab="Reservoir Storage (%)",
    xlab="",
    main=paste("Storage and Flows",sdate,"to",edate)
  )
  ymx <- ceiling(
    pmax(
      max(datpd$Qreach)
    )
  )
  # if this is a pump store, refill_pump_mgd > 0
  # then, plot Qreach first, overlaying impoundment_Qin
  if (pump_store) {
    flow_ts <- datpd$Qreach
  } else {
    flow_ts <- datpd$impoundment_Qin
  }
  plot(
    flow_ts,
    col='blue',
    xlab="",
    ylab='Flow/Demand (cfs)',
    #ylim=c(0,ymx),
    log="y",
    yaxt="n" # supress labeling till we format
  )
  #legend()
  y_ticks <- axTicks(2)
  y_ticks_fmt <- format(y_ticks, scientific = FALSE)
  axis(2, at = y_ticks, labels = y_ticks_fmt)
  ymx <- ceiling(
    pmax(
      max(datpd$refill_pump_mgd),
      max(datpd$impoundment_demand * 1.547)
    )
  )
  #par(new = TRUE)
  #plot(datpd$refill_pump_mgd * 1.547,col='green',xlab="",ylab="")
  lines(datpd$refill_pump_mgd * 1.547,col='red')
  lines(datpd$impoundment_demand * 1.547,col='green')
  #axis(side = 4)
  #mtext(side = 4, line = 3, 'Flow/Demand (cfs)')
  
  dev.off()
  print(paste("Saved file: ", fname, "with URL", furl))
  vahydro_post_metric_to_scenprop(scenprop$pid, 'dh_image_file', furl, 'fig.l90_imp_storage.2yr', 0.0, ds)
  
  # All Periods
  # this has an impoundment.  Plot it up.
  # Now zoom in on critical drought period
  datpd <- dat
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
  ymn <- 0
  ymx <- 100
  par(mar = c(5,5,2,5))
  par(mar = c(1,5,2,5),mfrow = c(2,1))
  plot(
    datpd$storage_pct * 100.0,
    ylim=c(0,100),
    ylab="Reservoir Storage (%)",
    xlab="",
    main=paste("Storage and Flows",sdate,"to",edate)
  )
  ymx <- ceiling(
    pmax(
      max(datpd$Qreach)
    )
  )
  # if this is a pump store, refill_pump_mgd > 0
  # then, plot Qreach first, overlaying impoundment_Qin
  if (pump_store) {
    flow_ts <- datpd$Qreach
  } else {
    flow_ts <- datpd$impoundment_Qin
  }
  plot(
    flow_ts,
    col='blue',
    xlab="",
    ylab='Flow/Demand (cfs)',
    #ylim=c(0,ymx),
    log="y",
    yaxt="n" # supress labeling till we format
  )
  y_ticks <- axTicks(2)
  y_ticks_fmt <- format(y_ticks, scientific = FALSE)
  axis(2, at = y_ticks, labels = y_ticks_fmt)
  ymx <- ceiling(
    pmax(
      max(datpd$refill_pump_mgd),
      max(datpd$impoundment_demand * 1.547)
    )
  )
  #par(new = TRUE)
  #plot(datpd$refill_pump_mgd * 1.547,col='green',xlab="",ylab="")
  if (pump_store) {
    lines(datpd$refill_pump_mgd * 1.547,col='red')
  }
  lines(datpd$impoundment_demand * 1.547,col='green')
  #axis(side = 4)
  #mtext(side = 4, line = 3, 'Flow/Demand (cfs)')
  
  dev.off()
  print(paste("Saved file: ", fname, "with URL", furl))
  vahydro_post_metric_to_scenprop(scenprop$pid, 'dh_image_file', furl, 'fig.imp_storage.all', 0.0, ds)
  
  # Low Elevation Period
  # Dat for Critical Period
  elevs <- zoo(dat$storage_pct, order.by = index(dat));
  loelevs <- group2(elevs);
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
  par(mar = c(5,5,2,5))
  plot(
    datpd$storage_pct * 100.0,
    ylim=c(ymn,ymx),
    main="Summer/Fall of L-90 Period",
    ylab="Reservoir Storage (%)",
    xlab=paste("Model Time Period",l90_elev_start,"to",l90_elev_end)
  )
  par(new = TRUE)
  if (pump_store) {
    flow_ts <- datpd$Qreach
  } else {
    flow_ts <- datpd$impoundment_Qin
  }
  plot(flow_ts,col='blue', axes=FALSE, xlab="", ylab="")
  lines(datpd$Qout,col='green')
  lines(datpd$wd_mgd * 1.547,col='red')
  axis(side = 4)
  mtext(side = 4, line = 3, 'Flow/Demand (cfs)')
  dev.off()
  print(paste("Saved file: ", fname, "with URL", furl))
  vahydro_post_metric_to_scenprop(scenprop$pid, 'dh_image_file', furl, 'elev90_imp_storage.all', 0.0, ds)
  
}
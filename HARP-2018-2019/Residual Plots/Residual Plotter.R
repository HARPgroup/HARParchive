# USER INPUTS -----

library(ggplot2)

# Address of "DEQ_Model_vs_USGS_Comparison" folder
# Include "DEQ_Model_vs_USGS_Comparison" in address!

container <-'C:\\Users\\Daniel\\Documents\\HARP\\GitHub\\hydro-tools\\HARP-2018\\DEQ_Model_vs_USGS_Comparison';

# Should new or original data be used?
new.or.original <- "new"

# CARRYOVER IF MASTER IS BEING RUN ----------------------------------------
if (exists("container.master") == TRUE) {
  container <- container.master
  siteNo <- siteNo.master
  new.or.original <- new.or.original.master
}

# NEW OR ORIGINAL DATA SWITCH ---------------------------------------------

if (new.or.original == "new") {
  container.cont <- "\\spatial_analysis\\user's_results\\"
} else if (new.or.original == "original") {
  container.cont <- "\\spatial_analysis\\harp_analysts'_results\\"
} else {
  print("ERROR: neither new or original data specified")
}

site <- "http://deq2.bse.vt.edu/d.bet"    #Specify the site of interest, either d.bet OR d.dh

basepath='C:\\Users\\Daniel\\Documents\\HARP\\GitHub\\hydro-tools';

# SETUP

source(paste(basepath,'config.local.private',sep='/'));
source(paste(hydro_tools,"VAHydro-2.0/rest_functions.R", sep = "/")); 
source(paste(hydro_tools,"VAHydro-1.0/fn_vahydro-1.0.R", sep = "/"));  
source(paste(hydro_tools,"LowFlow/fn_iha.R", sep = "/"));
#retrieve rest token
source(paste(hydro_tools,"auth.private", sep = "/"));#load rest username and password, contained in auth.private file
token <- rest_token(site, token, rest_uname, rest_pw);
options(timeout=120); # set timeout to twice default level to avoid abort due to high traffic

# IMPORT DATA -----
#The projection that you want the map to be output in requires a Proj4 code. 
#Search the projection you want (in this case WGS 1984 was used becuase it is compatible with VA Hydro) and choose the link for 'spatialreference.org'
#Once on the website click on the Proj4 link and copy and paste the provided line in quotes after its associated variable name 

#Importing desired data
#The desired data for this code is the metrics that have been run for the river segments located outside of the Cheasapeake Bay Watershed
#Metrics<- read.csv(paste0(container,'\\results\\all.segments.pct.error.csv'))    #Pull the (location of desired data table)
GageMetrics <- read.csv(file=paste(container,"\\spatial_analysis\\user's_results\\all.gage-metrics.pct.error.csv",sep=""), header=TRUE, sep=",")    #Pull the (location of desired data table)
ModelMetrics <- read.csv(file=paste(container,"\\spatial_analysis\\user's_results\\all.model-metrics.pct.error.csv",sep=""), header=TRUE, sep=",")    #Pull the (location of desired data table)
GageToSeg <- read.csv(file=paste0(container,'\\data\\SPLITSEG_Gage_To_Segment.csv'))

#Determining number of metrics
num.metrics <- length(GageMetrics)
num.segs <- length(GageToSeg$river_segment)

library(dataRetrieval)

for (i in 2: num.metrics) {
  metricname <- as.vector(colnames(GageMetrics))
  metricname <- metricname[i]
  all.residuals.and.areas <- data.frame(matrix(NA, nrow = num.segs, ncol = 2))
  colnames(all.residuals.and.areas) <- c('area', 'residual')
  
  for (j in 1: num.segs) {
    gage <- paste0('0', GageToSeg[j, 1])
    area <- readNWISsite(gage)
    area <- area$drain_area_va
    all.residuals.and.areas[j, 1] <- area
    seggagedata <- as.numeric(GageMetrics[j,i])
    segmodeldata <- as.numeric(ModelMetrics[j,i])
    residual <- segmodeldata - seggagedata
    all.residuals.and.areas[j, 2] <- residual
  }
  #jpeg(paste0(metricname, '.jpg'))
  #plot(all.residuals.and.areas$area, all.residuals.and.areas$residual, main = metricname, xlab = 'Drainage Area', ylab = 'Residual (Model-Gage)')
  plotname <- paste0(metricname, '.jpg')
  ggplot(data = all.residuals.and.areas, aes(x=area, y=residual)) + 
    geom_point() + 
    geom_smooth(method = lm) + 
    xlab('Drainage Area (Sq. Mile)') +
    ylab('Flow, Model-Gage (cfs)') +
    labs(title = metricname)
  ggsave(plotname)
  #dev.off()
}

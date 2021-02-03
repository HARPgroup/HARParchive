rm(list=ls())
library(httr);
save_directory <- "/var/www/html/files/fe/plots"
#----------------------------------------------
site <- "http://deq2.bse.vt.edu/d.bet"    #Specify the site of interest, either d.bet OR d.dh
#----------------------------------------------
# Load Libraries
basepath='C:\\Users\\HaileyMae\\Documents\\GitHub\\hydro-tools';
source(paste(basepath,'config.local.private',sep='/'));
source(paste(hydro_tools,"VAHydro-2.0/rest_functions.R", sep = "/")); 
source(paste(hydro_tools,"VAHydro-1.0/fn_vahydro-1.0.R", sep = "/"));  
source(paste(hydro_tools,"LowFlow/fn_iha.R", sep = "/"));  
#retrieve rest token
source(paste(hydro_tools,"auth.private", sep = "/"));#load rest username and password, contained in auth.private file
token <- rest_token(site, token, rest_uname, rest_pw);
options(timeout=120); # set timeout to twice default level to avoid abort due to high traffic

model_reader <- 'https://docs.google.com/spreadsheets/d/e/2PACX-1vRY5BkQ0Ha269jchgbuZYaND5_rxaSTV1rBoW6dDtr7NnlrCIcMaODmST-WAfSzUwUWwOuv3ghVUwNH/pub?output=csv'
model_segs = read.csv(model_reader, header = TRUE, sep = ",", stringsAsFactors = FALSE);
model_segs$Gage <- as.character(model_segs$GageID)

# in the line below, change the model_segs$...Metric of interest (the second one)
metric <- data.frame(model_segs$Gage, model_segs$FeatureID,model_segs$USGSArea) 
colnames(metric) <- c('Gage', 'FeatureID','Metric')
i <- 1

for (i in 1:nrow(metric)){
  riverseg = as.character(paste0("0",metric$Gage[i]));
  hydrocode = paste0("usgs_0",model_segs$Gage[i]);
  ftype = 'flowgage'; # nhd_huc8, nhd_huc10, vahydro
  inputs <- list (
    hydrocode = hydrocode,
    bundle = 'usgsgage'
    )
  #property dataframe returned
  feature = FALSE;
  odata <- getFeature(inputs, token, site, feature);
  hydroid <- odata[1,"hydroid"];
  fname <- as.character(odata[1,]$name );
  print(paste("Retrieved hydroid",hydroid,"for", fname,riverseg, sep=' '));
  
  # get the p5.3.2, scenario  model segment attached to this river feature
  #inputs <- list(
   # varkey = "om_model_element",
    #featureid = hydroid,
    #entity_type = "dh_feature",
    #propcode = "p532cal_062211"
 # )
  
#  model <- getProperty(inputs, site, model)
  
  # now, retrieve august low flow property if set
  alfinfo <- list(
    varkey = "wshed_drainage_area_sqmi",      # chane this line
    #propcode = 'p532cal_062211',                 # change this line 
    featureid =  as.integer(as.character(metric$FeatureID[i])),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  # this just sets our property to the 
  if (identical(alfprop, FALSE)) {
    # create
    alfprop = alfinfo
  }
  alfprop$propname = 'Drainage Area' #change this line 
  #alfprop$propcode = 'p532cal_062211'                #change this line 
  alfprop$propvalue =metric$Metric[i]
  alfinfo$startdate = format(as.POSIXlt('1984-01-01'),"%s") 
  alfinfo$enddate = format(as.POSIXlt('2005-12-31'),"%s")
  alfprop$pid = NULL
  postProperty(alfprop,fxn_locations,base_url = site,alfprop) 
  paste("finished segment ", i, " of ", nrow(metric))
  i <- i + 1
}
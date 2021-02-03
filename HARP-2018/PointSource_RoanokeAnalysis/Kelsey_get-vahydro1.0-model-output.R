library(pander);
library(httr);
library(hydroTSM);
save_directory <- "/var/www/html/files/fe/plots"
#----------------------------------------------
site <- "http://deq2.bse.vt.edu/d.dh"    #Specify the site of interest, either d.bet OR d.dh
#----------------------------------------------
# Load Libraries
basepath='/var/www/R';
basepath = 'C:/Users/Kelsey/Desktop/GitHub/hydro-tools';
source(paste(basepath,'config.local.private',sep='/'));
source(paste(hydro_tools,"VAHydro-2.0/rest_functions.R", sep = "/")); 
source(paste(hydro_tools,"VAHydro-1.0/fn_vahydro-1.0.R", sep = "/"));  
source(paste(hydro_tools,"LowFlow/fn_iha.R", sep = "/"));  
#retrieve rest token - DISABLED
#fxn_locations <-  '/usr/local/home/git/r-dh-ecohydro/ELFGEN';
#source(paste(fxn_locations,"elf_rest_token.R", sep = "/"));   
#elf_rest_token (site, token)
# to run in knit'r, need to preload token
#token = 'W-THcwwvstkINd9NIeEMrmNRls-8kVs16mMEcN_-jOA';

#Generate REST token              
rest_uname = FALSE
rest_pw = FALSE
source(paste(hydro_tools,"auth.private", sep = "\\")); #load rest username and password, contained in auth.private file
source(paste(hydro_tools,"VAHydro-2.0","rest_functions.R", sep = "\\")) #load REST functions
token <- rest_token(site, token, rest_uname, rest_pw)
options(timeout=1200); # set timeout to twice default level to avoid abort due to high traffic




hydrocode = "vahydrosw_wshed_OR2_8130_7900";
ftype = 'vahydro'; # nhd_huc8, nhd_huc10, vahydro
inputs <- list (
  hydrocode = hydrocode,
  bundle = 'watershed',
  ftype = 'vahydro'
)
#property dataframe returned
feature = FALSE;
odata <- getFeature(inputs, token, site, feature);
# Ex: flows <- fn_get_rundata(207885, 402);
#     fn_iha_7q10(flows);
# Get data frame for stashing multirun data
stash <- data.frame();
mostash <- data.frame();
tsstash = FALSE;
featureid <- odata[1,"hydroid"];
fname <- as.character(odata[1,]$name );
inputs <- list(
  varkey = "wshed_local_area_sqmi",
  featureid = featureid,
  entity_type = "dh_feature"
)



da <- getProperty(inputs, site, model) #model has not been specified anywhere above

inputs <- list(
  varkey = "om_model_element",
  featureid = featureid,
  entity_type = "dh_feature",
  propcode = "vahydro-1.0"
)
model <- getProperty(inputs, site, model)
mid = as.numeric(as.character(model[1,]$pid))
inputs <- list(
  varkey = "om_element_connection",
  featureid = mid,
  entity_type = "dh_properties"
)
prop <- getProperty(inputs, site, prop)
elid = as.numeric(as.character(prop[1,]$propvalue))

# Analsyis config
#runids = c(20021,20023);
#runids = c(20051,20054);
runid = 2 #original id was 1
targetyear = 2005;
eventstart = paste(targetyear,"-02-15",sep='')
eventend = paste(targetyear,"-06-15",sep='')
bigQ = 50;

wshed_summary_tbl = data.frame(
  "Run ID" = character(), 
  "Segment Name (D. Area)" = character(), 
  "7Q10/ALF/LF-90" = character(), 
  "WD (mean/max)" = character(), 
  stringsAsFactors = FALSE) ;
#pander(odata);
#pander(odata);

omsite = site <- "http://deq2.bse.vt.edu"
dat <- fn_get_runfile(elid, runid, site= omsite,  cached = TRUE);

dat <- window(dat, start = as.Date("1984-10-01"), end = as.Date("2005-09-30"))

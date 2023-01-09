# This script will convert the pwater csv to a data table and perform analysis & generate graphs 
#install.packages("IHA", repos="http://R-Forge.R-project.org")
#install_github("HARPGroup/hydro-tools", force=TRUE)
basepath='/var/www/R';
source("/var/www/R/config.R")
suppressPackageStartupMessages(library(zoo))

#message(R_TempDir)

# establishing location on server for storing images
omsite = "http://deq1.bse.vt.edu:81"
#landuse <- 'for' # needs to be commented when running on the server 
#land_segment_name <- 'A51019' # need to comment before using on server 
#scenario_name <- 'hsp2_2022'# need to comment before using on server 

# Accepting command arguments:
argst <- commandArgs(trailingOnly = T)
element_name <- argst[1]
element_code <- argst[2]
bundle <- argst[3]
ftype=argst[4]
model_version <- argst[5]

# Set up our data source
ds <- RomDataSource$new(site, rest_uname = rest_uname)
ds$get_token(rest_pw)

# TBD: get inputs from the comand line
#  For now we just load some samples
feature<- RomFeature$new(
  ds,
  list(
    hydrocode=element_code, 
    ftype=ftype,
    bundle=bundle
  ), 
  TRUE
)

model <- RomProperty$new(
  ds,
  list(
# try omitting this
#    varkey="om_model_element", 
    featureid=feature$hydroid, 
    entity_type="dh_feature", 
    propcode=model_version 
  ), 
  TRUE
)
if (!(model$pid > 0)) {
  model$save(TRUE)
}

print($model$pid)

# Insert info about this event into database
# attach a property showing the path to the original raster file
# attach a property with a web URL to the origina raster file
basepath='/var/www/R';
source("/var/www/R/config.R")
library("lubridate")
library("hydrotools")
# Set up our data source
source(paste0(HARParchive_location,"/HARP-2024-2025/basic_analytics_for_vahydro.R"))
       
ds <- RomDataSource$new(site, rest_uname = rest_uname)
ds$get_token(rest_pw)

# Accepting command arguments:
argst <- commandArgs(trailingOnly = T)
if (length(argst) < 7) {
  
  message("Use: Rscript met_store_info.R $ddate datasource coverage_hydrocode coverage_bundle coverage_ftype model_version met_file")
  message("Ex: Rscript met_store_info.R $ddate nldas2 N5113 landunit cbp6_landseg met_file")
  q('n')
}
ddate <- argst[1]
scenario_name <- argst[2]
coverage_hydrocode <- argst[3]
coverage_bundle <- argst[4]
coverage_ftype <- argst[5]
model_version <- argst[6] 
met_file <- argst[7] 
# load the feature -- get feature hydroid
# find the dh_timeseries_weather record for this event
# attach an image property to the record
# return 
  message(paste("Searching for feature hydrocode=", coverage_hydrocode,"with ftype",coverage_ftype))
feature <- RomFeature$new(
  ds,
  list(
    hydrocode=coverage_hydrocode,
    ftype=coverage_ftype,
    bundle=coverage_bundle
  ),
  TRUE
)
# this will create or retrieve a model scenario to house this summary data.
model <- om_model_object(ds, feature, model_version)
# if a matching model does not exist, this will go ahead and create one
scenario <- om_get_model_scenario(ds, model, scenario_name)

met_data <- read.table(met_file, header = TRUE, sep=",")


summary <- summary_analytics(met_data)

numrecs <- nrow(met_data) # what does this do?
vahydro_post_metric_to_scenprop(scenario$pid, 'om_class_Constant', NULL, 'num_records', numrecs, ds)

vahydro_post_metric_to_scenprop(scenario$pid, 'om_class_Constant', NULL, 'precip_annual_max_in', summary$precip_annual_max_in, ds)






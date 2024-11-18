# GET VAHydro 1.0 RIVERSEG l90_Qout DATA
mdf <- data.frame(
  'model_version' = c('cbp-6.1'),
  'runid' = c('stormVol_prism'),
  'metric' = c('precip_annual_max_in'),
  'runlabel' = c('precip_annual_max_in')
)
met_data <- om_vahydro_metric_grid(
  metric = metric, runids = mdf, bundle="watershed", ftype="usgs_full_drainage",
  base_url = paste(site,'entity-model-prop-level-export',sep="/"),
  ds = ds
)

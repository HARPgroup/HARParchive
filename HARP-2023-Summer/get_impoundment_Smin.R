# Get model data attached to an impoundment feature
df <- data.frame(
  'model_version' = c('vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0'),
  'runid' = c('runid_11', 'runid_400', 'runid_600', 'runid_13'),
  'metric' = c('usable_pct_p0','usable_pct_p0', 'usable_pct_p0', 'usable_pct_p0'),
  'runlabel' = c('Smin_pct_2', 'Smin_pct_perm', 'Smin_pct_prop', 'Smin_pct_800')
)


all_imp_data <- om_vahydro_metric_grid(
  metric = metric, runids = df, bundle = 'all', ftype = "all",
  base_url = paste(site,'entity-model-prop-level-export',sep="/"),
  ds = ds
)

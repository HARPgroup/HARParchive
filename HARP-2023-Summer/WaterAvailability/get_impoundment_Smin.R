# Get model data for anything reporting the value "usable_pct_p0"
df <- data.frame(
  'model_version' = c('vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0'),
  'runid' = c('runid_11', 'runid_400', 'runid_600', 'runid_13'),
  'metric' = c('usable_pct_p0','usable_pct_p0', 'usable_pct_p0', 'usable_pct_p0'),
  'runlabel' = c('Smin_pct_11', 'Smin_pct_perm', 'Smin_pct_prop', 'Smin_pct_800')
)
all_imp_data <- om_vahydro_metric_grid(
  metric = metric, runids = df, bundle = 'all', ftype = "all",
  base_url = paste(site,'entity-model-prop-level-export',sep="/"),
  ds = ds
)

# Get model data attached to ONLY an lake/impoundment feature
# this is just for testing, it should be a sub-set of the all_imp_data set from above
waterbody_imp_data <- om_vahydro_metric_grid(
  metric = metric, runids = df, bundle = 'waterbody', ftype = "all",
  base_url = paste(site,'entity-model-prop-level-export',sep="/"),
  ds = ds
)


df <- data.frame(
  'model_version' = c('vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0'),
  'runid' = c('runid_11', 'runid_400', 'runid_600', 'runid_13'),
  'metric' = c('l90_Qout','l90_Qout', 'l90_Qout', 'l90_Qout'),
  'runlabel' = c('L90_Qout_11', 'L90_Qout_perm', 'L90_Qout_prop', 'L90_Qout_800')
)
l90_data <- om_vahydro_metric_grid(
  metric = metric, runids = df, bundle = 'watershed', 
  base_url = paste(site,'entity-model-prop-level-export',sep="/"),
  ds = ds
)


# obviously this is a crapola version that doesn't give us what we want, it sums percents
# BUT, once we have an Smin_l90_mg property on every impopuondment, we can swap Smin_l90_mg_11
# for Smin_pct_11 and we will have a value
riverseg_storage_flow_summary <- sqldf(
  "
    select a.pid, a.riverseg, a.propname, min(a.L90_Qout_11), sum(Smin_pct_11) as min_pct
    from l90_data as a 
    left outer join all_imp_data as b 
    on (
      a.riverseg = b.riverseg
    )
    group by a.pid, a.riverseg, a.propname
")


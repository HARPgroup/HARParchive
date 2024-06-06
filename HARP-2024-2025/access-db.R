library("RPostgres")
library("sqldf")
library("dataRetrieval")

# works with only Rpostgres
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "drupal.dh03",
  host = "deq1.bse.vt.edu",
  port = 5431,
  user = readline("DB User Name: "),
  password =  getPass::getPass("REST Password: ")
  )

# this completes creating connection


get_feature_met <- function(con, hydrocode, varkey, band = 1, mcovcode = 'cbp6_met_coverage') {
  q = paste0(
    "select met.featureid, to_timestamp(met.tstime) as obs_date,
      extract(month from to_timestamp(met.tstime)) as mo, 
      (ST_summarystats(st_clip(met.rast, fgeo.dh_geofield_geom), ",
    band, ", TRUE)).mean as precip_mm,
      0.0393701 * (ST_summarystats(st_clip(met.rast, fgeo.dh_geofield_geom), ",
    band, ", TRUE)).mean as precip_in
    from dh_feature as f
    left outer join field_data_dh_geofield as fgeo
    on (
      fgeo.entity_id = f.hydroid
          and fgeo.entity_type = 'dh_feature'
    )
    left outer join dh_variabledefinition as v
    on (v.varkey = '",
    varkey, "')
    left outer join dh_feature as mcov
    on (
      mcov.hydrocode = '", mcovcode, "'
    )
    left outer join dh_timeseries_weather as met
    on ( mcov.hydroid = met.featureid and met.varid = v.hydroid)
    where f.hydrocode = '", hydrocode, "'
    order by met.tstime
"
  )
  dat <- sqldf(q, connection = con)
  return(dat)
}

# Inventory
inv_met <- sqldf("select extract(year from to_timestamp(met.tstime)) as year,
min(to_timestamp(met.tstime)) as start_date,
max(to_timestamp(met.tstime)) as end_date,
count(*)
from (
  select met.tstime,
  (ST_SummaryStatsAgg(met.rast, 1, TRUE)).mean as precip_in
  from dh_feature as mcov
  left outer join dh_variabledefinition as v
  on (
    v.varkey = 'nldas2_obs_hourly'
  )
  left outer join dh_timeseries_weather as met
  on (
    mcov.hydroid = met.featureid and met.varid = v.hydroid
    and met.entity_type = 'dh_feature'
  )
  where mcov.hydrocode = 'cbp6_met_coverage'
  and met.rast is not null
  group by met.tstime
) as met
group by extract(year from to_timestamp(met.tstime))
order by extract(year from to_timestamp(met.tstime))", connection = con)

# get all the tables from connection


sqldf("select max(tid) from dh_timeseries", connection = con)
gageid = '01634000'
hydrocode = paste0('usgs_ws_', gageid)
prism_data <- get_feature_met(con, hydrocode, 'prism_mod_daily')
prism_data[,c('yr', 'mo', 'da')] <- cbind(year(as.Date(prism_data$obs_date)), month(as.Date(prism_data$obs_date)), day(as.Date(prism_data$obs_date)) )

daymet_data <- get_feature_met(con, hydrocode, 'daymet_mod_daily')
daymet_data[,c('yr', 'mo', 'da')] <- cbind(year(as.Date(daymet_data$obs_date)), month(as.Date(daymet_data$obs_date)), day(as.Date(daymet_data$obs_date)) )
# USGS
gage_info <- memo_readNWISsite(gageid)
da <- gage_info$drain_area_va
usgs_data <- memo_readNWISdv(gageid,'00060')
usgs_data[,c('yr', 'mo', 'da')] <- cbind(year(as.Date(usgs_data$Date)), month(as.Date(usgs_data$Date)), day(as.Date(usgs_data$Date)) )

comp_data <- sqldf(
  "select a.obs_date, a.precip_in as prism_p_in, 
  b.precip_in as daymet_p_in, c.X_00060_00003 as usgs_cfs
  from prism_data as a
  left outer join daymet_data as b 
  on (
    a.yr = b.yr
    and a.mo = b.mo
    and a.da = b.da
  )
  left outer join usgs_data as c 
  on (
    a.yr = c.yr
    and a.mo = c.mo
    and a.da = c.da
  )
  "
)

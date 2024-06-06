library("RPostgres")
library("sqldf")
# RpostgreSQL is an older version?
# library("RPostgreSQL")
# require("RPostgreSQL")
# this completes installing packages

# now start creating connection
# Works with RpostgreSQL
con <- dbConnect(
  dbDriver("PostgreSQL"),
  dbname = "drupal.dh03",
  host = "deq1.bse.vt.edu",
  port = 5431,
  user = readline("DB User Name: "),
  password =  getPass::getPass("REST Password: ")
)
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

# Average met

all_met <- sqldf("select met.featureid, to_timestamp(met.tstime) as obs_date,
        extract(month from to_timestamp(met.tstime)) as mo, 
        (ST_summarystats(met.rast, 10, TRUE)).mean as precip_kgm3,
        0.0393701 * (ST_summarystats(met.rast, 10, TRUE)).mean as precip_in
from dh_feature as mcov
left outer join dh_variabledefinition as v
on (v.varkey = 'nldas2_obs_hourly')
left outer join dh_timeseries_weather as met
on ( mcov.hydroid = met.featureid and met.varid = v.hydroid)
where mcov.hydrocode = 'cbp6_met_coverage'
and extract(month from to_timestamp(met.tstime)) = 5
order by met.tstime", connection = con)


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

gage_met <- sqldf("select met.featureid, to_timestamp(met.tstime) as obs_date,
        extract(month from to_timestamp(met.tstime)) as mo, (ST_MemSize(st_clip(met.rast, fgeo.dh_geofield_geom))/1024)/1024 as size_mb,
        (ST_summarystats(st_clip(met.rast, fgeo.dh_geofield_geom), 10, TRUE)).mean as precip_kgm3,
        0.0393701 * (ST_summarystats(st_clip(met.rast, fgeo.dh_geofield_geom), 10, TRUE)).mean as precip_in
from dh_feature as f
left outer join field_data_dh_geofield as fgeo
on (
  fgeo.entity_id = f.hydroid
      and fgeo.entity_type = 'dh_feature'
)
left outer join dh_variabledefinition as v
on (v.varkey = 'nldas2_obs_hourly')
left outer join dh_feature as mcov
on (
  mcov.hydrocode = 'cbp6_met_coverage'
)
left outer join dh_timeseries_weather as met
on ( mcov.hydroid = met.featureid and met.varid = v.hydroid)
where f.hydrocode = 'usgs_ws_01634000'
order by met.tstime", connection = con)



sqldf("select max(tid) from dh_timeseries", connection = con)

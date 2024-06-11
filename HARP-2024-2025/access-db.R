library("RPostgres")
library("sqldf")
library("dataRetrieval")

# works with only Rpostgres. This is the connection string to the database.
# Using this allows us to directly query the database from R, rather than
# needing to save results via SQL and send off text files with results. These
# connection strings are similar for all databases and rely on the permissions
# within the db
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "drupal.dh03",
  host = "deq1.bse.vt.edu",
  port = 5431,
  user = readline("DB User Name: "),
  password =  getPass::getPass("REST Password: ")
  )

#A function to get mean meteorology data for a given VA Hydro feature's geometry
#extent. In other words, this function can pull down meteorology data from our
#database (which covers the entire chesapeake Bay watershed), clip it to a given
#feature (like the drainage area of a gage or a parituclar watershed), and get
#the mean precipitation per day
get_feature_met <- function(con,
                            #The hydro code of the VA Hydro feature to clip the
                            #dataset to (the hydrocode is often from the base
                            #database and is not generated in VAHydro, whereas
                            #the feature id is the unique VA Hydro auto
                            #generated index number)
                            hydrocode,
                            #The variable key from VA Hydro for the met data of interest
                            varkey,
                            #The band of the raster. Important for NLDAS, PRISM
                            #and dyamet only have 1 band
                            band = 1,
                            #The feature associated with the meteorology data
                            #records. For precip, this is the cbp6_met_coverage
                            #generally (the CB p6 watershed)
                            mcovcode = 'cbp6_met_coverage') {
  q = paste0(
    "select met.featureid, to_timestamp(met.tsendtime) as obs_date,
      extract(month from to_timestamp(met.tsendtime)) as mo, 
    --Note the syntax used to first clip the raster to the requested feature 
    --prior to summarizing, and note that we call mean as if it is a method of 
    --the object of ST_summarystats  
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
    order by met.tsendtime
"
  )
  dat <- sqldf(q, connection = con)
  return(dat)
}

# Inventory i.e show the number of records of a given meteorology data set per
# year and show the minimum and maximum dates tied to that data. We should
# expect to see 366 days on leap years and 365 days on non-leap years EXCEPT for
# daymet, which will ALWAYS have 365 days with 12-31 missing on leap years
varkey <- "nldas2_obs_hourly"
inv_met <- sqldf(paste0("select extract(year from to_timestamp(met.tsendtime)) as year,
min(to_timestamp(met.tsendtime)) as start_date,
max(to_timestamp(met.tsendtime)) as end_date,
count(*)
from (
  select met.tsendtime
  from dh_feature as mcov
  left outer join dh_variabledefinition as v
  on (
    v.varkey = '",varkey,"'
  )
  left outer join dh_timeseries_weather as met
  on (
    mcov.hydroid = met.featureid and met.varid = v.hydroid
    and met.entity_type = 'dh_feature'
  )
  where mcov.hydrocode = 'cbp6_met_coverage'
  and met.rast is not null
  group by met.tsendtime
) as met
group by extract(year from to_timestamp(met.tsendtime))
order by extract(year from to_timestamp(met.tsendtime))"), connection = con)

# get all the tables from connection
dbListTables(con)

#Set a USGS gage ID
gageid = '01634000'
#Format the ID to match the hydrocode used in the VA Hydro database
hydrocode = paste0('usgs_ws_', gageid)
#Get PRISM data, specified via the varkey argument in get_feature_met, and clip
#to the gage drainage area, specified via the hydrocode. Get the mean precip in
#the watershed for each day
prism_data <- get_feature_met(con, hydrocode, 'prism_mod_daily')

#Add more details about the date as needed. Note that format relies on a date
#defined via https://pubs.opengroup.org/onlinepubs/009695399/utilities/date.html
prism_data[,c('yr', 'mo', 'da')] <- cbind(format(prism_data$obs_date,"%Y"),
                                          format(prism_data$obs_date,"%m"),
                                          format(prism_data$obs_date,"%d"))
#Repeat the process, this time pulling data from the daymet data source
daymet_data <- get_feature_met(con, hydrocode, 'daymet_mod_daily')
daymet_data[,c('yr', 'mo', 'da')] <- cbind(format(daymet_data$obs_date,"%Y"),
                                           format(daymet_data$obs_date,"%m"),
                                           format(daymet_data$obs_date,"%d"))
# Get USGS gage data from NWIS, the USGS REST services. We pull this data using
# the dataRetrieval R package which has extensive documentation on the options
# therewithin. See https://waterdata.usgs.gov/blog/dataretrieval/. Below, we
# a cached version of the function defined in our config.R file:
gage_info <- memo_readNWISsite(gageid)
#Extract the drainage area of the gage
da <- gage_info$drain_area_va
#Get daily, mean streamflow (pcode 000060, see dataRetrieval::parameterCdFile)
#from the gage specified in gageid
usgs_data <- memo_readNWISdv(gageid,'00060')
#As above, add more details about the dates
usgs_data[,c('yr', 'mo', 'da')] <- cbind(format(usgs_data$Date,"%Y"),
                                         format(usgs_data$Date,"%m"),
                                         format(usgs_data$Date,"%d"))

#Join all data together. In other words, extract the relevant fields from the
#three data sources and combine them based on the day, month, and year. We could
#do this by joining by the dates, but you may notice that the USGS daily data
#may not come in as POSIX and doesn't have a time associated with it.
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
#If you are familiar with tidyverse verbiage, you may wonder why we use SQLDF
#here instead of a few dplyr::left_join()s and dplyr::select(). The reason is
#fairly simple. Our team is more well-versed in SQL than in tidyverse and
#tidyverse has a tendency to change these verbs slightly over time. Most of the
#time, this is not an issue. But, some of the more obscure commands often get
#depreceated. Look at the history of dplyr::across() as an example. Even now,
#the joins were changed just a few versions ago and will soon block certain code
#from executing without directly specifying all inputs!

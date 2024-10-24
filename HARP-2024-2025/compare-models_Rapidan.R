options(scipen=999)
library('hydrotools')
library('zoo')
library("knitr")
library("data.table")
basepath='/var/www/R';
source("/var/www/R/config.R")
source("https://raw.githubusercontent.com/HARPgroup/hydro-tools/master/R/fac_utils.R")
ds <- RomDataSource$new(site, rest_uname = rest_uname)
ds$get_token(rest_pw = rest_pw)

# GET VAHydro 1.0 RIVERSEG l90_Qout DATA
df <- data.frame(
  'model_version' = c('vahydro-1.0', 'cbp-6.0', 'cbp-6.1'),
  'runid' = c('runid_11', 'CFBASE30Y20180615_vadeq', 'subsheds'),
  'metric' = c('l30_Qout','l30_Qout', 'l30_Qout'),
  'runlabel' = c('l30_vahydro_11', 'l30_cbp6', 'l30_nldas2res')
)
wshed_data <- om_vahydro_metric_grid(
  metric = metric, runids = df,
  base_url = paste(site,'entity-model-prop-level-export',sep="/"),
  ds = ds
)

rap_data = fn_extract_basin(wshed_data,'RU3_6170_6040')
#rapp_data <- fn_extract_basin(wshed_data,'RU5_6030_0001')
james_data <- fn_extract_basin(wshed_data,'JL7_7070_0001')


# Case study
ds2_url <- "http://deq1.bse.vt.edu:81/p6/out/river/CFBASE30Y20180615_vadeq/hydr/RU2_6200_6170_hydrd_wy.csv"
ds1_url <- "http://deq1.bse.vt.edu:81/p6/out/river/subsheds/hydr/RU2_6200_6170_hydrd_wy.csv"

flow1 <- fread(ds1_url)
flow2 <- fread(ds2_url)
names(flow2)
names(flow1)
flow_co <- sqldf(
  "select a.year as yr, a.month as mo, a.day as da, 
   a.Qout as flow1, b.Qout as flow2
   from flow1 as a 
   left outer join flow2 as b
   on (
     a.year = b.year
     and a.month = b.month
     and a.day = b.day
   )
   order by a.date, a.year, a.month, a.day
  "
)
plot(flow_co$flow1 ~ flow_co$flow2, main="Flow compare")

flow_pd <- sqldf(
  "select * from flow_co 
   where (yr = 2017 and mo > 9)
   OR (yr = 2018 and mo < 4)
  "
)
plot(flow_pd$flow1 ~ index(flow_pd), col="blue")
points(flow_pd$flow2, col="red")


# GET VAHydro 1.0 RIVERSEG l90_Qout DATA
mdf <- data.frame(
  'model_version' = c('cbp-6.0', 'cbp-6.0'),
  'runid' = c('met2date', '1984010100-2022123123'),
  'metric' = c('precip_annual_mean_in', 'PRC_anomaly_count'),
  'runlabel' = c('mean_met2date', 'anomaly_2023')
)
met_data <- om_vahydro_metric_grid(
  metric = metric, runids = mdf, bundle="landunit", ftype="cbp6_landseg",
  base_url = paste(site,'entity-model-prop-level-export',sep="/"),
  ds = ds
)


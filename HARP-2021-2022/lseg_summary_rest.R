## Outline for using rest to import summary data into vahydro
## Script corresponds to github issue 99
## HARP group
## Last updated 7/27/2021


# declaring land seg and grabbing summary data for testing purposes
landseg <- "A51067"
df <- read.table(paste0("http://deq1.bse.vt.edu:81/met/out/lseg_csv/1984010100-2020123123/",landseg,"SummaryStats.csv"), header = TRUE, sep = ",")

# download hydrotools if needed
# library("devtools")
# install_github("HARPgroup/hydro-tools")

library("hydrotools")
ds <- RomDataSource$new("http://deq1.bse.vt.edu:81/d.alpha")
ds$get_token()
hid <- 471328


# loop goes through every year in summary statistics file
i <- 1
for (i in 1:nrow(summaryStats)) {
    
  # number of days in a year with no precip
  ts <- RomTS$new(
    ds, 
    list(
      varkey = "no_precip_days", 
      featureid = hid, 
      entity_type = "dh_feature", 
      tsvalue = summaryStats$no_precip_days[i], 
      tstime = as.numeric(as.POSIXct(paste0(summaryStats$year[i], "-01-01"))),
      tsendtime = as.numeric(as.POSIXct(paste0(summaryStats$year[i], "-12-31")))
    ), 
    TRUE
  )
  
  ts$save(TRUE)
  
  # maximum number of consecutive days in a year without precip
  ts1 <- RomTS$new(
    ds,
    list(
      varkey = "no_precip_max",
      featureid = hid,
      entity_type = "dh_feature",
      tsvalue = summaryStats$no_precip_max[i],
      tstime = as.numeric(as.POSIXct(paste0(summaryStats$year[i], "-01-01"))),
      tsendtime = as.numeric(as.POSIXct(paste0(summaryStats$year[i], "-12-31")))
    ),
    TRUE
  )
  
  ts1$save(TRUE)
  
  # total precipitation over the year
  ts2 <- RomTS$new(
    ds,
    list(
      varkey = "precip_annual",
      featureid = hid,
      entity_type = "dh_feature",
      tsvalue = summaryStats$precip_annual[i],
      tstime = as.numeric(as.POSIXct(paste0(summaryStats$year[i], "-01-01"))),
      tsendtime = as.numeric(as.POSIXct(paste0(summaryStats$year[i], "-12-31")))
    ),
    TRUE
  )
  
  ts2$save(TRUE)
  
  # max number of consecutive days where ET > precip
  ts3 <- RomTS$new(
    ds,
    list(
      varkey = "water_deficit_max",
      featureid = hid,
      entity_type = "dh_feature",
      tsvalue = summaryStats$water_deficit_max[i],
      tstime = as.numeric(as.POSIXct(paste0(summaryStats$year[i], "-01-01"))),
      tsendtime = as.numeric(as.POSIXct(paste0(summaryStats$year[i], "-12-31")))
    ),
    TRUE
  )
  
  ts3$save(TRUE)
  
  i <- i + 1

}




# which returns the exact same object as
# ts1 <- ds$get_ts(list(
#   varkey="geologic_map", 
#   featureid=hid, 
#   entity_type="dh_feature", 
#   tsvalue=1.0, 
#   tscode="test" 
# ), 'object', TRUE)
# 

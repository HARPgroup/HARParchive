## Function that uses rest to import nldas land seg summary data into vahydro
## Script corresponds to github issue 99
## HARP group
## Last updated 7/27/2021


# creating function for that requires land seg as input and posts summary stats to vahydro
fn_post_nldas_sumstats <- function(landseg, ds, token){
  
  # downloading nldas summary stats for inputed landseg
  summaryStats <- read.table(paste0("http://deq1.bse.vt.edu:81/met/out/lseg_csv/1984010100-2020123123/",landseg,"SummaryStats.csv"), header = TRUE, sep = ",")
  
  # getting hydroid using landseg through getFeature
  inputs <- list(hydrocode = landseg,
                 bundle = 'landunit',
                 ftype = 'cbp532_landseg')
  feat <- getFeature(inputs = inputs, token = token, base_url = site)
  hid <- feat$hydroid
  
  # new rest function use, for future use once uploaded to hydrotools
  #fn_get_rest(entity_type = 'dh_feature', inputs, site = site, token = token)
  
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

}

########################
### use

# declaring land seg and grabbing summary data for testing purposes
landseg <- "A51019"

# download hydrotools if needed
# library("devtools")
# install_github("HARPgroup/hydro-tools")

# load rest functions, datasource, hydrotools, and token
site <- "http://deq1.bse.vt.edu:81/d.alpha"
basepath <- '/var/www/R';
source(paste(basepath,'config.R',sep='/'))
library("hydrotools")
ds <- RomDataSource$new("http://deq1.bse.vt.edu:81/d.alpha")
token <- ds$get_token()

# run function
fn_post_nldas_sumstats(landseg = landseg, ds = ds, token = token)
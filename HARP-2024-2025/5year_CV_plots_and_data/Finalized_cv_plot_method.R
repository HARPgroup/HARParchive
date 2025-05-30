library(sf)
library(sqldf)
library(ggplot2)
library(dplyr)
library(units)
library(RCurl)



yearly_summary <- function(df,beginningyear,endyear){
  #Aggregate to daily data if provided hourly data
  daily_summary <- 
    sqldf(
      "SELECT yr, mo, da, obs_date, SUM(precip_in) AS daily_precip
      FROM df
      GROUP BY yr, mo, da, obs_date"
    )
  #creating a summary of precip, days with data, and missing days over year
  #range
  yearly_summary <- 
    fn$sqldf(
      "SELECT COUNT(daily_precip) AS ndays_with_data, 
              SUM(CASE WHEN daily_precip IS NULL THEN 1 ELSE 0 END) AS ndays_missing, 
              MIN(yr) as start_year, MAX(yr) as end_year,
              SUM(daily_precip) AS total_precip
      FROM daily_summary
      WHERE yr >= '$beginningyear'
      AND yr <= '$endyear'"
    )
  return(yearly_summary)
}
#hydrocode <- 'usgs_ws_02038000'
#beginningyear <- '2018'
#endyear <- '2023'
model_comparison <- function(hydrocode,beginningyear,endyear){
  default_dataframe <- data.frame(yr = numeric(),
                                 mo = numeric(),
                                 da = numeric(),
                                 obs_date = as.Date(character()),
                                 precip_in = numeric())
 if (url.exists(paste0("http://deq1.bse.vt.edu:81/met/PRISM/precip/",hydrocode,"_precip.csv"))){
   prism <- read.csv(paste0("http://deq1.bse.vt.edu:81/met/PRISM/precip/",hydrocode,"_precip.csv"))
 } else{
   prism <- default_dataframe
 }
  if (url.exists(paste0("http://deq1.bse.vt.edu:81/met/daymet/precip/",hydrocode,"_precip.csv"))){
    daymet <- read.csv(paste0("http://deq1.bse.vt.edu:81/met/daymet/precip/",hydrocode,"_precip.csv"))
  } else{
    daymet <- default_dataframe
  }
  if (url.exists(paste0("http://deq1.bse.vt.edu:81/met/nldas2/precip/",hydrocode,"_precip.csv"))){
    nldas2 <- read.csv(paste0("http://deq1.bse.vt.edu:81/met/nldas2/precip/",hydrocode,"_precip.csv"))
  } else{
    nldas2 <- default_dataframe
  }
  prism_sum <- yearly_summary(prism,beginningyear,endyear)
  daymet_sum <- yearly_summary(daymet,beginningyear,endyear)
  nldas_sum <- yearly_summary(nldas2,beginningyear,endyear)
  # A series of SQL joins and unions that produce a tidy, combined data frame
  ## joins prism and daymet
  j <- sqldf(
    "SELECT daymet_sum.start_year, daymet_sum.end_year, 
    daymet_sum.ndays_with_data as daymet_ndays_with_data,
    prism_sum.ndays_with_data as prism_ndays_with_data,
    nldas_sum.ndays_with_data as nldas2_ndays_with_data,
    prism_sum.total_precip AS prism_precip,
    daymet_sum.total_precip AS daymet_precip,
    nldas_sum.total_precip AS nldas_precip,
    daymet_sum.ndays_missing as daymet_missing,
    prism_sum.ndays_missing as prism_missing,
    nldas_sum.ndays_missing as nldas2_missing
    FROM prism_sum
    FULL JOIN daymet_sum ON prism_sum.start_year = daymet_sum.start_year
    FULL JOIN nldas_sum ON prism_sum.start_year = nldas_sum.start_year"
  )
  
  
  #Compute mean precip and differences from mean, included CV
  analytics <- j |>
    mutate(meanPrecip = mean(c(prism_precip,daymet_precip,nldas_precip))) |> 
    mutate(var = var(c(prism_precip,daymet_precip,nldas_precip)))|>
    mutate(CV = sqrt(var)/meanPrecip) |>
    mutate(nldas2_dif_from_mean = nldas_precip - meanPrecip) |> 
    mutate(prism_dif_from_mean = prism_precip - meanPrecip) |> 
    mutate(daymet_dif_from_mean = daymet_precip - meanPrecip) |> 
    mutate(hydrocode = hydrocode) |> 
    select(-meanPrecip)
  
  return(analytics)
}

#Get gage data and store in a spatial data frame. Read in from a csv or
#otherwise use data source if ds, bundle, and ftype has been provided
create_SF <- function(watershedWKT = "http://deq1.bse.vt.edu:81/met/usgsGageWatershedGeofield.csv",
                      wktField = 'wkt',
                      ds = NULL, bundle = NULL, ftype = NULL){
  if(!is.null(ds) && !is.null(bundle) && !is.null(ftype)){
    #Get the watershed coverages and geometries from the server
    gageWatershedSF <- fn$sqldf(
      connection = ds$connection,
      paste0("SELECT hydrocode,ST_asText(dh_geofield_geom) as ",wktField,"
       FROM dh_feature_fielded
       WHERE bundle = '$bundle'
       AND ftype = '$ftype'")
    )
    #Get the gage numbers as their own field and store a copy of the data
    gageWatershedSF <- gageWatershedSF[!is.na(gageWatershedSF$wkt),]
  }else{
    gageWatershedSF <- read.csv(watershedWKT)
  }
  #Transform hydrocodes to gage numbers
  gageWatershedSF$gage <- gsub(".*_(\\d+)","\\1",gageWatershedSF$hydrocode)
  #Convert to an sf data frame by providing the wktField name and a crs
  gageWatershedSF <- st_as_sf(gageWatershedSF,wkt = wktField,crs = 4326)
  #Repair broken geometries
  gageWatershedSF <- st_make_valid(gageWatershedSF)
  #Add shape area in coordinate system units (likely meaningless in crs 4326)
  gageWatershedSF$area <- st_area(gageWatershedSF)
  return(gageWatershedSF)
}

#Run model comparison on each hydrocode in a data frame gageWatershedSF from the
#beginningyear to endyear
CV_analysis <- function(beginningyear,endyear,
                        gageWatershedSF){
  #Empty data frame to store results
  stat_data <- data.frame()
  #Iterating over hydrocode, store results of model_comparison from input start
  #and end year
  for (i in gageWatershedSF$hydrocode){
    analytic_data <- model_comparison(i,beginningyear,endyear)
    #Bind results together for output
    stat_data <- rbind(stat_data, analytic_data)
  }
  return(stat_data)
}

gageWatershedSF <- create_SF()
stat_data <- data.frame()
final_data <- data.frame()

#Get the geometries of all VA USGS watersheds
gageWatershedSF2 <- create_SF()
beginningyear <- 1998
endyear <- 2003
stat_data <- CV_analysis(beginningyear,endyear,gageWatershedSF)

#Join on spatial data for each hydrocode so that the gage name, watershed area,
#and WKT can be included in final output
final_data <- stat_data
final_data <- cbind(final_data,
                    gageWatershedSF[match(gageWatershedSF$hydrocode,final_data$hydrocode),c('gage','area')]
)

#Write out data, excluding any WKT for speed of loading
write.csv(final_data[,-grep('^wkt$',names(final_data))],
          paste0("~/HarpData/Harp2025(correct)/",beginningyear,"_",endyear,"_final_data.csv"))
write.csv(stat_data, paste0("~/HarpData/Harp2025(correct)/",beginningyear,"_",endyear,"_stat_data.csv"))

#Add cv onto sf data frame so plot can be colored by CV
gageWatershedSF$CV <- final_data$CV[match(final_data$hydrocode, gageWatershedSF$hydrocode)]

#Plot SF polygons
ggplot(gageWatershedSF) + 
  geom_sf(aes(fill = CV))

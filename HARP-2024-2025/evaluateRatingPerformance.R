library("hydrotools")
basepath='/var/www/R'
source(paste(basepath,'config.R',sep='/'))


dateStart <- '2003-01-1'
dateEnd <- '2004-12-31'
scenario <- NULL #'simple_lm_PRISM'
coverage <- 'usgs_ws_01646000'

#If a scenario or coverage is provided, add into WHERE statement in ratings
#query below
coverageSQL <- ''
scenarioSQL <- ''
if(!is.null(scenario)){
  scenarioSQL <- paste0("AND scenProp.propname = '",scenario,"'")
}
if(!is.null(coverage)){
  coverageSQL <- paste0("AND feat.hydrocode = '",coverage,"'")
}


ratingsAll <- fn$sqldf(connection = ds$connection,paste0("
      SELECT feat.hydrocode, scenProp.propname,
      to_timestamp(ts.tstime) as tstime,
      to_timestamp(ts.tsendtime) as tsendtime,
      ts.tsvalue
      FROM dh_timeseries as ts
      LEFT JOIN dh_variabledefinition as v
      ON v.hydroid = ts.varid
      AND v.varkey = 'met_rating'
      LEFT JOIN dh_properties as scenProp
      ON ts.featureid = scenProp.pid \n",
      scenarioSQL,"
      LEFT JOIN dh_properties as modelProp
      ON modelProp.pid = scenProp.featureid
      AND modelProp.propcode = 'met-1.0'
      LEFT JOIN dh_feature as feat
      ON feat.hydroid = modelProp.featureid
      
      WHERE feat.bundle = 'watershed'
      AND feat.ftype = 'usgs_full_drainage'",
      coverageSQL,"
      AND modelProp.pid IS NOT NULL
      AND scenProp.pid IS NOT NULL
      AND v.hydroid IS NOT NULL
      AND ts.tsendtime >= extract(epoch FROM '$dateStart'::date)
      AND ts.tsendtime < extract(epoch FROM '$dateEnd'::date)
      
      ORDER BY ts.tsendtime
"))


#General boxplot of ratings across all non-amalgamated scenarios:
ratings_filter <- ratingsAll[
  !grepl('amalgamate',ratingsAll$propname),
]
library(ggplot2)
ggplot(data = ratings_filter) + 
  geom_boxplot(aes(x = propname,y = tsvalue)) + 
  coord_cartesian(ylim = c(-1,1)) + 
  xlab("")


#Add month and month/year onto the dataframe for additional plots
ratings_filter$mo <- lubridate::month(ratings_filter$tstime)
ratings_filter$moyear <- paste0(lubridate::month(ratings_filter$tstime),"-",lubridate::year(ratings_filter$tstime))

#Boxplots grouped by month for entire timeperiod to show seasonality
ggplot(data = ratings_filter) + 
  geom_boxplot(
    aes(col = propname,
        x = factor(mo,levels = 1:12),
        y = tsvalue
    )
  ) + 
  coord_cartesian(ylim = c(-1,1)) + 
  scale_color_manual(values = c('steelblue','darkblue','darkgreen',
                                'lightgreen','black','navy')) +
  xlab("")

#Boxplots showing each month of the dataset individually
ggplot(data = ratings_filter) + 
  geom_boxplot(
    aes(col = propname,
      x = factor(moyear,
                 levels = apply(MARGIN = 1, FUN = paste0, collapse = "",
                                expand.grid(paste0(1:12, "-"),
                                            unique(lubridate::year(tstime))))
      ),
      y = tsvalue)
  ) + 
  coord_cartesian(ylim = c(-1,1)) + 
  scale_color_manual(values = c('steelblue','darkblue','darkgreen',
                       'lightgreen','black','navy')) +
  xlab("")

#Summarize the ratings per month to get the mean rating
summaryData <- sqldf("
SELECT moyear, propname, avg(tsvalue) as avgValue
FROM ratings_filter
GROUP BY moyear, propname
")
ggplot(summaryData) + 
  geom_point(aes(moyear,avgValue,col = propname)) + 
  geom_line(aes(moyear,avgValue,col = propname,
                group = propname)) + 
  coord_cartesian(ylim = c(-1,1)) + 
  scale_color_manual(values = c('steelblue','darkblue','darkgreen',
                                'lightgreen','black','navy')) +
  xlab("")



#Amalgamated scenarios joined to varkey to show best-fit data selected for each
#time period
ratings_amal <- ratingsAll[
  grepl('amalgamate',ratingsAll$propname),
]
ratings_amal$mo <- lubridate::month(ratings_amal$tstime)
varIDs <- sqldf(connection = ds$connection,"
      SELECT hydroid, varkey
      FROM dh_variabledefinition
      WHERE hydroid IN (1453,1454,1464)")
dataToPlot <- sqldf("SELECT * FROM ratings_amal as a
      LEFT JOIN varIDs as v ON
      a.tsvalue = v.hydroid")
ggplot(data = dataToPlot) + 
  geom_point(
    aes(x = tstime,y = varkey, col = propname)
  ) + 
  xlab("")

#Can we show a better plot of the selected amalgamated dataset and perhaps include
#the rating for it?

#Pull JSON to show the monthly R squares for a given watershed
#With Nate's storm zoom in, add model flow and the storm start/stop point
#from storm vol separation

#Can intensity improve these relationships? We can summarize the NLDAS2 hourly
#fractions to find intensity at each gage and then look at mean intensity
#(excluding zeroes) for that day

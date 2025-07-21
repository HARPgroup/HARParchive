library(sf)
library(sqldf)
library(ggplot2)
library(dplyr)
library(units)
library(RCurl)


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


gageWatershedSF <- create_SF()

# Fubction for mean ratings table
mean_ratings_table <- function(gageWatershedSF, method) {
  ratings_table <- data.frame(hydrocode = character(), mean_rating = numeric(), stringsAsFactors = FALSE)
  
  for (i in gageWatershedSF$hydrocode) {
    ratings_url <- paste0("http://deq1.bse.vt.edu:81/met/",method,"/out/",i,"-rating-ts.csv")
    ratings_data <- tryCatch(read.csv(ratings_url), error = function(e) NULL)
    
    mean_rating <- mean(ratings_data$rating, na.rm = TRUE)
    
    ratings_table <- rbind(ratings_table, data.frame(hydrocode = i, mean_rating = mean_rating))
  }
  
  return(ratings_table)
}

# CReate mean ratings table
mean_ratings_data<-mean_ratings_table(gageWatershedSF, "simple_lm_PRISM")

# 
final_data <- cbind(mean_ratings_data,
                    gageWatershedSF[match(gageWatershedSF$hydrocode,mean_ratings_data$hydrocode),c('gage','area')]
)


#Add cv onto sf data frame so plot can be colored by CV
gageWatershedSF$mean_ratings <- final_data$mean_rating[match(final_data$hydrocode, gageWatershedSF$hydrocode)]


# Gage 02049500 has extremely low ratings, remove for now
gageWatershedSF <-subset(gageWatershedSF, hydrocode!="usgs_ws_02049500")

#Plot SF polygons
ggplot(gageWatershedSF) + 
  geom_sf(aes(fill = mean_ratings))+
  ggtitle("Mean Ratings")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_binned()



# SAME BUT FOR MAXIMUM
max_ratings_table <- function(gageWatershedSF, method) {
  ratings_table <- data.frame(hydrocode = character(), max_rating = numeric(), stringsAsFactors = FALSE)
  
  for (i in gageWatershedSF$hydrocode) {
    ratings_url <- paste0("http://deq1.bse.vt.edu:81/met/",method,"/out/",i,"-rating-ts.csv")
    ratings_data <- tryCatch(read.csv(ratings_url), error = function(e) NULL)
    
    max_rating <- max(ratings_data$rating, na.rm = TRUE)
    
    ratings_table <- rbind(ratings_table, data.frame(hydrocode = i, max_rating = max_rating))
  }
  
  return(ratings_table)
}

# CReate mean ratings table
max_ratings_data<-max_ratings_table(gageWatershedSF, "simple_lm_PRISM")

# 
final_data <- cbind(max_ratings_data,
                    gageWatershedSF[match(gageWatershedSF$hydrocode,max_ratings_data$hydrocode),c('gage','area')]
)


#Add cv onto sf data frame so plot can be colored by CV
gageWatershedSF$max_ratings <- final_data$max_rating[match(final_data$hydrocode, gageWatershedSF$hydrocode)]

#Plot SF polygons
ggplot(gageWatershedSF) + 
  geom_sf(aes(fill = max_ratings))+
  ggtitle("Maximum Ratings")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))




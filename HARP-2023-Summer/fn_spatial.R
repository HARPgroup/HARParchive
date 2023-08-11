# allows the usage of SQLDF with sf data frames:
sqldf_sf <- function(statemt, geomback="NA"){
  #statement is any SQLDF string; reference the data frames per usual
  #geomback is the character name of the data.frame you are filtering, where, if applicable, the sf geometry column will need to be added back from
  dfs <- as.environment(as.list(.GlobalEnv, all.names=TRUE)) #make a copy of the global environment
  for(i in names(dfs)){
    if(class(dfs[[i]])=="sf"){ #drop the geometry on any sf objects so they can go through SQLDF
      dfs[[i]] <- sf::st_drop_geometry( dfs[[i]] )
    }
  }
  dfs[["result"]] <- sqldf(statemt, envir=dfs) #SQLDF the non-sf dataframes in the new environment (so that the global envir. retains the sf objects)
  if(geomback!="NA"){
    output <- merge(x=dfs[["result"]], y=.GlobalEnv[[geomback]], all.x=TRUE, all.y=FALSE) #match the newly filtered obs. to their geometries
    output <- st_as_sf(output, crs=4326) #convert back to sf
  } else {
    output <- dfs[["result"]]
  }
}

# returns the name of the geometry column in the data:
geoCol <- function(data){
  colname <- grep("geo",colnames(data),value=TRUE)
  if(length(colname) > 1){
    colname <- grep("geom",colname,value=TRUE) #for the case of "dh_geofield.geom" "dh_geofield.geo_type" "dh_geofield.lat" "dh_geofield.lon", etc.
  }
  return(colname)
}

# legend_titling() --> Generate user-understandable legend titles
## Kept up here as a function so that it's easy to add if-statements when user-input options expand
legend_titling <- function(metric, runid_list){
  
  if (metric=="wd_mgd"){ #titles for wd_mgd
    name <- "Withdrawal"
    unit <- "(MGD)"
    legend_title <- runid_list
    year <- mgsub(runid_list, c("runid_11","runid_13"), c("2020","2040"))
    for (i in 1:length(runid_list)){
      legend_title[i] <- paste(year[i],name,unit,sep="\n")
    }
  } else if (metric=="fiveyr_avg_mgy"){ #this metric isn't associated w a runid
    name <- "5-yr Avg Use"
    unit <- "(MGY)"
    legend_title<- paste(name,unit,sep="\n")
  } else {
    legend_title<- paste(metric)
  }
  return(legend_title)
}
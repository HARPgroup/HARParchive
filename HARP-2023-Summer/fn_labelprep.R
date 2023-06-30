#goal for this fn(): df's are added incrementally to the labels list so they can be combined later into one df
#can pass in one or multiple raw df's at once so that these can be generated anywhere throughout the code; any order

#dataframes for labels:
# roads, waterbodies, flowlines, cities, counties
### TESTING
data <- list(roads$sf, cities, counties$df)
classes <- c("roads","city","county")

data <- list(counties$df)
classes <- "county"

data <- list(cities)
classes <- "city"

data <- list(roads$sf)
classes <- "roads"
###


labels <- fn_labelprep(data, classes){
  for(d in 1:length(data)){
    
    
    if( length(data[[d]][["lat"]] )!=0 & length(data[[d]][["lng"]] )!=0){
      #coordinates are ready
      print(paste(classes[d],"coords exist", sep=" "))
    }
    
    if( length(data[[d]][["lat"]] )==0 | length(data[[d]][["lng"]] )==0){
      print(paste("error with",classes[d],"coords: missing lat or lng", sep=" "))
      
      #proceed to check for a geometry column:
      if(length(grep("geo", colnames(data[[d]])))!=0){
        #has geometry column --> need to calculate centroid coords
        print(paste(classes[d],"coords can be calculated", sep=" "))
        
        if(length(grep("sfc", lapply(data[[d]], class)))==0){
          geoCol <- grep("geo", colnames(data[[d]]), value=TRUE) #returns all geo columns
          if(length(geoCol) > 1){geoCol <- geoCol[geoCol!="geometry"]}
          data[[d]] <- centroid_coords(data[[d]], geoCol)
        }
        
      }
      
    }
    
    data[[d]] <- with(data, sqldf(
      # try to replace any column name containing "name", "NAME", or class[[d]] with "label" ??? 
    ))
    # labels[[paste(class[[d]])]] <- data[[d]][,c("name","lat","lng")]
  }
  
  
}
#goal for this fn(): df's are added incrementally to the labels list so they can be combined later into one df
#can pass in one or multiple raw df's at once so that these can be generated anywhere throughout the code; any order

#dataframes for labels:
# roads, waterbodies, flowlines, cities, counties
### TESTING
labels <- list()

data <- list(roads, cities, counties$df)
classes <- c("roads","city","county")

data <- list(counties$df)
classes <- "county"

data <- list(cities)
classes <- "city"

data <- list(roads$sf)
classes <- "roads"
###

# Renaming a column:
mp_layer_sql <- paste('SELECT *, ',df$runlabel[1],' AS demand_metric
                         FROM facils_within'
                      , sep="") 


labels <- fn_labelprep(data, classes){
  names(data) <- classes
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
        
        ###-Special Cases-###
        
        if(classes[d]=="roads"){
          # Filter roads to only Interstate, US Hwy, State Rte
          data[[d]] <- subset(data[[d]], MTFCC=="S1100") #primary roads only
          ## Interstate, Us Hwy, or State Rte only
          data[[d]] <- subset(data[[d]], RTTYP=="I" | FULLNAME %in% grep("US Hwy.*", data[[d]][["FULLNAME"]], value=TRUE) | RTTYP=="S")
          ## Shorten to rte number only
          data[[d]][["FULLNAME"]] <- mgsub(data[[d]][["FULLNAME"]], pattern=c("\\I- ", "US Hwy ", "State Rte "), replacement=c("","",""))
          ## Remove remaining names with spaces (e.g. US Hwy Bus or Alt Rte)
          data[[d]] <- subset(data[[d]], !(FULLNAME %in% grep(".* .*", data[[d]][["FULLNAME"]], value=TRUE)))
          
          #roads <- data[[d]] #save processed roads elsewhere for plotting road lines
          #return(roads)
          data[[d]] <- roads[!duplicated(roads$FULLNAME),] #don't want same rte labeled more than once
          
        }
        
        #if(length(grep("sfc", lapply(data[[d]], class)))==0){
          geoCol <- grep("geo", colnames(data[[d]]), value=TRUE) #returns all geo columns
          if(length(geoCol) > 1){geoCol <- geoCol[geoCol!="geometry"]}
          print(paste("calculating", classes[d], "coords", sep=' '))
          data[[d]] <- centroid_coords(data[[d]], geoCol)
        #}
        
      }
      
    }
    
    # Search for possible label name columns and rename to "label":
    namecol_d <- grep("name", colnames(data[[d]]), ignore.case=TRUE, value=TRUE) #looks for anything containing name or NAME
    namecol_d <- c(namecol_d, grep(classes[d], colnames(data[[d]]), ignore.case=TRUE, value=TRUE) ) 
        #^looks for anything containing the label's class name
    colnames(data[[d]]) <- gsub(namecol_d, "label", colnames(data[[d]]))
    data[[d]][["class"]] <- rep(classes[d], nrow(data[[d]])) #add a class column
    data[[d]] <- data.frame(data[[d]]) #get rid of sfc geometry
    
    # Add prepared label to labels list:
    #labels[[paste(classes[d])]] <- data[[d]][,c("label","class","lat","lng")]
  }
  
  
}

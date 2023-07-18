# Goal for this fn(): df's are added incrementally to the maplabs list so they can be combined later into one df by fn_mapgen()
# Ability to pass in one or multiple raw df's at once so that these can be generated anywhere throughout the code; any order
library(mgsub)
# Note: requires function centroid_coords() , which is currently defined in mapping_codeReview.Rmd 

# Note: NHD-related data is passed in all at once & automatically separated into the different labels via a special case loop
# Note: Supply singular nouns into classes (city, road, etc. NOT cities, roads, etc)

# Example uses: 
## maplabs <- fn_labelprep(data=list(...,nhd), classes=c(...,"nhd"))
## maplabs <- fn_labelprep(data=list(counties$sf, cities, roads, nhd), classes=c("county","city","road","nhd"))

fn_labelprep <- function(data, classes){
# Check to make sure required function is loaded
  if(!exists("centroid_coords", ".GlobalEnv")){
    message("Error: Please load function centroid_coords() into Global Environment")
  }
# Create a list "maplabs" to append final output to, if it doesn't already exist
  if(!exists("maplabs", ".GlobalEnv")){
    maplabs <- list()
  }

# Begin iteration through all data sets supplied to function
  for(d in 1:length(data)){
    
     ## Roads
#    if(classes[d]=="road"){
#      colnames(data[[d]]) <- gsub("RTTYP", "class", colnames(data[[d]]) ) #identify class column
#    }
  ##
    
#----We want all coordinate columns to be titled "lat" and "lng"----
 # First: replace any existing alternate lat/lng names with lat & lng
    colnames(data[[d]]) <- mgsub(colnames(data[[d]]), 
                                 c("latitude","longitude"),  ignore.case=TRUE, #covers Latitude, LATITUDE, etc. as well
                                 c("lat", "lng") #replacement
                                 )
    
    for( i in colnames(data[[d]]) ) {
      if(i=="X" | i=="x") {
        num <- grep(i, colnames(data[[d]])) #obtain coordinate column's number
        colnames(data[[d]])[num] <- "lng" #replace "X" or "x" with "lng"
      }
      if(i=="Y" | i=="y") {
        num <- grep(i, colnames(data[[d]]))
        colnames(data[[d]])[num] <- "lat"
      }
    } #we use this loop to avoid poor gsubs
      #for e.g. substituting any "y" could result in: "facility" --> "facilitlat"
    
  # Next: after substituting names where applicable, check to make sure lat/lng even exist
    if( length(data[[d]][["lat"]] )!=0 & length(data[[d]][["lng"]] )!=0){
      #coordinates are ready
      print(paste(classes[d],"coords exist as lat/lng", sep=" "))
    }

#----When coordinates don't already exist----
    if( length(data[[d]][["lat"]] )==0 | length(data[[d]][["lng"]] )==0){
      #coordinates are not ready
      print(paste(classes[d],"coords are missing", sep=" "))
             
  # Proceed to check for a geometry column:
#----If there's a geom column----
      if(length(grep("geo", colnames(data[[d]])))!=0){
        #has geometry column --> can go ahead and calculate centroid coords
        print(paste(classes[d],"coords can be calculated", sep=" "))
      
####Special Case(s) Additional Filtering####
    ## Roads:
#        if(classes[d]=="road"){
          # Filter roads to only Interstate, US Hwy, State Rte
#          data[[d]] <- subset(data[[d]], MTFCC=="S1100") #primary roads only
          ## Interstate, Us Hwy, or State Rte only
#          data[[d]] <- subset(data[[d]], RTTYP=="I" | FULLNAME %in% grep("US Hwy.*", data[[d]][["FULLNAME"]], value=TRUE) | RTTYP=="S")
          ## Shorten to rte number only
#          data[[d]][["FULLNAME"]] <- mgsub(data[[d]][["FULLNAME"]], pattern=c("\\I- ", "US Hwy ", "State Rte "), replacement=c("","",""))
          ## Remove remaining names with spaces (e.g. US Hwy Bus or Alt Rte)
#          data[[d]] <- subset(data[[d]], !(FULLNAME %in% grep(".* .*", data[[d]][["FULLNAME"]], value=TRUE)))
          # Save the filtered roads w/ geometry for line plotting later
 #         maplabs[[paste(classes[d],"_sf",sep="")]] <- data[[d]]
          #data[[d]] <- roads[!duplicated(roads$FULLNAME),] #don't want same rte labeled more than once ?
#        }
    ##

#----Calculate Centroid Coordinates----
        geoCol <- grep("geo", colnames(data[[d]]), value=TRUE) #returns all geo columns
        if(length(geoCol) > 1){geoCol <- geoCol[geoCol=="geometry"]} #aka geometry was already put in sf format; use that column
        print(paste("calculating", classes[d], "coords", sep=' '))
        data[[d]] <- centroid_coords(data=data[[d]], geoCol)
        
      }
      
#----If no geom column----
      if(length(grep("geo", colnames(data[[d]])))==0){
        #has no geometry column --> cannot calculate centroid coords
        message(paste("Error in",classes[d],
                 "layer: no geometry column or improper data format. \n Hint: was data input class SpatialXxxDataFrame?",sep=" "))
      }
      
    }
#----Now that coords are set, get class and label columns----    
  # Add a class column
    if( length(grep("class", colnames(data[[d]]), ignore.case=TRUE))==0 ){
      data[[d]][["class"]] <- rep(classes[d], nrow(data[[d]]))
    }
  # Some data may already have a class column specifying city/village or river/stream
    if( length(grep("class", colnames(data[[d]]), ignore.case=TRUE))!=0 ){
      colnames(data[[d]]) <- gsub("CLASS", "class", colnames(data[[d]]), ignore.case=TRUE ) #any CLASS, Class, etc --> class
    }
 
       
  # Search for possible label name columns and rename to "label":
    
    # Some data may already have a label column
    if (length(grep("label", colnames(data[[d]]), ignore.case=TRUE))!=0 ){
    } else {
      namecol_d <- grep(paste("^",classes[d],"$",sep=''), colnames(data[[d]]), ignore.case=TRUE, value=TRUE) 
      #^looks for anything containing the label's class name; case insensitive
      # the "^" means beginning of the line "$" means end of the line; therefore it will return the class name only
      # e.g. would return "Facility" or "facility" but NOT "Facility_hydroid"
      namecol_d <- c(namecol_d,  grep("name", colnames(data[[d]]), ignore.case=TRUE, value=TRUE) ) #looks for anything containing name,Name,or NAME
      colnames(data[[d]]) <- gsub(namecol_d[1], "label", colnames(data[[d]])) 
      #namecol_d[1] used because: a name matching the label's class is 1st priority, so whatever grep returns a value first will be the column used for labeling
      }
    
    
  # Add prepared label to labels list:
    data[[d]] <- data.frame(data[[d]]) #get rid of sfc geometry
    maplabs[[classes[d]]] <- data[[d]][,c("label","class","lat","lng")]
    
  } #End loop for(d in 1:length(data))
  
  return(maplabs) 
} #End Function

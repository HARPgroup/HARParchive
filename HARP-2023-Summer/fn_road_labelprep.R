#NHD data labeling prep -- originally part of fn_labelprep.R
# for current specific use:
# data = roads
fn_road_labs <- function(data) {
  
  ## Roads
    colnames(data[[d]]) <- gsub("RTTYP", "class", colnames(data[[d]]) ) #identify class column
    # Filter roads to only Interstate, US Hwy, State Rte
    data[[d]] <- subset(data[[d]], MTFCC=="S1100") #primary roads only
    ## Interstate, Us Hwy, or State Rte only
    data[[d]] <- subset(data[[d]], RTTYP=="I" | FULLNAME %in% grep("US Hwy.*", data[[d]][["FULLNAME"]], value=TRUE) | RTTYP=="S")
    ## Shorten to rte number only
    data[[d]][["FULLNAME"]] <- mgsub(data[[d]][["FULLNAME"]], pattern=c("\\I- ", "US Hwy ", "State Rte "), replacement=c("","",""))
    ## Remove remaining names with spaces (e.g. US Hwy Bus or Alt Rte)
    data[[d]] <- subset(data[[d]], !(FULLNAME %in% grep(".* .*", data[[d]][["FULLNAME"]], value=TRUE)))
    # Save the filtered roads w/ geometry for line plotting later
    roadlabs[[paste(classes[d],"_sf",sep="")]] <- data[[d]]
    #data[[d]] <- roads[!duplicated(roads$FULLNAME),] #don't want same rte labeled more than once ?
    
    data[[d]] <- data.frame(data[[d]]) #get rid of sfc geometry
    roadlabs[[classes[d]]] <- data[[d]][,c("label","class","lat","lng")]
    
    return(roadlabs) 
}
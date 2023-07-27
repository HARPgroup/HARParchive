#Road data labeling prep -- originally part of fn_labelprep.R
# for current specific use:
# data = roads
fn_road_labs <- function(data) {

roads <- list(sf=data)
#roadlabs <- list()
  
## Roads
  # Filter roads to only Interstate, US Hwy, State Rte
  roads$sf <- subset( roads$sf, roads$sf$MTFCC=="S1100") #primary roads only
  ## Interstate, Us Hwy, or State Rte only
  roads$sf <- subset(roads$sf, RTTYP=="I" | FULLNAME %in% grep("US Hwy.*", roads$sf[["FULLNAME"]], value=TRUE) | RTTYP=="S")
  ## Shorten to rte number only
  roads$sf[["FULLNAME"]] <- mgsub(roads$sf[["FULLNAME"]], pattern=c("\\I- ", "US Hwy ", "State Rte "), replacement=c("","",""))
  ## Remove remaining names with spaces (e.g. US Hwy Bus or Alt Rte)
  roads$sf <- subset(roads$sf, !(FULLNAME %in% grep(".* .*", roads$sf[["FULLNAME"]], value=TRUE)))
  ## Filtered roads w/ geometry are saved in roads$sf for line plotting later.
  # Remove duplicate labels --> plot all road segments, but label each road only once
  roads$labels <- roads$sf[!duplicated(roads$sf$FULLNAME),]
  names(roads$labels)[names(roads$labels) == 'FULLNAME'] <- 'label' #rename as label col
  colnames(roads$labels) <- gsub("RTTYP", "class", colnames(roads$labels) ) #identify class column
    
  roads$labels <- data.frame(roads$labels) #get rid of sfc geometry
  roads$labels <- roads$labels[,c("label","class","lat","lng")] # !! doesn't work because centroids were never calculated

  return(roads) 
}
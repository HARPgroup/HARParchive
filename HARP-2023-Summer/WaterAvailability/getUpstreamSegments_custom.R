getUpstreamSegs_custom <- function(cia_data_frame){
  # cia_data_frame <- rsegs[grepl(".+_.*[1-9]+.*_.*",rsegs$riverseg),]
  #Find the individual id of the river segment each segment flows to
  cia_data_frame$toSeg <- gsub(".+_[0-9]{4}_([0-9]{4}).*","\\1",
                               cia_data_frame$riverseg)
  #Get the individual id of each river segment for reference
  cia_data_frame$thisSeg <- gsub(".+_([0-9]{4})_[0-9]{4}.*","\\1",
                                 cia_data_frame$riverseg)
  
  #A basic function that finds the segments immediately upstream of a given
  #segment(s). end_seg may be a list of segments.
  nextUpSeg <- function(cia_data_frame,end_seg){
    #Default output is NULL
    out <- NULL
    #If the end segment is defined (e.g. not NA and has length greater than
    #zero), then get all segments upstream of all segments in end_seg. 
    if(all(!is.na(end_seg)) && length(end_seg) > 0){
      #First, unlist end_seg to make it easier to work with
      thisSeg <- unlist(end_seg)
      #Get the individual segment IDs of each segment in end_seg (i.e. excluding
      #the segment it flows to)
      thisSeg <- gsub(".+_([0-9]{4})_[0-9]{4}.*","\\1",thisSeg)
      #Combine the segments of thisSeg (which come from end_seg) into a single
      #or statement in regex form
      thisSeg <- paste0("(",unlist(thisSeg),")",collapse="|")
      #Return all river segments that have a "toSeg" column matching the ID in
      #thisSeg. In other words, find all segments that flow into the segments in
      #end_seg.
      out <- cia_data_frame$riverseg[grepl(thisSeg,cia_data_frame$toSeg)]
    }
    #If no segments match a valid end_seg input, there are no upstream segments.
    #Return NULL instead of the empty character vector that is otherwise
    #returned by the grepl call above. This is done for consistency of output so
    #that any segment with no upstream segments return NULL.
    if(length(out) == 0){
      out <- NULL
    }
    #Return out from this function
    return(out)
  }
  #Add the next upstream segment to the data frame by calling nextUpSeg() and
  #using the riverseg column as the first input. For each river segment, this
  #will return a character vector of all upstream segments
  cia_data_frame$nextUpSeg <- mapply(nextUpSeg,end_seg = cia_data_frame$riverseg,
                                     MoreArgs = list(
                                       cia_data_frame = cia_data_frame),
                                     SIMPLIFY = TRUE,USE.NAMES = FALSE)
  
  #Now, we need to find all segments upstream of the upstream segments. These
  #may branch into different tributaries so we will need to run nextUpSeg
  #repeatedly. Lets first create an object to store the results in. We will want
  #to store the final list of segments in cia_data_frame so lets add that as a
  #column, allUpstreamSegs
  cia_data_frame$allUpstreamSegs <- cia_data_frame$nextUpSeg
  #Store upstream segments as their own vector. This will be updated in each
  #loop iteration
  upstreamSegs <- cia_data_frame$allUpstreamSegs
  #Define a variable that shows when there are no more upstream segments for a
  #given river segment
  allAreNull <- FALSE
  #For QC to check for infinite loop, print j
  j <- 1
  #While there are values in upstreamSegs, find all river segments upstream of
  #the values in upstreamSegs and add to allUpstreamSegs
  while(!allAreNull) {
    #Print j in each loop and iterate j by 1 each loop
    print(j);j <- j+1
    #Now get all segments upstream of the upstream segments e.g. the next
    #segment upstream of upstreamSegs. if upstreamSegs is NULL, then there are
    #no more upstream segments of a given river segment and nextUpSeg will
    #automatically return NULL
    upstreamSegs <- mapply(nextUpSeg,end_seg = upstreamSegs,
                           MoreArgs = list(
                             cia_data_frame = cia_data_frame),
                           SIMPLIFY = TRUE,USE.NAMES = FALSE)
    
    #Combine new list of next upstream segments with existing list and unlist
    #for convenience. This gives us a vector for each river segment in
    #cia_data_frame that has allUpstreamSegs so far. upstreamSegs may still have
    #picked up on additional segments that will be searched for in the next
    #loop. Otherwise, upstreamSegs is now NULL and will be skipped.
    allUpstreamSegs <- mapply(list,cia_data_frame$allUpstreamSegs,upstreamSegs,
                              SIMPLIFY = FALSE,USE.NAMES = FALSE)
    allUpstreamSegs <- mapply(unlist,allUpstreamSegs,
                              SIMPLIFY = FALSE,USE.NAMES = FALSE)
    
    # #Find which lists added additional segments e.g. may have more additional
    # #upstream segments and need to be checked again
    # checkOut <- unlist(
    #   mapply(identical,cia_data_frame$allUpstreamSegs,allUpstreamSegs,
    #        SIMPLIFY = FALSE,USE.NAMES = FALSE)
    # )
    # 
    #Store the new list of upstream river segments in allUpstreamSegs
    cia_data_frame$allUpstreamSegs <- allUpstreamSegs
    
    #Check if all upstream segments are NULL. This will occur when all branches
    #of all river segments have been sorted through as a NULL is only returned
    #when no upstream segments remain. Otherwise, upstreamSegs will have segment
    #IDs and will need to be searched for the segments upstream of those
    allAreNull <- mapply(is.null,upstreamSegs)
    allAreNull <- all(allAreNull)
  }
  
  return(cia_data_frame)
}

######################TESTING########################
#Get all riversegments for reference
basepath='/var/www/R'
source('/var/www/R/config.R')
ds <- RomDataSource$new(site, rest_uname)
ds$get_token(rest_pw)
#Get all river segments
rsegs <- ds$auth_read(uri = paste0(site,"/vahydro_riversegs_nogeom_export")) 
#Remove outlets of form XXX_0000_0000
cia_data_frame <- rsegs[grepl(".+_.*[1-9]+.*_.*",rsegs$riverseg),]

#Use new method to get a list of all upstream segments
upstreamSegsnew <- getUpstreamSegs_custom(cia_data_frame)

#For comparison with fn_extract_basin(), add original segment into list of
#upstream segments and add as new column into cia_data_frame
allSegments <- mapply(list,upstreamSegsnew$allUpstreamSegs,
                      upstreamSegsnew$riverseg,
                          SIMPLIFY = FALSE,USE.NAMES = FALSE)
allSegments <- mapply(unlist,allSegments,
                          SIMPLIFY = FALSE,USE.NAMES = FALSE)
upstreamSegsnew$allSegments <- allSegments

#Use mapply of getUpstreamSegs on rsegs and outletWatersheds to get a list of
#data frames with the upstream segments of each outletWatershed. WARNING: THIS
#WILL TAKE A LONG TIME TO RUN, LIKELY MORE THAN 10 MINUTES
upstreamSegsOG <- mapply(FUN = getUpstreamSegs,end_seg = cia_data_frame,
                       MoreArgs = list(cia_data_frame = rsegs),
                       SIMPLIFY = FALSE,USE.NAMES = FALSE)

results <- logical()
#For each segment in the original method, check the segment list and compare to
#new method to ensure all segments in new method are in old method and vice
#versa
for(i in upstreamSegsOG){
  browser()
  outletSeg <- i$outletseg[1]
  oldMethod <- i$riverseg
  newMethod <- unlist(upstreamSegsnew$allSegments[upstreamSegsnew$riverseg == outletSeg])
  
  resultsi <- all(oldMethod %in% newMethod) & all(newMethod %in% oldMethod)
  results <- c(results,resultsi)
}
all(results)
#Conclusion: The new method produces an identical list of upstream segments in
#seconds compared to the old method, which takes ~15 minutes for all
#riversegments

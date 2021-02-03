fn_upstream <- function(RivSeg, AllSegList) {
  library(stringr)
  library(rapportools)
  # Create dataframe for upstream and downstream segments based on code in string
  ModelSegments <- data.frame(matrix(nrow = length(AllSegList), ncol = 5))
  colnames(ModelSegments)<- c('RiverSeg', 'Middle', 'Last', 'Downstream', 'Upstream')
  ModelSegments$RiverSeg <- AllSegList
  
  # Pull out 4 digit codes in middle and end for upstream/downstream segments
  i <- 1
  for (i in 1:nrow(ModelSegments)){
    
    ModelSegments[i,2]<- str_sub(ModelSegments[i,1], start=5L, end=8L)
    ModelSegments[i,3]<- str_sub(ModelSegments[i,1], start=10L, end=-1L)
    i <- i + 1
  }
  
  # Determine Downstream Segment ----------
  j <- 1
  for (j in 1:nrow(ModelSegments)){
    Downstream <- which(ModelSegments$Middle==ModelSegments$Last[j])
    if (length(Downstream)==0){
      ModelSegments[j,4]  <- 'NA'
    }else if (length(Downstream)!=0){
      ModelSegments[j,4] <- as.character(ModelSegments[Downstream,1])
    }
    j<-j+1
  }
  # Determine Upstream Segment ----------
  k<-1
  for (k in 1:nrow(ModelSegments)){
    Upstream <- which(as.character(ModelSegments$Downstream)==as.character(ModelSegments$RiverSeg[k]))
    NumUp <- ModelSegments$RiverSeg[Upstream]
    ModelSegments[k,5]<- paste(NumUp, collapse = '+')
    if (is.empty(ModelSegments[k,5])==TRUE){
      ModelSegments[k,5]<- 'NA'
    } 
    k<-k+1
  }
  SegUpstream <- as.numeric(which(as.character(ModelSegments$RiverSeg)==as.character(RivSeg)))
  SegUpstream <- ModelSegments$Upstream[SegUpstream]
  SegUpstream <- strsplit(as.character(SegUpstream), "\\+")
  SegUpstream <- try(SegUpstream[[1]], silent = TRUE)
  if (class(SegUpstream)=='try-error') {
    SegUpstream <- NA
  }
  return(SegUpstream)
}

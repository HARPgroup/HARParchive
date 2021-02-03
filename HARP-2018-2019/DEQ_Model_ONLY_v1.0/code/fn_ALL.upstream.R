fn_ALL.upstream <- function(RivSeg, AllSegList) {
  UpstreamSeg <- fn_upstream(RivSeg, AllSegList)
  AllUpstream <- character(0)
  BranchedSegs <- character(0)
  while (is.na(UpstreamSeg[1])==FALSE || is.empty(BranchedSegs) == FALSE) {
    while (is.na(UpstreamSeg[1])==FALSE) {
      num.segs <- as.numeric(length(UpstreamSeg))
      if (num.segs > 1) {
        BranchedSegs[(length(BranchedSegs)+1):(length(BranchedSegs)+num.segs-1)] <- UpstreamSeg[2:num.segs]
        UpstreamSeg <- UpstreamSeg[1]
      }
      AllUpstream[length(AllUpstream)+1] <- UpstreamSeg
      UpstreamSeg <- fn_upstream(UpstreamSeg, AllSegList)
    }
    num.branched <- as.numeric(length(BranchedSegs))
    UpstreamSeg <- BranchedSegs[1]
    BranchedSegs <- BranchedSegs[-1]
  }
  AllUpstream <- AllUpstream[which(AllUpstream != 'NA')]
  if (is.empty(AllUpstream)==TRUE) {
    AllUpstream <- 'NA'
  }
  return(AllUpstream)
}
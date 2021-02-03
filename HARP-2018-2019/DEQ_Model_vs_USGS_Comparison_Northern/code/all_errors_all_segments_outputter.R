# DOCUMENTATION -----------------------------------------------------------

# Creates an outputted table containing the percent error of every metric from
# every gage.  This script must be run after metric_calculator.R has been run
# for EVERY gage listed on the GageToSegment.csv file.

# INPUTS ------------------------------------------------------------------

# Address of "DEQ_Model_vs_USGS_Comparison" folder
# Include "DEQ_Model_vs_USGS_Comparison" in address!
container <- "C:\\Users\\FujitsuT\\Downloads\\HARP\\GitHub\\hydro-tools\\HARP-2018\\DEQ_Model_vs_USGS_Comparison_Northern"

# Should new or original data be used?
new.or.original <- "new"

# CARRYOVER IF MASTER IS BEING RUN ----------------------------------------
if (exists("container.master") == TRUE) {
  container <- container.master
  siteNo <- siteNo.master
  new.or.original <- new.or.original.master
}

# NEW OR ORIGINAL DATA SWITCH ---------------------------------------------

if (new.or.original == "new") {
  container.cont <- "\\results\\user's_results\\"
} else if (new.or.original == "original") {
  container.cont <- "\\results\\HARP_analysts'_results\\"
} else {
  print("ERROR: neither new or original data specified")
}

# LOADING GAGE TO SEGMENT DATA ---------------------------------------------------

gage.to.segment <- read.csv(file.path(container, "data", "Gage_To_Segment_Northern.csv"),
                            header = TRUE, sep = ',', stringsAsFactors = FALSE)

# COUNTS NUMBER OF RIVER SEGMENTS ------------------------------------------------
seg.names <- character(0)
corr.gages <- character(0)
seg.names.incrementer <- 1
num.gages <- as.numeric(length(gage.to.segment$gage_number))
num.segs <- 0
for (i in 1:num.gages) {
  siteNo <- gage.to.segment$gage_number[i]
  RivSeg <- gage.to.segment$river_segment[i]
  # Splitting the River Segment string into each segment name
  RivSegStr <- strsplit(RivSeg, "\\+")
  RivSegStr <- RivSegStr[[1]]
  segs.no <- as.numeric(length(RivSegStr))
  num.segs <- num.segs + segs.no
  sub.ctr <- 1
  for (sub.ctr in 1:segs.no) {
    seg.names[seg.names.incrementer] <- RivSegStr[sub.ctr]
    corr.gages[seg.names.incrementer] <- siteNo
    sub.ctr <- as.numeric(sub.ctr + 1)
    seg.names.incrementer <- seg.names.incrementer + 1
  }
}

# CALCULATES TOTAL NUMBER OF METRICS IN ALL METRICS ------------------------------
siteNo <- as.character(paste0("0",gage.to.segment$gage_number[1]))
RivSeg <- gage.to.segment$river_segment[1]
all.metrics <- read.csv(paste0(container, container.cont, siteNo, "_vs_", RivSeg, "\\Tables\\All_Metrics.csv"))
num.metrics <- length(all.metrics)-1
metrics.names <- colnames(all.metrics)
metrics.names <- as.character(metrics.names[2:(num.metrics+1)])
# Counts number of metrics, ignoring label column

# CREATES EMPTY DATA FRAME WITH DIMENSIONS OF ALL METRICS BY NUM.SEGS ------------
all.errors.all.segments <- data.frame(matrix(NA, nrow = num.segs, ncol = num.metrics))
colnames(all.errors.all.segments) <- metrics.names
rownames(all.errors.all.segments) <- seg.names

# POPULATES DATA FRAME WITH ALL_METRICS PERCENT ERROR DATA -----------------------
all.errors.line.no <- 1
for (i in 1:num.gages) {
  siteNo <- as.character(paste0("0",gage.to.segment$gage_number[i]))
  RivSeg <- gage.to.segment$river_segment[i]
  all.metrics <- read.csv(paste0(container, container.cont, siteNo, "_vs_", RivSeg, "\\Tables\\All_Metrics.csv"))
  RivSegStr <- strsplit(RivSeg, "\\+")
  RivSegStr <- RivSegStr[[1]]
  segs.no <- as.numeric(length(RivSegStr))
  if (segs.no == 1) {
    all.errors.all.segments[all.errors.line.no,] <- all.metrics[3,2:num.metrics]
    all.errors.line.no <- all.errors.line.no + 1
  } else {
    for (i in 1:segs.no) {
      all.errors.all.segments[all.errors.line.no,] <- all.metrics[3,2:num.metrics]
      all.errors.line.no <- all.errors.line.no + 1
    }
  }
}

for (i in 1:ncol(all.errors.all.segments)){
  all.errors.all.segments[,i][is.infinite(all.errors.all.segments[,i]) & all.errors.all.segments[,i] > 0] = 1000000000
  all.errors.all.segments[,i][is.infinite(all.errors.all.segments[,i]) & all.errors.all.segments[,i] < 0] = -1000000000
  all.errors.all.segments[,i][is.na(all.errors.all.segments[,i])] = 0
  # NA only arises when both percent errors are 0 -- thus making this a 0% error
}

# EXPORTS .CSV CONTAINING ALL ERRORS ---------------------------------------------
write.csv(all.errors.all.segments, paste0(container, "//spatial_analysis//user's_results//all.segments.pct.error.csv"))
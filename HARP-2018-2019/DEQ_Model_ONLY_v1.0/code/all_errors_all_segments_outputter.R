# DOCUMENTATION -----------------------------------------------------------

# Creates an outputted table containing every metric from
# every model river segment.  This script must be run after metric_calculator.R has been run
# for EVERY river segment listed on the RivSegList.csv file.

# INPUTS ------------------------------------------------------------------

# Address of "DEQ_Model_vs_USGS_Comparison" folder
# Include "DEQ_Model_vs_USGS_Comparison" in address!
container <- "C:\\Users\\Daniel\\Documents\\HARP\\DEQ_Model_ONLY_v1.0"

# Should new or original data be used?
new.or.original <- "new"

# CARRYOVER IF MASTER IS BEING RUN ----------------------------------------
if (exists("container.master") == TRUE) {
  container <- container.master
  RivSeg <- RivSeg.master
  new.or.original <- new.or.original.master
}

# NEW OR ORIGINAL DATA SWITCH ---------------------------------------------

if (new.or.original == "new") {
  container.cont <- "\\results\\user's_results\\"
} else if (new.or.original == "original") {
  container.cont <- "\\results\\harp_analysts'_results\\"
} else {
  print("ERROR: neither new or original data specified")
}

# LOADING LIST OF RIVER SEGS ---------------------------------------------------

all.riv.segs <- read.csv(file.path(container, "data", "RivSegListTemp.csv"),
                            header = TRUE, sep = ',', stringsAsFactors = FALSE)

# COUNTS NUMBER OF RIVER SEGMENTS ------------------------------------------------
num.segs <- as.numeric(nrow(all.riv.segs))
seg.names <- all.riv.segs$River.Segment

# CALCULATES TOTAL NUMBER OF METRICS IN ALL METRICS ------------------------------
RivSeg <- all.riv.segs$River.Segment[1]
all.metrics <- read.csv(paste0(container, container.cont, RivSeg, "\\All_Metrics.csv"))
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

for (i in 1:num.segs) {
  RivSeg <- all.riv.segs$River.Segment[i]
  all.metrics <- read.csv(paste0(container, container.cont,RivSeg, "\\All_Metrics.csv"))
  all.errors.all.segments[all.errors.line.no,] <- all.metrics[,2:num.metrics]
  all.errors.line.no <- all.errors.line.no + 1
}

# EXPORTS .CSV CONTAINING ALL ERRORS ---------------------------------------------
write.csv(all.errors.all.segments, paste0(container, "//spatial_analysis//user's_results//all.model.metrics.csv"))
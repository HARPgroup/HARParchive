container <- "C:\\Users\\Daniel\\Documents\\HARP\\DEQ_Model_ONLY_v1.0"
RivSeg <- "OD2_8920_8830"
all.segs.list <- read.csv(file.path(container, "data", "RivSegListTemp.csv"),
                          header = TRUE, sep = ',', stringsAsFactors = FALSE)
source(paste0(container, "\\code\\fn_upstream.R"))
source(paste0(container, "\\code\\fn_ALL.upstream.R"))

UpstreamSegs <- fn_upstream(RivSeg, all.segs.list)
ALLUpstreamSegs <- fn_ALL.upstream(RivSeg, all.segs.list)
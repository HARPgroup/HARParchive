# DOCUMENTATION -----------------------------------------------------------

# Runs all scripts for one or all model segments.  Can either use existing data or
# download new data.

# CLEARING ENVIRONMENT ----------------------------------------------------

rm(list = ls())

# LOADING RMARKDOWN -------------------------------------------------------

library(rmarkdown)

# INPUTS ------------------------------------------------------------------

# Address of "DEQ_Model_vs_USGS_Comparison" folder
# Include "DEQ_Model_vs_USGS_Comparison" in address!
container.master <- "C:\\Users\\Daniel\\Documents\\HARP\\DEQ_Model_ONLY_v1.0"

# USGS Gage number
# If "all" is inputted as RivSeg.master, analysis will be run for ALL gages
# stored in the RivSegList.csv file.
RivSeg.master <- "all"

# Should new or original data be used?
new.or.original.master <- "new"

# LINKING MODEL SEGMENT ---------------------------------------------------

all.segs.list <- read.csv(file.path(container.master, "data", "RivSegListTemp.csv"),
                            header = TRUE, sep = ',', stringsAsFactors = FALSE)


# IF "ALL", RUNS ALL GAGES THROUGH SCRIPTS --------------------------------
# otherwise, runs one specific gage through the scripts up to spatial analysis
if (RivSeg.master == "all") {
  seg.list <- all.segs.list$River.Segment
  no.segs <- length(seg.list)
  for (i in 1:no.segs) {
    RivSeg.master <- as.character(all.segs.list$River.Segment[i])
    if (new.or.original.master == "new") {
      print(paste("Downloading data for River Segment", RivSeg.master, "..."))
      rm(list=setdiff(ls(), c("container.master", "container.master.cont", "new.or.original.master", "RivSeg.master", "i", "gage.to.segment", "gage.list", "no.gages", "all.segs.list")))
      source(paste0(container.master,"\\code\\data_downloader.R"))
      print(paste("Prepping data for River Segment", RivSeg.master, "..."))
      rm(list=setdiff(ls(), c("container.master", "container.master.cont", "new.or.original.master", "RivSeg.master", "i", "gage.to.segment", "gage.list", "no.gages", "all.segs.list")))
      source(paste0(container.master,"\\code\\data_prepper.R"))
      print(paste("Calculating metrics for River Segment", RivSeg.master, "..."))
      rm(list=setdiff(ls(), c("container.master", "container.master.cont", "new.or.original.master", "RivSeg.master", "i", "gage.to.segment", "gage.list", "no.gages", "all.segs.list")))
      source(paste0(container.master,"\\code\\metric_calculator.R"))
    } else if (new.or.original.master == "original") {
      print(paste("Calculating metrics for River Segment", RivSeg.master, "..."))
      rm(list=setdiff(ls(), c("container.master", "container.master.cont", "new.or.original.master", "RivSeg.master", "i", "gage.to.segment", "gage.list", "no.gages", "all.segs.list")))
      source(paste0(container.master,"\\code\\metric_calculator.R"))
    }
  }
  print(paste("Organizing Calculated Metrics for All River Segments..."))
  rm(list=setdiff(ls(), c("container.master", "container.master.cont", "new.or.original.master", "RivSeg.master", "i", "gage.to.segment", "gage.list", "no.gages", "all.segs.list")))
  source(paste0(container.master,"\\code\\all_errors_all_segments_outputter.R"))
  print(paste("Performing spatial analysis for all segments..."))
  rm(list=setdiff(ls(), c("container.master", "container.master.cont", "new.or.original.master", "RivSeg.master", "i", "gage.to.segment", "gage.list", "no.gages", "all.segs.list")))
  source(paste0(container.master,"\\code\\spatial_analysis.R"))
} else {
  if (new.or.original.master == "new") {
    print(paste("Downloading data for River Segment", RivSeg.master, "..."))
    rm(list=setdiff(ls(), c("container.master", "container.master.cont", "new.or.original.master", "RivSeg.master", "i", "gage.to.segment", "gage.list", "no.gages", "all.segs.list")))
    source(paste0(container.master,"\\code\\data_downloader.R"))
    print(paste("Prepping data for River Segment", RivSeg.master, "..."))
    rm(list=setdiff(ls(), c("container.master", "container.master.cont", "new.or.original.master", "RivSeg.master", "i", "gage.to.segment", "gage.list", "no.gages", "all.segs.list")))
    source(paste0(container.master,"\\code\\data_prepper.R"))
    print(paste("Calculating metrics for River Segment", RivSeg.master, "..."))
    rm(list=setdiff(ls(), c("container.master", "container.master.cont", "new.or.original.master", "RivSeg.master", "i", "gage.to.segment", "gage.list", "no.gages", "all.segs.list")))
    source(paste0(container.master,"\\code\\metric_calculator.R"))
  } else if (new.or.original.master == "original") {
    print(paste("Calculating metrics for River Segment", RivSeg.master, "..."))
    rm(list=setdiff(ls(), c("container.master", "container.master.cont", "new.or.original.master", "RivSeg.master", "i", "gage.to.segment", "gage.list", "no.gages", "all.segs.list")))
    source(paste0(container.master,"\\code\\metric_calculator.R"))
  }
}

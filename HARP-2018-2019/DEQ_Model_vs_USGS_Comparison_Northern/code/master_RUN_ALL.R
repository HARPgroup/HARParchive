# DOCUMENTATION -----------------------------------------------------------

# Runs all scripts for one or all gages.  Can either use existing data or
# download new data.

# CLEARING ENVIRONMENT ----------------------------------------------------

rm(list = ls())

# LOADING RMARKDOWN -------------------------------------------------------

library(rmarkdown)

# INPUTS ------------------------------------------------------------------

# Address of "DEQ_Model_vs_USGS_Comparison" folder
# Include "DEQ_Model_vs_USGS_Comparison" in address!
container.master <- "C:\\Users\\FujitsuT\\Downloads\\HARP\\GitHub\\hydro-tools\\HARP-2018\\DEQ_Model_vs_USGS_Comparison_Northern"

# USGS Gage number
# If "all" is inputted as siteNo.master, analysis will be run for ALL gages
# stored in the Gage.To.Segment.csv file.
siteNo.master <- "01660400"

# Should new or original data be used?
new.or.original.master <- "new"

# LINKING MODEL SEGMENT ---------------------------------------------------

gage.to.segment <- read.csv(file.path(container.master, "data", "Gage_To_Segment_Northern.csv"),
                            header = TRUE, sep = ',', stringsAsFactors = FALSE)


# IF "ALL", RUNS ALL GAGES THROUGH SCRIPTS --------------------------------
# otherwise, runs one specific gage through the scripts up to spatial analysis
if (siteNo.master == "all") {
  gage.list <- gage.to.segment$gage_number
  no.gages <- length(gage.list)
  for (i in 1:no.gages) {
    gage.to.segment <- read.csv(file.path(container.master, "data", "Gage_To_Segment_Northern.csv"),
                                header = TRUE, sep = ',', stringsAsFactors = FALSE)
    siteNo.master <- paste0("0", as.character(gage.list[i]))
    gage.to.segment <- subset(gage.to.segment, gage.to.segment$gage_number == as.numeric(siteNo.master))
    RivSeg.master <- gage.to.segment$river_segment
    if (new.or.original.master == "new") {
      rm(list=setdiff(ls(), c("container.master", "container.master.cont", "siteNo.master", "new.or.original.master", "RivSeg.master", "i", "gage.to.segment", "gage.list", "no.gages", "gage.to.segmentsub")))
      print(paste0('Downloading data for segment ', i, ' of ', as.numeric(no.gages)))
      source(paste0(container.master,"\\code\\data_downloader.R"))
      rm(list=setdiff(ls(), c("container.master", "container.master.cont", "siteNo.master", "new.or.original.master", "RivSeg.master", "i", "gage.to.segment", "gage.list", "no.gages", "gage.to.segmentsub")))
      print(paste0('Prepping data for segment ', i, ' of ', as.numeric(no.gages)))
      source(paste0(container.master,"\\code\\data_prepper.R"))
      rm(list=setdiff(ls(), c("container.master", "container.master.cont", "siteNo.master", "new.or.original.master", "RivSeg.master", "i", "gage.to.segment", "gage.list", "no.gages", "gage.to.segmentsub")))
      print(paste0('Calculating metrics for segment ', i, ' of ', as.numeric(no.gages)))
      source(paste0(container.master,"\\code\\metric_calculator.R"))
      rm(list=setdiff(ls(), c("container.master", "container.master.cont", "siteNo.master", "new.or.original.master", "RivSeg.master", "i", "gage.to.segment", "gage.list", "no.gages", "gage.to.segmentsub")))
      print(paste0('Creating plots for segment ', i, ' of ', as.numeric(no.gages)))
      source(paste0(container.master,"\\code\\plot_creator.R"))
      rm(list=setdiff(ls(), c("container.master", "container.master.cont", "siteNo.master", "new.or.original.master", "RivSeg.master", "i", "gage.to.segment", "gage.list", "no.gages", "gage.to.segmentsub")))
      print(paste0('Calculating highest error periods for segment ', i, ' of ', as.numeric(no.gages)))
      source(paste0(container.master,"\\code\\error_calculator.R"))
      rm(list=setdiff(ls(), c("container.master", "container.master.cont", "siteNo.master", "new.or.original.master", "RivSeg.master", "i", "gage.to.segment", "gage.list", "no.gages", "gage.to.segmentsub")))
      print(paste0('Creating dashboard document for segment ', i, ' of ', as.numeric(no.gages)))
      #rmarkdown::render(paste0(container.master, "\\code\\dashboard_creator.Rmd"), "pdf_document", 
                        #output_dir = paste0(container.master, "\\results\\user's_results\\", siteNo.master, "_vs_", RivSeg.master),
                        #output_file = paste0(siteNo.master, " - Gage vs. Model Dashboard.pdf"))
    } else if (new.or.original.master == "original") {
      rm(list=setdiff(ls(), c("container.master", "container.master.cont", "siteNo.master", "new.or.original.master", "RivSeg.master", "i", "gage.to.segment", "gage.list", "no.gages", "gage.to.segmentsub")))
      source(paste0(container.master,"\\code\\metric_calculator.R"))
      rm(list=setdiff(ls(), c("container.master", "container.master.cont", "siteNo.master", "new.or.original.master", "RivSeg.master", "i", "gage.to.segment", "gage.list", "no.gages", "gage.to.segmentsub")))
      # source(paste0(container.master,"\\code\\plot_creator.R"))
      # rm(list=setdiff(ls(), c("container.master", "container.master.cont", "siteNo.master", "new.or.original.master", "RivSeg.master", "i", "gage.to.segment", "gage.list", "no.gages", "gage.to.segmentsub")))
      # source(paste0(container.master,"\\code\\error_calculator.R"))
      # rm(list=setdiff(ls(), c("container.master", "container.master.cont", "siteNo.master", "new.or.original.master", "RivSeg.master", "i", "gage.to.segment", "gage.list", "no.gages", "gage.to.segmentsub")))
      # rmarkdown::render(paste0(container.master, "\\code\\dashboard_creator.Rmd"), "pdf_document",
      #                   output_dir = paste0(container.master, "\\results\\user's_results\\", siteNo.master, "_vs_", RivSeg.master),
      #                   output_file = paste0(siteNo.master, " - Gage vs. Model Dashboard.pdf"))
    }
  }
  rm(list=setdiff(ls(), c("container.master", "container.master.cont", "siteNo.master", "new.or.original.master", "RivSeg.master", "i", "gage.to.segment", "gage.list", "no.gages", "gage.to.segmentsub")))
  source(paste0(container.master,"\\code\\all_errors_all_segments_outputter.R"))
  rm(list=setdiff(ls(), c("container.master", "container.master.cont", "siteNo.master", "new.or.original.master", "RivSeg.master", "i", "gage.to.segment", "gage.list", "no.gages", "gage.to.segmentsub")))
  source(paste0(container.master,"\\code\\spatial_analysis.R"))
} else {
  gage.to.segment <- read.csv(file.path(container.master, "data", "Gage_To_Segment_Northern.csv"),
                              header = TRUE, sep = ',', stringsAsFactors = FALSE)
  gage.to.segment <- subset(gage.to.segment, gage.to.segment$gage_number == as.numeric(siteNo.master))
  RivSeg.master <- gage.to.segment$river_segment
  if (new.or.original.master == "new") {
    rm(list=setdiff(ls(), c("container.master", "container.master.cont", "siteNo.master", "new.or.original.master", "RivSeg.master", "i", "gage.to.segment", "gage.list", "no.gages", "gage.to.segmentsub")))
    source(paste0(container.master,"\\code\\data_downloader.R"))
    rm(list=setdiff(ls(), c("container.master", "container.master.cont", "siteNo.master", "new.or.original.master", "RivSeg.master", "i", "gage.to.segment", "gage.list", "no.gages", "gage.to.segmentsub")))
    source(paste0(container.master,"\\code\\data_prepper.R"))
    rm(list=setdiff(ls(), c("container.master", "container.master.cont", "siteNo.master", "new.or.original.master", "RivSeg.master", "i", "gage.to.segment", "gage.list", "no.gages", "gage.to.segmentsub")))
    source(paste0(container.master,"\\code\\metric_calculator.R"))
    rm(list=setdiff(ls(), c("container.master", "container.master.cont", "siteNo.master", "new.or.original.master", "RivSeg.master", "i", "gage.to.segment", "gage.list", "no.gages", "gage.to.segmentsub")))
    source(paste0(container.master,"\\code\\plot_creator.R"))
    rm(list=setdiff(ls(), c("container.master", "container.master.cont", "siteNo.master", "new.or.original.master", "RivSeg.master", "i", "gage.to.segment", "gage.list", "no.gages", "gage.to.segmentsub")))
    source(paste0(container.master,"\\code\\error_calculator.R"))
    rm(list=setdiff(ls(), c("container.master", "container.master.cont", "siteNo.master", "new.or.original.master", "RivSeg.master", "i", "gage.to.segment", "gage.list", "no.gages", "gage.to.segmentsub")))
    # rmarkdown::render(paste0(container.master, "\\code\\dashboard_creator.Rmd"), "pdf_document", 
    #                   output_dir = paste0(container.master, "\\results\\user's_results\\", siteNo.master, "_vs_", RivSeg.master),
    #                   output_file = paste0(siteNo.master, " - Gage vs. Model Dashboard.pdf"))
  } else if (new.or.original.master == "original") {
    rm(list=setdiff(ls(), c("container.master", "container.master.cont", "siteNo.master", "new.or.original.master", "RivSeg.master", "i", "gage.to.segment", "gage.list", "no.gages", "gage.to.segmentsub")))
    source(paste0(container.master,"\\code\\metric_calculator.R"))
    rm(list=setdiff(ls(), c("container.master", "container.master.cont", "siteNo.master", "new.or.original.master", "RivSeg.master", "i", "gage.to.segment", "gage.list", "no.gages", "gage.to.segmentsub")))
    source(paste0(container.master,"\\code\\plot_creator.R"))
    rm(list=setdiff(ls(), c("container.master", "container.master.cont", "siteNo.master", "new.or.original.master", "RivSeg.master", "i", "gage.to.segment", "gage.list", "no.gages", "gage.to.segmentsub")))
    source(paste0(container.master,"\\code\\error_calculator.R"))
    rm(list=setdiff(ls(), c("container.master", "container.master.cont", "siteNo.master", "new.or.original.master", "RivSeg.master", "i", "gage.to.segment", "gage.list", "no.gages", "gage.to.segmentsub")))
    rmarkdown::render(paste0(container.master, "\\code\\dashboard_creator.Rmd"), "pdf_document", 
                      output_dir = paste0(container.master, "\\results\\user's_results\\", siteNo.master, "_vs_", RivSeg.master),
                      output_file = paste0(siteNo.master, " - Gage vs. Model Dashboard.pdf"))
  }
}
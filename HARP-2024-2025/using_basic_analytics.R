source("C:/Users/natef/OneDrive - Virginia Tech/HARP/Github/HARParchive/HARP-2024-2025/basic analytics for vahydro.R")

prism <- read.csv("http://deq1.bse.vt.edu:81/met/stormVol_prism/precip/usgs_ws_01613900-PRISM-all.csv")
prism.summary <- prism_summary_analytics(prism)

daymet <- read.csv("http://deq1.bse.vt.edu:81/met/daymet/precip/usgs_ws_01613900-daymet-all.csv")
daymet.summary <- daymet_summary_analytics(daymet)

nldas2 <- read.csv("http://deq1.bse.vt.edu:81/met/nldas2/precip/usgs_ws_01613900-nldas2-all.csv")
nldas2.summary <- nldas2_summary_analytics(nldas2)

analytics <- data.frame(metric, prism.summary)#, daymet.summary, nldas2.summary)

precip_annual_max_in <- analytics$prism.summary[1]

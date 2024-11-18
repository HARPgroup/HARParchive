source("C:/Users/natef/OneDrive - Virginia Tech/HARP/Github/HARParchive/HARP-2024-2025/basic_analytics_for_vahydro.R")
library("IHA")
library("ggplot2")
library(sqldf)
prism <- read.csv("http://deq1.bse.vt.edu:81/met/stormVol_prism/precip/usgs_ws_01613900-PRISM-all.csv")
# prism_summary <- summary_analytics(prism)
# 
# 
daymet <- read.csv("http://deq1.bse.vt.edu:81/met/daymet/precip/usgs_ws_01613900-daymet-all.csv")
# daymet_summary <- summary_analytics(daymet)
# 
nldas2 <- read.csv("http://deq1.bse.vt.edu:81/met/nldas2/precip/usgs_ws_01613900-nldas2-all.csv")
# nldas2_summary <- summary_analytics(nldas2) 

#pull any value like this:

# prism_l90_precip_in <- prism_summary$l90_precip_in
# daymet_l90_precip_in <- daymet_summary$l90_precip_in
# nldas2_l90_precip_in <- nldas2_summary$l90_precip_in

#Or even without the summary:

summary_analytics(prism)$l90_precip_in


#visualizing data:

# visualizations <- function(prism.metric, daymet.metric, nldas2.metric, title = NULL){
# ggplot(data = NULL, mapping = aes(x = c("prism","dayment","nldas2"),
# y = c(prism.metric, daymet.metric, nldas2.metric)))+
#   geom_bar(stat = "identity", fill = "lightblue3")+
#   labs(title = title, y = "precip (in)", x = "model")}
# 
# visualizations(prism.metric = prism_summary$precip_annual_max_in,
#                daymet.metric = daymet_summary$precip_annual_max_in,
#                nldas2.metric = nldas2_summary$precip_annual_max_in,
#               title = "yearly max precip")
# visualizations(prism.metric = prism_summary$precip_annual_min_in,
#                daymet.metric = daymet_summary$precip_annual_min_in,
#                nldas2.metric = nldas2_summary$precip_annual_min_in,
#                title = "yearly min precip")

visualizations <- function(gage_id, metric){
  ggplot(data = NULL, mapping = aes(x = c("prism","dayment","nldas2"),
                                    y = c(summary_analytics(read.csv(paste0("http://deq1.bse.vt.edu:81/met/stormVol_prism/precip/",gage_id,"-PRISM-all.csv")))[[metric]], 
                                          summary_analytics(read.csv(paste0("http://deq1.bse.vt.edu:81/met/daymet/precip/",gage_id,"-daymet-all.csv")))[[metric]],
                                          summary_analytics(read.csv(paste0("http://deq1.bse.vt.edu:81/met/nldas2/precip/",gage_id,"-nldas2-all.csv")))[[metric]])))+
    geom_bar(stat = "identity", fill = "lightblue3")+
    labs(title = as.character(metric), y = "precip (in)", x = "model")}

# perform basic bar plot visualizations for any of these 5 metrics for any gage
visualizations(gage_id = "usgs_ws_01613900", metric = "precip_annual_max_in")
visualizations(gage_id = "usgs_ws_01613900", metric = "l90_precip_in")
visualizations(gage_id = "usgs_ws_01613900", metric = "precip_annual_mean_in")
visualizations(gage_id = "usgs_ws_01613900", metric = "precip_annual_min_in")
visualizations(gage_id = "usgs_ws_01613900", metric = "precip_daily_max_in")


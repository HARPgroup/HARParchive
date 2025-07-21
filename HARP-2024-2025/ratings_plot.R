suppressPackageStartupMessages(library(ggplot2))

# Set Arguments
args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 2) {
  message("Usage: Rscript ratings_plot.R ratings_ts_path final_plot_path")
  q()
}

 args[1]<-"http://deq1.bse.vt.edu:81/met/simple_lm_PRISM/out/usgs_ws_01613900-rating-ts.csv"
# args[1]<-"http://deq1.bse.vt.edu:81/met/simple_lm_daymet/out/usgs_ws_01613900-rating-ts.csv"
# args[1]<-"http://deq1.bse.vt.edu:81/met/simple_lm_nldas2_tiled/out/usgs_ws_01613900-rating-ts.csv"
# args[1]<-"http://deq1.bse.vt.edu:81/met/stormVol_prism/out/usgs_ws_01613900-rating-ts.csv"
# args[1]<-"http://deq1.bse.vt.edu:81/met/stormVol_daymet/out/usgs_ws_01613900-rating-ts.csv"
# args[1]<-"http://deq1.bse.vt.edu:81/met/stormVol_nldas2_tiled/out/usgs_ws_01613900-rating-ts.csv"

ratings_ts_path <- args[1]
final_plot_path <- args[2]
hydrocode <- args[3]
scenario <- args[4]

# Read ratings csv
ratings_ts <- read.csv(ratings_ts_path)

ratings_ts$start_date <- as.Date(ratings_ts$start_date)

plot_title<-paste(hydrocode,scenario)

# Create plot
ratings_plot <- ggplot(data=ratings_ts, aes(x=start_date, y=rating))+
  geom_point(size=0.5)+
  theme_bw()+
  coord_cartesian(ylim = c(-1,1))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_date(date_breaks = "5 years", date_labels = "%Y")+
  xlab("Date")+
  ylab("Rating")+
  ggtitle(plot_title)

ratings_plot

# Boxplot for all ratings data or quantile stats

write(ratings_plot, plot_final_path)


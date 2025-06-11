##Compares yearly precip values for daymet, nldas, and prism in a figure
###works for any gage input from the server.


library(sqldf)
library(ggplot2)


yearly_summary <- function(df){
  
  daily_summary <- 
    sqldf(
      "SELECT yr, mo, da, obs_date, SUM(precip_in) AS total_precip
  FROM df
  GROUP BY yr, mo, da"
    )
  
  #creating a yearly summary with each year and its total precip
  yearly_summary <- 
    sqldf(
      "SELECT COUNT(*) AS ndays, yr, SUM(total_precip) AS total_precip
  FROM daily_summary
  GROUP BY yr
      HAVING ndays >= 364"
    )
  
}

model_comparison <- function(gage_id){
prism <- read.csv(paste0("http://deq1.bse.vt.edu:81/met/PRISM/precip/",gage_id,"_precip.csv"))
daymet <- read.csv(paste0("http://deq1.bse.vt.edu:81/met/daymet/precip/",gage_id,"_precip.csv"))
nldas2 <- read.csv(paste0("http://deq1.bse.vt.edu:81/met/nldas2/precip/",gage_id,"_precip.csv"))

prism_sum <- yearly_summary(prism)
daymet_sum <- yearly_summary(daymet)
nldas_sum <- yearly_summary(nldas2)

# A series of SQL joins and unions that produce a tidy, combined data frame

## joins prism and daymet
j <- sqldf(
  "SELECT daymet_sum.yr , prism_sum.total_precip AS prism_precip , daymet_sum.total_precip AS daymet_precip
  FROM prism_sum
  FULL OUTER JOIN daymet_sum ON prism_sum.yr=daymet_sum.yr"
)

## joins nldas to prism&daymet data frame
j2 <- sqldf(
  "SELECT j.yr , j.prism_precip , j.daymet_precip, nldas_sum.total_precip AS nldas_precip
  FROM j
  FULL OUTER JOIN nldas_sum on j.yr=nldas_sum.yr"
)

## tidies data -- pivot longer
j3 <- sqldf(
  "SELECT yr , 'prism' AS model , prism_precip AS precip_in FROM j2
  UNION ALL
  SELECT yr , 'daymet' AS model , daymet_precip AS precip_in FROM j2
  UNION ALL
  SELECT yr , 'nldas' AS model , nldas_precip AS precip_in FROM j2"
)


# Plot with all models and years

yearly_precip_plot <- ggplot(j3, aes(x = yr, y = precip_in, fill = model, color = model))+
  geom_histogram(stat = "identity", position = "identity", binwidth = 1, alpha = 0.4)+
  theme_bw()+
  labs(title = 
  paste("Yearly Precip Model Comparison for gage",gage_id), x = "Year", y = "Precipitation (in)")
return(yearly_precip_plot)
}

model_comparison("usgs_ws_01613900")
model_comparison("usgs_ws_01615000") #NLDAS missing
model_comparison("usgs_ws_01616100")
model_comparison("usgs_ws_01620500") #NLDAS missing
model_comparison("usgs_ws_01621050")
model_comparison("usgs_ws_01622000")
model_comparison("usgs_ws_01625000")
model_comparison("usgs_ws_01626000") #NLDAS missing
model_comparison("usgs_ws_01626850")
model_comparison("usgs_ws_01627500")
model_comparison("usgs_ws_01631000")
model_comparison("usgs_ws_01628500")
model_comparison("usgs_ws_01629500")


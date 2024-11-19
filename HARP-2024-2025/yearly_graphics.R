library(sqldf)
library(ggplot2)
library(patchwork)
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

prism <- read.csv("http://deq1.bse.vt.edu:81/met/stormVol_prism/precip/usgs_ws_01613900-PRISM-all.csv")
daymet <- read.csv("http://deq1.bse.vt.edu:81/met/daymet/precip/usgs_ws_01613900-daymet-all.csv")
nldas2 <- read.csv("http://deq1.bse.vt.edu:81/met/nldas2/precip/usgs_ws_01613900-nldas2-all.csv")

prism_sum <- yearly_summary(prism)
daymet_sum <- yearly_summary(daymet)
nldas_sum <- yearly_summary(nldas2)


# Example plots for each 
ggplot(prism_sum, mapping = aes(x = yr, y = total_precip))+
  geom_histogram(stat = "identity", binwidth = 1, fill = "lightblue1")+
  theme_bw()

ggplot(daymet_sum, mapping = aes(x = yr, y = total_precip))+
  geom_histogram(stat = "identity", binwidth = 1, fill = "lightblue2")+
  theme_bw()

ggplot(nldas_sum, mapping = aes(x = yr, y = total_precip))+
  geom_histogram(stat = "identity", binwidth = 1, fill = "lightblue3")+
  theme_bw()


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
  "Yearly Precip Model Comparison
  Hogue Creek Near Hayfield VA", x = "Year", y = "Precipitation (in)")




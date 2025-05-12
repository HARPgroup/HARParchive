# Percent Diifference between models

## This should help us to decifer temporal and spatial model varation trends

## Create with all months
## filter 1984-2023

library(sqldf)
library(ggplot2)
library(dplyr)
library(cowplot)
monthly_summary <- function(df){
  
  daily_summary <- 
    sqldf(
      "SELECT yr, mo, da, obs_date, SUM(precip_in) AS total_precip
  FROM df
  GROUP BY yr, mo, da"
    )
  
  #creating a monthly summary with each month and its total precip
  monthly_summary <- 
    sqldf(
      "SELECT COUNT(*) AS ndays, mo, SUM(total_precip) AS total_precip
  FROM daily_summary
  WHERE yr BETWEEN '1984' AND '2023'
  GROUP BY mo"
    )
  
}

model_comparison <- function(gage_id){
  
  prism <- read.csv(paste0("http://deq1.bse.vt.edu:81/met/PRISM/precip/",gage_id,"_precip.csv"))
  daymet <- read.csv(paste0("http://deq1.bse.vt.edu:81/met/daymet/precip/",gage_id,"_precip.csv"))
  nldas2 <- read.csv(paste0("http://deq1.bse.vt.edu:81/met/nldas2/precip/",gage_id,"_precip.csv"))
  
  prism_sum <- monthly_summary(prism)
  daymet_sum <- monthly_summary(daymet)
  nldas_sum <- monthly_summary(nldas2)
  
  # A series of SQL joins and unions that produce a tidy, combined data frame
  
  ## joins prism and daymet
  j <- sqldf(
    "SELECT daymet_sum.mo , prism_sum.total_precip / prism_sum.ndays AS prism_precip , 
    daymet_sum.total_precip / daymet_sum.ndays AS daymet_precip
  FROM prism_sum
  FULL OUTER JOIN daymet_sum ON prism_sum.mo=daymet_sum.mo"
  )
  
  ## joins nldas to prism&daymet data frame
  j2 <- sqldf(
    "SELECT j.mo , j.prism_precip , j.daymet_precip, 
    nldas_sum.total_precip / nldas_sum.ndays AS nldas_precip
  FROM j
  FULL OUTER JOIN nldas_sum on j.mo=nldas_sum.mo"
  )
  
  ## tidies data -- pivot longer
  j3 <- sqldf(
    "SELECT mo , 'prism' AS model , prism_precip AS precip_in FROM j2
  UNION ALL
  SELECT mo , 'daymet' AS model , daymet_precip AS precip_in FROM j2
  UNION ALL
  SELECT mo , 'nldas' AS model , nldas_precip AS precip_in FROM j2
  ORDER BY mo"
  )
  j3
  
  percent_diff_abs <- j3|>
    group_by(mo)|>
    mutate(percent_diff_abs = abs(((precip_in-(sum(precip_in)/3))/(sum(precip_in)/3))*100))
  
  percent_diff <- j3|>
    group_by(mo)|>
    mutate(percent_diff = ((precip_in-(sum(precip_in)/3))/(sum(precip_in)/3))*100)
  
  p1 <- ggplot(percent_diff_abs,mapping = aes(x = mo, y = percent_diff_abs, fill = model))+
    geom_bar(stat = "identity", position = "dodge")+
    labs(title = gage_id)
  
  p2 <- ggplot(percent_diff,mapping = aes(x = mo, y = percent_diff, fill = model))+
    geom_bar(stat = "identity", position = "dodge")+
    labs(title = gage_id)
  
  
  plot_grid(p1,p2)
}

# examples
model_comparison("usgs_ws_01613900")
model_comparison("usgs_ws_01616100")
model_comparison("usgs_ws_01621050")
model_comparison("usgs_ws_01622000")
model_comparison("usgs_ws_01625000")
model_comparison("usgs_ws_01626850")
model_comparison("usgs_ws_01627500")
model_comparison("usgs_ws_01631000")
model_comparison("usgs_ws_01628500")
model_comparison("usgs_ws_01629500")

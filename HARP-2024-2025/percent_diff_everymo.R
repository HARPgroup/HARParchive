## This should help us to decifer temporal model varation trends

## Monthly precip data compared to each other and visualized
## filter 1984-2023

library(sqldf)
library(ggplot2)
library(dplyr)
library(cowplot)
library(tidyr)
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
      "SELECT COUNT(*) AS ndays, obs_date, SUM(total_precip) AS total_precip
  FROM daily_summary
  WHERE yr BETWEEN '1984' AND '2023'
  GROUP BY yr, mo"
    )
  separate(monthly_summary, col = "obs_date", into = c("obs_date", "obs_time"),
           sep = " ")
  
  
  
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
    "SELECT daymet_sum.obs_date , prism_sum.total_precip / prism_sum.ndays AS prism_precip_ave , 
    daymet_sum.total_precip / daymet_sum.ndays AS daymet_precip_ave
  FROM prism_sum
  FULL OUTER JOIN daymet_sum ON prism_sum.obs_date=daymet_sum.obs_date"
  )
  
  ## joins nldas to prism&daymet data frame
  j2 <- sqldf(
    "SELECT j.obs_date , j.prism_precip_ave , j.daymet_precip_ave, 
    nldas_sum.total_precip / nldas_sum.ndays AS nldas_precip_ave
  FROM j
  FULL OUTER JOIN nldas_sum on j.obs_date=nldas_sum.obs_date"
  )
  j2 <- drop_na(j2)
  ## tidies data -- pivot longer
  j3 <- sqldf(
    "SELECT obs_date , 'prism' AS model , prism_precip_ave AS precip_in FROM j2
  UNION ALL
  SELECT obs_date , 'daymet' AS model , daymet_precip_ave AS precip_in FROM j2
  UNION ALL
  SELECT obs_date , 'nldas' AS model , nldas_precip_ave AS precip_in FROM j2
  ORDER BY obs_date"
  )
  
  
  percent_diff_abs <- j3|>
    group_by(obs_date)|>
    mutate(percent_diff_abs = abs(((precip_in-(sum(precip_in)/3))/(sum(precip_in)/3))*100))
  
  percent_diff <- j3|>
    group_by(obs_date)|>
    mutate(percent_diff = ((precip_in-(sum(precip_in)/3))/(sum(precip_in)/3))*100)|>
    mutate(residual = precip_in-mean(precip_in))|>
    mutate(var = var(precip_in))|>
    mutate(CV = sqrt(var)/mean(precip_in))
  
  percent_diff$obs_date <- as.Date(percent_diff$obs_date)
  
  p1 <- ggplot(percent_diff_abs,mapping = aes(x = obs_date, y = percent_diff_abs, fill = model))+
    geom_bar(stat = "identity", position = "dodge")+
    labs(title = gage_id)
  
  p2 <- ggplot(percent_diff,mapping = aes(x = obs_date, y = percent_diff, fill = model))+
    geom_bar(stat = "identity", position = "dodge")+
    labs(title = gage_id)
  
  p3 <- ggplot(percent_diff, mapping = aes(x = obs_date, y = var))+
    geom_bar(stat = "identity")+
    labs(title = gage_id)
  
  p4 <- ggplot(percent_diff, mapping = aes(x = obs_date, y = CV))+
    geom_bar(stat = "identity")+
    labs(title = gage_id)
  
  plot_grid(p2,p3,p4,ncol = 1)
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

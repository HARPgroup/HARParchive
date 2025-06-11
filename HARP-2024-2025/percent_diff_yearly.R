# Percent Diifference between models


## This should help us to decifer temporal and spatial model varation trends
library(sqldf)
library(ggplot2)
library(dplyr)
library(cowplot)
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
  SELECT yr , 'nldas' AS model , nldas_precip AS precip_in FROM j2
  ORDER BY yr"
)
j3

percent_diff_abs <- j3|>
  group_by(yr)|>
  mutate(percent_diff = abs(((precip_in-(sum(precip_in)/3))/(sum(precip_in)/3))*100))

percent_diff <- j3|>
  group_by(yr)|>
  mutate(percent_diff = ((precip_in-(sum(precip_in)/3))/(sum(precip_in)/3))*100)

p1 <- ggplot(percent_diff_abs,mapping = aes(x = yr, y = percent_diff, color = model))+
  geom_smooth(method = "loess")+
  labs(title = gage_id)

p2 <- ggplot(percent_diff,mapping = aes(x = yr, y = percent_diff, color = model))+
  geom_smooth(method = "loess")+
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
model_comparison("usgs_ws_01626000")

## Figuring out how to preserve all value in a year while running analytics on that year as a whole...
#Using dplyr

# percent_diff_abs <- df|>
#   group_by(yr)|>
#   mutate(percent_diff = abs(((precip_in-(sum(precip_in)/3))/(sum(precip_in)/3))*100))
#   
# percent_diff <- df|>
#   group_by(yr)|>
#   mutate(percent_diff = ((precip_in-(sum(precip_in)/3))/(sum(precip_in)/3))*100)
# 
# # Can't figure out sql code for percent_diff
# # sqldf(
# #   "SELECT yr , COUNT(model) , COUNT(precip_in)
# #   FROM j3
# #   GROUP BY yr"
# # )
# # 
# # sqldf(
# #   "SELECT yr , model, precip_in, (precip_in-(SUM(precip_in)/3))/(SUM(precip_in)/3) AS percent_diff
# #   FROM df
# #   GROUP BY yr"
# # )
# 
# 
# p1 <- ggplot(percent_diff_abs,mapping = aes(x = yr, y = percent_diff, color = model))+
#   geom_smooth(method = "loess")
# 
# p2 <- ggplot(percent_diff,mapping = aes(x = yr, y = percent_diff, color = model))+
#   geom_smooth(method = "loess")
# 
# plot_grid(p1,p2)
# 
# model_comparison("usgs_ws_01613900")

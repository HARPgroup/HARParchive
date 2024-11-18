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

df1 <- yearly_summary(prism)
df2 <- yearly_summary(daymet)
df3 <- yearly_summary(nldas2)
p1 <- ggplot(df1, mapping = aes(x = yr, y = total_precip))+
  geom_histogram(stat = "identity", binwidth = 1, fill = "lightblue1")+
  theme_bw()

p2 <- ggplot(df2, mapping = aes(x = yr, y = total_precip))+
  geom_histogram(stat = "identity", binwidth = 1, fill = "lightblue2")+
  theme_bw()

p3 <- ggplot(df3, mapping = aes(x = yr, y = total_precip))+
  geom_histogram(stat = "identity", binwidth = 1, fill = "lightblue3")+
  theme_bw()
p1 / p2 / p3

j <- sqldf(
  "SELECT df2.yr , df1.total_precip AS prism_precip , df2.total_precip AS daymet_precip
  FROM df1
  FULL OUTER JOIN df2 ON df1.yr=df2.yr"
)

j2 <- sqldf(
  "SELECT j.yr , j.prism_precip , j.daymet_precip, df3.total_precip AS nldas_precip
  FROM j
  FULL OUTER JOIN df3 on j.yr=df3.yr"
)

j3 <- sqldf(
  "SELECT yr , 'prism' AS model , prism_precip AS precip_in FROM j2
  UNION ALL
  SELECT yr , 'daymet' AS model , daymet_precip AS precip_in FROM j2
  UNION ALL
  SELECT yr , 'nldas' AS model , nldas_precip AS precip_in FROM j2"
)

ggplot(j3, aes(x = yr, y = precip_in, fill = model))+
  geom_histogram(stat = "identity", position = "identity", binwidth = 1, alpha = 0.5)+
  theme_bw()

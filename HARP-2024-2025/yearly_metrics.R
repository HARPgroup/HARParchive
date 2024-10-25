#number of days with precip_in and longest duration storm per yr

library(sqldf)

#this function uses a series of SQL chunks to compute yearly data frames
#that include helpful QA metrics.
yearly_metrics <- function(df){
# Create Daily summary to make all data daily
daily_summary <- 
  sqldf(
    "SELECT yr, mo, da, obs_date, SUM(precip_in) AS precip_in
  FROM df
  GROUP BY yr, mo, da"
  ) #include obs_date after SQL to data frame

# Number of days with precip_in per yr
rainfall_days <- sqldf("SELECT yr, COUNT(*) 
                       AS DaysWithRainfall 
                       FROM daily_summary 
                       WHERE precip_in > 0 
                       GROUP BY yr")

# Longest duration storm per yr
storm_events <- sqldf("WITH RainEvents AS (
                        SELECT obs_date, precip_in, yr, 
                        (SELECT SUM(precip_in = 0) 
                        FROM daily_summary rd2 
                        WHERE rd2.obs_date <= rd1.obs_date) 
                        AS StormGroup
                        FROM daily_summary rd1),
                      StormDurations AS (
                        SELECT yr, StormGroup, COUNT(*) 
                        AS StormDuration
                        FROM RainEvents
                        WHERE precip_in > 0
                        GROUP BY yr, StormGroup)
                      SELECT yr, MAX(StormDuration) AS LongestStorm 
                      FROM StormDurations 
                      GROUP BY yr")

# Printing results
merge(rainfall_days, storm_events)
}
## It seems like NLDAS has much higher results...
# Analysis of Ratings factoring the drainage area of each measured data point
################################################################################
# Package Library
library(tidyverse)
library(sqldf)
library(dataRetrieval)

################################################################################
# Join data from all three sources
for(j in c("daymet","prism","nldas")){
  print(j)
  #Set path to read in ratings based on data source of outer loop
  pathToread <- paste0("C:/Users/ilona/OneDrive - Virginia Tech/HARP/Storm Tests/Ratings/",j,"/")
  for( i in list.files(pathToread) ){
    filei <- read.csv(paste0(pathToread,i))
    #Store the gage number
    filei$gage <- gsub(".*_(\\d+)-.*","\\1",i)
    #Combine the file into a single data frame
    if(!exists("combineFile")){
      combineFile <- filei
    }else{
      combineFile <- rbind(combineFile,filei)
    }
  }
  #Assign to a specific variable and delete the generic combineFile
  assign(paste0('combineFile',j),combineFile)
  rm(combineFile)
}

#Join the daymet and prism data together
joinData <- combineFileprism %>% 
  #Remove the row numbers (not important) and rename the r_squared for clarity
  select(-X,prismRating = r_squared) %>% 
  #Join in the dayment data, but first rename the r_squared column for clarity.
  #Join on gage and month
  left_join(combineFiledaymet %>% 
              select(daymetRating = r_squared,gage,mo),
            by = c("gage","mo")) %>% 
  #Join in the nldas data, but first rename the r_squared column for clarity.
  #Join on gage and month
  left_join(combineFilenldas %>% 
              select(nldasRating = r_squared,gage,mo),
            by = c("gage","mo")) %>% 
  #Pivot it longer to have a column with the data source and one for the
  #ratings, for plotting ease
  pivot_longer(c(prismRating,daymetRating,nldasRating),
               names_to = 'dataSource',
               values_to = 'rating')

################################################################################
# Import and add drainage are from USGS
drainage_area <- dataRetrieval::readNWISsite(c(unique(joinData$gage)))
joinData <- sqldf(
  "select a.mo as mo, a.gage as gageid, a.dataSource as dataSource, 
  a.rating as rating,
  b.drain_area_va as drainageArea_sqmi
  from joinData as a
  left outer join drainage_area as b
  on (
   a.gage = b.site_no
  )"
)

# Replace "Rating" with blank in data source column
joinData$dataSource <- gsub("Rating","",joinData$dataSource)

################################################################################
# Find Gage ID and month with biggest r-squared range across data sources

# Sql to find gage and month with maximum range of r-squared values (ChatGPT)
max_range <- sqldf("
WITH PivotedData AS (
    SELECT
        gageid,
        mo,
        MAX(CASE WHEN dataSource = 'prism' THEN rating END) AS prism,
        MAX(CASE WHEN dataSource = 'daymet' THEN rating END) AS daymet,
        MAX(CASE WHEN dataSource = 'nldas' THEN rating END) AS nldas
    FROM joinData
    GROUP BY gageid, mo
),
RangeCalculation AS (
    SELECT
        gageid,
        mo,
        CASE
            WHEN prism IS NOT NULL AND daymet IS NOT NULL AND nldas IS NOT NULL THEN 
                MAX(prism, daymet, nldas) - MIN(prism, daymet, nldas)
            WHEN prism IS NOT NULL AND daymet IS NOT NULL THEN
                MAX(prism, daymet) - MIN(prism, daymet)
            WHEN prism IS NOT NULL AND nldas IS NOT NULL THEN
                MAX(prism, nldas) - MIN(prism, nldas)
            WHEN daymet IS NOT NULL AND nldas IS NOT NULL THEN
                MAX(daymet, nldas) - MIN(daymet, nldas)
            ELSE
                NULL
        END AS rating_range
    FROM PivotedData
    GROUP BY gageid, mo
)
SELECT
    gageid,
    mo,
    rating_range
FROM RangeCalculation
WHERE rating_range IS NOT NULL
ORDER BY rating_range DESC
LIMIT 1
")

# Print the result
print(max_range)
  

################################################################################
# Find Gage ID and month with smallest r-squared range across data sources

# Same process for minimum (ChatGPT)
min_range <- sqldf( "
WITH PivotedData AS (
    SELECT
        gageid,
        mo,
        MAX(CASE WHEN dataSource = 'prism' THEN rating END) AS prism,
        MAX(CASE WHEN dataSource = 'daymet' THEN rating END) AS daymet,
        MAX(CASE WHEN dataSource = 'nldas' THEN rating END) AS nldas
    FROM joinData
    GROUP BY gageid, mo
),
RangeCalculation AS (
    SELECT
        gageid,
        mo,
        CASE
            WHEN prism IS NOT NULL AND daymet IS NOT NULL AND nldas IS NOT NULL THEN 
                MAX(prism, daymet, nldas) - MIN(prism, daymet, nldas)
            WHEN prism IS NOT NULL AND daymet IS NOT NULL THEN
                MAX(prism, daymet) - MIN(prism, daymet)
            WHEN prism IS NOT NULL AND nldas IS NOT NULL THEN
                MAX(prism, nldas) - MIN(prism, nldas)
            WHEN daymet IS NOT NULL AND nldas IS NOT NULL THEN
                MAX(daymet, nldas) - MIN(daymet, nldas)
            ELSE
                NULL
        END AS rating_range
    FROM PivotedData
    GROUP BY gageid, mo
)
SELECT
    gageid,
    mo,
    rating_range
FROM RangeCalculation
WHERE rating_range IS NOT NULL
ORDER BY rating_range ASC
LIMIT 1
")

# Print the result
print(min_range)

################################################################################
# maximum excluding months with nas

# SQL from ChatGPT
max_range_real <- sqldf("
WITH PivotedData AS (
    SELECT
        gageid,
        mo,
        MAX(CASE WHEN dataSource = 'prism' THEN rating END) AS prism,
        MAX(CASE WHEN dataSource = 'daymet' THEN rating END) AS daymet,
        MAX(CASE WHEN dataSource = 'nldas' THEN rating END) AS nldas
    FROM joinData
    GROUP BY gageid, mo
    HAVING COUNT(CASE WHEN dataSource = 'prism' AND rating IS NOT NULL THEN 1 END) > 0
       AND COUNT(CASE WHEN dataSource = 'daymet' AND rating IS NOT NULL THEN 1 END) > 0
       AND COUNT(CASE WHEN dataSource = 'nldas' AND rating IS NOT NULL THEN 1 END) > 0
),
RangeCalculation AS (
    SELECT
        gageid,
        mo,
        MAX(prism, daymet, nldas) - MIN(prism, daymet, nldas) AS rating_range
    FROM PivotedData
    GROUP BY gageid, mo
    HAVING COUNT(prism) > 0
       AND COUNT(daymet) > 0
       AND COUNT(nldas) > 0
)
SELECT
    gageid,
    mo,
    rating_range
FROM RangeCalculation
ORDER BY rating_range DESC
LIMIT 1
")

# Print the result
print(max_range_real)

################################################################################
# minimum excluding months with nas

# SQL from ChatGPT
min_range_real <- sqldf("
WITH PivotedData AS (
    SELECT
        gageid,
        mo,
        MAX(CASE WHEN dataSource = 'prism' THEN rating END) AS prism,
        MAX(CASE WHEN dataSource = 'daymet' THEN rating END) AS daymet,
        MAX(CASE WHEN dataSource = 'nldas' THEN rating END) AS nldas
    FROM joinData
    GROUP BY gageid, mo
    HAVING COUNT(CASE WHEN dataSource = 'prism' AND rating IS NOT NULL THEN 1 END) > 0
       AND COUNT(CASE WHEN dataSource = 'daymet' AND rating IS NOT NULL THEN 1 END) > 0
       AND COUNT(CASE WHEN dataSource = 'nldas' AND rating IS NOT NULL THEN 1 END) > 0
),
RangeCalculation AS (
    SELECT
        gageid,
        mo,
        CASE
            WHEN prism IS NOT NULL AND daymet IS NOT NULL AND nldas IS NOT NULL THEN 
                MAX(prism, daymet, nldas) - MIN(prism, daymet, nldas)
            WHEN prism IS NOT NULL AND daymet IS NOT NULL THEN
                MAX(prism, daymet) - MIN(prism, daymet)
            WHEN prism IS NOT NULL AND nldas IS NOT NULL THEN
                MAX(prism, nldas) - MIN(prism, nldas)
            WHEN daymet IS NOT NULL AND nldas IS NOT NULL THEN
                MAX(daymet, nldas) - MIN(daymet, nldas)
            ELSE
                NULL
        END AS rating_range
    FROM PivotedData
    GROUP BY gageid, mo
    HAVING COUNT(prism) > 0
       AND COUNT(daymet) > 0
       AND COUNT(nldas) > 0
)
SELECT
    gageid,
    mo,
    rating_range
FROM RangeCalculation
WHERE rating_range IS NOT NULL
ORDER BY rating_range ASC
LIMIT 1
")







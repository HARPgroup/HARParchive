library(tidyverse)
#Assumes all ratings files are in a folder ratings/daymet/ or ratings/prism/
for(j in c("daymet","prism","nldas")){
  print(j)
  #Set path to read in ratings  based on data source of outer loop
  pathToread <- paste0("C:/Users/natef/OneDrive - Virginia Tech/HARP/Ratings/",j,"/")
  for( i in list.files(pathToread) ){
    #For each file in the directory, read in the file i as a csv
    filei <- read.csv(paste0(pathToread,i))
    #Store the gage number
    filei$gage <- gsub(".*_(\\d+)-.*","\\1",i)
    #Store the analysis type
    filei$workflow <- gsub(".*_","",j)
    #Keep only the necessary columns, depending on the workflow:
    if(filei$workflow[1] == "simplelm"){
      filei <- filei[,c("mo","rating","gage","workflow")]
    }else{
      filei <- filei[,c("mo","r_squared","gage","workflow")]
    }
    names(filei) <- c("mo","rating","gage","workflow")
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
  #Rename the rating for clarity
  select(prismRatingstorm = rating,gage,mo,workflow) %>% 
  #Join in the dayment data, but first rename the rating column for clarity.
  #Join on gage, month, and workflow
  full_join(combineFiledaymet %>% 
              select(daymetRatingstorm = rating,gage,mo,workflow),
            by = c("gage","mo","workflow")) %>% 
  full_join(combineFiledaymet %>% 
              select(daymetRatinglm = rating,gage,mo,workflow),
            by = c("gage","mo","workflow")) %>% 
  #Add remaining prism data:
  full_join(combineFileprism %>% 
              select(prismRatinglm = rating,gage,mo,workflow),
            by = c("gage","mo","workflow")) %>% 
  #Join in the nldas data, but first rename the rating column for clarity.
  #Join on gage, month, and workflow
  full_join(combineFilenldas %>% 
              select(nldasRatingstorm = rating,gage,mo,workflow),
            by = c("gage","mo","workflow")) %>% 
  full_join(combineFilenldas %>%
              select(nldasRatinglm = rating,gage,mo,workflow),
            by = c("gage","mo","workflow")) %>%
  #For easier viewing, combine lm and storm columns such that there is only one
  #column for prism, daymet, nldas classified by the workflow column
  mutate(prismRating = coalesce(prismRatingstorm,prismRatinglm),
         daymetRating = coalesce(daymetRatingstorm,daymetRatinglm),
         nldasRating = coalesce(nldasRatingstorm,nldasRatinglm)
  ) %>% 
  #Remove now superflous columns:
  select(-prismRatingstorm,-prismRatinglm,
         -daymetRatingstorm,-daymetRatinglm,
         -nldasRatingstorm,-nldasRatinglm) %>% 
  #Pivot it longer to have a column with the data source and one for the
  #ratings, for plotting ease
  pivot_longer(c(prismRating,daymetRating,nldasRating),
               names_to = 'dataSource',
               values_to = 'rating') %>% 
  arrange(gage,mo,workflow)

#At each gage, does the best performing data source change between workflows?
#The below code is for ESTIMATES ONLY. The left_join assumes that the ratings
#are unique between datasources for each gage, workflow, month. This is brittle
#and could result in incorrect answers!

safe_max <- function(x) {
  if (length(na.omit(x)) == 0) {
    return(NA)  # or any other placeholder you prefer
  } else {
    return(max(x, na.rm = TRUE))
  }
}

gageCompare <- joinData %>% dplyr::ungroup() %>% 
  #Group by gage, workflow, and month and find the max rating:
  dplyr::group_by(gage,workflow,mo) %>% 
  dplyr::summarise(maxRating = mean(safe_max(rating))) %>% 
  #Join the joinData df back in matching by gage, workflow, mo, and rating. This
  #could be problematic with duplicate ratings as in a case where all ratings
  #across the data sources are the same value
  left_join(joinData,by = c('gage','workflow','mo','maxRating' = 'rating')) %>% 
  #Now pivot wider such that we can see ratings and best data sources side-by-side
  pivot_wider(names_from = workflow,values_from = c(maxRating,dataSource)) %>% 
  #Now filter to only find instances where the best data sources are different:
  filter(dataSource != dataSource) %>% 
  #Create a column showing the difference in rating and arrange by the difference
  mutate(differenceInRating = maxRating - maxRating) %>% 
  arrange(differenceInRating)



#Isolate one month
gageCompareMonth <- gageCompare[gageCompare$mo == 10,]

library(sf)
#Get the watershed coverage from the server
watershedGeo <- read.csv("http://deq1.bse.vt.edu:81/met/usgsGageWatershedGeofield.csv")

#Get the gage numbers as their own field and store a copy of the data
gageWatershedSF <- watershedGeo
gageWatershedSF$gage <- gsub(".*_(\\d+)","\\1",gageWatershedSF$hydrocode)

#Let's begin by plotting October daymet ratings for stormVol
forPlot <- joinData[joinData$dataSource == 'daymetRating' & 
                      joinData$workflow == 'stormvol' &
                      joinData$mo == 10,]
#Join the geometry data onto out plot data
joinDataSF <- forPlot %>% 
  left_join(gageWatershedSF %>% select(-hydrocode),
            by = 'gage') %>% 
  filter(!is.na(wkt))
#Create an SF object. Specify the coordinate system and the field name in the
#data frame that contains the well-known text. In this case, WKT is the name of
#the field with the polygon geometries
joinDataSF <- st_as_sf(joinDataSF,wkt = 'wkt',crs = 4326)
#Repair broken geometries
joinDataSF <- st_make_valid(joinDataSF)
#Add shape area in coordinate system units (likely meaningless in crs 4326)
joinDataSF$area <- st_area(joinDataSF)
#Order the data by largest to smallest area to make plotting more effective
joinDataSF <- joinDataSF[order(joinDataSF$area,decreasing = TRUE),]
#Plot SF polygons
ggplot(joinDataSF) + 
  geom_sf(aes(fill = rating)) + 
  scale_fill_viridis_c(option = 'magma') + 
  theme_classic()


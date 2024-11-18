# Load in libraries
library(sqldf)
library(ggplot2)
library(tidyverse)


################################################################################
# PRISM List of files

list_of_prism <- list.files(path="C:/Users/ilona/OneDrive - Virginia Tech/HARP/Storm Tests/Ratings/out-prism",
                            recursive = TRUE,
                            pattern = "\\.csv$",
                            full.names = TRUE)
prism_list <- read_csv(list_of_prism, id="gageid")

prism_list$gageid <- grep("0", unlist(strsplit(prism_list$gageid, "_")), value = TRUE, perl = TRUE)
prism_list$gageid <- grep("0", unlist(strsplit(prism_list$gageid, "-")), value = TRUE, perl = TRUE)
prism_list$...1 <- NULL

prism_gages <- as.list(unique(prism_list$gageid))

################################################################################
# DAYMET List of files

list_of_daymet <- list.files(path="C:/Users/ilona/OneDrive - Virginia Tech/HARP/Storm Tests/Ratings/out-daymet/out",
                            recursive = TRUE,
                            pattern = "\\.csv$",
                            full.names = TRUE)
daymet_list <- read_csv(list_of_daymet, id="gageid")

daymet_list$gageid <- grep("0", unlist(strsplit(daymet_list$gageid, "_")), value = TRUE, perl = TRUE)
daymet_list$gageid <- grep("0", unlist(strsplit(daymet_list$gageid, "-")), value = TRUE, perl = TRUE)
daymet_list$...1 <- NULL

daymet_gages <- as.list(unique(daymet_list$gageid))

################################################################################
# NLDAS2 List of Files

list_of_nldas2 <- list.files(path="C:/Users/ilona/OneDrive - Virginia Tech/HARP/Storm Tests/Ratings/out-daymet/out",
                             recursive = TRUE,
                             pattern = "\\.csv$",
                             full.names = TRUE)
nldas2_list <- read_csv(list_of_nldas2, id="gageid")

nldas2_list$gageid <- grep("0", unlist(strsplit(nldas2_list$gageid, "_")), value = TRUE, perl = TRUE)
nldas2_list$gageid <- grep("0", unlist(strsplit(nldas2_list$gageid, "-")), value = TRUE, perl = TRUE)
nldas2_list$...1 <- NULL

nldas2_gages <- as.list(unique(nldas2_list$gageid))

################################################################################
# Find gages which ran for each data source

complete_gages <- as.list(intersect(prism_gages,daymet_gages))
complete_gages <- as.list(as.numeric(intersect(complete_gages,nldas2_gages)))

################################################################################
# 
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
  "select a.mo as mo, a.gage as gageid, a.dataSource as dataSource, a.rating as rating,
  b.drain_area_va as drainageArea_sqmi
  from joinData as a
  left outer join drainage_area as b
  on (
   a.gage = b.site_no
  )"
)

joinData$dataSource <- gsub("Rating","",joinData$dataSource)


################################################################################
# Trends in DA and ratings

ggplot(data=joinData)+
  aes(y=mean(joinData$rating, group_by(gageid)),x=joinData$drainageArea_sqmi)


























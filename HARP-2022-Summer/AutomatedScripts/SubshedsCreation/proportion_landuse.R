#proportioning landuses of multiple land segments 
#when adding a new subshed to a river segment
suppressPackageStartupMessages(library("sqldf"))
#----
argst <- commandArgs(trailingOnly = T)
main_seg <- argst[1]
subshed <- argst[2]
area <- argst[3]
file <- argst[4]

#testing:----
main_seg <- 'PS2_5560_5100'
subshed <- 'PS2_5568_5560'
#main_seg <- 'OD3_8720_8900'
#main_seg <- 'JB0_7050_0000'
#subshed <- 'OD3_8723_8720'

# file <- '/opt/model/p6/vadeq/input/scenario/river/land_use/land_use_2013VAHYDRO2018615.csv'
# file <- '/Users/aa_HARP/aa_GitHub/HARParchive/HARP-2022-Summer/AutomatedScripts/SubshedsCreation/land_use_2013VAHYDRO2018615.csv'
# setwd("/Users/VT_SA/Documents/HARP") # for testing only
# landuse_full <- read.csv('land_use_2013VAHYDRO2018615.csv', sep =',')

#-extract/calculate main ws and subshed areas and calculate their proportion:----
landuse_full <- read.csv(file, sep=',')

receiving_landuses <- sqldf(paste0("select * from landuse_full where riverseg = '",main_seg,"'"))
mainws_area <- sum(receiving_landuses[-1:-2])/640 #calculate the main ws total area

subsheds <- sqldf(paste0("select * from landuse_full where riverseg = '",subshed,"'"))

#if subshed has no data, make it a data frame of zeros--
empty <- data.frame(matrix(0, 0,length(colnames(receiving_landuses))))
colnames(empty) <- colnames(receiving_landuses)

if (empty %in% subsheds) {
  subsheds <- data.frame(matrix(0, 1,length(colnames(receiving_landuses))))
  colnames(subsheds) <- colnames(receiving_landuses)
}
#--
sub_area <- sum(subsheds[-1:-2])/640 #convert to sq mi

#calculating the proportioning between main ws and subshed:
propor <- da/mainws_area

#--if existing subshed values are incorrect, add them back to main_seg to be re-proportioned--
if (sub_area != 0) { # add subshed areas back into main riverseg as fail safe
  
  for (i in 1:length(receiving_landuses$landseg)) {
    #for each landuse in subshed, find the corresponding data in receiving_landuses
    lseg_subshed <- sqldf(paste0("select * from subsheds where landseg = '", receiving_landuses$landseg[i], "'"))
    
    lseg_receive <- sqldf(paste0("select * from receiving_landuses where landseg = '", receiving_landuses$landseg[i], "'"))
    
    if  (!(empty %in% lseg_subshed)) {
      receiving_landuses[-1:-2] <- lseg_subshed[-1:-2] + lseg_receive[-1:-2]
    }
    
  }
}

#-calculate subsheds and remove from main seg----
new_landuses <- receiving_landuses #new as in creating subshed
new_landuses[-1:-2] <- new_landuses[-1:-2] * propor
new_landuses$riverseg <- subshed

receiving_landuses[-1:-2] <- receiving_landuses[-1:-2] * (1-propor) #to "subtract" propor from main_seg

remove <- subset(landuse_full, riverseg != main_seg) #remove old land use values
new_list <- rbind(remove, new_landuses, receiving_landuses) #adding in revisions
new_list <- new_list[order(new_list$landseg, new_list$riverseg),] #order by land seg first, then river seg


write.table(new_list,
            file=file,
            append = FALSE,
            quote = FALSE,
            sep = ",",
            row.names = FALSE,
            col.names = TRUE)

message('proportioned the river segment and subshed')

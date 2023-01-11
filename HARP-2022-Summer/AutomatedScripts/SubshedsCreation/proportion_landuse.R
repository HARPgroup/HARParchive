#proportioning landuses of multiple land segments 
#when adding a new subshed to a river segment

#----
argst <- commandArgs(trailingOnly = T)
rseg <- argst[1]
subshed <- argst[2]
file <- argst[3]

#testing:----
# rseg <- 'PS2_5560_5100'
# subshed <- 'PS2_5568_5560'
# file <- '/opt/model/p6/vadeq/input/scenario/river/land_use/land_use_2013VAHYDRO2018615.csv'
# setwd("/Users/VT_SA/Documents/HARP") # for testing only
# landuse_full <- read.csv('land_use_2013VAHYDRO2018615.csv', sep =',')

#proportion landuses for new subshed based on overall lang segment area:---- 
landuse_full <- read.csv(file, sep=',')

receiving_landuses <- sqldf("select * from landuse_full where riverseg = rseg")
mainws_area <- sum(receiving_landuses[-1:-2])

num_lseg <- as.numeric(length(receiving_landuses$landseg))
lseg_search <- receiving_landuses[2]

landsegments <- sqldf("select * from landuse_full where landseg = lseg_search")


for (rseg in landuse_full$riverseg) {
  row <- as.numeric(match(rseg, landuse_full$riverseg))
  lseg <- landuse_full[row,] 
  landuse <- lseg[-1:-2]
  
  lseg_dec <- (landuse - landuse/area*100)
  new_rseg <- cbind(rseg, lseg[2], lseg_dec)
  colnames(new_rseg) <- colnames(landuse_full)
  
  lseg_inc <- (landuse/area*100)
  new_subshed <- cbind(subshed, lseg[2], lseg_inc)
  colnames(new_subshed) <- colnames(landuse_full)
  
} 
 

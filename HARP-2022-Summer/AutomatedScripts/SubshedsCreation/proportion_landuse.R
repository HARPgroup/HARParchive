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

#identify all land segments within original riverseg:---- 
landuse_full <- read.csv(file, sep=',')

for (rseg in landuse_full$riverseg) {
  row <- as.numeric(match(rseg, landuse_full$riverseg))
  lseg <- landuse_full[row,] 
  landuse <- lseg[-1:-2]
  prop <- 
  lseg_prop <- cbind(lseg[2],prop)
  new_row <- cbind(subshed, lseg_prop)
  subtract 
} 
 
#which(rseg %in% landuse_full$riverseg)

#include all land segments with the new subshed 



#proportion all land uses within each land segment:






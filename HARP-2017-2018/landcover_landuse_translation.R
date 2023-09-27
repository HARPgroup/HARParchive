#Purpose: Translate old landuse data into new landuse data using up to date 
#         landcover data for the Southern Rivers portions of the Chesapeake Bay area

#Authors: Alex Domiano; Alyssa Ford

#Version: 1

#Expanded Purpose: Assigning landuse categories based on landcover, ulimately to use within HSPF water hydrologic model

#Inputs: 
#data_2005: Old 2005 landuse data for entire Chesapeake Bay area
#data_comp: Compiled 2011 NLCD (TN, NC) and 2016 VGIN(Virginia)(WorldView Solutions, Inc.) Southern Rivers landcover data

#Southern Rivers: Located in Southern Virginia and the Northern parts of Tennessee and North Carolina.
#Map: https://docs.google.com/document/d/19Y3Qwg5hCy8qxh17_zfgMPrzdTMWnVbzAnwZjpsOkUk/pub

#Super Categories: 
#wat: Water+NWI+NHD
#imp: Impervious Roads/Impervious Non-Roads
#frs: Forest/TC over Turf Grass/TC over Impervious/Tidal Wetlands/Floodplain Wetlands/Other Wetlands
#os: Open Space
#agr: Turf Grass/Pasture/Cropland

#Landuses:
#for.:	forest
#hvf:	harvested forest
#hwm:	hightill with manure
#nhi:	nutrient management hitil with manure
#lwm:	lowtill with manure
#nlo:	nutrient management lotil
#hom:	hightill without manure
#nho:	nutrient management hitil without manure
#hyw:	hay with nutrients
#nhy:	nutrient management hay
#hyo:	hay without nutrients
#alf:	alfalfa
#nal:	nutrient management alfalfa
#pas:	pasture
#npa:	nutrient management pasture
#trp:	degraded riparian pasture
#urs:	nursery
#cfo:	cafos
#afo:	non-cafo animal feeding operations
#rpd:	regulated pervious developed
#rid:	regulated impervious developed
#rcn:	regulated construction
#rex:	regulated extractive
#npd:	nonregulated pervious developed
#nid:	nonregulated impervious developed
#nex:	nonregulated extractive
#cpd:	combined pervious developed
#cid:	combined impervious developed
#ccn: combined construction
#cex: combined extractive
#wat:	Water

#Outputs: 
#current_land_use: Up to date land use translated from current landcover data

# Input -------------------------------------------------------------------

#Input/Load all data: land use data from 2005 and current land cover data
uri_2005 <- 'https://docs.google.com/spreadsheets/d/e/2PACX-1vSTXQoaH_jLCEoOnHxK4OTbfM6BSUlQtvn54ZkKeORSCFWdfpi_dTI9qkWj88GFVUIfL2IBLPlkWw-u/pub?output=csv';
data_2005 = read.csv(uri_2005, header = TRUE, sep = ",", stringsAsFactors = FALSE);
uri_comp <- 'https://docs.google.com/spreadsheets/d/1dT8bjhpyfa10Z9GMalOHBGXHpyXR7ID_FJMZezjtbjw/pub?output=csv'
data_comp = read.csv(uri_comp, header = TRUE, sep = ',', stringsAsFactors = FALSE);
uri_WGS <- 'https://docs.google.com/spreadsheets/d/1BILN-PBoPI05JuUmabuAzjlgfiPzIuw6sjY8ecYC72Q/pub?output=csv'
data_WGS <- read.csv(uri_WGS, header = TRUE, sep = ',', stringsAsFactors = FALSE);
data_2005$lndrvrseg <- with(data_2005, paste0(landseg, riverseg))


# Super Categories data frame ---------------------------------------------

#This creates a data frame listing which land uses are in which super category
wat <- 'Water+NWI+NHD';
imp <- 'Impervious Roads/Impervious Non-Roads';
frs <- 'Forest/TC over Turf Grass/TC over Impervious/Tidal Wetlands/Floodplain Wetlands/Other Wetlands';
os <- 'Open Space';
agr <- 'Turf Grass/Pasture/Cropland';
Sup_cat <- data.frame("LU" = c('wat', 'rid', 'nid', 'cid', 'for', 'hvf', 'rcn', 'ccn', 'rex', 'nex', 'cex',
                               'hwm', 'hyw', 'hyo', 'alf', 'hom', 'nhi', 'lwm', 'nlo', 'nho', 'nhy', 'nal',
                               'pas', 'npa', 'trp', 'urs', 'cfo', 'afo', 'rpd', 'npd', 'cpd'), "SC" = c(wat, 
                                imp, imp, imp, frs, frs, os, os, os, os, os, agr, agr, agr, agr, agr, 
                                agr, agr, agr, agr, agr, agr, agr, agr, agr, agr, agr, agr, agr, agr, agr));



# Find segments of interest -----------------------------------------------

#This loop find the segments of interest and creates a new data frame containing them
#The segments of interest are the segments contained in the southern rivers data

data_2005_landuse <- data.frame(data_2005[751,])

for(i in 1:nrow(data_2005)) {
  print(paste("Processing row ",i," of ",nrow(data_2005)," in the 2005 landuse data",sep=""))
  if (data_2005[i, "lndrvrseg"] %in% data_comp$lndrvrseg) {
    data_2005_landuse[i,] <- as.vector(data_2005[i,])
  }  
}

for (t in 1:nrow(data_comp)) {
  print(paste("Processing row ",t," of ",nrow(data_comp)," in the landcover data",sep=""))
  if (data_comp[t, "lndrvrseg"] %in% data_2005$lndrvrseg) {
  } else {
    warning(data_comp[t, "lndrvrseg"],': The Land River segment does not exist in the 2005 dataset');
  }
}

data_2005_landuse <- data_2005_landuse[complete.cases(data_2005_landuse),]
data_2005_landuse <- data_2005_landuse[-c(29),]
row.names(data_2005_landuse) <- 1:nrow(data_2005_landuse)


# Calculate percentages ---------------------------------------------------


#This calculates the percentage of each land use within each super category for every land river segment 
options(scipen = 999)
percentages <- data.frame(matrix(nrow=1, ncol = 33)) #creates empty data frame for percentages 
super_cat_2005 <- data.frame(matrix(nrow=1, ncol=8)) #creates empty data frame for the super categories for the 2005 data
colnames(super_cat_2005) <- c("landseg", "riverseg", "water", "impervious", "forest", "open space", "ag", "total acres")
colnames(percentages) <- c("landseg", "riverseg","wat","rid","nid","cid","for.","hvf","rcn","ccn",
                           "rex","nex","cex","hwm","hyw", "hyo","alf","hom","nhi","lwm","nlo","nho",
                           "nhy","nal","pas","npa","trp","urs","cfo","afo", "rpd","npd","cpd")
percent_diff_acres <- data.frame(matrix(nrow=1, ncol=5)) #creates an empty data frame to show the difference in acres
                                                         #between the HSPF data and the VGIN/NLCD data
colnames(percent_diff_acres) <- c("landseg", "riverseg","HSPF land use data acres", "VGIN/NLCD land cover data acres", "percent difference in acres")
percentages[(nrow(data_2005_landuse)+1),1] <- print('Average')
print("Creating the percentages table from the 2005 data.")
for (i in 1:nrow(data_2005_landuse)){ #the loop will go through every row in the landuse data from 2005
  print(paste("Processing row ",i," of ",nrow(data_2005_landuse)," in the landuse data",sep=""))
  x <- data_2005_landuse[i,]
  m <- x$landseg
  n <- x$riverseg
  y <- subset(data_comp, landseg==m & riverseg==n)
  percentages[i,1] <- x$landseg
  percentages[i,2] <- x$riverseg
  super_cat_2005[i,1] <- x$landseg
  super_cat_2005[i,2] <- x$riverseg
  percent_diff_acres[i,1] <- x$landseg
  percent_diff_acres[i,2] <- x$riverseg
  water <- x$wat
  super_cat_2005[i,3] <- water
  percentages[i,3] <- x$wat/water #calculates percentage of water land use in water supercategory 
  impervious <- x$rid + x$nid +x$cid
  super_cat_2005[i,4] <- impervious
  percentages[i,4] <- x$rid/impervious #calculates percentage of each impervious surface land
  percentages[i,5] <- x$nid/impervious #use in the impervious supercategory
  percentages[i,6] <- x$cid/impervious #this percentage is then placed in a new table
  forest_TC <- x$for.+ x$hvf
  super_cat_2005[i,5] <- forest_TC
  percentages[i,7] <- x$for./forest_TC 
  percentages[i,8] <- x$hvf/forest_TC  
  open_space <- x$rcn + x$ccn + x$rex + x$cex + x$nex
  super_cat_2005[i,6] <- open_space
  percentages[i,9] <- x$rcn/open_space 
  percentages[i,10] <- x$ccn/open_space 
  percentages[i,11] <- x$rex/open_space 
  percentages[i,12] <- x$nex/open_space
  percentages[i,13] <- x$cex/open_space
  ag_turf <- x$hwm + x$hyw + x$hyo + x$alf + x$hom + x$nhi +
    x$lwm + x$nlo + x$nho + x$nhy + x$nal + x$pas + x$npa +
    x$trp + x$urs + x$cfo + x$afo + x$rpd + x$npd +x$cpd
  super_cat_2005[i,7] <- ag_turf
  percentages[i,14] <- x$hwm/ag_turf 
  percentages[i,15] <- x$hyw/ag_turf 
  percentages[i,16] <- x$hyo/ag_turf  
  percentages[i,17] <- x$alf/ag_turf
  percentages[i,18] <- x$hom/ag_turf
  percentages[i,19] <- x$nhi/ag_turf
  percentages[i,20] <- x$lwm/ag_turf
  percentages[i,21] <- x$nlo/ag_turf
  percentages[i,22] <- x$nho/ag_turf
  percentages[i,23] <- x$nhy/ag_turf
  percentages[i,24] <- x$nal/ag_turf
  percentages[i,25] <- x$pas/ag_turf
  percentages[i,26] <- x$npa/ag_turf
  percentages[i,27] <- x$trp/ag_turf
  percentages[i,28] <- x$urs/ag_turf
  percentages[i,29] <- x$cfo/ag_turf
  percentages[i,30] <- x$afo/ag_turf
  percentages[i,31] <- x$rpd/ag_turf
  percentages[i,32] <- x$npd/ag_turf
  percentages[i,33] <- x$cpd/ag_turf
  total_acres <- water+impervious+forest_TC+open_space+ag_turf
  super_cat_2005[i,8] <- total_acres #2005 data
  percent_diff_acres[i,3] <- total_acres #2005 data
  percent_diff_acres[i,4] <- y$total_acres #current data
  percent_diff_acres[i,5] <- ((y$total_acres-total_acres)/y$total_acres)*100

}

percentages[is.na(percentages)] <- 0 #this sets all the undefined values to zero

#calculates the percentage of water land uses
percentage_water <- data.frame(matrix(nrow = 1, ncol = 3))
colnames(percentage_water) <- c("landseg", "riverseg", "wat")
count_water <- 1
for(i in 1:nrow(current_land_use)){
  x <- percentages[i,]
  sum_wat <- sum(x[3])
  if (sum_wat == 1){
    percentage_water[count_water,1] <- x$landseg
    percentage_water[count_water,2] <- x$riverseg
    percentage_water[count_water,3] <- x$wat
    count_water <- count_water+1
  }
}
percentage_water[count_water,1] <- print("Average")
for(i in 3){
  percentage_water[count_water,i] <- mean(percentage_water[(1:nrow(percentage_water)-1),i])
}
percentages[(nrow(percentages)),3] <- percentage_water[nrow(percentage_water),"wat"]

#calculates percentage of impervious land uses
percentage_impervious <- data.frame(matrix(nrow = 1, ncol = 5))
colnames(percentage_impervious) <- c("landseg", "riverseg", "rid", "cid", "nid")
count_imp <- 1
for(i in 1:nrow(current_land_use)){
  x <- percentages[i,]
  sum_imp <- sum(x[4:6])
  if (sum_imp == 1){
    percentage_impervious[count_imp,1] <- x$landseg
    percentage_impervious[count_imp,2] <- x$riverseg
    percentage_impervious[count_imp,3] <- x$rid
    percentage_impervious[count_imp,4] <- x$cid
    percentage_impervious[count_imp,5] <- x$nid
    count_imp = count_imp+1
  }
}
percentage_impervious[count_imp,1] <- print("Average")
for(i in 3:5){
  percentage_impervious[count_imp,i] <- mean(percentage_impervious[(1:nrow(percentage_impervious)-1),i])
}
percentages[(nrow(percentages)),4] <- percentage_impervious[nrow(percentage_impervious),"rid"]
percentages[(nrow(percentages)),5] <- percentage_impervious[nrow(percentage_impervious),"nid"]
percentages[(nrow(percentages)),6] <- percentage_impervious[nrow(percentage_impervious),"cid"]

#calculates percentage of forest land uses
percentage_forest <- data.frame(matrix(nrow = 1, ncol = 4))
colnames(percentage_forest) <- c("landseg", "riverseg", "for.", "hvf")
count_forest <- 1
for(i in 1:nrow(current_land_use)){
  x <- percentages[i,]
  sum_forest <- sum(x[7:8])
  if(sum_forest!=0){
    percentage_forest[count_forest, 1] <- x$landseg
    percentage_forest[count_forest, 2] <- x$riverseg
    percentage_forest[count_forest, 3] <- x$for.
    percentage_forest[count_forest, 4] <- x$hvf
    count_forest <- count_forest+1
  }
}
percentage_forest[count_forest,1] <- print("Average")
for(i in 3:4){
  percentage_forest[count_forest,i] <- mean(percentage_forest[(1:nrow(percentage_forest)-1),i])
}
percentages[(nrow(percentages)),7] <- percentage_forest[nrow(percentage_forest),"for."]
percentages[(nrow(percentages)),8] <- percentage_forest[nrow(percentage_forest),"hvf"]

#calculates percentage for open space land uses
percentage_open <- data.frame(matrix(nrow = 1, ncol = 7))
colnames(percentage_open) <- c("landseg", "riverseg", "rcn", "ccn", "rex", "nex", "cex")
count_open <- 1
for(i in 1:nrow(current_land_use)){
  x <- percentages[i,]
  sum_open <- sum(x[9:13])
  if(sum_open!=0){
    percentage_open[count_open, 1] <- x$landseg
    percentage_open[count_open, 2] <- x$riverseg
    percentage_open[count_open, 3] <- x$rcn
    percentage_open[count_open, 4] <- x$ccn
    percentage_open[count_open, 5] <- x$rex
    percentage_open[count_open, 6] <- x$nex
    percentage_open[count_open, 7] <- x$cex
    count_open <- count_open+1
  }
}
percentage_open[count_open,1] <- print("Average")
for(i in 3:7){
  percentage_open[count_open, i] <- mean(percentage_open[(1:nrow(percentage_open)-1),i])
}
percentages[(nrow(percentages)),9] <- percentage_open[nrow(percentage_open),"rcn"]
percentages[(nrow(percentages)),10] <- percentage_open[nrow(percentage_open),"ccn"]
percentages[(nrow(percentages)),11] <- percentage_open[nrow(percentage_open),"rex"]
percentages[(nrow(percentages)),12] <- percentage_open[nrow(percentage_open),"nex"]
percentages[(nrow(percentages)),13] <- percentage_open[nrow(percentage_open),"cex"]

#calculates percentage of agriculture land uses
percentage_ag <- data.frame(matrix(nrow = 1, ncol = 22))
colnames(percentage_ag) <- c("landseg", "riverseg", "hwm","hyw", "hyo","alf","hom","nhi","lwm","nlo",
                             "nho","nhy","nal","pas","npa","trp","urs","cfo","afo","rpd","npd","cpd")
count_ag <- 1
for(i in 1:nrow(current_land_use)){
  x <- percentages[i,]
  sum_ag <- sum(x[14:33])
  if(sum_ag!=0){
    percentage_ag[count_ag,1] <- x$landseg
    percentage_ag[count_ag,2] <- x$riverseg
    percentage_ag[count_ag,3] <- x$hwm
    percentage_ag[count_ag,4] <- x$hyw
    percentage_ag[count_ag,5] <- x$hyo
    percentage_ag[count_ag,6] <- x$alf
    percentage_ag[count_ag,7] <- x$hom
    percentage_ag[count_ag,8] <- x$nhi
    percentage_ag[count_ag,9] <- x$lwm
    percentage_ag[count_ag,10] <- x$nlo
    percentage_ag[count_ag,11] <- x$nho
    percentage_ag[count_ag,12] <- x$nhy
    percentage_ag[count_ag,13] <- x$nal
    percentage_ag[count_ag,14] <- x$pas
    percentage_ag[count_ag,15] <- x$npa
    percentage_ag[count_ag,16] <- x$trp
    percentage_ag[count_ag,17] <- x$urs
    percentage_ag[count_ag,18] <- x$cfo
    percentage_ag[count_ag,19] <- x$afo
    percentage_ag[count_ag,20] <- x$rpd
    percentage_ag[count_ag,21] <- x$npd
    percentage_ag[count_ag,22] <- x$cpd
    count_ag <- count_ag+1
  }
}
percentage_ag[count_ag,1] <- print("Average")
for(i in 3:22){
  percentage_ag[count_ag, i] <- mean(percentage_ag[(1:nrow(percentage_ag)-1),i])
}
percentages[(nrow(percentages)),14] <- percentage_ag[nrow(percentage_ag),"hwm"]
percentages[(nrow(percentages)),15] <- percentage_ag[nrow(percentage_ag),"hyw"]
percentages[(nrow(percentages)),16] <- percentage_ag[nrow(percentage_ag),"hyo"]
percentages[(nrow(percentages)),17] <- percentage_ag[nrow(percentage_ag),"alf"]
percentages[(nrow(percentages)),18] <- percentage_ag[nrow(percentage_ag),"hom"]
percentages[(nrow(percentages)),19] <- percentage_ag[nrow(percentage_ag),"nhi"]
percentages[(nrow(percentages)),20] <- percentage_ag[nrow(percentage_ag),"lwm"]
percentages[(nrow(percentages)),21] <- percentage_ag[nrow(percentage_ag),"nlo"]
percentages[(nrow(percentages)),22] <- percentage_ag[nrow(percentage_ag),"nho"]
percentages[(nrow(percentages)),23] <- percentage_ag[nrow(percentage_ag),"nhy"]
percentages[(nrow(percentages)),24] <- percentage_ag[nrow(percentage_ag),"nal"]
percentages[(nrow(percentages)),25] <- percentage_ag[nrow(percentage_ag),"pas"]
percentages[(nrow(percentages)),26] <- percentage_ag[nrow(percentage_ag),"npa"]
percentages[(nrow(percentages)),27] <- percentage_ag[nrow(percentage_ag),"trp"]
percentages[(nrow(percentages)),28] <- percentage_ag[nrow(percentage_ag),"urs"]
percentages[(nrow(percentages)),29] <- percentage_ag[nrow(percentage_ag),"cfo"]
percentages[(nrow(percentages)),30] <- percentage_ag[nrow(percentage_ag),"afo"]
percentages[(nrow(percentages)),31] <- percentage_ag[nrow(percentage_ag),"rpd"]
percentages[(nrow(percentages)),32] <- percentage_ag[nrow(percentage_ag),"npd"]
percentages[(nrow(percentages)),33] <- percentage_ag[nrow(percentage_ag),"cpd"]


# Calculate current land use ----------------------------------------------


#this uses the percentages just calculated and the current land cover data to calculate the current land use data
#this is done by multiplying the percentage of each land use within the super category from 2005 by the super
#category cotaining that land use from the current land cover data
current_land_use <- data.frame(matrix(nrow=1, ncol = 33)) #creates the table containing the amount of land 
#calculated to be in each land use
colnames(current_land_use) <- c("landseg", "riverseg","wat","rid","nid","cid","for.","hvf","rcn","ccn",
                                "rex","nex","cex","hwm","hyw", "hyo","alf","hom","nhi","lwm","nlo","nho",
                                "nhy","nal","pas","npa","trp","urs","cfo","afo", "rpd","npd","cpd")
Average <- subset(percentages[469,])
count = 1
print("Creating the table for current land use:")
for(i in 1:nrow(data_comp)){
  print(paste("Processing row ",i," of ",nrow(data_comp)," in the landcover data",sep=""))
  current_land_use[i,1] <- data_comp[i,1] #assigns the land segment to the first row of the table 
  current_land_use[i,2] <- data_comp[i,2] #assigns the river segment to the second row of the table
  ls <- data_comp[i,1] #specifies the land segment of interest
  rs <- data_comp[i,2] #specifies the river segment of interest 
  h <- subset(data_comp, landseg == ls & riverseg == rs) #finds the land cover data for a specific segment
  j <- subset(percentages, landseg==ls & riverseg == rs) #finds the percentages for a specific segment 
    if (h$water_acres != 0){
      current_land_use[i,3] <- h$water_acres
    }else{
      current_land_use[i,3] <- h$water_acres*j$wat #multiplies the amount of land in the super category by the 
    }
  if (j$rid +j$nid+j$cid == 0){
    if (h$impervious_acres != 0){
      current_land_use[i,4] <- h$impervious_acres*Average$rid
      current_land_use[i,5] <- h$impervious_acres*Average$nid
      current_land_use[i,6] <- h$impervious_acres*Average$cid
    }else{
      current_land_use[i,4] <- h$impervious_acres*j$rid #percentage of that land that the land use took up within
      current_land_use[i,5] <- h$impervious_acres*j$nid #the super category in 2005
      current_land_use[i,6] <- h$impervious_acres*j$cid #that amount of land is then put into a new table
    }
  }else{
    current_land_use[i,4] <- h$impervious_acres*j$rid #percentage of that land that the land use took up within
    current_land_use[i,5] <- h$impervious_acres*j$nid #the super category in 2005
    current_land_use[i,6] <- h$impervious_acres*j$cid #that amount of land is then put into a new table
  }
  if (j$for.+j$hvf == 0){
    if (h$forest.tree_canopy_acres != 0){
      current_land_use[i,7] <- h$forest.tree_canopy_acres*Average$for.+h$wetlands_acres
      current_land_use[i,8] <- h$forest.tree_canopy_acres*Average$hvf
    }else{
      current_land_use[i,7] <- h$forest.tree_canopy_acres*j$for.+h$wetlands_acres #wetlands added to forest because no wetlands land uses
      current_land_use[i,8] <- h$forest.tree_canopy_acres*j$hvf
    }
  }else{
    current_land_use[i,7] <- h$forest.tree_canopy_acres*j$for.+h$wetlands_acres #wetlands added to forest because no wetlands land uses
    current_land_use[i,8] <- h$forest.tree_canopy_acres*j$hvf
  }
  if (j$rcn+j$ccn+j$rex+j$nex+j$cex == 0){
    if (h$open_space_acres != 0){
      current_land_use[i,9] <- h$open_space_acres*Average$rcn
      current_land_use[i,10] <- h$open_space_acres*Average$ccn
      current_land_use[i,11] <- h$open_space_acres*Average$rex
      current_land_use[i,12] <- h$open_space_acres*Average$nex
      current_land_use[i,13] <- h$open_space_acres*Average$cex
    }else{
      current_land_use[i,9] <- h$open_space_acres*j$rcn
      current_land_use[i,10] <- h$open_space_acres*j$ccn
      current_land_use[i,11] <- h$open_space_acres*j$rex
      current_land_use[i,12] <- h$open_space_acres*j$nex
      current_land_use[i,13] <- h$open_space_acres*j$cex
    }
  }else{
    current_land_use[i,9] <- h$open_space_acres*j$rcn
    current_land_use[i,10] <- h$open_space_acres*j$ccn
    current_land_use[i,11] <- h$open_space_acres*j$rex
    current_land_use[i,12] <- h$open_space_acres*j$nex
    current_land_use[i,13] <- h$open_space_acres*j$cex
  }
  if (j$hwm+j$hyw+j$hyo+j$alf+j$hom+j$nhi+j$lwm+j$nlo+j$nho+j$nhy+j$nal+j$pas+
      j$npa+j$trp+j$urs+j$cfo+j$afo+j$rpd+j$npd+j$cpd == 0){
    if (h$ag.turfgrass_acres != 0){
      current_land_use[i,14] <- h$ag.turfgrass_acres*Average$hwm
      current_land_use[i,15] <- h$ag.turfgrass_acres*Average$hyw
      current_land_use[i,16] <- h$ag.turfgrass_acres*Average$hyo
      current_land_use[i,17] <- h$ag.turfgrass_acres*Average$alf
      current_land_use[i,18] <- h$ag.turfgrass_acres*Average$hom
      current_land_use[i,19] <- h$ag.turfgrass_acres*Average$nhi
      current_land_use[i,20] <- h$ag.turfgrass_acres*Average$lwm
      current_land_use[i,21] <- h$ag.turfgrass_acres*Average$nlo
      current_land_use[i,22] <- h$ag.turfgrass_acres*Average$nho
      current_land_use[i,23] <- h$ag.turfgrass_acres*Average$nhy
      current_land_use[i,24] <- h$ag.turfgrass_acres*Average$nal
      current_land_use[i,25] <- h$ag.turfgrass_acres*Average$pas
      current_land_use[i,26] <- h$ag.turfgrass_acres*Average$npa
      current_land_use[i,27] <- h$ag.turfgrass_acres*Average$trp
      current_land_use[i,28] <- h$ag.turfgrass_acres*Average$urs
      current_land_use[i,29] <- h$ag.turfgrass_acres*Average$cfo
      current_land_use[i,30] <- h$ag.turfgrass_acres*Average$afo
      current_land_use[i,31] <- h$ag.turfgrass_acres*Average$rpd
      current_land_use[i,32] <- h$ag.turfgrass_acres*Average$npd
      current_land_use[i,33] <- h$ag.turfgrass_acres*Average$cpd
    }else{
      current_land_use[i,14] <- h$ag.turfgrass_acres*j$hwm
      current_land_use[i,15] <- h$ag.turfgrass_acres*j$hyw
      current_land_use[i,16] <- h$ag.turfgrass_acres*j$hyo
      current_land_use[i,17] <- h$ag.turfgrass_acres*j$alf
      current_land_use[i,18] <- h$ag.turfgrass_acres*j$hom
      current_land_use[i,19] <- h$ag.turfgrass_acres*j$nhi
      current_land_use[i,20] <- h$ag.turfgrass_acres*j$lwm
      current_land_use[i,21] <- h$ag.turfgrass_acres*j$nlo
      current_land_use[i,22] <- h$ag.turfgrass_acres*j$nho
      current_land_use[i,23] <- h$ag.turfgrass_acres*j$nhy
      current_land_use[i,24] <- h$ag.turfgrass_acres*j$nal
      current_land_use[i,25] <- h$ag.turfgrass_acres*j$pas
      current_land_use[i,26] <- h$ag.turfgrass_acres*j$npa
      current_land_use[i,27] <- h$ag.turfgrass_acres*j$trp
      current_land_use[i,28] <- h$ag.turfgrass_acres*j$urs
      current_land_use[i,29] <- h$ag.turfgrass_acres*j$cfo
      current_land_use[i,30] <- h$ag.turfgrass_acres*j$afo
      current_land_use[i,31] <- h$ag.turfgrass_acres*j$rpd
      current_land_use[i,32] <- h$ag.turfgrass_acres*j$npd
      current_land_use[i,33] <- h$ag.turfgrass_acres*j$cpd
    }
  }else{
    current_land_use[i,14] <- h$ag.turfgrass_acres*j$hwm
    current_land_use[i,15] <- h$ag.turfgrass_acres*j$hyw
    current_land_use[i,16] <- h$ag.turfgrass_acres*j$hyo
    current_land_use[i,17] <- h$ag.turfgrass_acres*j$alf
    current_land_use[i,18] <- h$ag.turfgrass_acres*j$hom
    current_land_use[i,19] <- h$ag.turfgrass_acres*j$nhi
    current_land_use[i,20] <- h$ag.turfgrass_acres*j$lwm
    current_land_use[i,21] <- h$ag.turfgrass_acres*j$nlo
    current_land_use[i,22] <- h$ag.turfgrass_acres*j$nho
    current_land_use[i,23] <- h$ag.turfgrass_acres*j$nhy
    current_land_use[i,24] <- h$ag.turfgrass_acres*j$nal
    current_land_use[i,25] <- h$ag.turfgrass_acres*j$pas
    current_land_use[i,26] <- h$ag.turfgrass_acres*j$npa
    current_land_use[i,27] <- h$ag.turfgrass_acres*j$trp
    current_land_use[i,28] <- h$ag.turfgrass_acres*j$urs
    current_land_use[i,29] <- h$ag.turfgrass_acres*j$cfo
    current_land_use[i,30] <- h$ag.turfgrass_acres*j$afo
    current_land_use[i,31] <- h$ag.turfgrass_acres*j$rpd
    current_land_use[i,32] <- h$ag.turfgrass_acres*j$npd
    current_land_use[i,33] <- h$ag.turfgrass_acres*j$cpd
  }
}
write.csv(current_land_use, "C:\\Users\\alyssaf4\\Desktop\\R\\current_land_use.csv")
write.csv(percent_diff_acres, "C:\\Users\\alyssaf4\\Desktop\\R\\percent_diff_WGS_VGIN.csv")
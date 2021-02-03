# 12 March 2018
# version 3: final parse down of code to analyze corrected segments

# LOAD LIBRARIES AND SET WORKSPACES -----------------------
library(dataRetrieval)
library(stringr)
library(rapportools)


setwd("C:/Users/Kelsey/Desktop/GitHub/hydro-tools/HARP-2018/CPB_ModelGageAnalysis")
#file_path <- "C:/Users/Kelsey/Desktop/GitHub/hydro-tools/HARP-2018/CPB_ModelGageAnalysis/GIS_Seg"

#read csv from drive: with corrected segments
corsegs <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQgO8tqsSDMOCn0y3r0HDQjWiXxCamavU5F-5SpSSH-PQ2LF9dLyCjRacqg5VroZ_grvyfunKX16lnM/pub?gid=0&single=true&output=csv", stringsAsFactors = F)

#identify the duplicate gages: 

repeats <- corsegs[which(duplicated(corsegs$gage)==TRUE),]
sameerr <- corsegs[which(duplicated(corsegs$gage)==TRUE & duplicated(corsegs$error)==TRUE),]

#remove segments with the same exact values: 
corsegs <- corsegs[-which(duplicated(corsegs$gage)==TRUE & duplicated(corsegs$error)==TRUE),]
repeats <- corsegs[which(duplicated(corsegs$gage)==TRUE),]

#the repeated segment is gage 2041650 with 85% error. the unique one has .05% error. remove the duplicate
corsegs <- corsegs[-which(duplicated(corsegs$gage)==TRUE),]

rm(ls=repeats); rm(ls=sameerr) #remove dataframes no longer being used
# check to make sure you're back to original number of gages: 
length(unique(corsegs$gage))

# which segment is duplicated? 
length(unique(corsegs$segment))
dupseg <- corsegs[which(duplicated(corsegs$segment)==TRUE),1]
dupseg <- corsegs[which(corsegs$segment==dupseg),]
# same segment w/ different gages -- gages seem to be in the same exact location



# BEGIN TO FORMAT A CSV THAT WILL BE USABLE FOR A GIS IMAGE: 

GIS_seg <- corsegs[,1:5]
write.csv(GIS_seg, file="updated_gage_seg_link.csv")

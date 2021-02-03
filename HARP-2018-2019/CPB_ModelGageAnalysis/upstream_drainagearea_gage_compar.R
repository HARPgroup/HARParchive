# 2/20/2019
# create code to compare drainage areas of all upstream segments from gages

# LOAD LIBRARIES AND SET WORKSPACES -----------------------
library(dataRetrieval)
library(stringr)
library(rapportools)

setwd("C:/Users/Kelsey/Desktop/GitHub/hydro-tools/HARP-2018/CPB_ModelGageAnalysis")
file_path <- "C:/Users/Kelsey/Desktop/GitHub/hydro-tools/HARP-2018/CPB_ModelGageAnalysis/GIS_Seg"
  
# LOAD AND FORMAT DATA FROM FOLDER ---------------------
all_segs <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQCrbbMS-XgzxOG0btMencBB_6F7kTqTodlb9FQU67nlGCSKKmXsXZNC-G-sUghdI4ya8heUIugBMjn/pub?output=csv", stringsAsFactors = FALSE) 
all_segs <- data.frame(all_segs$RiverSeg, all_segs$AreaSqMi)
colnames(all_segs) <- c("rivseg", "model_da")
      #pull a list of the segments that we're analyzing (came from the model)
      #note: Northern Rivers Segments csv



# Pull gages from GIS file (p53 calibstats shapefile) 
gages <- read.csv("Gage_stations.csv", stringsAsFactors = FALSE)
gages <- data.frame(as.numeric(as.character(gages$STAID)), gages$NAME, gages$CATCODE2)
gages$gages.NAME <- as.character(gages$gages.NAME)
gages$gages.CATCODE2 <- as.character(gages$gages.CATCODE2)
gages <- gages[(complete.cases(gages)),] #remove rows with na values

#remove susquehanna gages
gages$letter <- str_sub(gages$gages.CATCODE2, start=0L, end=1L)
gages <- gages[which(gages$letter!= "S"),]
gages <- data.frame(gages$as.numeric.as.character.gages.STAID.., gages$gages.CATCODE2)
gages$gages.gages.CATCODE2 <- as.character(gages$gages.gages.CATCODE2)
colnames(gages) <- c("staid", "rivseg")

#it is improperly labeled in GIS -- the file reads JB7_7070_0001, but it is *JL*
fixerror <- which(gages$rivseg=="JB7_7070_0001")
gages$rivseg[fixerror] <- "JL7_7070_0001"
rm(fixerror)

# RETRIEVE GAGE DRAINAGE AREAS 
k <- 1
for (k in 1:nrow(gages)){
  siteNo <- (str_pad(gages$staid[k], 8, pad = 0))
  gagedata <- readNWISsite(siteNo)
  gages$DAsqmi[k] <- gagedata$drain_area_va
}
rm(gagedata)

# Find matching gage / segments, see if drainage areas compare

matches <- (merge(x=gages, y=all_segs, by.x="rivseg", by.y="rivseg"))
matches <- data.frame(matches$rivseg, matches$staid, matches$DAsqmi, matches$model_da)
colnames(matches)<- c("segment", "gage", "gageDA", "segDA")

# ERROR HAS BEEN RESOLVED ---------
# #it is improperly labeled in GIS -- the file reads JB7_7070_0001, but it is *JL*
# #fixed this in lines 34-35
# length(unique(matches$gage)) #134
# length(unique(gages$staid)) #135
# #find the missing gage 
# matched <- intersect(matches$gage, gages$staid)
# all <-  union(matches$gage, gages$staid)
# non.matched <- all[!all %in% matched]


matches$error <- 100*(matches$gageDA-matches$segDA) / (matches$gageDA)

goodmatch <- matches[which(abs(matches$error) <= 5),] #QA good segments
badmatch <- matches[-which(abs(matches$error) < 5),]

write.csv(goodmatch, "lowerror_segs.csv")
write.csv(badmatch, "higherror_segs.csv")


# CORRECTED BAD SEGMENTS FROM THE CSV: 

badmatch2 <- read.csv("corrected_higherror.csv", stringsAsFactors = FALSE)
badmatch2 <- data.frame(badmatch2$gage, badmatch2$gageDA, badmatch2$corrected_segs)
colnames(badmatch2) <- c('gage', 'gageDA', 'adjustedsegs')
badmatch2$adjustedsegs <- as.character(badmatch2$adjustedsegs)

i <- 1
for (i in 1:nrow(badmatch2)){
  studyseg <- badmatch2$adjustedsegs[i]
  
  RivSegStr <- strsplit(studyseg, "\\+")
  RivSegStr <- RivSegStr[[1]]
  num.segs <- length(RivSegStr)
  
  if (num.segs == 1) {
    DA<- all_segs$model_da[which(all_segs$rivseg==as.character(RivSegStr))]
  }
  else if (num.segs !=1){
    j <- 1
    DA<- 0
    for (j in 1:num.segs){
      segnum <- RivSegStr[j]
      newDA <- all_segs$model_da[which(all_segs$rivseg==as.character(segnum))]
      DA <- DA + newDA
    }
  }
  if (is.empty(DA)==TRUE){
    DA <- NA
  }
  badmatch2$segDA[i] <- DA
}

badmatch2$error <- 100*(as.numeric(badmatch2$gageDA) - as.numeric(badmatch2$segDA)) / as.numeric(badmatch2$gageDA)

goodmatchrnd2 <- badmatch2[which(abs(badmatch2$error) <= 5),]
badmatchrnd2 <- badmatch2[-which(abs(badmatch2$error) < 5),]

write.csv(goodmatchrnd2, "lowerror_segs_adjustpt2.csv")
write.csv(badmatchrnd2, "higherror_segs_adjustpt2.csv")












# BEGIN SECTION 2 OF CODE ANALYSIS OF ERRORS : ------------------------------------



# 6 March 2019
# code to determine total drainage area and compare it to USGS gage drainage area
# hopefully w/ little error

# LOAD LIBRARIES AND SET WORKSPACES -----------------------
library(dataRetrieval)
library(stringr)
library(rapportools)

setwd("C:/Users/Kelsey/Desktop/GitHub/hydro-tools/HARP-2018/CPB_ModelGageAnalysis")
file_path <- "C:/Users/Kelsey/Desktop/GitHub/hydro-tools/HARP-2018/CPB_ModelGageAnalysis/GIS_Seg"

# LOAD AND FORMAT DATA FROM DRIVE CSV FILE ---------------------
dasegs <- read.csv("corrected_higherror_adjustpt2.csv", stringsAsFactors = F)
dasegs[,7] <- trimws(dasegs[,7], "l")

all_segs <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQCrbbMS-XgzxOG0btMencBB_6F7kTqTodlb9FQU67nlGCSKKmXsXZNC-G-sUghdI4ya8heUIugBMjn/pub?output=csv", stringsAsFactors = FALSE) 
all_segs <- data.frame(all_segs$RiverSeg, all_segs$AreaSqMi)
colnames(all_segs) <- c("rivseg", "model_da")
all_segs$rivseg <- as.character(all_segs$rivseg)
#pull a list of the segments that we're analyzing (came from the model)
#note: Northern Rivers Segments csv

#check for duplicates within the all_segs: 
repeats <- all_segs[which(duplicated(all_segs$rivseg)==TRUE),]
#these are all 0000 and 0001 segments

all_segs <- all_segs[!duplicated(all_segs$rivseg),] #parse from 916 to 695

j <- 1
for (j in 1:nrow(dasegs)){
  list <- data.frame(strsplit(dasegs[j,7], "\\ "))
  list[,1] <- as.character(list[,1])
  
  i <- 1
  sumda <- 0
  for (i in 1:nrow(list)){
    findseg <- list[i,1]
    DA<- all_segs[which(all_segs$rivseg==findseg),2]
    sumda <- sumda + DA
  }
  dasegs[j,8] <- sumda
}

comparedrainage <- data.frame(dasegs$gage, dasegs$gageDA, dasegs$adjustedsegs, dasegs$V8, dasegs$X.1)
colnames(comparedrainage) <- c("gageno", "gageda", "modelsegs", "upstDA","upstsegs")
rownames(comparedrainage) <- 1:nrow(comparedrainage)

comparedrainage$error <- 100*(comparedrainage$gageda - comparedrainage$upstDA) / comparedrainage$gageda


goodsegs <- comparedrainage[which(abs(comparedrainage$error) <= 10),]
badsegs <- comparedrainage[-which(abs(comparedrainage$error) < 10),]
badsegs$modelsegs <- as.character(badsegs$modelsegs)
badsegs$upstsegs  <- as.character(badsegs$upstsegs)

write.csv(goodsegs, "lowerror_segs_adjustpt3.csv")
write.csv(badsegs, "higherror_segs_adjustpt3.csv")



# BEGIN NEXT ROUND OF ERROR ANALYSIS (DOWN TO 13 SEGMENTS)

badsegs3 <- read.csv("corrected_higherror_adjustpt3.csv",stringsAsFactors = F)
badsegs3$upstsegs2[1] <- "JA5_7480_0001"
badsegs3$upstsegs2[2] <- "JL1_7170_6800 JL3_7020_7100 JL2_6240_6520 JL2_7110_7120 JL1_6560_6440 JL1_6760_6910 JL2_7120_6970 JL2_6440_6441 JL2_6441_6520 JL1_6770_6850 JL1_6910_6960 JL1_7080_7190 JL4_6520_6710 JL2_6850_6890 JL1_7530_7430 JL4_6710_6740 JL2_7240_7350 JL1_6940_7200 JU2_7180_7380 JL1_7190_7250 JL2_7350_7090 JL1_7200_7250 JU1_7750_7560 JU1_6880_7260 JL2_7250_7090 JL3_7090_7150 JU2_6410_6640 JU1_7630_7490 JU3_6640_6790 JU3_6790_7260 JU1_7560_7500 JU1_6300_6650 JU4_7260_7380 JU1_7690_7490 JU2_7450_7360 JU1_6340_6650 JU4_7380_7160 JU3_7490_7400 JU3_7400_7510 JU3_6650_7300 JU2_7140_7330 JU2_7360_7000 JU1_6290_6590 JU3_6380_6900 JU1_6590_6600 JU2_6600_6810 JU2_6810_6900 JU3_6900_6950 JU3_6950_7330 JU4_7330_7000 JU4_7000_7300 JU5_7300_7510 JU5_7510_7500 JU5_7500_7420 JU5_7420_7160 JL6_7160_7440 JL6_7440_7430 JL6_7430_7320 JL6_7320_7150 JL6_7150_6890 JL6_6890_6990 JL6_6990_6960 JL6_6960_6970 JL6_6970_6740 JL6_6740_7100 JL7_7100_7030 JL7_7030_6800 JL7_6800_7070"
badsegs3$upstsegs2[3] <- "JL1_6770_6850 JL1_7080_7190 JL2_6850_6890 JL1_7530_7430 JL2_7240_7350 JL1_6940_7200 JU2_7180_7380 JL1_7190_7250 JL2_7350_7090 JL1_7200_7250 JU1_7750_7560 JU1_6880_7260 JL2_7250_7090 JL3_7090_7150 JU2_6410_6640 JU1_7630_7490 JU3_6640_6790 JU3_6790_7260 JU1_7560_7500 JU1_6300_6650 JU4_7260_7380 JU1_7690_7490 JU2_7450_7360 JU1_6340_6650 JU4_7380_7160 JU3_7490_7400 JU3_7400_7510 JU3_6650_7300 JU2_7140_7330 JU2_7360_7000 JU1_6290_6590 JU3_6380_6900 JU1_6590_6600 JU2_6600_6810 JU2_6810_6900 JU3_6900_6950 JU3_6950_7330 JU4_7330_7000 JU4_7000_7300"
badsegs3$upstsegs2[4] <- "PL0_5000_0001"
badsegs3$upstsegs2[5] <- "PL1_5060_0000"
badsegs3$upstsegs2[6] <- "PL7_4910_0000"
badsegs3$upstsegs2[7] <- "PL7_4910_0000"
badsegs3$upstsegs2[8] <- "PM1_3710_4040 PM1_3120_3400 PM2_2860_3040 PM1_3450_3400 PM2_3400_3340 PM3_3040_3340 PM4_3340_3341 PM4_3341_4040"
badsegs3$upstsegs2[9] <- "PS2_5550_5560 PS2_5560_5100"
badsegs3$upstsegs2[10] <- "RL1_6180_0001"
badsegs3$upstsegs2[11] <- "RL1_6180_0001"
badsegs3$upstsegs2[12] <- "RU2_5220_5640 RU2_5940_6200 RU2_5500_5610 RU2_5810_5610 RU2_6090_6220 RU3_5610_5640 RU4_5640_6030 RU2_6200_6170 RU2_6220_6170 RU3_6170_6040 RU4_6040_6030 RU5_6030_0001"
badsegs3$upstsegs2[13] <- "RU2_5220_5640 RU2_5940_6200 RU2_5500_5610 RU2_5810_5610 RU2_6090_6220 RU3_5610_5640 RU4_5640_6030 RU2_6200_6170 RU2_6220_6170 RU3_6170_6040 RU4_6040_6030 RU5_6030_0001"


j <- 1
for (j in 1:nrow(badsegs3)){
  list <- data.frame(strsplit(badsegs3[j,9], "\\ "))
  list[,1] <- as.character(list[,1])
  
  i <- 1
  sumda <- 0
  for (i in 1:nrow(list)){
    findseg <- list[i,1]
    DA<- all_segs[which(all_segs$rivseg==findseg),2]
    sumda <- sumda + DA
  }
  badsegs3[j,10] <- sumda
}

badsegs3$newerror <- 100*(badsegs3$gageda - badsegs3$V10) / badsegs3$gageda

compare <- data.frame(badsegs3$gageno, badsegs3$gageda, badsegs3$V10, badsegs3$newerror, badsegs3$upstsegs2)

goodsegs <- compare[which(abs(compare$badsegs3.newerror) <= 10),]
badsegs <- compare[-which(abs(compare$badsegs3.newerror) < 10),]

write.csv(goodsegs, "lowerror_segs_adjustpt4.csv")
write.csv(badsegs, "higherror_segs_adjustpt4.csv")




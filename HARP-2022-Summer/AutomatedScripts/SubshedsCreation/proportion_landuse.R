#proportioning landuses of multiple land segments 
#when adding a new subshed to a river segment

suppressPackageStartupMessages(library("sqldf"))
suppressPackageStartupMessages(library("hydrotools"))#needed to pull values from VAHydro 

#----
argst <- commandArgs(trailingOnly = T)
main_seg <- argst[1]
subshed <- argst[2]
area <- argst[3]
file <- argst[4]

#testing:----
#main_seg <- 'PS2_5560_5100'
#subshed <- 'PS2_5568_5560'
#main_seg <- 'OD3_8720_8900'
#subshed <- 'OD3_8723_8720'

# file <- '/opt/model/p6/vadeq/input/scenario/river/land_use/land_use_2013VAHYDRO2018615.csv'
# file <- '/Users/aa_HARP/aa_GitHub/HARParchive/HARP-2022-Summer/AutomatedScripts/SubshedsCreation/land_use_2013VAHYDRO2018615.csv'
# setwd("/Users/VT_SA/Documents/HARP") # for testing only
# landuse_full <- read.csv('land_use_2013VAHYDRO2018615.csv', sep =',')

#retrieve subshed da----
site <- "http://deq1.bse.vt.edu/d.dh"    #Specify the site of interest, either d.bet OR d.dh

# Load Libraries
basepath='/var/www/R';
source(paste(basepath,'config.R',sep='/'))
ds = RomDataSource$new(site, rest_uname)
ds$get_token(rest_pw)

# get the DA, need to grab a model output first in order to ensure segments with a channel subcomp
# are included

#token <- 'wrRjwZK6jtr975TbkEmwxBtEbW3ff_ICJ5PzMEFsrzM'

# GET DA
df_area <- data.frame(
  'model_version' = c('vahydro-1.0',  'vahydro-1.0',  'vahydro-1.0'),
  'runid' = c('runid_11', '0.%20River%20Channel', 'local_channel'),
  'runlabel' = c('QBaseline_2020', 'comp_da', 'subcomp_da'),
  'metric' = c('Qbaseline', 'drainage_area', 'drainage_area')
)
da_data <- om_vahydro_metric_grid(
  metric = metric, runids = df_area,
  base_url = paste(site,'entity-model-prop-level-export',sep="/"),
  ds = ds
)
da_data <- sqldf(
  "select pid, comp_da, subcomp_da, riverseg,
   CASE
    WHEN comp_da is null then subcomp_da
    ELSE comp_da
    END as da
   from da_data
  ")

#isolate da for our subshed:
vahydro_subs <- subset(da_data, nchar(da_data$riverseg)>13)
vahydro_sub <- vahydro_subs[grep(main_seg, vahydro_subs$riverseg), ]
da <- vahydro_sub$da

#extract/calculate main ws and subshed areas and calculate their proportion:----
landuse_full <- read.csv(file, sep=',')

receiving_landuses <- sqldf(paste0("select * from landuse_full where riverseg = '",main_seg,"'"))
mainws_area <- sum(receiving_landuses[-1:-2])/640 #calculate the main ws total area

subsheds <- sqldf(paste0("select * from landuse_full where riverseg = '",subshed,"'"))

#subsheds$landseg <- "N51165" #take this out later

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

#check if subtraction/addition has already been performed; if not, proceed to calculate new subsheds:----
if (da - da*0.05 <= sub_area & sub_area <= da + da*0.05) {
  message('Main river segment and subsheds have already been proportioned.')
  q("n") #ENDING 1
}

lsegs <- receiving_landuses[-1:-1]
#lsegs_revert <- data.frame(matrix(0, 0, length(colnames(lsegs))))
lsegs_revert <- data.frame(matrix(0, length(lsegs$landseg), length(colnames(lsegs))))
#lsegs_revert <- data.frame()
colnames(lsegs_revert) <- colnames(lsegs)
lsegs_revert$landseg <- lsegs$landseg

if (sub_area != 0) {
  # add subshed areas back into main riverseg as fail safe:
 
  for (i in 1:length(subsheds$landseg)) {
    lseg_sort <- sqldf(paste0("select * from lsegs where landseg = '", subsheds$landseg[i], "'"))
    lsegs_new <- lseg_sort[i, -1] + subsheds[i, -1:-2]
    lsegs_new <- cbind(subsheds$landseg[i], lsegs_new)
    colnames(lsegs_new) <- colnames(lsegs)
    #rows <- grep(subsheds$landseg[i], lsegs_revert$landseg)
    lsegs_revert <- rbind(lsegs_revert, lsegs_new)
  }
  landuse <- lsegs_revert[-1:-1]
  
} else {    # if no previous subshed data - they have to be created for each lseg
  landuse <- receiving_landuses[-1:-2]
  lsegs_revert <- data.frame(matrix(0, length(lsegs$landseg), length(colnames(lsegs))))
  colnames(lsegs_revert) <- colnames(lsegs)
}

# create or re-create subsheds and remove from main seg:

sub_inc <- landuse * propor #add the proportion to the new subshed land uses
new_subshed <- cbind(subshed, lsegs_revert[1], sub_inc)
colnames(new_subshed) <- colnames(landuse_full)
  
lseg_dec <- (landuse - sub_inc) #subtract the proportion from main river segment
new_rseg <- cbind(receiving_landuses[1], lsegs_revert[1], lseg_dec)
colnames(new_rseg) <- colnames(landuse_full)
  
remove <- subset(landuse_full, riverseg != main_seg) #remove old land use values
new_list <- rbind(remove, new_rseg, new_subshed)
new_list <- new_list[order(new_list$landseg, new_list$riverseg),] #order new list by land seg first and then river seg
 
write.table(new_list,
            file=file,
            append = FALSE,
            quote = FALSE,
            sep = ",",
            row.names = FALSE,
            col.names = TRUE)

message('proportioned the river segment and subshed')

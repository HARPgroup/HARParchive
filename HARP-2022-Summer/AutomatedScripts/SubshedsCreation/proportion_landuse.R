#proportioning landuses of multiple land segments 
#when adding a new subshed to a river segment

suppressPackageStartupMessages(library("sqldf"))
suppressPackageStartupMessages(library("hydrotools"))#needed to pull values from VAHydro 

# Link data source (ds) to VAHydro
basepath='/var/www/R';
source("/var/www/R/config.R") 
ds <- RomDataSource$new(site, rest_uname = rest_uname)
ds$get_token(rest_pw)

#----
argst <- commandArgs(trailingOnly = T)
main_seg <- argst[1]
subshed <- argst[2]
channel <- argst[3]
file <- argst[4]

#testing:----
# main_seg <- 'PS2_5560_5100'
# subshed <- 'PS2_5568_5560'
# channel <- 'local_channel'
# file <- '/opt/model/p6/vadeq/input/scenario/river/land_use/land_use_2013VAHYDRO2018615.csv'
# setwd("/Users/VT_SA/Documents/HARP") # for testing only
# landuse_full <- read.csv('land_use_2013VAHYDRO2018615.csv', sep =',')


#pull subshed area from vahydro:----
  #pull old name with rseg???
  #use old name to pull area for subshed
rseg<- RomFeature$new(
  ds,
  list(
    hydrocode= paste("vahydrosw_wshed",riverseg,sep = "_"), 
    ftype='vahydro',
    bundle='watershed'
    ), 
  TRUE)
  
model <- RomProperty$new(
  ds,
  list(
    varkey="om_water_model_node",
    featureid=rseg$hydroid,
    entity_type="dh_feature", 
    propcode="vahydro-1.0"
    ), 
  TRUE)
  
channel_prop <- RomProperty$new(
  ds,
  list(
    #varkey="om_USGSChannelGeomObject", #for local_channel it needs _sub added to end
    featureid=model$pid,
    entity_type='dh_properties',
    propname = channel
    ),
  TRUE)
  
drainage_area <- RomProperty$new(
  channel_prop[["datasource"]],
  list(
    varkey="om_class_Constant",
    featureid=channel_prop$pid,
    entity_type='dh_properties',
    propname='drainage_area'
    ),
  TRUE)
da <- drainage_area$propvalue


vahydro_sub_area <- 

#extract/calculate main ws and subshed areas and calculate their proportion:----
landuse_full <- read.csv(file, sep=',')

main_seg <- paste(main_seg)

receiving_landuses <- sqldf("select * from landuse_full where riverseg = 'main_seg'")
mainws_area <- sum(receiving_landuses[-1:-2]) #calculate the main ws total area

subsheds <- sqldf("select * from landuse_full where riverseg = 'PS2_5568_5560'")
sub_area <- sum(subsheds[-1:-2])

list_prop <- sub_area/mainws_area
vahydro_prop <- vahydro_sub_area/mainws_area

#check if subtraction/addition has already been performed; if not, proceed to calculate new subsheds:----

if (list_prop == vahydro_prop) {
  message('Main river segment and subsheds have already been proportioned.')
  q("n") #ENDING 1
}


if (list_prop != vahydro_prop) {
  landuse <- receiving_landuses[-1:-2] #extract just the land uses
  
  lseg_dec <- (landuse - prop) #subtract the proportion from main river segment
  new_rseg <- cbind(receiving_landuses[1], receiving_landuses[2], lseg_dec)
  colnames(new_rseg) <- colnames(landuse_full)
  
  lseg_inc <- (landuse - lseg_dec) #add the proportion to the new subshed land uses
  new_subshed <- cbind(subshed, receiving_landuses[2], lseg_inc)
  colnames(new_subshed) <- colnames(landuse_full)
  
  remove <- subset(landuse_full, riverseg!= main_seg) #remove old land use values
  new_list <- rbind(remove, new_rseg, new_subshed)
  new_list <- new_list[order(new_list$landseg, new_list$riverseg),] #order new list by land seg first and then river seg
}
 
write.table(new_list,
            file=file,
            append = FALSE,
            quote = FALSE,
            sep = ",",
            row.names = FALSE,
            col.names = TRUE)

message('proportioned the river segment and subshed')

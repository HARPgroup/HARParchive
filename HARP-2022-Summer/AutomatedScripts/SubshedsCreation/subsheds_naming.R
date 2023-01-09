# This script generates new names for sub watersheds, and checks that the name is unique

# Setup----
suppressPackageStartupMessages(library("hydrotools")) #needed to pull values from VAHydro
# Link data source (ds) to VAHydro
basepath='/var/www/R';
source("/var/www/R/config.R") #will need file in same folder/directory
ds <- RomDataSource$new(site, rest_uname = rest_uname)
ds$get_token(rest_pw)
#----
argst <- commandArgs(trailingOnly = T)
subshed <- argst[1]
list <- argst[2] #rivernames.csv (the master list)

#testing:
#subshed <- 'PS2_5560_5100_linville_creek'
#subshed <- 'OD3_8720_8900_ut_leatherwood' #!!! interesting: 8720 not in master list
#subshed <- 'RU5_6030_0001_motts_run'
#list <- 'http://deq1.bse.vt.edu:81/p6/vadeq/config/catalog/geo/vahydro/rivernames.csv'
#unique_ids <- as.character(seq(0002,9999,1))
#sub_id <- 65

#isolate downstream
splits <- strsplit(subshed, "_") #creates a list of 1
downstr <- as.numeric(splits[[1:2]]) #reads 2nd item
sub_id <- as.numeric(splits[[1:2]])

#pull master list
full_list <- read.csv(list, sep=',')
ids <- full_list[1]
ids <- t(data.frame(strsplit(ids$river, "_")))
rownames(ids) <- NULL
unique_ids <- ids[,2]

#check VAhydro for existing model----
sub <- RomFeature$new(ds,list(
  hydrocode= paste("vahydrosw_wshed",subshed,sep = "_"), 
  ftype='vahydro',
  bundle='watershed'), 
  TRUE)

model <- RomProperty$new(ds,list(
  varkey="om_water_model_node",
  featureid=sub$hydroid,
  entity_type="dh_feature", 
  propcode="vahydro-1.0"), 
  TRUE)
model$save(TRUE)

wordname <- sub[['name']]

#create new id----
repeat {
  if(length(as.numeric(unique_ids)) >= 9998) {
    print('No unique names available.')
    break #script stops here and will not write a new name
  }
  if(sub_id %in% unique_ids) {
    sub_id <- sub_id + 1
  } 
  if(sub_id > 9999) { 
    sub_id <- 0002
  }
  else {
    sub_id <- formatC(sub_id, width = 4, format = "d", flag = "0") #make sure new id stays 4 digits
    new_name <- paste(splits[[1]][[1]],sub_id, downstr, sep = "_") 
    row <- data.frame(new_name, wordname)
    colnames(row) <- colnames(full_list)
    new_list <- rbind(head(full_list,-1), row)
    new_list <- new_list[order(new_list$river), ]
    end <- data.frame(tail(full_list,1))
    new_list <- rbind(new_list, end)
    
    write.table(new_list,
                file=list,
                append = TRUE,
                quote = FALSE,
                sep = ",",
                row.names = FALSE,
                col.names = TRUE)
    
    break
  }
}


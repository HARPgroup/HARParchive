# This script generates new names for sub watersheds, and checks that the name is unique

#----
argst <- commandArgs(trailingOnly = T)
subshed <- argst[1]
list <- argst[2] #rivernames.csv (the master list)
model_version <- argst[3] #cbp-6.0

#testing:----
#subshed <- 'PS2_5560_5100_linville_creek'
#subshed <- 'OD3_8720_8900_ut_leatherwood' #!!! interesting: 8720 not in master list
#subshed <- 'RU5_6030_0001_motts_run'
#list <- 'http://deq1.bse.vt.edu:81/p6/vadeq/config/catalog/geo/vahydro/rivernames.csv'
#model_version <- "cbp-6.0"
#unique_ids <- as.character(seq(0002,9999,1))
#sub_id <- 65

# Load----
suppressPackageStartupMessages(library("hydrotools")) #needed to pull values from VAHydro
# Link data source (ds) to VAHydro
basepath='/var/www/R';
source("/var/www/R/config.R") #will need file in same folder/directory
ds <- RomDataSource$new(site, rest_uname = rest_uname)
ds$get_token(rest_pw)

#check VAhydro for existing model
sub <- RomFeature$new(ds,list(
  hydrocode= paste("vahydrosw_wshed",subshed,sep = "_"), 
  ftype='vahydro',
  bundle='watershed'), 
  TRUE)

model <- RomProperty$new(
  ds,
  list(
    varkey = "om_water_model_node",
    featureid = sub$hydroid,
    entity_type = "dh_feature",
    propcode = "vahydro-1.0"
  ),
  TRUE
)
wordname <- model$propname

model6 <- RomProperty$new(
  ds,
  list(
    varkey="om_water_model_node",
    featureid=sub$hydroid,
    entity_type="dh_feature", 
    propcode=model_version,
    propname=paste(model$propname, 'p6')
  ), 
  TRUE)
model6$save(TRUE)

#pull master list
full_list <- read.csv(list, sep=',')
ids <- full_list[1]
ids <- t(data.frame(strsplit(ids$river, "_")))
rownames(ids) <- NULL
unique_ids <- ids[,2]

#----
taken <- length(as.numeric(unique_ids)) >= 9998
if (taken==TRUE) {
  message('No unique names available.')
  q("n")
} #ENDING 1

#create function
check_exist <- function(item, collection) {
  if (item %in% collection) {
    exist <- TRUE
  }
  else {
    exist <- FALSE
  }
  return(exist)
}

#check riverseg identifier #### in VaHydro
rseg <- RomProperty$new(
  ds,
  list(
    varkey = "om_class_AlphanumericConstant",
    featureid = model6$pid,
    entity_type = "dh_properties",
    propname = 'riverseg',
  ),
  TRUE
)

make_rseg_name <- function(subshed, unique_ids, wordname){
  splits <- strsplit(subshed, "_") #creates a list of 1
  downstr <- as.numeric(splits[[1:2]]) #reads 2nd item
  sub_id <- as.numeric(splits[[1:2]])
  
  # after getting id, check again in list
  repeat {
    exist <- check_exist(sub_id,unique_ids)
    if(exist == TRUE) {
      sub_id <- sub_id + 1
      if(sub_id > 9999) {sub_id <- 0002}
      next
    } 
    else {
      break
    }
  }
  
  #format & save new id:
  sub_id <- formatC(sub_id, width = 4, format = "d", flag = "0") #make sure new id stays 4 digits
  new_name <- paste(splits[[1]][[1]],sub_id, downstr, sep = "_") 
  return(new_name)
}

if ((is.na(rseg$pid) == TRUE) || (rseg$propcode == "")) {
  # unique name doesn't exist yet
  new_name <- make_rseg_name(subshed, unique_ids, wordname)
  
  row <- data.frame(new_name, wordname)
  colnames(row) <- colnames(full_list)
  new_list <- rbind(head(full_list,-1), row)
  new_list <- new_list[order(new_list$river), ]
  end <- data.frame(tail(full_list,1))
  new_list <- rbind(new_list, end)
  colnames(new_list) <- c('river','name***')
  
  write.table(new_list,
              file=list,
              append = FALSE,
              quote = FALSE,
              sep = ",",
              row.names = FALSE,
              col.names = TRUE)
  
  #add new id to model6 as property riverseg:
  rseg$propcode <- new_name
  rseg$save(TRUE)
  
  message(new_name)

  } else {
    splits <- strsplit(rseg$propcode, "_") #creates a list of 1
    sub_id <- as.numeric(splits[[1:2]])
    exist <- check_exist(sub_id,unique_ids)
    if (exist == FALSE){
      message('rseg in VaHydro; not on master list')
      
      row <- data.frame(rseg$propcode, wordname)
      colnames(row) <- colnames(full_list)
      new_list <- rbind(head(full_list,-1), row)
      new_list <- new_list[order(new_list$river), ]
      end <- data.frame(tail(full_list,1))
      new_list <- rbind(new_list, end)
      colnames(new_list) <- c('river','name***')
      
      write.table(new_list,
                  file=list,
                  append = FALSE,
                  quote = FALSE,
                  sep = ",",
                  row.names = FALSE,
                  col.names = TRUE)
      
      message('added rseg to master list')
      }
  }


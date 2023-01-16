# This script generates new names for sub watersheds, and checks that the name is unique

#----Receive Arguments----
argst <- commandArgs(trailingOnly = T)
subshed <- argst[1]
list <- argst[2] #full path to rivernames.csv (the master list)
model_version <- argst[3] #e.g. cbp-6.0
#----!testing,comment!----
#subshed <- 'PS2_5560_5100_linville_creek'
#list <- 'http://deq1.bse.vt.edu:81/p6/vadeq/config/catalog/geo/vahydro/rivernames.csv'
#model_version <- "cbp-6.0"
#unique_ids <- as.character(seq(0002,9999,1))
#sub_id <- 65

#----Load VAhydro models and Master List----
suppressPackageStartupMessages(library("hydrotools")) #needed to pull values from VAHydro

#link data source (ds) to VAHydro
basepath='/var/www/R';
source("/var/www/R/config.R") #will need file in same folder/directory
ds <- RomDataSource$new(site, rest_uname = rest_uname)
ds$get_token(rest_pw)

#check VAhydro for existing vahydro-1.0 model
sub <- RomFeature$new(
  ds,
  list(
    hydrocode= paste("vahydrosw_wshed",subshed,sep = "_"), 
    ftype='vahydro',
    bundle='watershed'
  ), 
  TRUE
)

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
#save the formatted verbal name for the river seg.
wordname <- model$propname #e.g. "Linville Creek"
#this^ will be used in the second column of master list

#check VAhydro for existing user-chosen model version
model6 <- RomProperty$new(
  ds,
  list(
    varkey="om_water_model_node",
    featureid=sub$hydroid,
    entity_type="dh_feature", 
    propcode=model_version,
    propname=paste(model$propname, 'p6') #if not designating cbp-6.0, may have to change 'p6' ?
  ), 
  TRUE
)
model6$save(TRUE) #we save this one if it doesn't already exist. The previous ones should always exist. 

#pull master list
full_list <- read.csv(list, sep=',')
ids <- full_list[1] #isolate the 1st column with the riverseg full ids (e.g. PS2_5560_5100)
ids <- t(data.frame(strsplit(ids$river, "_"))) #e.g. PS2_5560_5100 becomes 3 columns: PS2 5560 5100
rownames(ids) <- NULL
unique_ids <- ids[,2] #take just the middle column, which is just the unique riverseg ids (e.g. 5560)

#----Are There Open ID's Available?----
taken <- length(as.numeric(unique_ids)) >= 9998
if (taken==TRUE) {
  message('No unique names available.')
  q("n") #ENDING 1
} 
#if master list has all possible ids from (0002, 9999), script ends here.

#----Create Functions----
check_exist <- function(item, collection) {
  if (item %in% collection) {
    exist <- TRUE
  }
  else {
    exist <- FALSE
  }
  return(exist)
}

make_rseg_name <- function(subshed, unique_ids, wordname){
  splits <- strsplit(subshed, "_") #separates 3 segments of rseg name; creates a list of 1
  downstr <- as.numeric(splits[[1:2]]) #reads 2nd item to isolate rseg id
  sub_id <- as.numeric(splits[[1:2]]) #set sub_id equal to downstream id as a starting point
  
  #after getting id, check if it's in the list
  repeat {
    exist <- check_exist(sub_id,unique_ids)
    if(exist == TRUE) {
      sub_id <- sub_id + 1
      if(sub_id > 9999) {sub_id <- 0002}
      next
    } 
    else {
      break #a unique id is found and we move on
    }
  }
  
  #format & save new id:
  sub_id <- formatC(sub_id, width = 4, format = "d", flag = "0") #make sure new id stays 4 digits
  new_name <- paste(splits[[1]][[1]],sub_id, downstr, sep = "_") 
  return(new_name)
}

#----Load Riverseg Property of model_version in VaHydro----
rseg <- RomProperty$new(
  ds,
  list(
    varkey = "om_class_AlphanumericConstant",
    featureid = model6$pid,
    entity_type = "dh_properties",
    propname = 'riverseg'
  ),
  TRUE
)

#----Identify what is being made & saved----

#either no value saved for either pid or propcode
#or VaHydro already contains unique name

if ((is.na(rseg$pid) == TRUE) || (rseg$propcode == "")) {
  #thus unique name doesn't exist yet
  #this is where we apply make_rseg_name function:
  new_name <- make_rseg_name(subshed, unique_ids, wordname)
  
  #add to master list & save
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

  } else { #unique name has already been saved in VaHydro
    #retrieve unique #### id from propcode
    splits <- strsplit(rseg$propcode, "_")
    sub_id <- as.numeric(splits[[1:2]])
    exist <- check_exist(sub_id,unique_ids)
    
    #check to see if it's been saved in master list yet:
    if (exist == FALSE){ #it's not
      message('rseg in VaHydro; not on master list')
      
      #so we add it to the list
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
      } #otherwise, unique name is saved in both VAhydro & master list. We do nothing.
  }

# print subshed new name
# print main_seg aka downstream riverseg name
splits <- strsplit(subshed, "_")
downstream <- paste(splits[[1]][[1]],splits[[1]][[2]],splits[[1]][[3]], sep = "_")
print(paste(new_name, downstream))

#Updating vahydro metrics incl. Smin by calling 
# Load Libraries
library(hydrotools)
basepath='/var/www/R';
source(paste(basepath,'config.R',sep='/'))
# Authenticate
ds <- RomDataSource$new(site, rest_uname)
ds$get_token(rest_pw)

# Read args
argst <- commandArgs(trailingOnly=T)
pid <- as.integer(argst[2]) #model element pid
runid <- as.integer(argst[1]) #numeric part of a runid (ex. 11)


token = ds$get_token(rest_pw) #needed for elid function 
elid <- om_get_model_elementid(base_url = site, mid = pid) #get om_element_connection value for elid
rm(token) 

om_model <- RomProperty$new(ds,list(pid=pid),TRUE)
object_class <- RomProperty$new(ds,list(featureid = om_model$pid, propname = 'object_class'),TRUE)$propcode

#Run 1 of 3 om post-processing scripts based on type of impoundment
if (object_class=="waterSupplyModelNode") {
  commandArgs <- function(...) c(pid,elid,runid)
  source(paste0("~/HARParchive/HARP-2023-Summer/waterSupplyModelNode.R")) #riverseg imp
} else if (object_class=="waterSupplyElement") {
  commandArgs <- function(...) c(pid,elid,runid)
  source(paste0("~/HARParchive/HARP-2023-Summer/waterSupplyElement.R")) #facility imp
} else if (object_class=="hydroImpoundment") {
  commandArgs <- function(...) c(pid,elid,runid)
  source(paste0("~/HARParchive/HARP-2023-Summer/hydroImpoundment.R")) #standalone imp
}


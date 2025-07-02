#Load hydrotools and the config file
library(hydrotools)
basepath='/var/www/R'
source(paste(basepath,'config.R',sep='/'))

#Set the hydrocode of interest
# hydrocode <- 'vahydrosw_wshed_PS2_5550_5560'
hydrocode <- 'vahydrosw_wshed_PS2_5560_5100'
# hydrocode <- 'vahydrosw_wshed_PS2_5560_5100'
# hydrocode <- 'vahydrosw_wshed_PS3_5100_5080'
pullModelData <- function(hydrocode, filepath) {
  #Get the watershed featuer
  wshdFeat <- RomFeature$new(ds, config = list(hydrocode = hydrocode), TRUE)
  #Grab the model
  wshdModel <- wshdFeat$get_prop(propcode = 'vahydro-1.0')
  #Is runid 600 present?
  any(grepl("runid_600",wshdModel$propvalues()[,'propname']))
  #Grab the model results and write out as csv
  modelResults <- as.data.frame(
    om_get_rundata(wshdModel$propvalues("om_element_connection")$propvalue,
                   600, site = omsite))
  #Write out results specifying a simplified name of the model and the
  #riversegment hydrocode
  write.csv(modelResults, paste0(filepath,
                                 gsub("(.*@ )| ","",wshdModel$propname), "_",
                                 gsub("vahydrosw_wshed_","",wshdFeat$hydrocode),
                                 "_results.csv"))
}




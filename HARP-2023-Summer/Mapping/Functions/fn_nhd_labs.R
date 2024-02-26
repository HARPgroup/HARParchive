#NHD data labeling prep -- originally part of fn_labelprep.R
# for current specific use:
# data = nhd

# JK 8.8.23: Need to load the config file for this to run
# Will need to explicitly pass in params such as nhd_rivname_pattern
source(paste0(github_uri,"/HARP-2023-Summer/Mapping/Config/mapstyle_config.R"),local = TRUE) #load mapping aesthetics
fn_nhd_labs <- function(data) {

  for(d in 1:length(data)){
  ## NHD Data
    # First organize flowline data with major rivers/streams
    ## major rivs = orders 5 & 6; streams = order 4
    flow <- nhd$flowline[nhd$flowline$gnis_name!=' ' & #name!=blank & order 4, 5, or 6
                           (nhd$flowline$StreamOrde==6 | nhd$flowline$StreamOrde==5 | nhd$flowline$StreamOrde==4),]
    ## no duplicate names; prioritize higher order names and then the longest segment of each duplicate
    flow <- flow[order(-flow$StreamOrde, flow$gnis_name, -flow$LENGTHKM) & !duplicated(flow$gnis_name),]
    ## shorten long names
    flow$gnis_name <- mgsub(flow$gnis_name,
                            nhd_rivname_pattern, #pattern
                            nhd_rivname_replacements) #replacement
    flow$StreamOrde <- mgsub(flow$StreamOrde, nhd_streamorders, nhd_streamclasses)
    flow <- flow[,c("gnis_name","StreamOrde")] #geometry is still attached
    colnames(flow) <- gsub("StreamOrde", "class", colnames(flow))
    
    # Now do the same for the water bodies
    wtbd <- rbind(nhd$network_wtbd, nhd$off_network_wtbd)
    ## remove ones without names and ponds/swamps
    wtbd <- wtbd[!(wtbd$gnis_name==' ' | wtbd$gnis_name=='Noname'),]
    wtbd <- wtbd[!grepl(c(wtbd_names_rm), wtbd$gnis_name),] #remove certain names from waterbody labeling, wtbd_names_rm from mapstyles_config
    wtbd <- wtbd[!is.na(wtbd$lakevolume),]
    #wtbd <- wtbd[!grepl("Millpond", wtbd$gnis_name),] 
   # wtbd <- wtbd[!grepl("Swamp", wtbd$gnis_name),]
    
    
    #classification of waterbodies with classes based on their size
    wtbd_small <- wtbd[wtbd$lakevolume > quantile(wtbd$lakevolume, wtbd_sm_pct_range[1], na.rm = T) & 
                         wtbd$lakevolume < quantile(wtbd$lakevolume, wtbd_sm_pct_range[2], na.rm = T),]
    wtbd_small$class <- rep("waterbody_sm", nrow(wtbd_small)) #add class column
    
    wtbd_med <- wtbd[wtbd$lakevolume > quantile(wtbd$lakevolume, wtbd_med_pct_range[1], na.rm = T) & 
                       wtbd$lakevolume < quantile(wtbd$lakevolume, wtbd_med_pct_range[2], na.rm = T),]
    wtbd_med$class <- rep("waterbody_med", nrow(wtbd_med)) #add class column
    
    wtbd_large <- wtbd[wtbd$lakevolume > quantile(wtbd$lakevolume, wtbd_med_pct_range[2], na.rm = T),]
    wtbd_large$class <- rep("waterbody_lg", nrow(wtbd_large)) #add class column
    
    ##something wrong -- too many bodies classified as large 
    
    wtbd <- rbind(wtbd_small,wtbd_med,wtbd_large)
    wtbd <- wtbd[,c("gnis_name","class")]
    wtbd <- wtbd[!is.na(wtbd$gnis_name),]
    
    nhdlabs <- rbind(flow, wtbd)
    names(nhdlabs)[names(nhdlabs) == 'gnis_name'] <- 'label'
    # now it is ready to have coords calculated like the rest of the labeling data
#    assign('data', data, envir = globalenv())#save df to environment
    }
  return(nhdlabs)
}  
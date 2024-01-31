#NHD data labeling prep -- originally part of fn_labelprep.R
# for current specific use:
# data = nhd
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
                            c('North Fork','South Fork','East Fork','West Fork','Middle Fork'), #pattern
                            c('NF','SF','EF','WF','MF')) #replacement
    flow$StreamOrde <- mgsub(flow$StreamOrde, c(4,5,6), c("stream","majorRiver","majorRiver"))
    flow <- flow[,c("gnis_name","StreamOrde")] #geometry is still attached
    colnames(flow) <- gsub("StreamOrde", "class", colnames(flow))
    
    # Now do the same for the water bodies
    wtbd <- rbind(nhd$network_wtbd, nhd$off_network_wtbd)
    ## remove ones without names, ponds, and & filter to largest 15%
    wtbd <- wtbd[!grepl("Pond", wtbd$gnis_name),] #remove ponds
    wtbd <- wtbd[!grepl("Millpond", wtbd$gnis_name),] 
    wtbd <- wtbd[!grepl("Swamp", wtbd$gnis_name),]
    wtbd <- wtbd[!(wtbd$gnis_name==' ' | wtbd$gnis_name=='Noname'),]
    
    #wtbd <- wtbd[!(wtbd$gnis_name==' ' | wtbd$gnis_name=='Noname') & wtbd$lakevolume > quantile(wtbd$lakevolume, 0.85),]
    #wtbd$class <- rep("waterbody", nrow(wtbd)) #add class column
    #wtbd <- wtbd[,c("gnis_name","class")] #geometry is still attached
    
    ##new 7/17: add more classification of waterbodies with classes based on their size
    wtbd_small <- wtbd[!(wtbd$gnis_name==' ' | wtbd$gnis_name=='Noname') & 
                         wtbd$lakevolume > quantile(wtbd$lakevolume, 0.0, na.rm = T) & 
                         wtbd$lakevolume < quantile(wtbd$lakevolume, 0.5, na.rm = T),]
    wtbd_small$class <- rep("waterbody_sm", nrow(wtbd_small)) #add class column
    
    wtbd_med <- wtbd[!(wtbd$gnis_name==' ' | wtbd$gnis_name=='Noname') & 
                       wtbd$lakevolume > quantile(wtbd$lakevolume, 0.5, na.rm = T) & 
                       wtbd$lakevolume < quantile(wtbd$lakevolume, 0.75, na.rm = T),]
    wtbd_med$class <- rep("waterbody_med", nrow(wtbd_med)) #add class column
    
    wtbd_large <- wtbd[!(wtbd$gnis_name==' ' | wtbd$gnis_name=='Noname') & 
                         wtbd$lakevolume > quantile(wtbd$lakevolume, 0.75, na.rm = T),]
    wtbd_large$class <- rep("waterbody_lg", nrow(wtbd_large)) #add class column
    
    wtbd <- rbind(wtbd_small,wtbd_med,wtbd_large)
    wtbd <- wtbd[,c("gnis_name","class")]
    wtbd <- wtbd[!is.na(wtbd$gnis_name),]
    
    nhdlabs <- rbind(flow, wtbd)
    # now it is ready to have coords calculated like the rest of the labeling data
#    assign('data', data, envir = globalenv())#save df to environment
    }
  return(nhdlabs)
}  
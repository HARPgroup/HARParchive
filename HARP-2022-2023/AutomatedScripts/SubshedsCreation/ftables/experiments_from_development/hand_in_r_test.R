
# exploring a flood mapping package that implements HAND method

remotes::install_github("mikejohnson51/FloodMapping")
remotes::install_github("mikejohnson51/AOI")

remotes::install_github('mikejohnson51/nwmTools')
install.packages('stars')

library(FloodMapping)
library(AOI)
library(dplyr)

library(nwmTools) #acess to National Water Model
library(stars) #for reading, manipulating, writing, plotting rasters,etc 

#Mapping Tools:
#library(leaflet)
#library(leafem)

#choose directory for data:
raw.dir <- '/aa_HARP/ftables'

#choose Area of Interest:
AOI <-  aoi_get("Rockfish River, VA")
#RoanokeGeom <-  aoi_get("Roanoke River, VA")
#BearClubGeom <-  aoi_get("Bear Club Creek, VA")

#state project name:
project.name <- "Rockfish River"

#Download, process, and save HAND, catchmask and Rating Curve Data
files <- getRawData(AOI, dir = dir, name)

#debug attempt:
AOI <-  aoi_get("Rockfish River, VA")
dir <- '/aa_HARP/ftables'
name <- "Rockfish River"

download_ut(HUC6, dir, type = "hand")

#----download_ut Function-------------------------------------------------------------

download_ut <- function(HUC6, outdir, type = 'hand'){
  
  files = c('catchmask', 'rating', 'hand')
  
  if(any(!type %in% c(files, 'all'))){
    stop(paste("Type must be one or more of: ", paste(files, collapse = ", "), "or all"))
  }
  
  if(type == 'all'){ type = files}
  
  base.url = 'http://web.corral.tacc.utexas.edu/nfiedata/HAND/'
  
  g = expand.grid(HUC6, type)
  
  urls = paste0(base.url, g$Var1, "/", g$Var1, g$Var2, '.tif')
  
  urls[g$Var2 == "rating"] = paste0(base.url, g$Var1, "/hydroprop-fulltable-", g$Var1, ".nohand0.csv") [g$Var2 == "rating"]
  
  
  message("Beginning download of HUC6-", HUC6, " files...")
  for(i in 1:length(urls)){
    downloader(outdir, url = urls[i])
  }
}


downloader <- function(dir, url) {
  
  if (!dir.exists(dir)) { dir.create(dir, recursive = T) }
  
  file = paste0(dir, basename(url))
  
  if (!file.exists(file)) {
    message("\tDownloading ", basename(url))
    
    resp <-  httr::GET(url,
                       httr::write_disk(file, overwrite = TRUE),
                       httr::progress())
    
    if (resp$status_code != 200) {
      warning(basename(url), " Download unsuccessfull :(")
    }
  } #else {
    #message("\t", basename(url), " already exists ...")
  #}
  
  return(file)
}

#-----getRawData Function------------------------------------------------------

getRawData = function(AOI, dir, name){
  
  HUC6 = findHUC6(AOI, level = 6)$huc6
  
  lapply(HUC6, download_ut, outdir = dir)
  
  # Make name folder
  name.dir = paste0(dir,"//", name)
  catch.path = paste0(name.dir, "/catchmask_", name,".tif")
  hand.path = paste0(name.dir, "/hand_", name,".tif")
  
  if(!file.exists(catch.path) |
     !file.exists(hand.path)){
    
    if(!dir.exists(name.dir)){ dir.create(name.dir) }
    # Write AOI
    aoi.path = paste0(name.dir, "//", name, ".gpkg")
    AOI      = st_transform(AOI, 3857)
    
    sf::write_sf(AOI, dsn = aoi.path)
    
    # Build list
    to_process = data.frame(HUC6 = rep(HUC6, each = 1),
                            name = name,
                            raw.files = grep("tif$",
                                             list.files(dir, pattern = paste(HUC6, collapse = "|"), full.names = TRUE), 
                                             value = TRUE),
                            stringsAsFactors = FALSE) %>%
      mutate(cropped = paste0(name.dir, "/", basename(.data$raw.files))) %>%
      mutate(type    = ifelse(grepl("hand", .data$raw.files), "HAND", "CATCH")) %>%
      mutate(method  = ifelse(.data$type == "HAND", 'bilinear', 'near'))
    
    # Crop
    message('Cropping and Projecting Rasters...')
    for(i in 1:nrow(to_process)){
      crop_project(input    = to_process$raw.files[i],
                   output   = to_process$cropped[i],
                   name     = to_process$name[i],
                   aoi.path = aoi.path,
                   method   = to_process$method[i])
    }
    
    # Align
    message('Aligning Rasters...')
    for(i in 1:length(HUC6)){
      align_rasters(huc6 = HUC6[i], name.dir)
    }
    
    # Merge Rasters
    message('Merging Rasters...')
    files  = merge_rasters(name.dir, name)
    hand = raster(hand.path)
    cat = nhdplusTools::get_nhdplus(AOI, realization = "catchment")
    catchmask = fasterize::fasterize(sf::st_cast(cat), hand, field = "featureid")
    writeRaster(catchmask, catch.path)
    #comids = unique(getValues(raster(files$catch.path)))
    
    files = list(hand.path = hand.path,
                 catch.path = catch.path)
    
  } else {
    message('Files Already Processed...')
    files = list(hand.path = hand.path,
                 catch.path = catch.path)
  }
  
  return(files)
}

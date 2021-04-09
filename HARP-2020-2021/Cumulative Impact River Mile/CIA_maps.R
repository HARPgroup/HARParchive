
###################################################################################################### 
# LOAD MAP LAYERS
######################################################################################################
load_MapLayers <- function(site,localpath = tempdir()){
  library(ggplot2)
  library(rgeos)
  library(ggsn)
  library(rgdal) # needed for readOGR()
  library(dplyr) # needed for case_when()
  library(sf) # needed for st_read()
  library(sqldf)
  library(kableExtra)
  library(viridis) #magma
  #library(wicket) #wkt_centroid() (package was removed from CRAN on 1/27/21)
  library(wellknown) #Replaces the wicket package for wkt_centroid()
  library(cowplot) #plot static legend
  library(magick) #plot static legend
  library(ggrepel) #needed for geom_text_repel()
  library(ggmap) #used for get_stamenmap, get_map
  
#DOWNLOAD STATES AND MINOR BASIN LAYERS DIRECT FROM GITHUB
print(paste("DOWNLOADING STATES AND MINOR BASIN LAYERS DIRECT FROM GITHUB...",sep=""))  

if(!exists("STATES")) {  
  STATES_item <- "https://raw.githubusercontent.com/HARPgroup/HARParchive/master/GIS_layers/STATES.tsv"
  STATES_filename <- "STATES.tsv"
  #file downloaded into local directory, as long as file exists it will not be re-downloaded
  if (file.exists(paste(localpath, STATES_filename, sep = '/')) == FALSE) {
    print(paste("__DOWNLOADING STATES LAYER", sep = ''))
    destfile <- paste(localpath,STATES_filename,sep="\\")
    download.file(STATES_item, destfile = destfile, method = "libcurl")
  } else {
    print(paste("__STATES LAYER PREVIOUSLY DOWNLOADED", sep = ''))
  }
  #read csv from local directory
  print(paste("__LOADING STATES LAYER...", sep = ''))
  STATES <- read.csv(file=paste(localpath,STATES_filename,sep="\\"), header=TRUE, sep="\t")
  print(paste("__COMPLETE!", sep = ''))
}  
  
if(!exists("MinorBasins.csv")) {  
  MinorBasins.csv_item <- "https://raw.githubusercontent.com/HARPgroup/HARParchive/master/GIS_layers/MinorBasins.csv"
  MinorBasins.csv_filename <- "MinorBasins.csv.tsv"
  #file downloaded into local directory, as long as file exists it will not be re-downloaded
  if (file.exists(paste(localpath, MinorBasins.csv_filename, sep = '/')) == FALSE) {
    print(paste("__DOWNLOADING MinorBasins.csv LAYER", sep = ''))
    destfile <- paste(localpath,MinorBasins.csv_filename,sep="\\")
    download.file(MinorBasins.csv_item, destfile = destfile, method = "libcurl")
  } else {
    print(paste("__MinorBasins.csv LAYER PREVIOUSLY DOWNLOADED", sep = ''))
  }
  #read csv from local directory
  print(paste("__LOADING MinorBasins.csv LAYER...", sep = ''))
  MinorBasins.csv <- read.csv(file=paste(localpath,MinorBasins.csv_filename,sep="\\"), header=TRUE, sep=",")
  print(paste("__COMPLETE!", sep = ''))  
}
    
#DOWNLOAD RSEG LAYER DIRECT FROM VAHYDRO
if(!exists("RSeg.csv")) {  
print(paste("DOWNLOADING RSEG LAYER DIRECT FROM VAHYDRO...",sep=""))
  RSeg.csv_item <- paste(site,"/vahydro_riversegs_export",sep="")
  RSeg.csv_filename <- "RSeg.csv"
  #file downloaded into local directory, as long as file exists it will not be re-downloaded
  if (file.exists(paste(localpath, RSeg.csv_filename, sep = '/')) == FALSE) {
    print(paste("__DOWNLOADING RSeg.csv LAYER", sep = ''))
    destfile <- paste(localpath,RSeg.csv_filename,sep="\\")
    download.file(RSeg.csv_item, destfile = destfile, method = "libcurl")
  } else {
    print(paste("__RSeg.csv LAYER PREVIOUSLY DOWNLOADED", sep = ''))
  }
  #read csv from local directory
  print(paste("__LOADING RSeg.csv LAYER...", sep = ''))
  RSeg.csv <- read.csv(file=paste(localpath,RSeg.csv_filename,sep="\\"), header=TRUE, sep=",")
  print(paste("__COMPLETE!", sep = ''))  
}
  
#DOWNLOAD MAJORRIVERS LAYER DIRECT FROM GITHUB
if(!exists("MajorRivers.csv")) {  
print(paste("DOWNLOADING MAJORRIVERS LAYER DIRECT FROM GITHUB...",sep=""))
  MajorRivers.csv_item <- "https://raw.githubusercontent.com/HARPgroup/HARParchive/master/GIS_layers/MajorRivers.csv"
  MajorRivers.csv_filename <- "MajorRivers.csv"
  #file downloaded into local directory, as long as file exists it will not be re-downloaded
  if (file.exists(paste(localpath, MajorRivers.csv_filename, sep = '/')) == FALSE) {
    print(paste("__DOWNLOADING MajorRivers.csv LAYER", sep = ''))
    destfile <- paste(localpath,MajorRivers.csv_filename,sep="\\")
    download.file(MajorRivers.csv_item, destfile = destfile, method = "libcurl")
  } else {
    print(paste("__MajorRivers.csv LAYER PREVIOUSLY DOWNLOADED", sep = ''))
  }
  #read csv from local directory
  print(paste("__LOADING MajorRivers.csv LAYER...", sep = ''))
  MajorRivers.csv <- read.csv(file=paste(localpath,MajorRivers.csv_filename,sep="\\"), header=TRUE, sep=",")
  print(paste("__COMPLETE!", sep = ''))  
}
  
#DOWNLOAD FIPS LAYER DIRECT FROM VAHYDRO
if(!exists("fips.csv")) {  
print(paste("DOWNLOADING FIPS LAYER DIRECT FROM VAHYDRO...",sep=""))
  fips.csv_item <- paste(site,"/usafips_centroid_export",sep="")
  fips.csv_filename <- "fips.csv"
  #file downloaded into local directory, as long as file exists it will not be re-downloaded
  if (file.exists(paste(localpath, fips.csv_filename, sep = '/')) == FALSE) {
    print(paste("__DOWNLOADING fips.csv LAYER", sep = ''))
    destfile <- paste(localpath,fips.csv_filename,sep="\\")
    download.file(fips.csv_item, destfile = destfile, method = "libcurl")
  } else {
    print(paste("__fips.csv LAYER PREVIOUSLY DOWNLOADED", sep = ''))
  }
  #read csv from local directory
  print(paste("__LOADING fips.csv LAYER...", sep = ''))
  fips.csv <- read.csv(file=paste(localpath,fips.csv_filename,sep="\\"), header=TRUE, sep=",")
  print(paste("__COMPLETE!", sep = '')) 
}
  
#DOWNLOAD RESERVOIR LAYER FROM LOCAL REPO
if(!exists("WBDF")) { 
print(paste("__LOADING RESERVOIR LAYER FROM LOCAL REPO...",sep=""))
WBDF <- read.table(file=paste(github_location,"HARPArchive/GIS_layers","WBDF.csv",sep="/"), header=TRUE, sep=",")
}

#LOAD ANY ADDITIONL MAPPING FUNCTIONS
source(paste(vahydro_location,"R/wsp/wsp2020/FoundationDataset/geo_summaries/mb.extent.R",sep = '/'))

layers <- list("STATES" = STATES, 
                "MinorBasins.csv" = MinorBasins.csv,
                "RSeg.csv" = RSeg.csv,
                "MajorRivers.csv" = MajorRivers.csv,
                "fips.csv" = fips.csv,
                "WBDF" = WBDF
                )
return(layers)
}

###################################################################################################### 
# GENERATE MAP
######################################################################################################
CIA_maps <- function(cia_data,map_layers){

    # LOAD MAP LAYERS FROM THE map_layers LIST 
    STATES <- map_layers[[which(names(map_layers) == "STATES")]]
    MinorBasins.csv <- map_layers[[which(names(map_layers) == "MinorBasins.csv")]]
    RSeg.csv <- map_layers[[which(names(map_layers) == "RSeg.csv")]]
    MajorRivers.csv <- map_layers[[which(names(map_layers) == "MajorRivers.csv")]]
    fips.csv <- map_layers[[which(names(map_layers) == "fips.csv")]]
    WBDF <- map_layers[[which(names(map_layers) == "WBDF")]]
    
    # SELECT MINOR BASIN CODE
    mb_code <- sqldf(paste('SELECT substr(riv_seg, 1, 2) AS code
                        FROM cia_data
                        LIMIT 1'
                          )
                    )
    minorbasin <- as.character(mb_code)
    print(paste("PROCESSING: ",minorbasin,sep=""))
    
    minorbasin.df <- data.frame(minorbasin)
    mb_name <-sqldf(paste('SELECT 
                          CASE
                            WHEN minorbasin == "BS" Then "Big Sandy"
                            WHEN minorbasin == "EL" Then "Eastern Shore"
                            WHEN minorbasin == "ES" Then "Eastern Shore"
                            WHEN minorbasin == "JA" Then "James Appomattox"
                            WHEN minorbasin == "JB" Then "Lower James"
                            WHEN minorbasin == "JL" Then "Middle James"    
                            WHEN minorbasin == "JU" Then "Upper James"
                            WHEN minorbasin == "MN" Then "Meherrin Nottoway"
                            WHEN minorbasin == "NR" Then "New River" 
                            WHEN minorbasin == "PL" Then "Lower Potomac"     
                            WHEN minorbasin == "PM" Then "Middle Potomac"
                            WHEN minorbasin == "PS" Then "Potomac Shenandoah" 
                            WHEN minorbasin == "PU" Then "Upper Potomac"  
                            WHEN minorbasin == "RL" Then "Lower Rappahannock"
                            WHEN minorbasin == "RU" Then "Upper Rappahannock"
                            WHEN minorbasin == "OR" Then "Roanoke" 
                            WHEN minorbasin == "OD" Then "Roanoke Dan"     
                            WHEN minorbasin == "TU" Then "Upper Tennessee"
                            WHEN minorbasin == "YL" Then "Lower York" 
                            WHEN minorbasin == "YM" Then "York Mattaponi"
                            WHEN minorbasin == "YP" Then "York Pamunkey"
                            ELSE minorbasin
                          END AS name
                        FROM "minorbasin.df" ',sep=""))
    mb_name <- mb_name$name                   
    
    ######################################################################################################
    # DETERMINE MAP EXTENT FROM MINOR BASIN CENTROID
    extent <- mb.extent(minorbasin,MinorBasins.csv)
    ######################################################################################################
    
    # BOUNDING BOX
    bb=readWKT(paste0("POLYGON((",extent$x[1]," ",extent$y[1],",",extent$x[2]," ",extent$y[1],",",extent$x[2]," ",extent$y[2],",",extent$x[1]," ",extent$y[2],",",extent$x[1]," ",extent$y[1],"))",sep=""))
    bbProjected <- SpatialPolygonsDataFrame(bb,data.frame("id"), match.ID = FALSE)
    bbProjected@data$id <- rownames(bbProjected@data)
    bbPoints <- fortify(bbProjected, region = "id")
    bbDF <- merge(bbPoints, bbProjected@data, by = "id")
    
    ######################################################################################################
    ### PROCESS STATES LAYER  ############################################################################
    ######################################################################################################

    # NEED TO REMOVE INDIANA DUE TO FAULTY GEOM
    STATES <- sqldf(paste('SELECT * FROM STATES WHERE state != "IN"',sep=""))
    
    STATES$id <- as.numeric(rownames(STATES))
    state.list <- list()
    
    for (i in 1:length(STATES$state)) {
      state_geom <- readWKT(STATES$geom[i])
      state_geom_clip <- gIntersection(bb, state_geom)
      
      if (is.null(state_geom_clip) == TRUE) {
        # print("STATE OUT OF MINOR BASIN EXTENT - SKIPPING") 
        next
      }
      
      stateProjected <- SpatialPolygonsDataFrame(state_geom_clip, data.frame('id'), match.ID = TRUE)
      stateProjected@data$id <- as.character(i)
      state.list[[i]] <- stateProjected
    }
    
    length(state.list)
    #REMOVE THOSE STATES THAT WERE SKIPPED ABOVE (OUT OF MINOR BASIN EXTENT)
    state.list <- state.list[which(!sapply(state.list, is.null))]
    length(state.list)
    
    state <- do.call('rbind', state.list)
    state@data <- merge(state@data, STATES, by = 'id')
    state@data <- state@data[,-c(2:3)]
    state.df <- fortify(state, region = 'id')
    state.df <- merge(state.df, state@data, by = 'id')
    
    ######################################################################################################
    ### PROCESS Minor Basin LAYER  #######################################################################
    ######################################################################################################
    mb_data <- MinorBasins.csv
    
    MB_df_sql <- paste('SELECT *
              FROM mb_data 
              WHERE code = "',minorbasin,'"'
                       ,sep="")
    
    if (minorbasin == "ES") {
      print("COMBINING 2 EASTERN SHORE MINOR BASINS")
      MB_df_sql <- paste('SELECT * FROM mb_data WHERE code = "ES" OR code = "EL"' ,sep="")
    }
    
    mb_data <- sqldf(MB_df_sql)
    
    mb_data$id <- as.character(row_number(mb_data$code))
    MB.list <- list()
    
    
    for (z in 1:length(mb_data$code)) {
      MB_geom <- readWKT(mb_data$geom[z])
      MB_geom_clip <- gIntersection(bb, MB_geom)
      MBProjected <- SpatialPolygonsDataFrame(MB_geom_clip, data.frame('id'), match.ID = TRUE)
      MBProjected@data$id <- as.character(z)
      MB.list[[z]] <- MBProjected
    }
    MB <- do.call('rbind', MB.list)
    MB@data <- merge(MB@data, mb_data, by = 'id')
    MB@data <- MB@data[,-c(2:3)]
    MB.df <- fortify(MB, region = 'id')
    MB.df <- merge(MB.df, MB@data, by = 'id')
    
    ######################################################################################################
    ### PROCESS FIPS LAYER  #############################################################################
    ######################################################################################################
    fips_layer <- fips.csv
    fips_layer$id <- fips_layer$fips_hydroid
    fips.list <- list()
    
    for (f in 1:length(fips_layer$fips_hydroid)) {
      fips_geom <- readWKT(fips_layer$fips_centroid[f])
      fips_geom_clip <- gIntersection(MB_geom, fips_geom) #SHOW ONLY FIPS NAMES WITHIN MINOR BASIN
      
      if (is.null(fips_geom_clip) == TRUE) {
        # print("FIPS OUT OF MINOR BASIN EXTENT - SKIPPING") 
        next
      }
      
      fipsProjected <- SpatialPointsDataFrame(fips_geom_clip, data.frame('id'), match.ID = TRUE)
      fipsProjected@data$id <- as.character(fips_layer[f,]$id)
      fips.list[[f]] <- fipsProjected
    }
    
    length(fips.list)
    #REMOVE THOSE FIPS THAT WERE SKIPPED ABOVE (OUT OF MINOR BASIN EXTENT)
    fips.list <- fips.list[which(!sapply(fips.list, is.null))]
    length(fips.list)
    
    if (length(fips.list) != 0) {
      #  print("NO FIPS GEOMS WITHIN MINOR BASIN EXTENT - SKIPPING")
      fips <- do.call('rbind', fips.list)
      fips@data <- merge(fips@data, fips_layer, by = 'id')
      fips@data <- fips@data[,-c(2:3)]
      fips.df <- data.frame(fips)
    } else {
      print("NO FIPS GEOMS WITHIN MINOR BASIN EXTENT")
      
      fips.df <- data.frame(id=c(1,2),
                            fips_latitude =c(1,2), 
                            fips_longitude =c(1,2),
                            fips_name = c(1,2),
                            stringsAsFactors=FALSE) 
      
    }
    
    ######################################################################################################
    ### PROCESS MajorRivers.csv LAYER  ###################################################################
    ######################################################################################################
    rivs_layer <- MajorRivers.csv
    
    riv.centroid.df <-  data.frame(feature=rivs_layer$feature,
                                   GNIS_NAME=rivs_layer$GNIS_NAME,
                                   centroid_longitude="",
                                   centroid_latitude="",
                                   stringsAsFactors=FALSE) 
    
    rivs_layer$id <- rivs_layer$feature
    rivs.list <- list()
    
    for (r in 1:length(rivs_layer$feature)) {
      riv_geom <- readWKT(rivs_layer$geom[r])
      
      
      riv_geom_centroid <- gCentroid(riv_geom,byid=TRUE)
      riv.centroid.df$centroid_longitude[r] <- riv_geom_centroid$x
      riv.centroid.df$centroid_latitude[r] <- riv_geom_centroid$y  
      
      
      # riv_geom_clip <- gIntersection(MB_geom, riv_geom)
      riv_geom_clip <- riv_geom
      
      if (is.null(riv_geom_clip) == TRUE) {
        # print("OUT OF MINOR BASIN EXTENT - SKIPPING") 
        next
      }
      
      rivProjected <- SpatialLinesDataFrame(riv_geom_clip, data.frame('id'), match.ID = TRUE)
      rivProjected@data$id <-  as.character(rivs_layer[r,]$id)
      rivs.list[[r]] <- rivProjected
    }
    
    length(rivs.list)
    #REMOVE THOSE rivs_layer THAT WERE SKIPPED ABOVE (OUT OF MINOR BASIN EXTENT)
    rivs.list <- rivs.list[which(!sapply(rivs.list, is.null))]
    length(rivs.list)
    
    rivs <- do.call('rbind', rivs.list)
    rivs@data <- merge(rivs@data, rivs_layer, by = 'id')
    rivs.df <- rivs

    ######################################################################################################
    ### PROCESS RSegs
    ######################################################################################################
    RSeg_data <- paste('SELECT *
                  FROM "RSeg.csv" AS a
                  LEFT OUTER JOIN cia_data AS b
                  ON (a.hydrocode = b.hydrocode)
                  WHERE a.hydrocode LIKE "%wshed_',minorbasin,'%"
		  ORDER BY hydroid ASC
		  ',sep = '')  
    RSeg_data <- sqldf(RSeg_data)

    # NEED TO REMOVE SECOND "hydrocode" COLUMN TO PREVENT ERROR LATER ON
    RSeg_data <- RSeg_data[,-max(which(grepl("hydrocode",colnames(RSeg_data),fixed = TRUE)==TRUE))]
    
    # REMOVE ANY WITH EMPTY GEOMETRY FIELD (NEEDED PRIOR TO GEOPROCESSING)
    RSeg_valid_geoms <- paste("SELECT *
                  FROM RSeg_data
                  WHERE geom != ''")  
    RSeg_data <- sqldf(RSeg_valid_geoms)

    ######################################################################################################
    ### GENERATE MAPS  ###################################################################################
    ######################################################################################################
    print(paste("RETRIEVING BASEMAP:",sep=""))
    
    tile_layer <- get_map(
      location = c(left = extent$x[1],
                   bottom = extent$y[1],
                   right = extent$x[2],
                   top = extent$y[2]),
      source = "osm", zoom = 9, maptype = "satellite" #good
    )
    
    base_layer <- ggmap(tile_layer)

    base_map <- base_layer + 
    geom_polygon(data = MB.df,aes(x = long, y = lat, group = group), color="black", fill = NA,lwd=0.5)
    
    base_scale <-  ggsn::scalebar(data = bbDF, location = 'bottomleft', dist = 25, dist_unit = 'mi', 
                                  transform = TRUE, model = 'WGS84',st.bottom=FALSE, 
                                  st.size = 3, st.dist = 0.03,
                                  anchor = c(
                                    x = (((extent$x[2] - extent$x[1])/2)+extent$x[1])-0.45,
                                    y = extent$y[1]+(extent$y[1])*0.001
                                  ))
    
    base_theme <- theme(legend.title = element_text(size = 7.4),
                        #legend.position=c(1.137, .4), #USE TO PLACE LEGEND TO THE RIGHT OF MAP
                        legend.position=c(1.2, .65),
                        plot.margin = unit(c(0.5,-0.2,0.25,-3), "cm"),
                        plot.title = element_text(size=12),
                        plot.subtitle = element_text(size=10),
                        axis.title.x=element_blank(),
                        axis.text.x=element_blank(),
                        axis.ticks.x=element_blank(),
                        axis.title.y=element_blank(),
                        axis.text.y=element_blank(),
                        axis.ticks.y=element_blank(),
                        panel.grid.major = element_blank(), 
                        panel.grid.minor = element_blank(),
                        panel.background = element_blank(),
                        panel.border = element_blank())
    
    deqlogo <- draw_image(paste(github_location,'/HARParchive/GIS_layers/HiResDEQLogo.tif',sep=''),scale = 0.175, height = 1,  x = -.384, y = 0.32)
    
    ######################################################################################################

    #SUBSET THE CIA RSEGS OF INTEREST
    RSeg_subset <- paste('SELECT *
                  FROM RSeg_data
                  WHERE seglist IS NOT NULL
		              ',sep = '')  
    RSeg_subset <- sqldf(RSeg_subset)
    RSeg_subset_centroids <- wkt_centroid(RSeg_subset$geom)
    RSeg_subset <- cbind(RSeg_subset,RSeg_subset_centroids)
    
    rseg_subset  <- st_as_sf(RSeg_subset, wkt = 'geom')

    ######################################################################################################
    rseg_base <- st_as_sf(RSeg_data, wkt = 'geom')
    source_current <- base_map + geom_sf(data = rseg_base,aes(geometry = geom),color = "black", fill = "gray55", inherit.aes = FALSE, show.legend = FALSE)+
                                 geom_sf(data = rseg_subset,aes(geometry = geom),color = "black", fill = "lightgreen", inherit.aes = FALSE, show.legend = FALSE)
    
      map <- ggdraw(source_current +
                      geom_polygon(data = MB.df,aes(x = long, y = lat, group = group), color="black", fill = NA,lwd=0.7) +
                      ggtitle(paste(" ",sep = '')) +
                      labs(subtitle = paste("Minor Basin: ",mb_name,sep='')) +
                      
                      #ADD STATE BORDER LAYER ON TOP
                      geom_path(data = state.df,aes(x = long, y = lat, group = group), color="gray20",lwd=0.5) +
                      
                      #ADD RIVERS LAYER ON TOP
                      geom_path(data = rivs.df, aes(x = long, y = lat, group = group), color="dodgerblue3",lwd=0.4) +
                      
                      # ADD WATERBODIES
                      geom_point(data = WBDF, aes(x = long, y = lat), color="dodgerblue3", size=0.09)+
                    
                      #ADD BORDER 
                      #geom_polygon(data = bbDF,aes(x = long, y = lat, group = group), color="black", fill = NA,lwd=0.5)+
                      
                      #ADD RIVER LABELS
                      geom_text_repel(data = riv.centroid.df, aes(x = as.numeric(centroid_longitude), y = as.numeric(centroid_latitude), group = 1, label = GNIS_NAME),size = 2, color = "dodgerblue3")+
  
                      #ADD FIPS POINTS
                      geom_point(data = fips.df, aes(x = fips_longitude, y = fips_latitude, group = 1),size =1, shape = 20, fill = "black")+
                      
                      #ADD FIPS LABELS
                      geom_text_repel(data = fips.df, aes(x = fips_longitude, y = fips_latitude, group = 1, label = fips_name),size = 2)+
                     
                      #ADD RSEG SUBSET LABELS
                      #geom_text_repel(data = rseg_subset, aes(x = lng, y = lat, group = 1, label = propname),size = 2)+
                      ##geom_label_repel(data = rseg_subset, aes(x = lng, y = lat, group = 1, label = propname),size = 2,fill="white",box.padding =1)+
                      
                      #OPTION WITH LABELS SPACED OUT WITH LINE TO RSEG CENTROID
                      geom_label_repel(data = rseg_subset, aes(x = lng, y = lat, group = 1, label = seglist),size = 2,fill="white",box.padding =1)+
                      #OPTION WITH LABELS RIGHT AT RSEG CENTROID
                      #geom_label(data = rseg_subset, aes(x = lng, y = lat, group = 1, label = seglist),size = 2,fill="white")+ 
                      
                      #ADD NORTH BAR
                      north(bbDF, location = 'topright', symbol = 3, scale=0.12) +
                      base_scale +
                      base_theme) +
                      deqlogo

      return(map)

} # CLOSE FUNCTION CIA_maps()


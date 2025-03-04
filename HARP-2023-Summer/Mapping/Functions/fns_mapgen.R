##Establishes a collection of the functions utilized for map plotting. 
##fn_mapgen() is the final call used in WSP_Regional_Summaries.Rmd, and it calls these modular functions within it.
#Load required libraries
#install.packages("extrafont")
library(extrafont)
loadfonts(device = "win")
library(ggplot2)
library(geosphere)
library(arcpullr)
library(ggnewscale)
library(ggrepel)
library(mgsub)
library(sf)
library(ggspatial)

fn_catchMapErrors <- function(map_layer, layer_description="blank", map=FALSE){ #making the root of an error clear
  #if layer name needs further description for the error message, input it into 'layer_description' as a character string. 
  #otherwise, the layer's variable name will be used:
  if(layer_description=="blank"){ layer_description <- deparse(substitute(map_layer)) }
  if(is.logical(map)){ #aka no map to add to yet
    test <- try(ggplot2::ggplot() + map_layer, silent=TRUE)
  }else{
    test <- try(map + map_layer, silent=TRUE)
  }
  errors <- grep("Error", test, ignore.case=TRUE)
  if(length(errors)!=0){#there's an issue with the layer and it will not be added to the map
    if(is.logical(map)){ #aka no basemap yet
      stop(paste0("Cannot generate maps due to a conflict in the basemap layer. See fns_mapgen.R for more."), call. = FALSE)
    } else{
      if("errors" %in% names(map)){
        map$errors <- append(map$errors, layer_description)
      } else{map$errors <- layer_description}
    }
  } else{#no issue with the layer & it will be plotted
      if(is.logical(map)){ #aka no map to add to yet
        map <- ggplot2::ggplot() + map_layer
      }else{
        map <- map + map_layer
      }
    }
  return(map)
}

fn_labelsAndFilter <- function(labelset=maplabs, bbox_coord_df, nhd, roads, map_style_set, bbox_sf, crs_default, rsegs){ 
  #combines all text labels into 1 df and associates them w/ aesthetics from mapstyle_config.R
  #filters data to control the amount of map detail based on extent of the map
  for(i in 1:length(labelset)){ #Combine all text labels into one df:
    if(i==1){ labelset$all <- labelset[[i]] }
    if(i!=1){ labelset$all <- rbind(labelset$all, labelset[[i]]) }
    if(i==length(labelset)-1){ labelset <- labelset$all }
  }
  
  text_aes <- map_style_set$text
  labelset <- sqldf( #join text aesthetics with sqldf 
    "SELECT text_aes.*, labelset.label, labelset.lat, labelset.lng
    FROM text_aes 
    LEFT OUTER JOIN labelset
      on (labelset.class = text_aes.class)"
  )
  class(labelset$bg.r) = "numeric"
  
  #Find distance of diagonal of bbox in miles -- for filtering what will be plotted
  #distance used instead of 'extent' because DEQ vocab has extent synonymous w bbox  
  distance <-  geosphere::distHaversine(bbox_coord_df) / 1609.34 #distHaversine() defaults to meters, so convert to miles
  
  roads_plot <- roads
  if (distance > 300) {
    nhd_plot<- nhd$flowline[nhd$flowline$StreamOrde!=1 & nhd$flowline$StreamOrde!=2 & nhd$flowline$StreamOrde!=3,]
    roads_plot <- roads[roads$RTTYP=="I",]
    labels_plot <- labelset[labelset$class=="county" | labelset$class=="majorRiver" |
                            labelset$class=="I"| labelset$class=="city" | labelset$class!="waterbody_lg",]
    textsize <- c(4,4,5,6,5,0) #c(I/S/U , town/majC/LakePond/str , majR , county ,   facility num , segs$basin_sf lwd)
  } else if (distance > 130) {
    nhd_plot <- nhd$flowline[nhd$flowline$StreamOrde!=1 & nhd$flowline$StreamOrde!=2,]
    labels_plot <- labelset[labelset$class=="county" | labelset$class=="majorRiver" |
                            labelset$class=="I" | labelset$class=="city",]
    textsize <- c(5,5,6,11,5,1)
  } else if (distance > 70) {
    nhd_plot <- nhd$flowline[nhd$flowline$StreamOrde!=1,]
    labels_plot <- labelset[labelset$class!="waterbody_sm" & labelset$class!="waterbody_med" & labelset$class!= "smallTown",]
    textsize <- c(6,7,9,12,5,1.2)
    labelset$segsize <- as.numeric( gsub(1, 0, labelset$segsize) ) #no label "lollipop" for counties @ small distances
  } else {
    nhd_plot <- nhd$flowline
    labels_plot <- labelset[labelset$class!="waterbody_sm" & labelset$class!="waterbody_med",]
    textsize <- c(7,8,10,13,5,1.5)
    labelset$segsize <- as.numeric( gsub(1, 0, labelset$segsize) ) 
  }
  #crop data to extent:
  roads_plot <- sf::st_crop(roads_plot, bbox_sf)
  nhd_plot <- sf::st_transform(nhd_plot, crs_default) #use instead of st_crs (st_crs doesn't transform--only changes metadata)
  nhd_plot <- sf::st_crop(nhd_plot, bbox_sf)
  
  ## Removing NAs, they cause errors and cannot be plotted
  labels_plot <- labels_plot[!is.na(labels_plot$lat),]
  labelPlot <- st_as_sf(labels_plot,coords = c("lng","lat"),remove = FALSE)
  labelPlot <- st_set_crs(labelPlot,crs_default)
  #labelPlot <- sf::st_transform(labelPlot, crs_default)
  labels_plot <- sf::st_crop(labelPlot, bbox_sf)
  #Crop nhd_plot to the appropriate bbox and reproject from NAD83 to 4326:
  nhdplot_new <- st_transform(nhd_plot,4326)
  nhd_plot <- st_crop(nhdplot_new,bbox_sf)
  
  #only keep road labels outside of origin:
  roadlabs_only <- labels_plot[labels_plot$class %in% c("I","S","U"),]
  # roadlabs_in_origin <- sf::st_intersects(roadlabs_only, sf::st_union(rsegs), 
  #                            sparse=FALSE) #determines which road labels intersect the highlighted map area (the origin)
  roadlabs_in_origin <- sf::st_is_within_distance(roadlabs_only, sf::st_union(rsegs), 
                            dist=units::set_units(1.5, "mi"), sparse=FALSE) #determines which road labels are within 1.5mi of the highlighted map area (the origin)
  for(i in 1:nrow(roadlabs_only)){
    if(roadlabs_in_origin[i]==FALSE && nrow(roadlabs_only) > 0){ #aka road label coordinate is outside of the origin
      if(!exists("roadlabs_to_keep")){
        roadlabs_to_keep <- roadlabs_only[i,]
      }else{
        roadlabs_to_keep <- rbind(roadlabs_to_keep, roadlabs_only[i,])
      }
    }
  }
  if(exists("roadlabs_to_keep")){
    labels_plot <- rbind(roadlabs_to_keep, 
                          labels_plot[!labels_plot$class %in% c("I","S","U"),] #non-roads
                          )
  }
  #remove repeated road labels:
  labels_plot <- labels_plot[!duplicated(labels_plot$label,
                                 incomparables= !labels_plot$class %in% c("I","S","U") #don't remove duplicated labels that aren't of road classes
                                 ), ]

  #save:
  assign('labels_plot', labels_plot, envir = globalenv())
  assign('nhd_plot', nhd_plot, envir = globalenv())
  assign('roads_plot', roads_plot, envir = globalenv())
  assign('textsize', textsize, envir = globalenv())
}

fn_basemap <- function(map_server, base_layer, bbox_coord_df=bbox_coords){ #generates a basemap
  map_url <- paste(map_server,base_layer,sep ="/")
  mapdata <- arcpullr::get_spatial_layer(map_url)
  mapdata <- st_make_valid(mapdata)
  mapdata <- sf::st_crop(mapdata, c(xmin= min(bbox_coord_df$lng), ymin = min(bbox_coord_df$lat), 
                                xmax = max(bbox_coord_df$lng), ymax = max(bbox_coord_df$lat))) #crop to our extent 
  basemap <- ggplot2::geom_sf(data = mapdata)
  return(basemap)
}

fn_shadow <- function(rsegs, bbox_sfc, map_style_set){ #generates a "reverse fill" layer that will shadow the area outside the basins of interest
  basin_union <- sf::st_union(rsegs)
  nonbasin <- sf::st_difference(bbox_sfc, basin_union) #method of erasing
  sf::st_crs(nonbasin) <- crs_default
  shadow <- ggplot2::geom_sf(data = nonbasin, inherit.aes=FALSE, color=NA, fill = map_style_set$color$sf["shadow",], lwd=1 )
  return(shadow)
}

fn_mp_bubbles <- function(mp_layer, metric_unit, featr_type, map_style_set){
  #for binned legend 
  breaks <- seq(1:7)
  lims <- c(min(breaks),max(breaks)) #limits based on range of breaks
  if (metric_unit == "mgd") { 
    labs = c(0.5, 1.0, 2, 10, 25, 100, 1000)
  } else if (metric_unit == "mgy") {
    labs = c(1, 5, 10, 50, 250, 1000, 10000)
  } else {
    labs = c(0.5, 1.0, 2, 10, 25, 100, 1000) } #default to mgd if unit is neither mgd or mgy
  
  #remove bubbles for MPs with no metric value -- stored with bin = X
  mp_layer <- mp_layer[!mp_layer$bin == "X" , ]
  class(mp_layer$bin) <- "numeric" #make sure bin column is type numeric for sizing data points
  #text for number labels:
  num_labels <- ggplot2::geom_text(data = mp_layer,
                          aes(lng, lat, label=NUM, fontface="bold"),
                          color="black", size=textsize[5], check_overlap=TRUE)
  #colored bubbles:
  if (featr_type == "facility") {
    map_layer <- ggplot2::geom_point(data = mp_layer, aes(x = lng, y = lat, 
                                 size = bin), color = map_style_set$color$metrics["Surface Water",],
                                 shape = 19, alpha = 0.6)
    scale_size <- ggplot2::scale_size_binned(range = c(2,20), 
                                    breaks = breaks, 
                                    labels = labs,
                                    limits = lims,
                                    name = str_wrap(legend_title[1], width = 20)
    )
    bubbles <- list(map_layer, scale_size, num_labels)
  }
  if (featr_type == "source") {
    map_layer <- ggplot2::geom_point(data = mp_layer, aes(x = lng, y = lat, 
                                                      color = Source_Type, 
                                                      size = bin),
                                                      shape = 19)
    
    scale_size <- ggplot2::scale_size_binned(range = c(2,20),
                                             breaks = breaks,
                                             labels = labs,
                                             limits = lims,
                                             name = legend_title[1],
                                             guide= guide_legend(override.aes=list(color = map_style_set$color$metrics["Groundwater",],
                                                                                   fill = map_style_set$color$metrics["Surface Water",],
                                                                                   #stroke = seq(3,7,length.out=length(breaks)),
                                                                                   shape=21 ))
    )
    scale_color <- ggplot2::scale_colour_manual(values= c(map_style_set$color$metrics[c("Surface Water", "Groundwater"),],'blue','red') ,
                                                breaks= c("Surface Water", "Groundwater"),
                                                labels= c("Surface Water", "Groundwater"),
                                                name= "Source Type",
                                                guide= guide_legend(override.aes=list(size=9))
    )
    
    bubbles <- list(map_layer, scale_size, scale_color, num_labels)
  }
  return(bubbles)
}

fn_borders <- function(rsegs, counties, regions, origin, bbox_sf, crs_default, textsize, map_style_set){ #merges all polygon polyg_borders into 1 df and associates them w/ aesthetics from mapstyle_config.R
  #merging polyg_borders into 1 df so they can be on the same legend
  polyg_borders <- data.frame(counties[,"name"] , bundle= rep("county", nrow(counties)) )
  names(polyg_borders) <- c("name", "geometry", "bundle")
  ## Only run if rsegs is passed into function (exclusion for GW maps)
  if (!is.null(rsegs)) {
    sf::st_geometry(rsegs) <- "geometry"
    sf::st_crs(rsegs) <- crs_default
    #rsegs <- sf::st_transform(rsegs, crs_default)
    polyg_borders <- rbind(polyg_borders, data.frame(rsegs[,c("name")], bundle = "watershed" )  )
  }
  
  if (origin_type=="region") {
    region_OI <- regions[regions$region==origin,] #region of interest
    sf::st_geometry(region_OI) <- "geometry"
    region_OI <- data.frame(name="region", bundle="region", geometry=region_OI[fn_geoCol(region_OI)] )
    polyg_borders <- rbind(polyg_borders, region_OI)
    polyg_borders <- sf::st_as_sf(polyg_borders)
    sf::st_crs(polyg_borders) <- crs_default
    #polyg_borders <- sf::st_transform(polyg_borders, crs_default)
  } else {polyg_borders <- sf::st_as_sf(polyg_borders)}
  sf::st_crs(polyg_borders) <- crs_default
  #polyg_borders <- sf::st_transform(polyg_borders, crs_default)
  polyg_borders <- polyg_borders[polyg_borders$bundle %in% c('region','county','watershed'), ]
  polyg_borders <- sf::st_crop(polyg_borders, bbox_sf)  
  
  #generate layer for mapping
  if (origin_type == "region") {
    map_layer <- ggplot2::geom_sf(data= polyg_borders, inherit.aes=FALSE, fill=NA,
                              ggplot2::aes(color= bundle,
                                  lwd= as.numeric(mgsub::mgsub(bundle, 
                                                      pattern=c("county","watershed","region"), 
                                                      replacement=c(2.5,textsize[6],4.5) )),
                                  linetype= bundle )
             )
    scale_color <- ggplot2::scale_colour_manual(values= c(map_style_set$color$sf[c("region","county","rsegs"),]) ,
                                       breaks= c("region","county","watershed"),
                                       labels= c("Region","County","Basin"),
                                       name= "Borders" )
    scale_linetype <- ggplot2::scale_linetype_manual(values= c("region"= 1,"county"= 1,"watershed"= 2), 
                            labels= c("Region","County","Basin"),
                            name= "Borders" )
    scale_linewidth <- ggplot2::scale_linewidth(range= range(c(2.5,textsize[6],4.5)), 
                          breaks= c(4.5,2.5,textsize[6]),
                          labels= c("Region","County","Basin"),
                          name= "Borders" )
  } else if (origin_type == "locality" & is.null(rsegs)) {
    ## Need a specific exception for a GW map for localities, since rsegs are not included. Unsure of why
    map_layer <- ggplot2::geom_sf(data = polyg_borders, inherit.aes=FALSE, fill=NA,
                                  ggplot2::aes(color = bundle,
                                               lwd = as.numeric(mgsub::mgsub(bundle, 
                                                                             pattern=c("county"), 
                                                                             replacement=c(2.5)) ),
                                               linetype = bundle )
    )
    scale_color <- ggplot2::scale_colour_manual(values = c(map_style_set$color$sf[c("county"),]) ,
                                                breaks = c("county"),
                                                labels = c("County"),
                                                name = "Borders" )
    scale_linetype <- ggplot2::scale_linetype_manual(values = c("county"= 1),
                                                     labels = c("County"),
                                                     name = "Borders" )
    scale_linewidth <- ggplot2::scale_linewidth(range = range(c(textsize[6],4.5)),
                                                breaks = c(2.5),
                                                labels = c("County"),
                                                name = "Borders" )
  } else {
    map_layer <- ggplot2::geom_sf(data = polyg_borders, inherit.aes=FALSE, fill=NA,
                              ggplot2::aes(color = bundle,
                                    lwd = as.numeric(mgsub::mgsub(bundle, 
                                                        pattern=c("county","watershed"), 
                                                        replacement=c(2.5,textsize[6])) ),
                                    linetype = bundle )
                        )
    scale_color <- ggplot2::scale_colour_manual(values = c(map_style_set$color$sf[c("county","rsegs"),]) ,
                                       breaks = c("county","watershed"),
                                       labels = c("County","Basin"),
                                       name = "Borders" )
    scale_linetype <- ggplot2::scale_linetype_manual(values= c("county"= 1,"watershed"= 2),
                          labels = c("County","Basin"),
                          name = "Borders" )
    scale_linewidth <- ggplot2::scale_linewidth(range= range(c(textsize[6],4.5)),
                          breaks = c(2.5,textsize[6]),
                          labels = c("County","Basin"),
                          name = "Borders" )
  }
  borders_layer <- list(ggnewscale::new_scale("color"), ggnewscale::new_scale("linetype"), ggnewscale::new_scale("linewidth"), 
                  map_layer[[1]], map_layer[[2]], scale_color, scale_linetype, scale_linewidth)
  return(borders_layer)
}

fn_polygonFill <- function(rsegs, map_style_set, mapnum, rseg_leg_title, rivmap_ramp){
  if (mapnum == 2) {
    #fill tidal rsegs 
    # rsegTidal <- subset(rsegs, riverseg %in% grep("0000", rsegs$riverseg, value=TRUE)) #PROBLEM w/ DF
    # rsegTidal <- sf::st_transform(rsegTidal, crs_default)
    # #fix tidal df
    # names(rsegTidal)[1:(ncol(rsegTidal)-6)] <- names(rsegTidal)[2:(ncol(rsegTidal)-5)]
    # rsegTidal[ncol(rsegTidal)-5] <- NULL
    # #create tidal map layer
    # tidal <- ggplot2::geom_sf(data = rsegTidal, inherit.aes = FALSE, aes(fill=bundle), alpha = 1)
    # tidal_scale_fill <- ggplot2::scale_fill_manual(values = map_style_set$color$sf["tidal",], #color set in config
    #                                                breaks = "watershed",
    #                                                labels = "Tidal/Unmodeled",
    #                                                name = NULL)
    # rseg_fill <- list(ggnewscale::new_scale("fill"), tidal, tidal_scale_fill)
    
    
    #rseg fill based on drought metric % difference for rivseg maps:
    rivseg_pct_vect <- rivmap_ramp[,"rivseg_pct_vect"]
    # place Tidal rsegs in a bin that will be listed after the bins with data values: 
    rsegs[rsegs$riverseg %in% grep("0000", rsegs$riverseg, value=TRUE) 
          #& is.na(rsegs$bin)
          , "bin"] <- 1+length(rivseg_pct_vect)
    # place rsegs with no data in an "unmodeled" bin at the end of the list:
    rsegs[is.na(rsegs$bin), "bin"] <- 2+length(rivseg_pct_vect)
    
    class(rsegs$bin) <- "numeric"
    metric_fill <- ggplot2::geom_sf(data = rsegs, inherit.aes = FALSE, 
                                    mapping = aes(fill = as.factor(bin)), 
                                    alpha = 0.7, show.legend = TRUE)
    metric_scale_fill <- ggplot2::scale_fill_manual(values = c(rivmap_ramp[,"rivmap_colors"], 
                                                               map_style_set$color$sf["tidal",], 
                                                               map_style_set$color$sf["unmodeled",]), #from config
                                           breaks = seq(1:(2+length(rivseg_pct_vect))), #append all these lists to incorporate bins for tidal segs & unmodeled data
                                           labels = c(rivmap_ramp[,"rivmap_labs"], "Tidal", "Unmodeled"),
                                           limits = as.factor(seq(1:(2+length(rivseg_pct_vect)))),
                                           name = str_wrap(rseg_leg_title, width = 20)
                                           )
    metric_legend <- ggplot2::theme(legend.spacing.y = unit(0.1, 'cm')) #adjust spacing of legend 
    rseg_fill <- list(ggnewscale::new_scale("fill"), metric_fill, metric_scale_fill,
                      ggplot2::guides(fill = guide_legend(byrow = TRUE))
                      #,ggnewscale::new_scale("fill"), tidal, tidal_scale_fill2
                      )
  }
  return(rseg_fill)
}

fn_nhdLines <- function(nhd_plot, map_style_set, nhd, bbox_sf){
  nhd_layer <- ggplot2::geom_sf(data = nhd_plot, 
                 inherit.aes=FALSE, color= map_style_set[["color"]][["sf"]]["nhd",], 
                 mapping=aes(lwd=nhd_plot$StreamOrde), #line thickness based on stream order
                 show.legend=FALSE)
  scale_linewidth <- ggplot2::scale_linewidth(range= c(0.4,2), guide = FALSE) 
  
  ## Cropping the nhd water bodies to the bbox
  nhd_water_shp <- rbind(nhd$off_network_wtbd, nhd$network_wtbd)
  wtbd_layer <- ggplot2::geom_sf(data = nhd_water_shp,  
                       inherit.aes=FALSE, fill= map_style_set[["color"]][["sf"]]["nhd",], size=1)
  nhd_map <- list(ggnewscale::new_scale("color"), ggnewscale::new_scale("linetype"), ggnewscale::new_scale("linewidth"), 
              nhd_layer[[1]], nhd_layer[[2]], scale_linewidth, wtbd_layer[[1]], wtbd_layer[[2]])
  return(nhd_map)
}

fn_roadsAndCityPoints <- function(roads_plot, map_style_set, labels_plot, mp_layer){
  rd_lines <- ggplot2::geom_sf(data = roads_plot, inherit.aes=FALSE, color= map_style_set[["color"]][["sf"]]["roads",], fill=NA, lwd=1, linetype="twodash")
  city_dots <- ggplot2::geom_point(data = labels_plot[labels_plot$class=="city"|labels_plot$class=="town",], 
                          aes(x=lng, y=lat), color= map_style_set[["color"]][["sf"]]["citypts",], size=2)
  #mp labels placeholder to have other labels repel:
  mp_placeholder <- ggplot2::geom_text(data = mp_layer, aes(lng, lat, label=NUM),colour=NA, size=textsize[4],check_overlap=TRUE)
  rd_bubbles <- ggrepel::geom_label_repel(data = labels_plot[labels_plot$class %in% c("I","S","U"), ],
                   aes(x=lng, y=lat, label=label,
                       fontface=fontface, family=fontfam,
                       color=as.factor(colcode),
                       fill=fillcode),
                   show.legend=NA,
                   size=textsize[1],
                   label.r=0.6, label.size=0.12,
                   max.overlaps=4
                   )
  scale_colour <- ggplot2::scale_colour_manual(values=map_style_set[["color"]][["text"]][,"color"], breaks=c(1,2,3),
                        labels=c("Interstate","State Route", "US Hwy"), name="Roads")
  scale_fill <- ggplot2::scale_fill_manual(values=map_style_set[["color"]][["fill"]][,"color"], breaks=c(1,2,3),
                      labels=c("Interstate","State Route", "US Hwy"), name="Roads")
  roadsNcitydots <- list(
    ggnewscale::new_scale("color"), ggnewscale::new_scale("linetype"), ggnewscale::new_scale("linewidth"),
                         rd_lines, ggnewscale::new_scale("color"), ggnewscale::new_scale("size"),
                         city_dots, mp_placeholder
                         ,
                         ggnewscale::new_scale("color"), ggnewscale::new_scale("fill"),
                         rd_bubbles, scale_colour, scale_fill
                         )
  return(roadsNcitydots)
}
  
fn_textRepel <- function(rsegs, labels_plot, textsize, map_style_set, bbox_coords){
  textcol <- map_style_set$color$text$color
   # All Other Text Labels:
  map_layer <- ggrepel::geom_text_repel(data = labels_plot[!(labels_plot$class == "I" | 
                                                labels_plot$class == "S" | 
                                                labels_plot$class == "U"), 
                                              ], #labels other than roads
                    aes(x=lng, y=lat, label=label,
                        fontface=fontface, family=fontfam, angle=angle,
                        segment.color=segcol, segment.size=segsize,
                        bg.color="white", bg.r=bg.r,
                        size=sizecode, 
                        color=as.factor(colcode),
                    ), 
                    show.legend=FALSE,
                    force=40, direction="both",
                    min.segment.length=0.5,
                    #max.overlaps=getOption("ggrepel.max.overlaps",20)
        )
  scale_size <- ggplot2::scale_size(range= range(textsize[2:4]), breaks= textsize[2:4]) 
  scale_color <- ggplot2::scale_colour_manual(values=textcol, breaks=seq(1,length(textcol)), guide="none")
  scale_x_cont <- ggplot2::scale_x_continuous(limits = bbox_coords$lng, expand = c(0, 0))
  scale_y_cont <- ggplot2::scale_y_continuous(limits = bbox_coords$lat, expand = c(0, 0))   
  
  ## Carving an exeption for GW maps, which dont use rsegs
  if (!is.null(rsegs)) {
    # Basin Labels (by riverseg ID):
    basins <- ggplot2::geom_text(data=rsegs, aes(x=lng, y=lat, label=riverseg),color="black",size=textsize[5],check_overlap=TRUE)
    
    textRepel <- list(basins, ggnewscale::new_scale("size"), ggnewscale::new_scale("color"), 
                      map_layer, scale_size, scale_color, scale_x_cont, scale_y_cont)
  } else {
    textRepel <- list( ggnewscale::new_scale("size"), ggnewscale::new_scale("color"), 
                      map_layer, scale_size, scale_color, scale_x_cont, scale_y_cont)
  }
  
  
  return(textRepel)
}


fn_mapgen <- function(bbox, crs_default, metric_unit, mp_layer, featr_type, 
                      maptitle, mapnum, rseg_leg_title, map_server, base_layer, maplabs, nhd, 
                      roads, rsegs, map_style_set, rivmap_ramp){ #applies results of the above functions to plot the map
  #getting various bbox formats:
  bbox_coords <- data.frame(lng = c(bbox[1], bbox[3]), lat = c(bbox[2], bbox[4]), row.names = NULL) 
  bbox_sf <- sf::st_as_sf(bbox_coords, coords = c('lng','lat'), crs = 4326) 
  bbox_sfc <- sf::st_as_sfc(sf::st_bbox(bbox))
  #prep labels & filter plotted data:
  fn_labelsAndFilter(maplabs, bbox_coords, nhd, roads, map_style_set, bbox_sf, crs_default, rsegs)
  #begin mapping:
  map <- ggplot() 
  map <- fn_catchMapErrors(map_layer = ggplot2::theme(text=ggplot2::element_text(size=20), 
                                                  title=ggplot2::element_text(size=40), #setting text sizes
                                                  legend.title = ggplot2::element_text(size=25), 
                                                  axis.title.x=ggplot2::element_blank(), 
                                                  axis.title.y=ggplot2::element_blank(),
                                                  panel.grid = ggplot2::element_blank()
                                                  ),
                                  layer_description = "map theme", map = map)
  map <- fn_catchMapErrors(map_layer = ggplot2::ggtitle(maptitle), layer_description = "map title", map = map)
  
  if(mapnum==2){ #because fn_polygonFill() returns NULL object if mapnum==1 (since it was desired to only shade tidal rsegs on riverseg maps)
    map <- fn_catchMapErrors(map_layer = fn_polygonFill(rsegs, map_style_set, mapnum, rseg_leg_title, rivmap_ramp),
                             layer_description = "fn_polygonFill(): tidal rseg fill and, if applicable, rivseg metric fill", map = map)
  }
  
  map <- fn_catchMapErrors(map_layer = fn_nhdLines(nhd_plot, map_style_set, nhd, bbox_sf),
                           layer_description = "fn_nhdLines(): nhd flowlines and waterbodies", map = map)
  map <- fn_catchMapErrors(map_layer = fn_roadsAndCityPoints(roads_plot, map_style_set, labels_plot, mp_layer),
                           layer_description = "fn_roadsAndCityPoints(): road lines, road labels, mp placeholder text, and/or city dots", map = map)
  map <- fn_catchMapErrors(map_layer = fn_borders(rsegs, counties, regions, origin, bbox_sf, crs_default, textsize, map_style_set),
                           layer_description = "fn_borders(): county, region, and/or rseg polyg_borders", map = map)
  map <- fn_catchMapErrors(map_layer = fn_textRepel(rsegs, labels_plot, textsize, map_style_set, bbox_coords),
                          layer_description = "fn_textRepel(): basin IDs, county, river, and/or city text", map = map)
  map <- fn_catchMapErrors(map_layer = fn_mp_bubbles(mp_layer, metric_unit, featr_type, map_style_set),
                           layer_description = "fn_mp_bubbles(): feature metric bubbles", map = map)
  map <- fn_catchMapErrors(map_layer = fn_shadow(rsegs, bbox_sfc, map_style_set),
                           layer_description = "fn_shadow(): reverse fill shadow", map = map)
  map <- map + coord_sf(xlim = bbox[c(1,3)],ylim = bbox[c(2,4)], expand =F)
  map <- fn_catchMapErrors(map_layer = ggspatial::annotation_scale(unit_category="imperial"),
                           layer_description = "scalebar", map = map)
  map <- fn_catchMapErrors(map_layer = ggspatial::annotation_north_arrow(which_north="true", location="tr",
                                                                     height= unit(4,"cm"), width= unit(3, "cm"), 
                                                                     style= north_arrow_orienteering(text_size=35)),
                           layer_description = "north arrow", map = map)
  return(map)
} 

#--!!for testing only!!--
# textcol <- styles[[map_style]]$color$text$color #from mapping aesthetics function
# mapnum <- 2
# bbox_as_sf <- bbox_sf
#---
# #example usage:
# map <- fn_mapgen(bbox, crs_default, metric_unit, mp_layer, featr_type, maptitle, mapnum=1,
#                   rseg_leg_title=NULL, map_server, map_layer, maplabs, nhd, roads, rsegs, map_style, styles)
# map_rivseg4 <- fn_mapgen(bbox, crs_default, metric_unit, mp_layer, featr_type,
#                          maptitle = paste0(run_config$riverseg_metrics[[4]]$run_label, ", ",
#                                            paste0(run_config$riverseg_metrics[[4]]$metric)),
#                          mapnum=2,
#                          rseg_leg_title=legend_titling(run_config$riverseg_metrics[[4]]$metric, runid_list),
#                          map_server, map_layer, maplabs, nhd, roads, rsegs, map_style_set)

## Function for making the critical cell groundwater maps for Coastal Plain origins
fn_gw_mapgen <- function(bbox, crs_default, mp_layer, featr_type, 
                      maptitle, maplabs, nhd, 
                      roads, map_style_set, rivmap_ramp, aquifer_shp, origin_shape){ 
  #getting various bbox formats:
  bbox_coords <- data.frame(lng = c(bbox[1], bbox[3]), lat = c(bbox[2], bbox[4]), row.names = NULL) 
  bbox_sf <- sf::st_as_sf(bbox_coords, coords = c('lng','lat'), crs = 4326) 
  bbox_sfc <- sf::st_as_sfc(sf::st_bbox(bbox))
  #prep labels & filter plotted data:
  fn_labelsAndFilter(maplabs, bbox_coords, nhd, roads, map_style_set, bbox_sf, crs_default, rsegs)
  #begin mapping:
  map <- ggplot() + coord_sf(xlim=bbox_coords$lng,ylim=bbox_coords$lat)
  map <- fn_catchMapErrors(map_layer = ggplot2::theme(text=ggplot2::element_text(size=20), 
                                                      title=ggplot2::element_text(size=40), #setting text sizes
                                                      legend.title = ggplot2::element_text(size=25), 
                                                      axis.title.x=ggplot2::element_blank(), 
                                                      axis.title.y=ggplot2::element_blank(),
                                                      panel.grid = ggplot2::element_blank()
                                                      ),
                          layer_description = "map theme", map = map)
  map <- fn_catchMapErrors(map_layer = ggplot2::ggtitle(maptitle), layer_description = "map title", map = map)
  
  map <- fn_catchMapErrors(map_layer = fn_roadsAndCityPoints(roads_plot, map_style_set, labels_plot, mp_layer),
                           layer_description = "fn_roadsAndCityPoints(): road lines, road labels, mp placeholder text, and/or city dots", map = map)
  map <- fn_catchMapErrors(map_layer = fn_borders(rsegs = NULL, counties, regions, origin, bbox_sf, crs_default, textsize, map_style_set),
                           layer_description = "fn_borders(): county, region, and/or rseg polyg_borders", map = map)
  
  ## Removing water body labels
  labels_gw_plot <- labels_plot[!(labels_plot$class %in% c("waterbody_lg","stream","majorRiver")),]
  
  map <- fn_catchMapErrors(map_layer = fn_textRepel(rsegs = NULL, labels_gw_plot, textsize, map_style_set, bbox_coords),
                           layer_description = "fn_textRepel(): basin IDs, county, river,http://127.0.0.1:39445/graphics/4c1aabb9-10f4-4e3c-9af0-e81f34909255.png and/or city text", map = map)
  map <- map + geom_sf(data = aquifer_shp, fill = 'red')
  map <- fn_catchMapErrors(map_layer = fn_mp_bubbles(mp_layer, metric_unit, featr_type, map_style_set),
                           layer_description = "fn_mp_bubbles(): feature metric bubbles", map = map)
  
  nonorigin <- sf::st_difference(bbox_sfc, origin_shape) #method of erasing
  sf::st_crs(nonorigin) <- crs_default
  shadow <- ggplot2::geom_sf(data = nonorigin, inherit.aes=FALSE, color=NA, fill = map_style_set$color$sf["shadow",], lwd=1 )

  map <-  map + shadow
  map <- map + coord_sf(xlim = bbox[c(1,3)],ylim = bbox[c(2,4)], expand =F)
  
  map <- fn_catchMapErrors(map_layer = ggspatial::annotation_scale(unit_category="imperial"),
                           layer_description = "scalebar", map = map)
  map <- fn_catchMapErrors(map_layer = ggspatial::annotation_north_arrow(which_north="true", location="tr",
                                                                         height= unit(4,"cm"), width= unit(3, "cm"), 
                                                                         style= north_arrow_orienteering(text_size=35)),
                           layer_description = "north arrow", map = map)

  return(map)
}
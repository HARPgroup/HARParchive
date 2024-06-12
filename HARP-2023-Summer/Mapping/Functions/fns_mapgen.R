##Establishes a collection of the functions utilized for map plotting. 
##fn_mapgen() is the final call used in WSP_Regional_Summaries.Rmd, and it calls these modular functions within it.
#Load required libraries
library(ggplot2)
library(geosphere)
library(arcpullr)
library(ggnewscale)
library(ggrepel)
library(mgsub)
library(sf)
library(ggspatial)

fn_catchMapErrors <- function(layer, layer_description="blank", map=NULL){ #making the root of an error clear
  #if layer name needs further description for the error message, input it into 'layer_description' as a character string. 
  #otherwise, the layer's variable name will be used:
  if(layer_description=="blank"){ layer_description <- deparse(substitute(layer)) }
  if(is.null(map)){ #aka no map to add to yet
    test <- try(ggplot2::ggplot() + layer, silent=TRUE)
  }else{
    test <- try(map + layer, silent=TRUE)
  }
  errors <- grep("Error", test, ignore.case=TRUE)
  if(length(errors)!=0){#there's an issue with the layer and it will not be added to the map
    if(is.null(map)){ #aka no basemap yet
      stop(paste0("Cannot generate maps due to a conflict in the basemap layer. See fns_mapgen.R for more."), call. = FALSE)
    } else{
      if("errors" %in% names(map)){
        map$errors <- append(map$errors, layer_description)
      } else{map$errors <- layer_description}
    }
  } else{#no issue with the layer & it will be plotted
      if(is.null(map)){ #aka no map to add to yet
        map <- ggplot2::ggplot() + layer
      }else{
        map <- map + layer
      }
    }
  return(map)
}

fn_labelsAndFilter <- function(labels=maplabs, bbox_coord_df, nhd, roads, style, bbox_sf, crs_default){ 
  #combines all text labels into 1 df and associates them w/ aesthetics from mapstyle_config.R
  #filters data to control the amount of map detail based on extent of the map
  
  for(i in 1:length(labels)){ #Combine all text labels into one df:
    if(i==1){ labels$all <- labels[[i]] }
    if(i!=1){ labels$all <- rbind(labels$all, labels[[i]]) }
    if(i==length(labels)-1){ labels <- labels$all }
  }
  text_aes <- style$text
  labels <- sqldf( #join text aesthetics with sqldf 
    "SELECT text_aes.*, labels.label, labels.lat, labels.lng
    FROM text_aes 
    LEFT OUTER JOIN labels
      on (labels.class = text_aes.class)"
  )
  class(labels$bg.r) = "numeric"
  #labels <- labels[!is.na(labels$label),] #rm labels without a label name
  
  #Find distance of diagonal of bbox in miles -- for filtering what will be plotted
  #distance used instead of 'extent' because DEQ vocab has extent synonymous w bbox  
  distance <-  geosphere::distHaversine(bbox_coord_df) / 1609.34 #distHaversine() defaults to meters, so convert to miles
  
  roads_plot <- roads
  if (distance > 300) {
    nhd_plot<- nhd$flowline[nhd$flowline$StreamOrde!=1 & nhd$flowline$StreamOrde!=2 & nhd$flowline$StreamOrde!=3,]
    roads_plot <- roads[roads$RTTYP=="I",]
    labels_plot <- labels[labels$class=="county" | labels$class=="majorRiver" |
                            labels$class=="I"| labels$class=="city" | labels$class!="waterbody_lg",]
    textsize <- c(4,4,5,6,5,0) #c(I/S/U , town/majC/LakePond/str , majR , county ,   facility num , segs$basin_sf lwd)
  } else if (distance > 130) {
    nhd_plot <- nhd$flowline[nhd$flowline$StreamOrde!=1 & nhd$flowline$StreamOrde!=2,]
    labels_plot <- labels[labels$class=="county" | labels$class=="majorRiver" |
                            labels$class=="I" | labels$class=="city",]
    textsize <- c(5,5,6,11,5,1)
  } else if (distance > 70) {
    nhd_plot <- nhd$flowline[nhd$flowline$StreamOrde!=1,]
    labels_plot <- labels[labels$class!="waterbody_sm" & labels$class!="waterbody_med" & labels$class!= "smallTown",]
    textsize <- c(6,7,9,12,5,1.2)
    labels$segsize <- as.numeric( gsub(1, 0, labels$segsize) ) #no label "lollipop" for counties @ small distances
  } else {
    nhd_plot <- nhd$flowline
    labels_plot <- labels[labels$class!="waterbody_sm" & labels$class!="waterbody_med",]
    textsize <- c(7,8,10,13,5,1.5)
    labels$segsize <- as.numeric( gsub(1, 0, labels$segsize) ) 
  }
  #crop data to extent:
  roads_plot <- sf::st_crop(roads_plot, bbox_sf)
  
  #save:
  assign('labels_plot', labels_plot, envir = globalenv())
  assign('nhd_plot', nhd_plot, envir = globalenv())
  assign('roads_plot', roads_plot, envir = globalenv())
  assign('textsize', textsize, envir = globalenv())
}

fn_basemap <- function(map_server, map_layer, bbox_coord_df){ #generates a basemap
  map_url <- paste(map_server,map_layer,sep ="/")
  mapdata <- arcpullr::get_spatial_layer(map_url)
  mapdata <- sf::st_crop(mapdata, c(xmin= min(bbox_coord_df$lng), ymin = min(bbox_coord_df$lat), 
                                xmax = max(bbox_coord_df$lng), ymax = max(bbox_coord_df$lat))) #crop to our extent 
  basemap <- ggplot2::geom_sf(data = mapdata)
  return(basemap)
}

fn_shadow <- function(rsegs, bbox_sfc, style){ #generates a "reverse fill" layer that will shadow the area outside the basins of interest
  basin_union <- sf::st_union(rsegs)
  nonbasin <- sf::st_difference(bbox_sfc, basin_union) #method of erasing
  sf::st_crs(nonbasin) <- crs_default
  shadow <- ggplot2::geom_sf(data = nonbasin, inherit.aes=FALSE, color=NA, fill = style$color$sf["shadow",], lwd=1 )
  return(shadow)
}

fn_mp_bubbles <- function(mp_layer, metric_unit, featr_type, style){
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
    layer <- ggplot2::geom_point(data = mp_layer, aes(x = lng, y = lat, 
                                 size = bin), color = style$color$metrics["Surface Water",],
                                 shape = 19)
    scale_size <- ggplot2::scale_size_binned(range = c(2,20), 
                                    breaks = breaks, 
                                    labels = labs,
                                    limits = lims,
                                    name = legend_title[1])
    bubbles <- list(layer, scale_size, num_labels)
  }
  if (featr_type == "source") {
    layer <- ggplot2::geom_point(data = mp_layer, aes(x = lng, y = lat, 
                                                      color = Source_Type, 
                                                      size = bin),
                                                      shape = 19)
      scale_size <- ggplot2::scale_size_binned(range = c(2,20),
                                      breaks = breaks,
                                      labels = labs,
                                      limits = lims,
                                      name = legend_title[1],
                                      guide= guide_legend(override.aes=list(color = style$color$metrics["Groundwater",],
                                                                            fill = style$color$metrics["Surface Water",],
                                                                            stroke = seq(3,7,length.out=length(breaks)),
                                                                            shape=21 ))
      )
      scale_color <- ggplot2::scale_colour_manual(values= style$color$metrics[c("Surface Water", "Groundwater"),] ,
                                         breaks= c("Surface Water", "Groundwater"),
                                         labels= c("Surface Water", "Groundwater"),
                                         name= "Source Type",
                                         guide= guide_legend(override.aes=list(size=9))
      )
      bubbles <- list(layer, scale_size, scale_color, num_labels)
  }
  return(bubbles)
}

fn_borders <- function(rsegs, counties, regions, origin, bbox_sf, crs_default, textsize, style){ #merges all polygon borders into 1 df and associates them w/ aesthetics from mapstyle_config.R
  #merging borders into 1 df so they can be on the same legend
  borders <- data.frame(counties[,"name"] , bundle= rep("county", nrow(counties)) )
  names(borders) <- c("name", "geometry", "bundle")
  sf::st_geometry(rsegs) <- "geometry"
  sf::st_crs(rsegs) <- crs_default
  borders <- rbind(borders, data.frame(rsegs[,c("name", "bundle")] )  )
  if (origin_type=="region") {
    region_OI <- regions[regions$region==origin,] #region of interest
    sf::st_geometry(region_OI) <- "geometry"
    region_OI <- data.frame(name="region", bundle="region", geometry=region_OI[fn_geoCol(region_OI)] )
    borders <- rbind(borders, region_OI)
    borders <- sf::st_as_sf(borders)
    sf::st_crs(borders) <- crs_default
  } else {borders <- sf::st_as_sf(borders)}
  sf::st_crs(borders) <- crs_default
  borders <- borders[borders$bundle %in% c('region','county','watershed'), ]
  borders <- sf::st_crop(borders, bbox_sf)  
  
  #generate layer for mapping
  if (origin_type == "region") { 
    layer <- ggplot2::geom_sf(data= borders, inherit.aes=FALSE, fill=NA,
                              ggplot2::aes(color= bundle,
                                  lwd= as.numeric(mgsub::mgsub(bundle, 
                                                      pattern=c("county","watershed","region"), 
                                                      replacement=c(2.5,textsize[6],4.5) )),
                                  linetype= bundle )
             )
    scale_color <- ggplot2::scale_colour_manual(values= c(style$color$sf[c("region","county","rsegs"),]) ,
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
  } else {
    layer <- ggplot2::geom_sf(data= borders, inherit.aes=FALSE, fill=NA,
                              ggplot2::aes(color= bundle,
                                    lwd= as.numeric(mgsub::mgsub(bundle, 
                                                        pattern=c("county","watershed"), 
                                                        replacement=c(2.5,textsize[6])) ),
                                    linetype= bundle )
                        )
    scale_color <- ggplot2::scale_colour_manual(values= c(style$color$sf[c("county","rsegs"),]) ,
                                       breaks= c("county","watershed"),
                                       labels= c("County","Basin"),
                                       name= "Borders" )
    scale_linetype <- ggplot2::scale_linetype_manual(values= c("county"= 1,"watershed"= 2),
                          labels= c("County","Basin"),
                          name= "Borders" )
    scale_linewidth <- ggplot2::scale_linewidth(range= range(c(textsize[6],4.5)),
                          breaks= c(2.5,textsize[6]),
                          labels= c("County","Basin"),
                          name= "Borders" )
  }
  borders_layer <- list(ggnewscale::new_scale("color"), ggnewscale::new_scale("linetype"), ggnewscale::new_scale("linewidth"), 
                  layer[[1]], layer[[2]], scale_color, scale_linetype, scale_linewidth)
  return(borders_layer)
}

fn_polygonFill <- function(rsegs, style, mapnum, rseg_leg_title){
  #fill tidal rsegs 
  rsegTidal <- subset(rsegs, riverseg %in% grep("0000", rsegs$riverseg, value=TRUE)) #PROBLEM w/ DF
  st_crs(rsegTidal) <- crs_default 
  #fix tidal df
  names(rsegTidal)[1:(ncol(rsegTidal)-6)] <- names(rsegTidal)[2:(ncol(rsegTidal)-5)]
  rsegTidal[ncol(rsegTidal)-5] <- NULL
  #create tidal map layer
  tidal <- ggplot2::geom_sf(data = rsegTidal, inherit.aes = FALSE, aes(fill=bundle), alpha = 1)
  tidal_scale_fill <- ggplot2::scale_fill_manual(values = style$color$sf["tidal",], #color set in config
                                        breaks = "watershed",
                                        labels = "Tidal/Unmodeled",
                                        name = NULL)
  rseg_fill <- list(ggnewscale::new_scale("fill"), tidal, tidal_scale_fill)
  
  #rseg fill based on drought metric % difference for rivseg maps:
  if (mapnum == 2) {
    class(rsegs$bin) <- "numeric"
    metric_fill <- ggplot2::geom_sf(data = rsegs, inherit.aes = FALSE, mapping = aes(fill = as.factor(bin)), alpha = 0.7 )
    metric_scale_fill <- ggplot2::scale_fill_manual(values = rivmap_colors, #from config
                                           breaks = rivbreaks,
                                           labels = rivmap_labs,
                                           limits = as.factor(rivbreaks),
                                           name = rseg_leg_title)
    metric_legend <- ggplot2::theme(legend.spacing.y = unit(0.1, 'cm')) #adjust spacing of legend 
    rseg_fill <- list(ggnewscale::new_scale("fill"), metric_fill, metric_scale_fill,
                      ggplot2::guides(fill = guide_legend(byrow = TRUE)),
                      ggnewscale::new_scale("fill"), tidal, tidal_scale_fill)
  }
  return(rseg_fill)
}

fn_nhdLines <- function(nhd_plot, style, nhd){
  nhd_layer <- ggplot2::geom_sf(data = nhd_plot, 
                 inherit.aes=FALSE, color= style[["color"]][["sf"]]["nhd",], 
                 mapping=aes(lwd=nhd_plot$StreamOrde), #line thickness based on stream order
                 show.legend=FALSE)
  scale_linewidth <- ggplot2::scale_linewidth(range= c(0.4,2), guide = FALSE) 
  wtbd_layer <- ggplot2::geom_sf(data = rbind(nhd$off_network_wtbd, nhd$network_wtbd),  
                       inherit.aes=FALSE, fill= style[["color"]][["sf"]]["nhd",], size=1)
  nhd_map <- list(ggnewscale::new_scale("color"), ggnewscale::new_scale("linetype"), ggnewscale::new_scale("linewidth"), 
              nhd_layer[[1]], nhd_layer[[2]], scale_linewidth, wtbd_layer[[1]], wtbd_layer[[2]])
  return(nhd_map)
}

fn_roadsAndCityPoints <- function(roads_plot, style, labels_plot, mp_layer){
  rd_lines <- ggplot2::geom_sf(data = roads_plot, inherit.aes=FALSE, color= style[["color"]][["sf"]]["roads",], fill=NA, lwd=1, linetype="twodash")
  city_dots <- ggplot2::geom_point(data = labels_plot[labels_plot$class=="city"|labels_plot$class=="town",], 
                          aes(x=lng, y=lat), color= style[["color"]][["sf"]]["citypts",], size=2)
  #mp labels placeholder to have other labels repel:
  mp_placeholder <- ggplot2::geom_text(data = mp_layer, aes(lng, lat, label=NUM),colour=NA, size=textsize[4],check_overlap=TRUE)
  rd_bubbles <- ggrepel::geom_label_repel(data = labels_plot[labels_plot$class == c("I","S","U"), ],
                   aes(x=lng, y=lat, label=label,
                       fontface=fontface, family=fontfam,
                       color=as.factor(colcode),
                       fill=fillcode),
                   show.legend=NA,
                   size=textsize[1],
                   label.r=0.6, label.size=0.12,
                   max.overlaps=4
                   )
  scale_colour <- ggplot2::scale_colour_manual(values=style[["color"]][["text"]][,"color"], breaks=c(1,2,3),
                        labels=c("Interstate","State Route", "US Hwy"), name="Roads")
  scale_fill <- ggplot2::scale_fill_manual(values=style[["color"]][["fill"]][,"color"], breaks=c(1,2,3),
                      labels=c("Interstate","State Route", "US Hwy"), name="Roads")
  roadsNcitydots <- list(ggnewscale::new_scale("color"), ggnewscale::new_scale("linetype"), ggnewscale::new_scale("linewidth"), 
                         rd_lines, ggnewscale::new_scale("color"), ggnewscale::new_scale("size"), 
                         city_dots, mp_placeholder, 
                         ggnewscale::new_scale("color"), ggnewscale::new_scale("fill"),
                         rd_bubbles, scale_colour, scale_fill)
  return(roadsNcitydots)
}
  
fn_textRepel <- function(rsegs, labels_plot, textsize, style){
  textcol <- style$color$text$color
  # Basin Labels (by riverseg ID):
  basins <- ggplot2::geom_text(data=rsegs, aes(x=lng, y=lat, label=riverseg),color="black",size=textsize[5],check_overlap=TRUE)
  # All Other Text Labels:
  layer <- ggrepel::geom_text_repel(data = labels_plot[!(labels_plot$class == "I" | 
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
                    force= 40, direction="both",
                    min.segment.length=0.5
        )
  scale_size <- ggplot2::scale_size(range= range(textsize[2:4]), breaks= textsize[2:4]) 
  scale_color <- ggplot2::scale_colour_manual(values=textcol, breaks=seq(1,length(textcol)), guide="none")
  scale_x_cont <- ggplot2::scale_x_continuous(limits = bbox_coords$lng, expand = c(0, 0))
  scale_y_cont <- ggplot2::scale_y_continuous(limits = bbox_coords$lat, expand = c(0, 0))   
  textRepel <- list(basins, ggnewscale::new_scale("size"), ggnewscale::new_scale("color"), 
                    layer, scale_size, scale_color, scale_x_cont, scale_y_cont)
  return(textRepel)
}


fn_mapgen <- function(bbox, crs_default, metric_unit, mp_layer, featr_type, 
                      maptitle, mapnum, rseg_leg_title, map_server, map_layer, maplabs, nhd, 
                      roads, rsegs, map_style, styles){ #applies results of the above functions to plot the map
  
  assign('style', envir=.GlobalEnv, styles[[map_style]])
  #getting various bbox formats:
  assign('bbox_coords', envir=.GlobalEnv, 
         data.frame(lng = c(bbox[1], bbox[3]), lat = c(bbox[2], bbox[4]), row.names = NULL) )
  assign('bbox_sf', envir=.GlobalEnv, 
         sf::st_as_sf(bbox_coords, coords = c('lng','lat'), crs = 4326) )
  assign('bbox_sfc', envir=.GlobalEnv, 
         sf::st_as_sfc(sf::st_bbox(bbox)) )
  #prep labels & filter plotted data:
  fn_labelsAndFilter(maplabs, bbox_coords, nhd, roads, style, bbox_sf, crs_default)
  #begin mapping:
  map <- fn_catchMapErrors(layer = fn_basemap(map_server, map_layer, bbox_coords)) 
  map <- fn_catchMapErrors(layer = ggplot2::theme(text=ggplot2::element_text(size=20), 
                                                  title=ggplot2::element_text(size=40), #setting text sizes
                                                  legend.title = ggplot2::element_text(size=25), 
                                                  axis.title.x=ggplot2::element_blank(), 
                                                  axis.title.y=ggplot2::element_blank()
                                                  ),
                                  layer_description = "map theme", map = map)
  map <- fn_catchMapErrors(layer = ggplot2::ggtitle(maptitle), layer_description = "map title", map = map)
  map <- fn_catchMapErrors(layer = fn_polygonFill(rsegs, style, mapnum, rseg_leg_title),
                           layer_description = "tidal rseg fill and, if applicable, rivseg metric fill", map = map)
  map <- fn_catchMapErrors(layer = fn_nhdLines(nhd_plot, style, nhd),
                           layer_description = "nhd flowlines and waterbodies", map = map)
  map <- fn_catchMapErrors(layer = fn_roadsAndCityPoints(roads_plot, style, labels_plot, mp_layer),
                           layer_description = "road lines, road labels, mp placeholder text, and/or city dots", map = map)
  map <- fn_catchMapErrors(layer = fn_borders(rsegs, counties, regions, origin, bbox_sf, crs_default, textsize, style),
                           layer_description = "county, region, and/or rseg borders", map = map)
  map <- fn_catchMapErrors(layer = fn_textRepel(rsegs, labels_plot, textsize, style),
                          layer_description = "basin IDs, county, river, and/or city text", map = map)
  map <- fn_catchMapErrors(layer = fn_mp_bubbles(mp_layer, metric_unit, featr_type, style),
                           layer_description = "feature metric bubbles", map = map)
  map <- fn_catchMapErrors(layer = fn_shadow(rsegs, bbox_sfc, style),
                           layer_description = "reverse fill shadow", map = map)
  map <- fn_catchMapErrors(layer = ggspatial::annotation_scale(unit_category="imperial"),
                           layer_description = "scalebar", map = map)
  map <- fn_catchMapErrors(layer = ggspatial::annotation_north_arrow(which_north="true", location="tr",
                                                                     height= unit(4,"cm"), width= unit(3, "cm"), 
                                                                     style= north_arrow_orienteering(text_size=35)),
                           layer_description = "north arrow", map = map)
  map$mapnum <- mapnum
  return(map)
} 

#--!!for testing only!!--
# mapnum <- 1
# #example usage:
# map_test <- fn_mapgen(bbox, crs_default, metric_unit, mp_layer, featr_type, maptitle, mapnum=1,
#                   rseg_leg_title=NULL, map_server, map_layer, maplabs, nhd, roads, rsegs, map_style, styles)

# map_rivseg4 <- fn_mapgen(bbox, crs_default, metric_unit, mp_layer, featr_type, 
#                          maptitle = paste0(run_config$riverseg_metrics[[4]]$run_label, ", ", 
#                                            paste0(run_config$riverseg_metrics[[4]]$metric)), 
#                          mapnum=2,
#                          rseg_leg_title=legend_titling(run_config$riverseg_metrics[[4]]$metric, runid_list), 
#                          map_server, map_layer, maplabs, nhd, roads, rsegs, map_style, styles)
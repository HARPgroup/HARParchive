##Establishes a collection of the functions utilized for map plotting. 
##fn_mapgen() is the final call used in WSP_Regional_Summaries.Rmd, and it calls these modular functions within it.
#Load required libraries
library(ggplot2)
library(ggnewscale)
library(mgsub)
library(sf)
library(ggspatial)

fn_catchMapErrors <- function(layer, layer_description="blank", map=NA){ #making the root of an error clear
  #if layer name needs further description for the error message, input it into 'layer_description' as a character string. 
  #otherwise, the layer's variable name will be used:
  if(layer_description=="blank"){ layer_description <- deparse(substitute(layer)) }
  if(is.na(map)){ #aka no map to add to yet
    test <- try(ggplot2::ggplot() + layer, silent=TRUE)
  }else{
    test <- try(map + layer, silent=TRUE)
  }
  errors <- grep("Error", test, ignore.case=TRUE)
  if(length(errors)!=0){#there's an issue with the layer and it will not be added to the map
    if(is.na(map)){ #aka no basemap yet
      stop(paste0("Cannot generate maps due to a conflict in the basemap layer. See fns_mapgen.R for more."), call. = FALSE)
    } else{
      if("errors" %in% names(map)){
        map$errors <- append(map$errors, layer_description)
      } else{map$errors <- layer_description}
    }
  } else{#no issue with the layer & it will be plotted
      if(is.na(map)){ #aka no map to add to yet
        map <- ggplot2::ggplot() + layer
      }else{
        map <- map + layer
      }
    }
  return(map)
}

fn_labelsAndFilter <- function(labels, bbox_coord_df, nhd, roads, style){ 
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
  
  #Find distance of diagonal of bbox in miles -- for filtering what will be plotted
  #distance used instead of 'extent' because DEQ vocab has extent synonymous w bbox  
  distance <-  distHaversine(bbox_coord_df) / 1609.34 #distHaversine() defaults to meters, so convert to miles
  
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
  
  assign('labels_plot', labels_plot, envir = globalenv())
  assign('nhd_plot', nhd_plot, envir = globalenv())
  assign('roads_plot', roads_plot, envir = globalenv())
  assign('textsize', textsize, envir = globalenv())
}

fn_basemap <- function(map_server, map_layer, bbox_coord_df){ #generates a basemap
  map_url <- paste(map_server,map_layer,sep ="/")
  mapdata <- get_spatial_layer(map_url)
  mapdata <- st_crop(mapdata, c(xmin= min(bbox_coord_df$lng), ymin = min(bbox_coord_df$lat), 
                                xmax = max(bbox_coord_df$lng), ymax = max(bbox_coord_df$lat))) #crop to our extent 
  basemap <- ggplot2::geom_sf(data = mapdata)
  return(basemap)
}

fn_shadow <- function(filtered_rsegs, bbox_sfc, style){ #generates a "reverse fill" layer that will shadow the area outside the basins of interest
  basin_union <- sf::st_union(filtered_rsegs)
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
  
  if (featr_type == "facility") {
    layer <- ggplot2::geom_point(data = mp_layer, aes(x = lng, y = lat, 
                                 size = bin), color = style$color$metrics["Surface Water",],
                                 shape = 19)
    scale_size <- ggplot2::scale_size_binned(range = c(2,20), 
                                    breaks = breaks, 
                                    labels = labs,
                                    limits = lims,
                                    name = legend_title[1])
    bubbles <- list(layer, scale_size)
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
      bubbles <- list(layer, scale_size, scale_color)
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
  borders <- list(ggnewscale::new_scale("color"), ggnewscale::new_scale("linetype"), ggnewscale::new_scale("linewidth"), 
                  layer[[1]], layer[[2]], scale_color, scale_linetype, scale_linewidth)
  return(borders)
}

fn_textRepel <- function(labels_plot, textsize, style){
  textcol <- style$color$text$color
  layer <- geom_text_repel(data = labels_plot[!(labels_plot$class == "I" | 
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
  scale_size <- scale_size(range= range(textsize[2:4]), breaks= textsize[2:4]) 
  scale_color <- scale_colour_manual(values=textcol, breaks=seq(1,length(textcol)), guide="none")
  scale_x_cont <- scale_x_continuous(limits = bbox_coords$lng, expand = c(0, 0))
  scale_y_cont <- scale_y_continuous(limits = bbox_coords$lat, expand = c(0, 0))   
  textRepel <- list(ggnewscale::new_scale("size"), new_scale("color"), layer, 
                    scale_size, scale_color, scale_x_cont, scale_y_cont)
  return(textRepel)
}


fn_mapgen <- function(bbox, crs_default, metric_unit, mp_layer, featr_type, 
                      maptitle, mapnum, map_server, map_layer, labels, nhd, 
                      roads, rsegs, map_style, styles){ #applies results of these functions to plot the map
  
  ##style <- styles[[map_style]]
  assign('style', envir=.GlobalEnv, styles[[map_style]])
  
  #getting various bbox formats
  ##bbox_coords <- data.frame(lng = c(bbox[1], bbox[3]), lat = c(bbox[2], bbox[4]), row.names = NULL)
  assign('bbox_coords', envir=.GlobalEnv, 
         data.frame(lng = c(bbox[1], bbox[3]), lat = c(bbox[2], bbox[4]), row.names = NULL) )
  
  ##bbox_sf <- sf::st_as_sf(bbox_coords, coords = c('lng','lat'), crs = 4326)
  assign('bbox_sf', envir=.GlobalEnv, 
         sf::st_as_sf(bbox_coords, coords = c('lng','lat'), crs = 4326) )
  
  ##bbox_sfc <- sf::st_as_sfc(sf::st_bbox(bbox))
  assign('bbox_sfc', envir=.GlobalEnv, 
         sf::st_as_sfc(sf::st_bbox(bbox)) )
  
  
  if (mapnum==2) { class(rsegs$bin) <- "numeric" }
  
  fn_labelsAndFilter(labels, bbox_coords, nhd, roads, style)
  
  map <- fn_catchMapErrors(layer = fn_basemap(map_server, map_layer, bbox_coords)) 
  map <- fn_catchMapErrors(layer = ggplot2::theme(text=ggplot2::element_text(size=20), 
                                                  title=ggplot2::element_text(size=40), #setting text sizes
                                                  legend.title = ggplot2::element_text(size=25), 
                                                  axis.title.x=ggplot2::element_blank(), 
                                                  axis.title.y=ggplot2::element_blank()
                                                  ),
                                  layer_description = "map theme", map = map)
  map <- fn_catchMapErrors(layer = ggplot2::ggtitle(maptitle), layer_description = "map title", map = map)
  
  map <- fn_catchMapErrors(layer = fn_borders(rsegs, counties, regions, origin, bbox_sf, crs_default, textsize, style),
                           layer_description = "county, region, and/or rseg borders", map = map)
  map <- fn_catchMapErrors(layer = fn_textRepel(labels_plot, textsize, style),
                           layer_description = "county, river, and/or city text", map = map)
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
  return(map)
} 

#--!!for testing only!!--
# labels <- maplabs
# textcol <- styles[[map_style]]$color$text$color #from mapping aesthetics function
# mapnum <- 1
# bbox_as_sf <- bbox_sf
#---
#example usage:
map <- fn_mapgen(bbox, crs_default, metric_unit, mp_layer, featr_type, maptitle, mapnum=1, 
          map_server, map_layer, labels=maplabs, nhd, roads, rsegs, map_style, styles)
  
## Establishing a function to generate maps when given data and aesthetics 
# Loading required libraries for mapping
library(sp)
library(rgeos)
library(ggmap)
library(raster)
library(ggplot2)
library(ggnewscale)
library(mgsub)
library(sf)
library(ggsn)
library(ggspatial)
library(ggrepel)
library(geosphere)
library(arcpullr)
source(paste0(github_location,"/HARParchive/HARP-2023-Summer/fn_filter_map.R"),local = TRUE) 

## nhd layer will be pulled and processed before function is called but filtering of flowlines to plot will be done within this function 
## bbox should come in with format of named list of coords: xmin, ymin, xmax, ymax
## metric is the specific name of the value/metric that bubbles will be sized with, includes runid & metric name (e.g. runid_11_wd_mgd)
## "featr_type" will be either basin, locality, or region
## "style" dictates which mapping aesthetics are desired from mapstyle_config.R (options right now are custom or default) 
## "mapnum": either 1 (facility/source maps) or 2 (riverseg maps)
## "title": so we can specify titles for the riverseg maps, either pass in rivseg section or use "default"(for table 1)

#Map 1 Args:
# mapnum = 1; style = styles[[map_style]]; metric = featrs_file_map_bubble_column[i]; segs = rsegs
#Map 2 Args:
# mapnum = 2; style = styles[[map_style]]; metric = rivseg_metric[i]; segs = rsegs_sf

fn_mapgen <- function(mapnum, featr_type, origin_type, style, metric, origin, bbox, segs, counties, roads,
                       nhd, maplabs, mp_layer, metric_unit, title) { 
  
  # Combine all map labels into one df:
  for(i in 1:length(maplabs)){
    if(i==1){ maplabs$all <- maplabs[[i]] }
    if(i!=1){ maplabs$all <- rbind(maplabs$all, maplabs[[i]]) }
  }
  
  ## aesthetics from styles$custom need to be joined to maplabs$all using the class column 
  text_aes <- style$text
  maplabs_all <- maplabs$all  
  
  textcol <- styles[[map_style]]$color$text$color #from mapping aesthetics function
  label_fill <- styles[[map_style]]$color$fill$color
  
  ## extract colors for sf borders and metric bubbles
  colors_sf <- style$color$sf
  colors_metric <- style$color$metrics
  
  # join with sqldf 
  maplabs$final <- sqldf(
    "SELECT text_aes.*, maplabs_all.label,maplabs_all.lat, maplabs_all.lng
    FROM text_aes 
    LEFT OUTER JOIN maplabs_all
      on (maplabs_all.class = text_aes.class)"
  )
  
  labels <- maplabs$final
  
  #----Generating Basemap-Specific Data----  
  #For the scalebar:  
  bbox_points <- data.frame(long = c(bbox[1], bbox[3]), lat = c(bbox[2], bbox[4]))
  colnames(bbox_points) <- c('x','y')
  bbox_sf <- st_as_sf(bbox_points, coords = c('x','y'), crs = 4326)
  anchor_vect <- c(x = (((bbox_points$x[2] - bbox_points$x[1])/5) + bbox_points$x[1]), y = bbox_points$y[1]+((bbox_points$y[1])/3)*0.001) 
  
  #Find distance of diagonal of bbox in miles -- for filtering what will be plotted
  #distance used instead of 'extent' because DEQ vocab has extent synonymous w bbox  
  distance <- data.frame(lng = bbox[c("xmin", "xmax")], lat = bbox[c("ymin", "ymax")])
  distance <-  distHaversine(distance) / 1609.34 #distHaversine() defaults to meters, so convert to miles
  
  #Filter labels & flowlines 
  fn_filter_map(labels, nhd, roads, distance) #creates labelsP
  
  #Generate basemap using the given boundary box --- ERROR
  # bbox <- setNames(st_bbox(bbox), c("left", "bottom", "right", "top")) #required to use get_stamenmap()
  # basemap <- ggmap(ggmap::get_stamenmap(maptype="terrain-background", color="color", bbox=bbox, zoom=10))
  #basemap <- ggmap(basemap_0)
  
  #### TEMPORARY work-around: use get_map to get a sattelite map
  # bbox <- setNames(st_bbox(bbox), c("left", "bottom", "right", "top")) #required to use bbox in get_map or get_stamenmap
  # basemap <- ggmap(get_map(location = bbox, maptype = "satellite"))
  # basemap <- ggmap(get_map(location = c(long = mean(bbox_points$x), lat = mean(bbox_points$y)), maptype = "satellite", zoom = (zoomval-2)))
  # basemap <- basemap +
  #   scale_x_continuous(limits = bbox_points$x, expand = c(0, 0)) +
  #   scale_y_continuous(limits = bbox_points$y, expand = c(0, 0))
  ####  
  
  ## Alternative basemap/background
  gg_map_layer <- function(map_server, map_layer) {
    map_url <- paste(map_server,map_layer,sep ="/")
    mapdata <- get_spatial_layer(map_url)
    mapdata <- st_crop(mapdata, c(xmin= min(bbox_points$x), ymin = min(bbox_points$y), 
                                    xmax = max(bbox_points$x), ymax = max(bbox_points$y))) #crop to our extent 
    plotted_map <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = mapdata)
    return(plotted_map)
  }
  map_server <- "https://gismaps.vdem.virginia.gov/arcgis/rest/services"
  # VA LandCover - very sparse, 
  map_layer = "Download/LandCover_Downloads/MapServer/0"
  basemap <- gg_map_layer(map_server, map_layer)
  
  #For reverse-fill: darken area of map outside basins 
  rsegs_union <- st_union(segs)
  bbox_st <- st_as_sfc(st_bbox(bbox))
  nonbasin <- st_difference(bbox_st, rsegs_union) #method of erasing
  st_crs(nonbasin) <- crs_default

  st_crs(nhd_plot) <- crs_default  #nhd_plot created in filtering function above
  
  #labelsP <- labelsP[ ,!duplicated(colnames(labelsP))]
  class(labelsP$bg.r) = "numeric"
  
  # Legend & Titling
  # For map title:
  
  if (title == "default"){
    if (origin_type == "basin") {
      title <- (paste("Basin Upstream of", segs$name[segs$riverseg==origin] , origin, ",", metric, sep=" ") )
    } else if (origin_type == "locality") {
      title <- paste0(origin, " Locality, ", metric)
    }  else if (origin_type == "region") {
      title <- paste0(origin, " Region, ", metric)
    } 
  } else {
    if (origin_type == "basin") {
      title <- ( paste("Basin Upstream of", segs$name[segs$riverseg==origin] , origin, ",", title, sep=" ") )
    } else if (origin_type == "locality") {
      title <- paste0(origin, " Locality, ", title)
    }  else if (origin_type == "region") {
      title <- paste0(origin, " Region, " ,title)
    } 
  }
  
  # For binned legend 
  breaks <- seq(1:7)
  lims <- c(min(breaks),max(breaks)) #limits based on range of breaks
  if (metric_unit == "mgd") { 
    labs = c(0.5, 1.0, 2, 10, 25, 100, 1000)
  } else if (metric_unit == "mgy") {
    labs = c(1, 5, 10, 50, 250, 1000, 10000)
  } else {
    labs = c(0.5, 1.0, 2, 10, 25, 100, 1000) } #default to mgd if unit is neither mgd or mgy
  
  # Remove bubbles for MPs with no metric value -- stored with bin = X
  mp_layer_plot <- mp_layer[!mp_layer$bin == "X" , ]
  class(mp_layer_plot$bin) <- "numeric" #make sure bin column is type numeric for sizing data points 
  if (mapnum ==2) {
    class(segs$bin) <- "numeric"
  }
  # Tidal riversegs
  rivsegTidal <- subset(segs, riverseg %in% grep("0000", segs$riverseg, value=TRUE)) #PROBLEM w/ DF
  st_crs(rivsegTidal) <- crs_default 
  #fix tidal df
  names(rivsegTidal)[1:(ncol(rivsegTidal)-6)] <- names(rivsegTidal)[2:(ncol(rivsegTidal)-5)]
  rivsegTidal[ncol(rivsegTidal)-5] <- NULL
  
  region_OI <- regions[regions$region==origin,] #region of interest
  
  # Merging Borders into 1 df #ERROR HERE
  #names(segs)[names(segs) == "WKT"] <- "geometry"
  st_geometry(segs) <- "geometry"
  borders <- data.frame(counties[,"name"] , bundle= rep("county", nrow(counties)) )
  #`st_crs(rsegs) <- crs(borders)
  st_crs(segs) <- crs_default
  names(borders) <- c("name", "geometry", "bundle")
  borders <- rbind(borders, data.frame(segs[,c("name", "bundle")] )  )
  if (origin_type=="region") {
    st_geometry(region_OI) <- "geometry"
    #st_crs(region_OI) <- crs(borders)
    region_OI <- data.frame(name="region", bundle="region", geometry=region_OI[geoCol(region_OI)] )
    borders <- rbind(borders, region_OI)
    borders <- st_as_sf(borders)
    sf::st_crs(borders) <- crs_default
  } else {borders <- st_as_sf(borders)}
  sf::st_crs(borders) <- crs_default
  
  borders <- borders[borders$bundle %in% c('region','county','watershed'), ]
  
  #Crop layers to our extent
  borders <- st_crop(borders, bbox_sf)  
  roads_plot <- st_crop(roads_plot, bbox_sf) 
  
  ###### GENERATE MAP #######
  map <- basemap +  
    # Titles
    theme(text=element_text(size=20), title=element_text(size=40), #setting text sizes
          legend.title = element_text(size=25), axis.title.x=element_blank(), axis.title.y=element_blank()) +
    ggtitle(title)
  # Rivseg fill based on drought metric % difference for rivseg maps 
  if (mapnum == 2) {
    map <- map + new_scale("fill") +
      geom_sf(data = segs, inherit.aes = FALSE, mapping = aes(fill = as.factor(bin)), alpha = 0.7 ) +
      scale_fill_manual(values = rivmap_colors, #from config
                        breaks = rivbreaks,
                        labels = rivmap_labs,
                        limits = as.factor(rivbreaks),
                        name = rseg_leg_title) +
      theme(legend.spacing.y = unit(0.1, 'cm')) + #adjust spacing of legend 
      guides(fill = guide_legend(byrow = TRUE))
  }
  # Tidal Rivsegs
  map <- map + new_scale("fill") +
    geom_sf(data = rivsegTidal, inherit.aes = FALSE, aes(fill=bundle), alpha = 1) +
    scale_fill_manual(values = colors_sf["tidal",], #color set in config
                      breaks = "watershed",
                      labels = "Tidal/Unmodeled",
                      name = NULL) 
    # Flowlines & Waterbodies
  map <- map + geom_sf(data = nhd_plot, 
            inherit.aes=FALSE, color= colors_sf["nhd",], 
            mapping=aes(lwd=nhd_plot$StreamOrde), #line thickness based on stream order
            show.legend=FALSE) + 
    scale_linewidth(range= c(0.4,2), guide = FALSE) 
  map <- map + geom_sf(data = rbind(nhd$off_network_wtbd, nhd$network_wtbd),  
            inherit.aes=FALSE, fill= colors_sf["nhd",], size=1) 
  # Mapping all Borders (basins, localities, regions)
  if (origin_type == "region") { 
    map <- map +
      new_scale("color") + new_scale("linetype") + new_scale("linewidth") +
      geom_sf(data= borders, inherit.aes=FALSE, fill=NA,
              aes(color= bundle,
                  lwd= as.numeric(mgsub(bundle, pattern=c("county","watershed","region"), replacement=c(2.5,textsize[6],4.5))),
                  linetype= bundle )
      ) +
      scale_linetype_manual(values= c("region"= 1,"county"= 1,"watershed"= 2), 
                            labels= c("Region","County","Basin"),
                            name= "Borders"
      ) +    
      scale_colour_manual(values= c(colors_sf[c("region","county","rsegs"),]) ,
                          breaks= c("region","county","watershed"),
                          labels= c("Region","County","Basin"),
                          name= "Borders",
      ) +
      scale_linewidth(range= range(c(2.5,textsize[6],4.5)), 
                      breaks= c(4.5,2.5,textsize[6]),
                      labels= c("Region","County","Basin"),
                      name= "Borders")
  } else {
    map <- map + 
      new_scale("color") + new_scale("linetype") + new_scale("linewidth") +
      geom_sf(data= borders, inherit.aes=FALSE, fill=NA,
              aes(color= bundle,
                  lwd= as.numeric(mgsub(bundle, pattern=c("county","watershed"), replacement=c(2.5,textsize[6]))),
                  linetype= bundle )
      )  +
      scale_linetype_manual(values= c("county"= 1,"watershed"= 2),
                            labels= c("County","Basin"),
                            name= "Borders"
      ) +
      scale_colour_manual(values= c(colors_sf[c("county","rsegs"),]) ,
                          breaks= c("county","watershed"),
                          labels= c("County","Basin"),
                          name= "Borders",
      ) +
      scale_linewidth(range= range(c(textsize[6],4.5)),
                      breaks= c(2.5,textsize[6]),
                      labels= c("County","Basin"),
                      name= "Borders")
  }
  map <- map + 
    new_scale("color") + new_scale("linetype") + new_scale("linewidth") +
    # Road Lines
    geom_sf(data = roads_plot, inherit.aes=FALSE, color= colors_sf["roads",], fill=NA, lwd=1, linetype="twodash")
    # City Points
  map <- map + new_scale("color") + new_scale("size") +
    geom_point(data = labelsP[labelsP$class=="majC"|labelsP$class=="town",], 
               aes(x=lng, y=lat), color= colors_sf["citypts",], size=2)
    # Facility Labels Placeholder (to have other labels repel)
  map <- map + geom_text(data = mp_layer, aes(lng, lat, label=NUM),colour=NA,size=textsize[4],check_overlap=TRUE)
    # Road Labels --- enlarges extent of map too much
  map <- map + new_scale("color") + new_scale("fill") +
    geom_label_repel(data = labelsP[labelsP$class == c("I","S","U"), ],
                     aes(x=lng, y=lat, label=label, 
                         fontface=fontface, family=fontfam,
                         color=as.factor(colcode), 
                         fill=fillcode
                     ), 
                     show.legend=NA,
                     size=textsize[1],
                     label.r=0.6, label.size=0.12, 
                     max.overlaps=4
    ) +
    scale_colour_manual(values=textcol, breaks=c(1,2,3), 
                        labels=c("Interstate","State Route", "US Hwy"), name="Roads") + 
    scale_fill_manual(values=label_fill, breaks=c(1,2,3), 
                      labels=c("Interstate","State Route", "US Hwy"), name="Roads")
    
    # Rivseg Tidal Labels- not fully functional
    #geom_text(data = rivsegTidal, aes(x=lng, y=lat, label=riverseg1),color="blue",size=textsize[5],check_overlap=TRUE)+
    
    # Basin Labels (by riverseg ID)
    map <- map + geom_text(data = segs, aes(x=lng, y=lat, label=riverseg),color="black",size=textsize[5],check_overlap=TRUE)
    # Text Labels
    map <- map + new_scale("size") + new_scale("color") +
    geom_text_repel(data = labelsP[!(labelsP$class == "I" | labelsP$class == "S" | labelsP$class == "U"), ], #labels other than roads
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
    ) + 
    scale_size(range= range(textsize[2:4]), breaks= textsize[2:4]) + 
    scale_colour_manual(values=textcol, breaks=seq(1,length(textcol)), guide=FALSE) +
    scale_x_continuous(limits = bbox_points$x, expand = c(0, 0)) +
    scale_y_continuous(limits = bbox_points$y, expand = c(0, 0))   
  
  ## Plotting sources/MPs
  if (featr_type == "source") {
    map <- map +
      # Plotting using bins in a single layer:
      new_scale("size") + new_scale("color") + 
      
      geom_point(data = mp_layer_plot, aes(x = lng, y = lat, 
                                           color = Source_Type, size = bin),
                 shape = 19) +
      
      scale_size_binned(range = c(2,20),
                        breaks = breaks,
                        labels = labs,
                        limits = lims,
                        name = legend_title[1],
                        guide= guide_legend(override.aes=list(color = colors_metric["Groundwater",],
                                                              fill = colors_metric["Surface Water",],
                                                              stroke = seq(3,7,length.out=length(breaks)),
                                                              shape=21 ))
                        ) +
      
      scale_colour_manual(values= colors_metric[c("Surface Water", "Groundwater"),] ,
                          breaks= c("Surface Water", "Groundwater"),
                          labels= c("Surface Water", "Groundwater"),
                          name= "Source Type",
                          guide= guide_legend(override.aes=list(size=9))
      )            
  }  else if (featr_type == "facility") { ## Plotting facilities 
    map <- map + 
      new_scale("size") +
      geom_point(data = mp_layer_plot, aes(x = lng, y = lat, 
                                           size = bin), color= colors_metric["Surface Water",],
                 shape = 19) +
      
      scale_size_binned(range = c(2,20), 
                        breaks = breaks, 
                        labels = labs,
                        limits = lims,
                        name = legend_title[1]) 
    #      theme(legend.spacing.y = unit(0.1, 'cm')) + #spacing out items in legend 
    #      guides(size = guide_legend(byrow = TRUE))
  }
  
  # Source or Facility Labels
  map <- map +
    geom_text(data = mp_layer, 
              aes(lng, lat, label=NUM, fontface="bold"), 
              color="black", size=textsize[5], check_overlap=TRUE)
    
  map <- map + geom_sf(data = nonbasin, inherit.aes=FALSE, color=NA, fill= colors_sf["shadow",], lwd=1 ) # Reverse Fill
    
  map <- map + ggsn::scalebar(data = bbox_sf, dist= round((distance/20),digits=0), # previously: data = rsegs
                   dist_unit='mi', location='bottomleft', transform=TRUE, model='WGS84', 
                   st.bottom=FALSE, st.size=textsize[4], st.dist=0.03, anchor = anchor_vect #,box.color="#FF00FF", border.size=12 
    )
  map <- map + ggspatial::annotation_north_arrow(which_north="true", location="tr",
                                      height= unit(4,"cm"), width= unit(3, "cm"), 
                                      style= north_arrow_orienteering(text_size=35)
                                      )
                                        
  return(map)
}


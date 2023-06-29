## Establishing a function to generate maps when given data and aesthetics 
# Loading required libraries for mapping
library(sp)
library(rgeos)
library(ggmap)
library(raster)
library(ggplot2)
library(ggnewscale)   
library(ggsn)
library(ggspatial)
library(ggrepel)
library(geosphere)

## nhd layer will be pulled and processed before function is called but filtering of flowlines to plot will be done within this function 
## bbox should come in with format of named list of coords: xmin, ymin, xmax, ymax
## metric is the specific name of the value/metric that bubbles will be sized with, includes runid & metric name (e.g. runid_11_wd_mgd)
## type will be either basin, locality, or region

fn_mapgen <- function(type, metric, rivseg, bbox, segs, facils, counties, roads, nhd, labelsP, locality, region) { 
  
  #For the scalebar:  
  bbox_points <- data.frame(long = c(bbox[1], bbox[3]), lat = c(bbox[2], bbox[4]))
  colnames(bbox_points) <- c('x','y')
  bbox_sf <- st_as_sf(bbox_points, coords = c('x','y'), crs = 4326) # for use within scalebar 
  anchor_vect <- c(x = (((bbox_points$x[2] - bbox_points$x[1])/3) + bbox_points$x[1])-0.45, y = bbox_points$y[1]+(bbox_points$y[1])*0.001)
  
  
 #Find distance of diagonal of bbox in miles -- for filtering what will be plotted
 #distance used instead of 'extent' because DEQ vocab has extent synonymous w bbox  
  distance <- data.frame(lng = bbox[c("xmin", "xmax")], lat = bbox[c("ymin", "ymax")])
  distance <-  distHaversine(distance) / 1609.34 #distHaversine() defaults to meters, so convert to miles
  
 #Generate basemap using the given boundary box 
  bbox <- setNames(st_bbox(bbox), c("left", "bottom", "right", "top")) #required to use get_stamenmap() 
  basemap_0 <- ggmap::get_stamenmap(maptype="terrain-background", color="color", bbox=bbox, zoom=10) #used for reverse fill
  basemap <- ggmap(basemap_0)

  #Reverse polygon fill (highlight basin) -- for type basin
  bb <- unlist(attr(basemap_0, "bb"))
  coords <- cbind( bb[c(2,2,4,4)], bb[c(1,3,3,1)] )
  basemap_0 <- sp::SpatialPolygons(
    list(sp::Polygons(list(Polygon(coords)), "id")), 
    proj4string = CRS(proj4string(segs$basin_sp)))
  remove(coords) #job done
  
  nonbasin <- raster::erase(basemap_0, segs$basin_sp)
  nonbasin <- st_as_sf(nonbasin)
  st_crs(nonbasin) <- 4326
  
  #Lighten terrain basemap -- this might be what is causing error in locality mapping
  basemap_0 <- st_as_sf(basemap_0)
  st_crs(basemap_0) <- 4326
    
 #Filtering what's plotted by size of boundary box  
  if(distance > 300) {
    #zoom = 8 #basemap resolution
    nhd$plot<- nhd$flowline[nhd$flowline$StreamOrde!=1 & nhd$flowline$StreamOrde!=2 & nhd$flowline$StreamOrde!=3,]
    roads$plot <- roads$sf[roads$sf$RTTYP=="I",]
    labelsP <- labels[labels$class=="county" | labels$class=="majR" | labels$class=="majC" | labels$class=="I",]
    textsize <- c(4,4,5,6,  5,0) #c(I/S/U , town/majC/LakePond/str , majR , county ,   facility num , segs$basin_sf lwd)
  } else if(distance > 130){
    #zoom = 9
    nhd$plot <- nhd$flowline[nhd$flowline$StreamOrde!=1 & nhd$flowline$StreamOrde!=2,]
    roads$plot <- roads$sf
    labelsP <- labels[labels$class!="town" & labels$class!="LakePond",]
    textsize <- c(5,5,6,11,  5,1)
  } else if(distance > 70){
    #zoom = 10
    nhd$plot <- nhd$flowline[nhd$flowline$StreamOrde!=1,]
    roads$plot <- roads$sf
    labelsP <- labels[labels$class!="town"& labels$class!="LakePond",]
    textsize <- c(6,7,9,12,  5,1.2)
    labels$segsize <- as.numeric( gsub(1, 0, labels$segsize) ) #no label "lollipop" for counties @ small distances
  } else {
    #zoom = 10
    nhd$plot <- nhd$flowline
    roads$plot <- roads$sf
    labelsP <- labels
    textsize <- c(7,8,10,13,  5,1.5)
    labels$segsize <- as.numeric( gsub(1, 0, labels$segsize) ) 
  }
  st_crs(nhd$plot) <- 4326  

  #For map title:
  if (type == "basin") {
    title <- ( paste("Basin Upstream of", segs$basin$name[segs$basin$riverseg==rivseg] , rivseg, sep=" ") ) 
  } 
  if (type == "locality") {
    title <- paste0(locality)  
  }  
  if (type == "region") {
    title <- paste0(region)  
  } 
  
 #Generate map gg object
  map <- basemap + #ggplot2::
    # Titles
    theme(text=element_text(size=30), title=element_text(size=40),
          axis.title.x=element_blank(), axis.title.y=element_blank()  ) +
    ggtitle(title) +
    # Lighten base-map to help readability
    geom_sf(data = basemap_0, inherit.aes=FALSE, color=NA, fill="honeydew", alpha=0.3) +
    # Flowlines & Waterbodies
    geom_sf(data = nhd$plot, 
            inherit.aes=FALSE, color="deepskyblue3", 
            mapping=aes(lwd=nhd$plot$StreamOrde), #line thickness based on stream order
            show.legend=FALSE) + 
    scale_linewidth(range= c(0.4,2)) + 
    geom_sf(data = rbind(nhd$off_network_wtbd, nhd$network_wtbd),  
            inherit.aes=FALSE, fill="deepskyblue3", size=1) +
    # County Borders
    geom_sf(data = counties$sf, inherit.aes=FALSE, color="black", fill=NA, lwd=2.5) +
    # Road Lines
    geom_sf(data = roads$plot, inherit.aes=FALSE, color="black", fill=NA, lwd=1, linetype="twodash") +
    # City Points
    geom_point(data = labelsP[labelsP$class=="majC"|labelsP$class=="town",], 
               aes(x=lng, y=lat), color ="black", size=2) +
    # Basin Outlines
    geom_sf(data = segs$basin_sf, inherit.aes=FALSE, color="sienna1", fill=NA, lwd=textsize[6], linetype="dashed") +
    # Facility Labels Placeholder (to have other labels repel)
    geom_text(data = facils$within, aes(Longitude, Latitude, label=NUM),colour=NA,size=textsize[4],check_overlap=TRUE) +
    # Road Labels
    geom_label_repel(data = labelsP[labelsP$road=="yes",],
                     aes(x=lng, y=lat, label=name, 
                         fontface=fontface, family=fontfam,
                         color=colcode, 
                         fill=fill
                     ), 
                     show.legend=NA,
                     size=textsize[1],
                     label.r=0.6, label.size=0.12, 
                     #force=0, max.overlaps=1
    ) +
    scale_colour_manual(values=textcol, breaks=c(1,2,3),
                        labels=c("Interstate","State Route", "US Hwy"), name="") + 
    scale_fill_manual(values=label_fill, breaks=c(1,2,3),
                      labels=c("Interstate","State Route", "US Hwy"), name="" ) +
    # Text Labels
    new_scale("size") + new_scale("color") +
    geom_text_repel(data = labelsP[labelsP$road=="no",], 
                    aes(x=lng, y=lat, label=name,
                        fontface=fontface, family=fontfam, angle=angle,
                        segment.color=segcol, segment.size=segsize,
                        bg.color="white", bg.r=bg.r,
                        size=sizecode, 
                        color=colcode,
                    ), 
                    show.legend=FALSE,
                    force= 40, direction="both",
                    min.segment.length=0.5
    ) + 
    scale_size(range= range(textsize[2:4]), breaks=textsize[2:4] ) + 
    scale_colour_manual(values=textcol, breaks=seq(1,length(textcol)) ) +
    
    # Facility Points; Metric 1
    new_scale("size") + new_scale("color") +
    geom_point(data = facils$within, 
               aes(x=Longitude, y=Latitude, size= facils$within[, metric], color=facils$within[,"Source Type"]), 
               alpha=0.75, shape = 19, stroke = 0.75 ) +
    scale_size(range= c(15,30), 
               breaks= round(seq(max(facils$within[, metric], na.rm = TRUE), 0, length.out=5), digits =3), # source of error 
               labels= round(seq(max(facils$within[, metric], na.rm = TRUE), 0, length.out=5), digits=3), # source of error 
               name= legend_title[1],
               guide= guide_legend(override.aes=list(label=""))
    ) + #NOTE: two scales would need identical "name" and "labels" to become one simultaneous legend
    scale_colour_manual(values=c("#F7FF00","#FF00FF"),
                        breaks= c("Surface Water", "Groundwater"),
                        labels= c("Surface Water", "Groundwater"),
                        name= "Source Type",
                        guide= guide_legend(override.aes=list(label=""))
    ) +
    # Facility Labels
    geom_text(data = facils$within, 
              aes(Longitude, Latitude, label=NUM, fontface="bold"), 
              colour="black", size=textsize[5], check_overlap=TRUE) +
    # Reverse Fill
    geom_sf(data = nonbasin, inherit.aes=FALSE, color=NA, fill="#4040408F", lwd=1 ) +
    # Scalebar & North Arrow
    ggsn::scalebar(data = segs$basin_sf, dist= round((distance/20),digits=0), # previously: data = segs$basin_sf, or bbox_sf
                   dist_unit='mi', location='bottomleft', transform=TRUE, model='WGS84', 
                   st.bottom=FALSE, st.size=textsize[4], st.dist=0.03 #anchor = anchor_vect #,box.color="#FF00FF", border.size=12 
    ) +
    ggspatial::annotation_north_arrow(which_north="true", location="tr",
                                      height= unit(4,"cm"), width= unit(3, "cm"), 
                                      style= north_arrow_orienteering(text_size=35)
    )
  assign('map', map, envir = globalenv()) #save the map in the global environment
  
  print('Map stored in environment as: map')
  return(map)
}
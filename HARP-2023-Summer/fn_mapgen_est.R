## Establishing a function to generate maps when given data and aesthetics 
# Loading required libraries for mapping
library(sp)
library(rgeos)
library(sf)
library(nhdplusTools)
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

fn_mapgen <- function(metric, rivseg, bbox, segs, facils, counties, roads, nhd, labelsP) { 

 #Find distance of diagonal of bbox in miles -- for filtering what will be plotted
  distance <- data.frame(lng = bbox[c("xmin", "xmax")], lat = bbox[c("ymin", "ymax")])
  distance <-  distHaversine(distance) / 1609.34 #distHaversine() defaults to meters, so convert to miles
  
 #Generate basemap using the given boundary box 
  bbox <- setNames(st_bbox(bbox), c("left", "bottom", "right", "top")) #required to use get_stamenmap() 
  basemap_0 <- ggmap::get_stamenmap(maptype="terrain-background", color="color", bbox=bbox, zoom=10) #used for reverse fill
  basemap <- ggmap(basemap_0)
  
 #Reverse polygon fill (highlight basin)
  bb <- unlist(attr(basemap_0, "bb"))
  coords <- cbind( bb[c(2,2,4,4)], bb[c(1,3,3,1)] )
  basemap_0 <- sp::SpatialPolygons(
    list(sp::Polygons(list(Polygon(coords)), "id")), 
    proj4string = CRS(proj4string(segs$basin_sp)))
  remove(coords) #job done
  
  nonbasin <- raster::erase(basemap_0, segs$basin_sp)
  nonbasin <- st_as_sf(nonbasin)
  st_crs(nonbasin) <- 4326
  
 #Lighten terrain basemap
  basemap_0 <- st_as_sf(basemap_0)
  st_crs(basemap_0) <- 4326
  
 #Filtering what's plotted by size of boundary box  
  if(distance > 300) {
    #zoom = 8 #basemap resolution
    nhd$plot <- nhd$flowline[nhd$flowline$StreamOrde!=1 & nhd$flowline$StreamOrde!=2 & nhd$flowline$StreamOrde!=3,]
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
   
 #Create a vector of only the metric column of interest
  metric_vect <- facils$basin[, metric] # for use in scalebar
  max <- max(metric_vect)
  
 #Generate map gg object
  map <- basemap + #ggplot2::
    # Titles
    theme(text=element_text(size=30), title=element_text(size=40),
          axis.title.x=element_blank(), axis.title.y=element_blank()  ) +
    ggtitle( paste("Basin Upstream of", segs$basin$name[segs$basin$riverseg==rivseg] , rivseg, sep=" ") ) +
    
    # Lighten base-map to help readability
    geom_sf(data = basemap_0, inherit.aes=FALSE, color=NA, fill="honeydew", alpha=0.3) +
    # County Borders
    geom_sf(data = counties$sf, inherit.aes=FALSE, color="#0033337F", fill=NA, lwd=2.5) +
    # Flowlines & Waterbodies
    geom_sf(data = nhd$plot, 
            inherit.aes=FALSE, color="deepskyblue3", 
            mapping=aes(lwd=nhd$plot$StreamOrde), #line thickness based on stream order
            show.legend=FALSE) + 
    scale_linewidth(range= c(0.4,2)) + 
    geom_sf(data = rbind(nhd$off_network_wtbd, nhd$network_wtbd),
            inherit.aes=FALSE, color="deepskyblue3", size=1) +
    # Road Lines
    geom_sf(data = roads$plot, inherit.aes=FALSE, color="black", fill=NA, lwd=1, linetype="twodash") +
    # City Points
    geom_point(data = labelsP[labelsP$class=="majC"|labelsP$class=="town",], 
               aes(x=lng, y=lat), color ="black", size=2) +
    # Basin Outlines
    geom_sf(data = segs$basin_sf, inherit.aes=FALSE, color="sienna1", fill=NA, lwd=textsize[6], linetype="dashed") +
    
    # Facility Labels Placeholder (to have other labels repel)
    geom_text(data = facils$basin, aes(Longitude, Latitude, label=NUM),colour=NA,size=textsize[4],check_overlap=TRUE) +
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
    geom_point(data = facils$basin, 
               aes(x=Longitude, y=Latitude, size= metric, color=facils$basin[,"Source Type"]), 
               alpha=0.75, shape = 19, stroke = 0.75 ) +
#    scale_size(range= c(10,28), 
#               breaks= round(seq(max(facils$basin[, metric]), 0, length.out=5), digits =3), # source of error 
#               labels= round(seq(max(facils$basin[, metric]), 0, length.out=5), digits=3), # source of error 
#               name= legend_title[1],
#               guide= guide_legend(override.aes=list(label=""))
#    ) + #NOTE: two scales would need identical "name" and "labels" to become one simultaneous legend
    scale_colour_manual(values=c("#F7FF00","#FF00FF"),
                        breaks= c("Surface Water", "Groundwater"),
                        labels= c("Surface Water", "Groundwater"),
                        name= "Source Type",
                        guide= guide_legend(override.aes=list(label=""))
    ) +
    # Facility Labels
    geom_text(data = facils$basin, 
              aes(Longitude, Latitude, label=NUM, fontface="bold"), 
              colour="black", size=textsize[5], check_overlap=TRUE ) +
    # Reverse Fill
    geom_sf(data = nonbasin, inherit.aes=FALSE, color=NA, fill="#4040408F", lwd=1 ) +
    # Scalebar & North Arrow
    ggsn::scalebar(data = segs$basin_sf, dist= round((distance/20),digits=0), 
                   dist_unit='mi', location='bottomleft', transform=TRUE, model='WGS84', 
                   st.bottom=FALSE, st.size=textsize[4], st.dist=0.03 #,box.color="#FF00FF", border.size=12 
    ) +
    ggspatial::annotation_north_arrow(which_north="true", location="tr",
                                      height= unit(4,"cm"), width= unit(3, "cm"), 
                                      style= north_arrow_orienteering(text_size=35)
    )
  assign('map', map, envir = globalenv()) #save the map in the global environment
  
  print('Map stored in environment as: map')
}
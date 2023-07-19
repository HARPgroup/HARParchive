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
source(paste0(getwd(), '/', 'mapstyle_config.R' )) #load mapping aesthetics


## nhd layer will be pulled and processed before function is called but filtering of flowlines to plot will be done within this function 
## bbox should come in with format of named list of coords: xmin, ymin, xmax, ymax
## metric is the specific name of the value/metric that bubbles will be sized with, includes runid & metric name (e.g. runid_11_wd_mgd)
## "type" will be either basin, locality, or region
## "style" dictates which mapping aesthetics are desired from mapstyle_config.R (options right now are custom or default) 

fn_mapgen <- function(type, map_type, style, metric, rivseg, bbox, segs, counties, roads,
                      nhd, maplabs, locality, region, mp_layer, metric_unit) { 

# Combine all map labels into one df:
for(i in 1:length(maplabs)){
  if(i==1){ maplabs$all <- maplabs[[i]] }
  if(i!=1){ maplabs$all <- rbind(maplabs$all, maplabs[[i]]) }
  }

## aesthetics from styles$custom need to be joined to maplabs$all using the class column 
styles_cus <- styles$custom
maplabs_all <- maplabs$all  

# join with sqldf 
maplabs$final <- sqldf(
  "SELECT styles_cus.*, maplabs_all.*  
   FROM styles_cus 
   LEFT OUTER JOIN maplabs_all
      on (maplabs_all.class = styles_cus.class)"
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
  
 #Generate basemap using the given boundary box 
  bbox <- setNames(st_bbox(bbox), c("left", "bottom", "right", "top")) #required to use get_stamenmap() 
  basemap_0 <- ggmap::get_stamenmap(maptype="terrain-background", color="color", bbox=bbox, zoom=10) #used for reverse fill
  basemap <- ggmap(basemap_0)

 #For reverse-fill: darken area of map outside basins 
  segs$union <- st_union(segs$basin_sf)
  bbox_st <- st_as_sfc(st_bbox(bbox))
  nonbasin <- st_difference(bbox_st, segs$union) #method of erasing
  st_crs(nonbasin) <- 4326
  
 #Lighten terrain basemap 
#  bb <- unlist(attr(basemap_0, "bb"))
#  coords <- cbind( bb[c(2,2,4,4)], bb[c(1,3,3,1)] )
#  basemap_0 <- sp::SpatialPolygons(
#      list(sp::Polygons(list(Polygon(coords)), "id")), 
#      proj4string = CRS(proj4string(as_Spatial(segs$union))))
#  basemap_0 <- st_as_sf(basemap_0)
#  st_crs(basemap_0) <- 4326
    
#----Filtering what's plotted by size of boundary box---- 
  if (distance > 300) {
    #zoom = 8 #basemap resolution
    nhd$plot<- nhd$flowline[nhd$flowline$StreamOrde!=1 & nhd$flowline$StreamOrde!=2 & nhd$flowline$StreamOrde!=3,]
    roads_plot <- roads[roads$RTTYP=="I",]
    labelsP <- labels[labels$class=="county" | labels$class=="majorRiver" |
                        labels$class=="I"| labels$class=="city",]
    textsize <- c(4,4,5,6,  5,0) #c(I/S/U , town/majC/LakePond/str , majR , county ,   facility num , segs$basin_sf lwd)
  } else if (distance > 130) {
    #zoom = 9
    nhd$plot <- nhd$flowline[nhd$flowline$StreamOrde!=1 & nhd$flowline$StreamOrde!=2,]
    roads_plot <- roads
    labelsP <- labels[labels$class=="county" | labels$class=="majorRiver" | labels$class=="stream" |
                        labels$class=="I" | labels$class=="S" | labels$class=="U" |labels$class=="city",]
    textsize <- c(5,5,6,11,  5,1)
  } else if (distance > 70) {
    #zoom = 10
    nhd$plot <- nhd$flowline[nhd$flowline$StreamOrde!=1,]
    roads_plot <- roads
    labelsP <- labels[labels$class!="waterbody_sm" & labels$class!="waterbody_med" & labels$class!= "smallTown",]
    textsize <- c(6,7,9,12,  5,1.2)
    labels$segsize <- as.numeric( gsub(1, 0, labels$segsize) ) #no label "lollipop" for counties @ small distances
  } else {
    #zoom = 10
    nhd$plot <- nhd$flowline
    roads_plot <- roads
    labelsP <- labels[labels$class!="waterbody_sm" & labels$class!="waterbody_med",]
    textsize <- c(7,8,10,13,  5,1.5)
    labels$segsize <- as.numeric( gsub(1, 0, labels$segsize) ) 
  }
st_crs(nhd$plot) <- 4326  

labelsP <- labelsP[ ,!duplicated(colnames(labelsP))]
class(labelsP$bg.r) = "numeric"
  
#----Legend & Titling----
  #For map title:
  if (map_type == "basin") {
    title <- ( paste("Basin Upstream of", segs$basin$name[segs$basin$riverseg==rivseg] , rivseg, sep=" ") )
  } else if (map_type == "locality") {
    title <- paste0(locality)
  }  else if (map_type == "region") {
    title <- paste0(region)
  } 
  
  #For binned legend 
  breaks <- seq(1:7)
  lims <- c(min(breaks),max(breaks)) #limits based on range of breaks
  
  if (metric_unit == "mgd") { 
    labs = c(0.5, 1.0, 2, 10, 25, 100, 1000)
  } else if (metric_unit == "mgy") {
    labs = c(1, 5, 10, 50, 250, 1000, 10000)
  } else {
    labs = c(0.5, 1.0, 2, 10, 25, 100, 1000) #default to mgd if unit is neither mgd or mgy
  }

 #We don't want any bubbles for MPs with no metric value -- stored with bin = X
 mp_layer_plot <- mp_layer[!mp_layer$bin == "X" , ]
 class(mp_layer_plot$bin) <- "numeric" #make sure bin column is type numeric for sizing data points 
 
 
 #Merging different outline layers into 1 df for mapping & legend 
# county_outlines <- counties$sf[c("name","geometry")]
# county_outlines$class <- "countyline"
# county_outlines$color = "gray27"
# basin_outlines <- segs$basin_sf[c("name","bundle","geometry")]
# names(basin_outlines)[names(basin_outlines) == 'bundle'] <- 'class'
# basin_outlines$color = "sienna1"
# 
#if (map_type=="region") {
#  region_outline <- segs$region_sf
#  region_outline$name <- region
#  region_outline$class <- 'region'
#  names(region_outline)[names(region_outline) == 'x'] <- 'geometry'
#  st_geometry(region_outline) <- "geometry" #letting sf know which col now holds geometry
#  region_outline$color = "black"
#  outlines <- rbind(county_outlines,basin_outlines,region_outline)
#} else { #for map types other than region
#  outlines <- rbind(county_outlines,basin_outlines)
#}
 
 #Generate map gg object
  map <- basemap + #ggplot2::
    # Titles
    theme(text=element_text(size=30), title=element_text(size=40),
          axis.title.x=element_blank(), axis.title.y=element_blank()  ) +
    ggtitle(title) +
    # Lighten base-map to help readability
#    geom_sf(data = basemap_0, inherit.aes=FALSE, color=NA, fill="honeydew", alpha=0.3) +
    # Flowlines & Waterbodies
    geom_sf(data = nhd$plot, 
            inherit.aes=FALSE, color="deepskyblue3", 
            mapping=aes(lwd=nhd$plot$StreamOrde), #line thickness based on stream order
            show.legend=FALSE) + 
    scale_linewidth(range= c(0.4,2), guide = FALSE) + 
    geom_sf(data = rbind(nhd$off_network_wtbd, nhd$network_wtbd),  
            inherit.aes=FALSE, fill="deepskyblue3", size=1) +
    # County Borders
   #   new_scale("color") +
    geom_sf(data = counties$sf, color="gray27", 
            fill=NA, lwd=2.5, inherit.aes = F) +
    # Basin Outlines
    geom_sf(data = segs$basin_sf, color="sienna1", 
            fill=NA, lwd=textsize[6], linetype="dashed", inherit.aes = F) 
   
   #   scale_color_identity(guide = "legend")
  
    # Region Outline
   if (map_type == "region") { # thicker boundary around region
    map <- map + 
      geom_sf(data = segs$region_sf, color="black", fill=NA, lwd=4.5, inherit.aes=F)
   }
  
  #Mapping all borders using 1 df called outlines, which will have 1 region line for map type region
 #     new_scale_color() +
 #   geom_sf(data = outlines, inherit.aes=FALSE, mapping = aes(color=color)) + #for identity scale, or (color = class) for manual scale
    
  #     scale_color_identity(guide="legend") + 
        
 #   scale_colour_manual(values= c("gray27","sienna1","black"), ### doesnt work 
 #                       breaks= c("county","watershed","region"),
 #                       labels= c("County Border", "Basin Border", "Region Border"),
  #                      name= "Outlines",
  #                      guide= guide_legend(override.aes=list()) ) +
  
  map <- map +
    # Road Lines
    geom_sf(data = roads_plot, inherit.aes=FALSE, color="black", fill=NA, lwd=1, linetype="twodash") +
    # City Points
    geom_point(data = labelsP[labelsP$class=="majC"|labelsP$class=="town",], 
               aes(x=lng, y=lat), color ="black", size=2) +
    # Facility Labels Placeholder (to have other labels repel)
    geom_text(data = mp_layer, aes(Longitude, Latitude, label=NUM),colour=NA,size=textsize[4],check_overlap=TRUE) +
    # Road Labels
    geom_label_repel(data = labelsP[labelsP$class == c("I","S","U"), ], #road column no longer exists
                     aes(x=lng, y=lat, label=label, 
                         fontface=fontface, family=fontfam,
                         color=as.factor(colcode), 
                         fill=as.factor(fillcode)
                     ), 
                     show.legend=NA,
                     size=textsize[1],
                     label.r=0.6, label.size=0.12, 
                     #force=0, max.overlaps=1
    ) +
    scale_colour_manual(values=textcol, breaks=c(1,2,3), 
                        labels=c("Interstate","State Route", "US Hwy"), name="") + 
    scale_fill_manual(values=label_fill, breaks=c(1,2,3), #Error: Continuous value supplied to discrete scale
                      labels=c("Interstate","State Route", "US Hwy"), name="" ) +
    # Basin Labels (by riverseg ID)
    geom_text(data = segs$basin_sf, aes(x=lng, y=lat, label=riverseg),color="sienna",size=textsize[5],check_overlap=TRUE) + # no error up to here 
    # Text Labels
    new_scale("size") + new_scale("color") +
    #lb_wtbd <- lb_wtbd[!(lb_wtbd$gnis_name==' ' | lb_wtbd$gnis_name=='Noname')
    geom_text_repel(data = labelsP[!(labelsP$class == "I" | labelsP$class == "S" | labelsP$class == "U"), ], #road column no longer exists
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
    scale_size(range= range(textsize[2:4]), breaks=textsize[2:4] ) + 
    scale_colour_manual(values=textcol, breaks=seq(1,length(textcol)), guide=FALSE ) 
    
## Plotting sources/MPs
  if (type == "source") {
    map <- map +
    # Plotting using bins in a single layer:
    new_scale("size") + new_scale("color") +
    
    geom_point(data = mp_layer_plot, aes(x = Longitude, y = Latitude, 
              color = Source_Type, size = bin), 
              shape = 19) +
      
    scale_size_binned(range = c(2,20), 
                      breaks = breaks, 
                      labels = labs,
                      limits = lims,
                      name = legend_title[1]) +
    
    scale_colour_manual(values=c("#F7FF00","#FF00FF"),
                        breaks= c("Surface Water", "Groundwater"),
                        labels= c("Surface Water", "Groundwater"),
                        name= "Source Type",
                        guide= guide_legend(override.aes=list(label="", size =5)) )            
  }  else if (type == "facility") { ## Plotting facilities 
    map <- map + 
      new_scale("size") +
    geom_point(data = mp_layer_plot, aes(x = Longitude, y = Latitude, 
              size = bin), color = "#F7FF00",
              shape = 19) +

      scale_size_binned(range = c(2,20), 
                        breaks = breaks, 
                        labels = labs,
                        limits = lims,
                        name = legend_title[1])
  }
 
  # Source or Facility Labels
  map <- map +
  geom_text(data = mp_layer, 
            aes(Longitude, Latitude, label=NUM, fontface="bold"), 
            colour="black", size=textsize[5], check_overlap=TRUE) +
    
  geom_sf(data = nonbasin, inherit.aes=FALSE, color=NA, fill="#4040408F", lwd=1 ) + # Reverse Fill

  ggsn::scalebar(data = bbox_sf, dist= round((distance/20),digits=0), # previously: data = segs$basin_sf
                  dist_unit='mi', location='bottomleft', transform=TRUE, model='WGS84', 
                  st.bottom=FALSE, st.size=textsize[4], st.dist=0.03, anchor = anchor_vect #,box.color="#FF00FF", border.size=12 
    ) +
  ggspatial::annotation_north_arrow(which_north="true", location="tr",
                                    height= unit(4,"cm"), width= unit(3, "cm"), 
                                    style= north_arrow_orienteering(text_size=35)
    )
  assign('map', map, envir = globalenv()) #save the map in the global environment
  #print('Map stored in environment as: map')
  #return(map)
}

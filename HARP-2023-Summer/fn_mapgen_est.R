## Etsablishing a function to generate maps when given data and aesthetics 
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

# Parameters likely needed for mapping: boundarybox/extent, points layer, shapes/boundary layer, aesthetics data frame, rsegs layer (segs), metric to be plotted

fn_mapgen <- function(bbox, zoomval, labels, boundaries, map_type, segs, metric) {
  
 #Assumption that the labels data frame contains coordinates, labels, and aesthetics
 #Extent should be of type bbox with correct labels right,left etc.
 #Metrics will be contained in the points data frame as they're associated with source or facility point locations
 #Metric param will be the specific value that bubble sizes are based on 
  
 #Generate nhd layer based on the boundary box provided 
 
  nhd  <- plot_nhdplus(bbox=bbox, actually_plot = FALSE)
  
  # River & stream labels
  ## major rivs = orders 5 & 6; streams = order 4
  lb_rivr <- nhd$flowline[nhd$flowline$gnis_name!=' ' & #name!=blank & order 4, 5, or 6
                            (nhd$flowline$StreamOrde==6 | nhd$flowline$StreamOrde==5 | nhd$flowline$StreamOrde==4),] 
  ## no duplicate names; prioritize higher order names and then the longest segment of each duplicate
  lb_rivr <- lb_rivr[order(-lb_rivr$StreamOrde, lb_rivr$gnis_name, -lb_rivr$LENGTHKM) & !duplicated(lb_rivr$gnis_name),]
  ## shorten long names
  lb_rivr$gnis_name <- mgsub(lb_rivr$gnis_name, 
                             c('North Fork','South Fork','East Fork','West Fork','Middle Fork'), #pattern
                             c('NF','SF','EF','WF','MF')) #replacement
  lb_rivr$StreamOrde <- mgsub(lb_rivr$StreamOrde, c(4,5,6), c("str","majR","majR"))
  ## calculate label coordinates
  lb_rivr <- centroid_coords(lb_rivr, "geometry")
  
  # Waterbody labels
  lb_wtbd <- rbind(nhd$network_wtbd, nhd$off_network_wtbd)
  ## remove ones without names & filter to largest 50%
  lb_wtbd <- lb_wtbd[!(lb_wtbd$gnis_name==' ' | lb_wtbd$gnis_name=='Noname') & lb_wtbd$AreaSqKM > quantile(lb_wtbd$AreaSqKM, 0.5),]
  lb_wtbd <- centroid_coords(lb_wtbd, "geometry")
  
 #Generate basemap using the given boundary box/extent, map type, and zoom  
#  bbox <- setNames(st_bbox(bbox), c("left", "bottom", "right", "top")) #otherwise get_stamenmap() won't run -- example of setting bbox names
  basemap_0 <- ggmap::get_stamenmap(maptype=map_type, color="color", bbox=bbox, zoom=zoomval)
  basemap <- ggmap(basemap_0)
  
  ######################################################################
  # generate map gg object
  # copied from the mapping_codeReview rmd
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
               aes(x=Longitude, y=Latitude, size=metric_runid1, color=facils$basin[,"Source Type"]), 
               alpha=0.75, shape = 19, stroke = 0.75 ) +
    scale_size(range= c(10,28), 
               breaks= seq(max(facils$basin$metric_runid1), 0, length.out=5), 
               labels= round(seq(max(facils$basin$metric_runid1), 0, length.out=5), digits=3),
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
    geom_text(data = facils$basin, 
              aes(Longitude, Latitude, label=NUM, fontface="bold"), 
              colour="black", size=textsize[5], check_overlap=TRUE ) +
    # Reverse Fill
    geom_sf(data = nonbasin, inherit.aes=FALSE, color=NA, fill="#4040408F", lwd=1 ) +
    # Scalebar & North Arrow
    ggsn::scalebar(data = segs$basin_sf, dist= round((extent/20),digits=0), 
                   dist_unit='mi', location='bottomleft', transform=TRUE, model='WGS84', 
                   st.bottom=FALSE, st.size=textsize[4], st.dist=0.03 #,box.color="#FF00FF", border.size=12 
    ) +
    ggspatial::annotation_north_arrow(which_north="true", location="tr",
                                      height= unit(4,"cm"), width= unit(3, "cm"), 
                                      style= north_arrow_orienteering(text_size=35)
    )
  
  return(map)
}
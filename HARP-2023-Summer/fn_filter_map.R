##Filtering elements to be mapped & labeled 
## for use within fn_mapgen_st.R

fn_filter_map <- function(labels, nhd, roads, distance) {

if (distance > 300) {
  #zoom = 8 #basemap resolution
  nhd_plot<- nhd$flowline[nhd$flowline$StreamOrde!=1 & nhd$flowline$StreamOrde!=2 & nhd$flowline$StreamOrde!=3,]
  roads_plot <- roads[roads$RTTYP=="I",]
  labelsP <- labels[labels$class=="county" | labels$class=="majorRiver" |
                      labels$class=="I"| labels$class=="city" | labels$class!="waterbody_lg",]
  textsize <- c(4,4,5,6,  5,0) #c(I/S/U , town/majC/LakePond/str , majR , county ,   facility num , segs$basin_sf lwd)
} else if (distance > 130) {
  #zoom = 9
  nhd_plot <- nhd$flowline[nhd$flowline$StreamOrde!=1 & nhd$flowline$StreamOrde!=2,]
  roads_plot <- roads
  labelsP <- labels[labels$class=="county" | labels$class=="majorRiver" |
                      labels$class=="I" | labels$class=="city",]
  textsize <- c(5,5,6,11,  5,1)
} else if (distance > 70) {
  #zoom = 10
  nhd_plot <- nhd$flowline[nhd$flowline$StreamOrde!=1,]
  roads_plot <- roads
  labelsP <- labels[labels$class!="waterbody_sm" & labels$class!="waterbody_med" & labels$class!= "smallTown",]
  textsize <- c(6,7,9,12,  5,1.2)
  labels$segsize <- as.numeric( gsub(1, 0, labels$segsize) ) #no label "lollipop" for counties @ small distances
} else {
  #zoom = 10
  nhd_plot <- nhd$flowline
  roads_plot <- roads
  labelsP <- labels[labels$class!="waterbody_sm" & labels$class!="waterbody_med",]
  textsize <- c(7,8,10,13,  5,1.5)
  labels$segsize <- as.numeric( gsub(1, 0, labels$segsize) ) 
}
  assign('labelsP', labelsP, envir = globalenv())
  assign('nhd_plot', nhd_plot, envir = globalenv())
  assign('roads_plot', roads_plot, envir = globalenv())
  assign('textsize', textsize, envir = globalenv())
}
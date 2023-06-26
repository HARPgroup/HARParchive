# defaults and custom mapping aesthetics configuration for fn_mapgen()

#--Current Possible Text Layers: Require MANY font/color/size aesthetics--
# Roads: Interstate(I), US Hwy(U), State Rte(S)
# Cities: Major Cities, Towns
# Water: Major Rivers (nhd class 5,6), Streams (nhd class 4), Waterbodies (nhd network and off network wtbd's)
# Polygons: Counties, Potentially Riversegs/Watersheds

# Separate: facility point labels

#--Current Possible Point Layers: Require colors & point sizes--
# Cities: Major Cities, Towns
# Facilities: Metric-scaled bubbles of either:
### - surface water for model data at the facility-level (default color 1)
### - distinct surface water (default color 1) and groundwater (default color 2) for data at MP-level
### - neither purely surface water nor groundwater at the facility level (default color 3)

#--Current Possible sf Layers: Require colors & line widths--
# Roads: Interstate(I), US Hwy(U), State Rte(S)
# NHD: Whichever flowlines were kept for the extent, scaled by streamOrde ; Waterbodies
# Polygons: Counties, Reverse fill of basin, Riverseg borders

#- - - - - - - - - - - 

colors <- list()
styles <- list()
#----Custom Aesthetics:----
colors$custom$sf <- data.frame(geomsf_layer=c("lightenBase","county","nhd","roads","citypts","rsegs","shadow"),
                               color=c("honeydew","#0033337F","deepskyblue3","black","black","sienna1", "#4040408F")
)
colors$custom$metrics <- data.frame(SourceType=c("Surface Water","Groundwater","Nondistinctive"),
                                    color=c("#F7FF00","#FF00FF","#FF9851")
)
colors$custom$text <- c("red","white","black","deepskyblue4","#003333")
colors$custom$fill <- c("blue","#0B5F14","white")


styles$custom$a <- data.frame(class="I", #label type identifier; should match up with the labels df so aesthetics can be joined to it
                               fontface="plain", #plain, bold, italic, bold.italic
                               fontfam="Comic Sans MS",
                               angle=0, #angle of the text; e.g. river names are tilted 15 degrees for clarity
                               bg.r="NA", #thickness of the text's white outline; not applicable to labels with backgrounds (i.e. roads)
                               segsize=0, #lollipop label line width
                               segcol="NA", #lollipop label line color; typically matches colcode
                               colcode=1, #denotes color of the text; corresponds to item in textcol
                               sizecode=1, #size of text; ranges smallest=1 to largest=4; corresponds to variable textsize, which depends on extent
                               fillcode=1 #denotes label background colors; corresponds to item in fillcol; applies to road bubbles
)
styles$custom$b <- data.frame(class="S",
                               fontface="plain",
                               fontfam="Comic Sans MS",
                               angle=0,
                               bg.r="NA",
                               segsize=0,
                               segcol="NA",
                               colcode=2,
                               sizecode=1, 
                               fillcode=2
)
styles$custom$c <- data.frame(class="U",
                               fontface="plain",
                               fontfam="Comic Sans MS",
                               angle=0,
                               bg.r="NA",
                               segsize=0,
                               segcol="NA",
                               colcode=3,
                               sizecode=1, 
                               fillcode=3
)
styles$custom$d <- data.frame(class="town",
                               fontface="plain",
                               fontfam="sans",
                               angle=0,
                               bg.r=.05,
                               segsize=.5,
                               segcol=3,
                               colcode=3,
                               sizecode=2, 
                               fillcode="NA"
)
styles$custom$e <- data.frame(class="majC",
                               fontface="plain",
                               fontfam="sans",
                               angle=0,
                               bg.r=.05,
                               segsize=.5,
                               segcol=3,
                               colcode=3,
                               sizecode=2, 
                               fillcode="NA"
)
styles$custom$f <- data.frame(class="LakePond",
                               fontface="bold",
                               fontfam="serif",
                               angle=0,
                               bg.r=.1,
                               segsize=.5,
                               segcol=4,
                               colcode=4,
                               sizecode=2, 
                               fillcode="NA"
)
styles$custom$g <- data.frame(class="str",
                               fontface="bold",
                               fontfam="serif",
                               angle=15,
                               bg.r=.1,
                               segsize=.5,
                               segcol=4,
                               colcode=4,
                               sizecode=2, 
                               fillcode="NA"
)
styles$custom$h <- data.frame(class="majR",
                               fontface="bold",
                               fontfam="serif",
                               angle=15,
                               bg.r=.05,
                               segsize=.75,
                               segcol=4,
                               colcode=4,
                               sizecode=3, 
                               fillcode="NA"
)
styles$custom$i <- data.frame(class="county",
                               fontface="bold.italic",
                               fontfam="Luminari",
                               angle=0,
                               bg.r=.03,
                               segsize=.1,
                               segcol=5,
                               colcode=5,
                               sizecode=4, 
                               fillcode="NA"
)


#----Default Aesthetics:----
colors$default$sf <- data.frame(geomsf_layer=c("lightenBase","county","nhd","roads","citypts","rsegs","shadow"),
                               color=c("honeydew","#0033337F","deepskyblue3","black","black","sienna1", "#4040408F")
)
colors$default$metrics <- data.frame(SourceType=c("Surface Water","Groundwater","Nondistinctive"),
                                    color=c("#F7FF00","#FF00FF","#FF9851")
)
colors$default$text <- c("red","white","black","deepskyblue4","#003333")
colors$default$fill <- c("blue","#0B5F14","white")


styles$default$a <- data.frame(class="Interstate",
                              fontface="plain",
                              fontfam="Comic Sans MS",
                              angle=0,
                              bg.r="NA",
                              segsize=0,
                              segcol="NA",
                              colcode=1,
                              sizecode=1, 
                              fillcode=1
)
styles$default$b <- data.frame(class="StateRoute",
                                 fontface="plain",
                                 fontfam="Comic Sans MS",
                                 angle=0,
                                 bg.r="NA",
                                 segsize=0,
                                 segcol="NA",
                                 colcode=2,
                                 sizecode=1, 
                                 fillcode=2
)
styles$default$c <- data.frame(class="USHwy",
                                       fontface="plain",
                                       fontfam="Comic Sans MS",
                                       angle=0,
                                       bg.r="NA",
                                       segsize=0,
                                       segcol="NA",
                                       colcode=3,
                                       sizecode=1, 
                                       fillcode=3
)
styles$default$d <- data.frame(class="Town",
                                       fontface="plain",
                                       fontfam="sans",
                                       angle=0,
                                       bg.r=.05,
                                       segsize=.5,
                                       segcol=3,
                                       colcode=3,
                                       sizecode=2, 
                                       fillcode="NA"
)
styles$default$e <- data.frame(class="MajorCity",
                               fontface="plain",
                               fontfam="sans",
                               angle=0,
                               bg.r=.05,
                               segsize=.5,
                               segcol=3,
                               colcode=3,
                               sizecode=2, 
                               fillcode="NA"
)
styles$default$f <- data.frame(class="Waterbody",
                               fontface="bold",
                               fontfam="serif",
                               angle=0,
                               bg.r=.1,
                               segsize=.5,
                               segcol=4,
                               colcode=4,
                               sizecode=2, 
                               fillcode="NA"
)
styles$default$g <- data.frame(class="Stream",
                               fontface="bold",
                               fontfam="serif",
                               angle=15,
                               bg.r=.1,
                               segsize=.5,
                               segcol=4,
                               colcode=4,
                               sizecode=2, 
                               fillcode="NA"
)
styles$default$h <- data.frame(class="MajorRivr",
                               fontface="bold",
                               fontfam="serif",
                               angle=15,
                               bg.r=.05,
                               segsize=.75,
                               segcol=4,
                               colcode=4,
                               sizecode=3, 
                               fillcode="NA"
)
styles$default$i <- data.frame(class="County",
                               fontface="bold.italic",
                               fontfam="Luminari",
                               angle=0,
                               bg.r=.03,
                               segsize=.1,
                               segcol=5,
                               colcode=5,
                               sizecode=4, 
                               fillcode="NA"
)



#----Creating Final List of Map Styles Data Frames:----
for(i in 1:length(styles)){
  end <- length(styles[[i]])
  
  for(s in 1:length(styles[[i]])){
    if(s==1){
      styles[[i]]$all <- styles[[i]][[s]]
    }
    if(s!=1){
      styles[[i]]$all <- rbind(styles[[i]]$all, styles[[i]][[s]])
    }
    if(s==end){
      styles[[i]] <- data.frame(styles[[i]]$all)
      remove(end,i,s)
    }
  }
  
}
# Within either the RMD or the fn_mapgen(), we will join the desired map style configuration 
## onto the labels data frame by matching up the "class" columns. 



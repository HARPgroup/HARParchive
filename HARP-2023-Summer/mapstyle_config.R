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
styles <- list()
# Custom Aesthetics:
styles$custom$interstate <- data.frame(class="Interstate",
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
styles$custom$state_rte <- data.frame(class="StateRoute",
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

# Default Aesthetics:
styles$default$interstate <- data.frame(class="Interstate",
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
styles$default$state_rte <- data.frame(class="StateRoute",
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



# Final List of Map Style Aesthetics as data frames:
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



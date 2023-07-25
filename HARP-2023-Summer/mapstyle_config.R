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

#Everything in custom aesthetics can be adjusted, if not changed, then defaults(below custom) will be used 
#NHD flowline & waterbody classification & substitution:
#flowlines:
nhd_rivname_pattern <- c('North Fork','South Fork','East Fork','West Fork','Middle Fork','North Branch','South Branch') #pattern to be replaced in NHD river/stream names 
nhd_rivname_replacements <- c('NF','SF','EF','WF','MF','NB','SB') #replacements                    


nhd_streamorders <- c(4,5,6) #nhd streamorders to be classified & labeled                       
nhd_streamclasses <- c("stream","majorRiver","majorRiver") #assigned to the classes above in fn_nhd_labs, and determine label aesthetics outlined in styles below
#waterbodies:
wtbd_names_rm <- paste("Pond","Millpond","Swamp", sep = "|") #waterbody names containing these will be removed from labeling by fn_nhd_labs
wtbd_sm_pct_range <- c(0.25,0.6) #quantile range for classifying waterbodies as small for mapping
wtbd_med_pct_range <- c(0.6,0.9) #quantile range for classifying waterbodies as medium for mapping
#large waterbodies will use max value from med_pct_range as min
  
styles <- list()
#----Custom Aesthetics:----
styles$custom$color$sf <- data.frame(row.names=c("lightenBase","county","nhd","roads","citypts","rsegs","region","shadow"),
                               color=c("honeydew","#0033337F","deepskyblue3","black","black","sienna1","black", "#4040408F")
                              )
styles$custom$color$metrics <- data.frame(row.names=c("Surface Water","Groundwater","Nondistinctive"),
                                    color=c("#F7FF00","#FF00FF","#FF9851")
                                    )
styles$custom$color$text <- data.frame(row.names=c("interstate","staterte","ushwy_cities","nhd","county","rsegs"),
                                 color=c("red","white","black","deepskyblue4","#003333", "sienna3")
                                 )
styles$custom$color$fill <- data.frame(row.names=c("interstate","staterte","ushwy"),
                                 color=c("blue","#0B5F14","white")
                                 )


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
styles$custom$e <- data.frame(class="city",
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
styles$custom$f <- data.frame(class="waterbody_lg",
                               fontface="bold",
                               fontfam="serif",
                               angle=0,
                               bg.r=.1,
                               segsize=.5,
                               segcol=4,
                               colcode=4,
                               sizecode=1, 
                               fillcode="NA"
)
styles$custom$g <- data.frame(class="waterbody_med",
                              fontface="bold",
                              fontfam="serif",
                              angle=0,
                              bg.r=.1,
                              segsize=.5,
                              segcol=4,
                              colcode=4,
                              sizecode=.75, 
                              fillcode="NA"
)
styles$custom$h <- data.frame(class="waterbody_sm",
                              fontface="bold",
                              fontfam="serif",
                              angle=0,
                              bg.r=.1,
                              segsize=.5,
                              segcol=4,
                              colcode=4,
                              sizecode=.5, 
                              fillcode="NA"
)
styles$custom$i <- data.frame(class="stream",
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
styles$custom$j <- data.frame(class="majorRiver",
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
styles$custom$k <- data.frame(class="county",
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
styles$custom$l <- data.frame(class="smallTown",
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


#----Default Aesthetics:----
styles$default$color$sf <- data.frame(row.names=c("lightenBase","county","nhd","roads","citypts","rsegs","region","shadow"),
                                     color=c("honeydew","#0033337F","deepskyblue3","black","black","sienna1","black", "#4040408F")
                                     )
styles$default$color$metrics <- data.frame(row.names=c("Surface Water","Groundwater","Nondistinctive"),
                                          color=c("#F7FF00","#FF00FF","#FF9851")
                                          )
styles$default$color$text <- data.frame(row.names=c("interstate","staterte","ushwy_cities","nhd","county","rsegs"),
                                       color=c("red","white","black","deepskyblue4","#003333", "sienna3")
                                       )
styles$default$color$fill <- data.frame(row.names=c("interstate","staterte","ushwy"),
                                       color=c("blue","#0B5F14","white")
                                       )


styles$default$a <- data.frame(class="I", #label type identifier; should match up with the labels df so aesthetics can be joined to it
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
styles$default$b <- data.frame(class="S",
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
styles$default$c <- data.frame(class="U",
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
styles$default$d <- data.frame(class="town",
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
styles$default$e <- data.frame(class="city",
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
styles$default$f <- data.frame(class="waterbody_lg",
                              fontface="bold",
                              fontfam="serif",
                              angle=0,
                              bg.r=.1,
                              segsize=.5,
                              segcol=4,
                              colcode=4,
                              sizecode=1, 
                              fillcode="NA"
)
styles$default$g <- data.frame(class="waterbody_med",
                              fontface="bold",
                              fontfam="serif",
                              angle=0,
                              bg.r=.1,
                              segsize=.5,
                              segcol=4,
                              colcode=4,
                              sizecode=.75, 
                              fillcode="NA"
)
styles$default$h <- data.frame(class="waterbody_sm",
                              fontface="bold",
                              fontfam="serif",
                              angle=0,
                              bg.r=.1,
                              segsize=.5,
                              segcol=4,
                              colcode=4,
                              sizecode=.5, 
                              fillcode="NA"
)
styles$default$i <- data.frame(class="stream",
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
styles$default$j <- data.frame(class="majorRiver",
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
styles$default$k <- data.frame(class="county",
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
styles$default$l <- data.frame(class="smallTown",
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

#----Creating Final List of Map Styles Data Frames:----
for(i in 1:length(styles)){
  end <- length(styles[[i]])
  
  for(s in 1:length(styles[[i]])){
  #  if(names(styles[[i]][s])=="color"){
  #    s <- s+1
  #  }
    if(names(styles[[i]][s])!="color" & exists("text", styles[[i]]) ){
      styles[[i]]$text <- rbind(styles[[i]]$text, styles[[i]][[s]])
    }
    if(names(styles[[i]][s])!="color" & !exists("text", styles[[i]]) ){
      styles[[i]]$text <- styles[[i]][[s]]
    }
    if(s==end){
      styles[[i]] <- list(text=data.frame(styles[[i]]$text),color=styles[[i]]$color)
      remove(end,i,s)
    }
  }
  
}
# Within either the RMD or the fn_mapgen(), we will join the desired map style configuration 
## onto the labels data frame by matching up the "class" columns. 



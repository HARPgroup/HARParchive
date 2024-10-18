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

#----Run Sets----
# run_sets: complex, allows us to render with fewer arguments to Rmarkdown
# format:
# wsp_run_set <- list(
#    riverseg_metrics=list( 
#      list([parameter style name], [column in CSV file])
# )
run_sets <- list(
  wsp_2020_2040 = list(
    riverseg_metrics=list( 
      list(
        metric='l90_Qout',
        data_set = 'rseg_no_geom',
        column_name='percentDiff_l90_Qout_runid_11_runid_13', 
        run_label = '90 Day Low Flow (Percent Change 2020 to 2040)',
        show_map = TRUE,
        ramp = 'default',
        tables_cols = c('name', 'riverseg', 'Metric', 'runid_11_l90_Qout', 'runid_13_l90_Qout', 'percentDiff_l90_Qout_runid_11_runid_13'),
        sort_col = 'percentDiff_l90_Qout_runid_11_runid_13',
        floor = NULL, #table only displays values above this (not equal to)
        ceiling = NULL, #table only displays values below this (not equal to)
        n_entries = 999, #max num. of table entries
        sort = 'increasing', #Options: 'increasing', 'decreasing', or NULL --> orders table entries by increasing or decreasing values of the specified sort_col
        exlude_NAs = FALSE
      ),
      list(
        metric='l30_Qout', 
        data_set = 'rseg_no_geom',
        column_name='percentDiff_l30_Qout_runid_11_runid_13', 
        run_label = '30 Day Low Flow (Percent Change 2020 to 2040)', 
        show_map = TRUE,
        ramp = 'default',
        tables_cols = c('name', 'riverseg', 'Metric', 'runid_11_l30_Qout','runid_13_l30_Qout', 'percentDiff_l30_Qout_runid_11_runid_13'),
        sort_col = 'percentDiff_l30_Qout_runid_11_runid_13',
        floor = NULL, 
        ceiling = NULL, 
        n_entries = 999,
        sort = 'increasing',
        exlude_NAs = FALSE
      ),
      list(
        metric='7q10', 
        data_set = 'rseg_no_geom',
        column_name='percentDiff_7q10_runid_11_runid_13', 
        run_label = '7Q10 (Percent Change 2020 to 2040)', 
        show_map = TRUE,
        ramp = 'default',
        tables_cols = c('name', 'riverseg', 'Metric', 'runid_11_7q10','runid_13_7q10', 'percentDiff_7q10_runid_11_runid_13'),
        sort_col = 'percentDiff_7q10_runid_11_runid_13',
        floor = NULL, 
        ceiling = NULL, 
        n_entries = 999,
        sort = 'increasing',
        exlude_NAs = FALSE
      ),
      list(
        metric='Qout', 
        data_set = 'rseg_no_geom',
        column_name='percentDiff_Qout_runid_0_runid_13', 
        run_label = 'Overall Change in Flow in 2040', 
        show_map = TRUE,
        ramp = 'default',
        tables_cols = c('name', 'riverseg', 'Metric', 'runid_0_Qout','runid_13_Qout', 'percentDiff_Qout_runid_0_runid_13'),
        sort_col = 'percentDiff_Qout_runid_0_runid_13',
        floor = NULL, 
        ceiling = NULL, 
        n_entries = 999,
        sort = 'increasing',
        exlude_NAs = FALSE
      ),
      list(
        metric='l90_Qout', 
        data_set = 'rseg_no_geom',
        column_name='percentDiff_l90_Qout_runid_11_runid_17', 
        run_label = '90 Day Low Flow (Percent Change 2020 to Dry Climate Change)', 
        show_map = TRUE,
        ramp = 'default',
        tables_cols = c('name', 'riverseg', 'Metric', 'runid_11_l90_Qout','runid_17_l90_Qout', 'percentDiff_l90_Qout_runid_11_runid_17'),
        sort_col = 'percentDiff_l90_Qout_runid_11_runid_17',
        floor = NULL, 
        ceiling = NULL, 
        n_entries = 999,
        sort = 'increasing',
        exlude_NAs = FALSE
      ),
      list(
        metric='l30_Qout', 
        data_set = 'rseg_no_geom',
        column_name='percentDiff_l30_Qout_runid_11_runid_17', 
        run_label = '30 Day Low Flow (Percent Change 2020 to Dry Climate Change)', 
        show_map = TRUE,
        ramp = 'default',
        tables_cols = c('name', 'riverseg', 'Metric', 'runid_11_l30_Qout','runid_17_l30_Qout', 'percentDiff_l30_Qout_runid_11_runid_17'),
        sort_col = 'percentDiff_l30_Qout_runid_11_runid_17',
        floor = NULL, 
        ceiling = NULL, 
        n_entries = 999,
        sort = 'increasing',
        exlude_NAs = FALSE
      ),
#      list(metric='water_available_mgd', column_name='water_available_mgd_runid_13'),
      list(
        metric='7q10', 
        data_set = 'rseg_no_geom',
        column_name='percentDiff_7q10_runid_11_runid_17', 
        run_label = '7Q10 Day Low Flow (Percent Change 2020 to Dry Climate Change)', 
        show_map = TRUE,
        ramp = 'default',
        tables_cols = c('name', 'riverseg', 'Metric', 'runid_11_7q10', 'runid_17_7q10', 'percentDiff_7q10_runid_11_runid_17'),
        sort_col = 'percentDiff_7q10_runid_11_runid_17',
        floor = NULL, 
        ceiling = NULL, 
        n_entries = 999,
        sort = 'increasing',
        exlude_NAs = FALSE
      ),
      list(
        metric='Smin_L30_mg', 
        data_set = 'rseg_no_geom',
        column_name='runid_13_Smin_L30_mg', 
        run_label = 'Lowest 30 Day Minimum Storage (MG)', 
        show_map = TRUE,
        ramp = 'default',
        tables_cols = c('name', 'riverseg', 'Metric', 'runid_11_Smin_L30_mg', 'runid_13_Smin_L30_mg', 'percentDiff_Smin_L30_mg_runid_11_runid_17'), 
        sort_col = 'runid_13_Smin_L30_mg',
        floor = NULL, 
        ceiling = NULL, 
        n_entries = 999,
        sort = 'increasing',
        exlude_NAs = FALSE
      ), 
      list(
        data_set = 'facils_nogeom',
        metric='unmet30_mgd', #replace with unmet demand req 
        column_name='runid_11_unmet30_mgd', 
        run_label = 'Highest 30 Day Potential Unmet Demand (MGD)', 
        show_map = FALSE,
        ramp = 'default',
        tables_cols = c('Facility', 'Facility_hydroid', 'riverseg', 'runid_11_unmet30_mgd',  'runid_13_unmet30_mgd', 'runid_17_unmet30_mgd' ,'gw_frac'),
        sort_col = 'runid_13_unmet30_mgd',
        floor = NULL, #floor = 0.001, 
        ceiling = NULL,
        n_entries = 999,
        sort = 'decreasing',
        exlude_NAs = TRUE #exlude_NAs = FALSE
      )
  ),
  permit_dev = list(
    riverseg_metrics=list( 
      list(
        data_set = 'rseg_no_geom',
        metric='l30_Qout', 
        column_name='percentDiff_l30_Qout_runid_0_runid_601', 
        run_label = '2040 WSP L30 %', 
        show_map = TRUE,
        ramp = 'default',
        tables_cols = c('name', 'riverseg', 'Metric', 'runid_401_l30_Qout', 'runid_601_l30_Qout', 'percentDiff_l30_Qout_runid_0_runid_601'),
        sort_col = 'percentDiff_l30_Qout_runid_401_runid_601',
        floor = NULL, 
        ceiling = NULL, 
        n_entries = NULL,
        sort = 'increasing',
        exlude_NAs = FALSE
      ),
      list(
        data_set = 'rseg_no_geom',
        metric='7q10', 
        column_name='percentDiff_7q10_runid_0_runid_601', 
        run_label = '2040 WSP 7q10 %', 
        show_map = TRUE,
        ramp = 'default',
        tables_cols = c('name', 'riverseg', 'Metric', 'runid_401_7q10', 'runid_601_7q10', 'percentDiff_7q10_runid_0_runid_601'),
        sort_col = 'percentDiff_7q10_runid_401_runid_601',
        floor = NULL, 
        ceiling = NULL, 
        n_entries = NULL,
        sort = 'increasing',
        exlude_NAs = FALSE
      ),
      list(
        data_set = 'rseg_no_geom',
        metric='Smin_L30_mg', 
        column_name='runid_601_Smin_L30_mg', 
        run_label = 'Minimum Storage', 
        show_map = TRUE,
        ramp = 'default',
        tables_cols = c('name', 'riverseg', 'Metric', 'runid_401_Smin_L30_mg', 'runid_601_Smin_L30_mg', 'percentDiff_Smin_L30_mg_runid_0_runid_601'),
        sort_col = 'runid_601_Smin_L30_mg',
        floor = NULL, 
        ceiling = NULL, 
        n_entries = NULL,
        sort = 'increasing',
        exlude_NAs = FALSE
      )
    )
  )
)
)

#----Rivseg Maps Customization----
#For riverseg drought metric maps 
rivmap_ramps <- list( #a specific ramp per map & corresponding highlight limit per table can be chosen via runset
  'default' = cbind(rivseg_pct_vect = c(-20,-10,-2,2,10,20,500), #vector of values for rivseg drought maps
                    #^last value should be higher than any % difference value expected, since classification is done using <=
                    rivmap_colors = c("firebrick2","darkorange","#FFCC99",
                                      "white","palegreen","limegreen","green4"),#colors for fills based on % diff
                    rivmap_labs = c(" <= -20", #less than or equal to first value in pct vector
                                     "-20 to -10", #labeling the ranges between each value from pct vector 
                                     "-10 to -2",
                                     "-2 to +2",
                                     "+2 to +10",
                                     "+10 to +20",
                                     " > +20"), #last label should be greater than 2nd-to-last value in pct vector 
                    #^needs to be same length as rivseg_pct_vect
                   highlight_limit = -10 #sets upper limit of % difference for highlighting values/rows in the riverseg TABLE
              )
)

# rivseg_pct_vect <- c(-20,-10,-2,2,10,20,500) #vector of values for rivseg drought maps
# #^last value should be higher than any % difference value expected, since classification is done using <=
# rivmap_colors <- c("firebrick2","darkorange","#FFCC99",
#                    "white","palegreen","limegreen","green4") #colors for fills based on % diff
# #^needs to be same length as rivseg_pct_vect
# rivmap_labs <- c(" <= -20", #less than or equal to first value in pct vector
#                  "-20 to -10", #labeling the ranges between each value from pct vector 
#                  "-10 to -2",
#                  "-2 to +2",
#                  "+2 to +10",
#                  "+10 to +20",
#                  " > +20") #last label should be greater than 2nd-to-last value in pct vector 
# #^needs to be same length as rivseg_pct_vect
# rseg_highlight_limit <- -10 #sets upper limit of % difference for highlighting values/rows in the riverseg table

#----Overrides for Custom Bbox----
#Edit here:
custom_bboxes <- list(
  'SoutheastVirginia'= cbind(c(xmin=-77.75660,xmax=-75.77736),c(ymin=36.4, ymax=37.4))
)

#----Human-readable metric names: add here any new metric names being used and their readable version----
readable <- data.frame(rbind(
  #scenarios:
  c('runid_0', 'Pre-Condition'),
  c('runid_1', 'Historical Condition'),
  c('runid_3', 'Permit Term Max'),
  c('runid_11', '2020 Demand Scenario'),
  c('runid_12', '2030 Demand Scenario'),
  c('runid_13', '2040 Demand Scenario'),
  c('runid_14', 'Median Climate Change Scenario (50/50)- 2020 Demand'),
  c('runid_15', 'Dry Climate Change Scenario (10/10) - 2020 Demand'),
  c('runid_16', 'Wet Climate Change Scenario (90/90) - 2020 Demand'),
  c('runid_17', 'Dry Climate Change Scenario (10/20) - 2040 Demand'),
  c('runid_18', '2020 Exempt User Runs'),
  c('runid_19','Median Climate Change Scenario (50/50)- 2040 Demand'),
  c('runid_20', 'Wet Climate Change Scenario (90/90) - 2040 Demand'),
  c('runid_21', '2015 Demand 2010'),
  c('runid_22', '2015 Demand 2040'),
  #metrics:
  c('fiveyr_avg_mgy', 'Five Year Avg Use (MGY)'),
  c('wd_mgd', 'Withdraws (MGD)'),
  c('gw_demand_mgd', 'Ground Water Demand (MGD)'),
  c('ps_mgd', 'Point Source (MGD)'),
  c('wsp2020_2040_mgy', 'Water Supply Plan 2020-2040 Demand (MGY)'),
  c('l90_Qout', '90 Day Low Flow (cfs)'),
  c('l30_Qout', '30 Day Low Flow (cfs)'),
  c('Smin_L30_mg', 'Lowest 30 Day Minimum Storage (MG)'),
  c('unmet30_mgd', 'Highest 30 Day Potential Unmet Demand (MGD)'),
  #table column names:
  c('name', 'Name'),
  c('riverseg', 'River Segment ID'),
  c('five_yr_avg', '5-yr Avg Use (MGY)'),
  c('runid_11_wd_mgd', '2020 Demand Scenario Withdraws (MGD)'),
  c('runid_13_wd_mgd', '2040 Demand Scenario Withdraws (MGD)'),
  c('runid_11_l90_Qout', '2020 90 Day Low Flow (cfs)'),
  c('runid_13_l90_Qout', '2040 90 Day Low Flow (cfs)'),
  c('percentDiff_l90_Qout_runid_11_runid_13', 'Percent Difference in 90 Day Low Flow From 2020-2040'),
  c('runid_11_l30_Qout', '2020 30 Day Low Flow (cfs)'),
  c('runid_13_l30_Qout', '2040 30 Day Low Flow (cfs)'),
  c('percentDiff_l30_Qout_runid_11_runid_13', 'Percent Difference in 30 Day Low Flow From 2020-2040'),
  c('runid_11_7q10', '2020 7Q10 (cfs)'),
  c('runid_13_7q10', '2040 7Q10 (cfs)'),
  c('percentDiff_7q10_runid_11_runid_13', 'Percent Difference in 7Q10 From 2020-2040'),
  c('runid_11_consumptive_use_frac', '2020 Consumptive Use Fraction'),
  c('runid_13_consumptive_use_frac', '2040 Consumptive Use Fraction'),
  c('percentDiff_Qout_runid_11_runid_13', 'Percent Change in Flow From 2020-2040'),
  c('percentDiff_Qout_runid_0_runid_13', 'Percent Change in Flow in 2040'),
  c('runid_11_Qout', '2020 Average Flow (cfs)'),
  c('runid_13_Qout', '2040 Average Flow (cfs)'),
  c('runid_0_Qout', 'Base Flow (cfs)'),
  c('runid_17_l90_Qout', '2040 Dry Climate Change 90 Day Low Flow (cfs)'),
  c('runid_17_l30_Qout', '2040 Dry Climate Change 30 Day Low Flow (cfs)'),
  c('percentDiff_l90_Qout_runid_11_runid_17', 'Percent Difference in 90 Day Low Flow From 2020-2040 (Dry Climate Scenario)'),
  c('percentDiff_l30_Qout_runid_11_runid_17', 'Percent Difference in 30 Day Low Flow From 2020-2040 (Dry Climate Scenario)'),
  c('runid_17_7q10', '2040 Dry Climate Change 7Q10 (cfs)'),
  c('percentDiff_7q10_runid_11_runid_17', 'Percent Difference in 7Q10 From 2020-2040 (Dry Climate Scenario)'),
  c('runid_11_Smin_L30_mg', '2020 Lowest 30 Day Minimum Storage (MG)'),
  c('runid_13_Smin_L30_mg', '2040 Lowest 30 Day Minimum Storage (MG)'),
  c('percentDiff_Smin_L30_mg_runid_11_runid_17', 'Percent Difference in Lowest 30 Day Minimum Storage From 2020-2040 (Dry Climate Scenario)'),
  c('runid_11_unmet30_mgd', '2020 Highest 30 Day Potential Unmet Demand (MGD)'),
  c('runid_13_unmet30_mgd', '2040 Highest 30 Day Potential Unmet Demand (MGD)'),
  c('runid_17_unmet30_mgd', '2040 Dry Climate Change 30 day Potential Unmet Demand (MGD)'),
  c('gw_frac', 'GW Fraction'),
  c('Facility_hydroid', 'VAHydro ID')
  
))
colnames(readable) <- c('computer', 'human')

# adapt this to permit us to store pre-configured sets
# run_set is parameter to use in WSP_Regional_Summaries.Rmd

#----NHD flowline & waterbody classification & substitution:----
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

wd_mgd = c(0, 0.5, 1.0, 2, 10, 25, 100, 1000) #set ranges for bins mgd and mgy 
wd_mgy = c(0, 1, 5, 10, 50, 250, 1000, 10000)
# replace the above with a generic holder that allow lookup by metric name
metrc_ramps = list(
  wd_mgd = c(0, 0.5, 1.0, 2, 10, 25, 100, 1000),
  wd_mgy = c(0, 1, 5, 10, 50, 250, 1000, 10000),
  water_available_mgd = c(0, 1, 5, 10, 20, 50, 100)
)
#----Basemap URL----
# location of data for the map background. 
# these are inputs for fn_basemap(), located in fns_mapgen.R
map_server <- "https://vginmaps.vdem.virginia.gov/arcgis/rest/services" 
base_layer = "Download/LandCover_Downloads/MapServer/0" #VA LandCover -> very sparse

#--
styles <- list()
#----Custom Aesthetics:----
styles$custom$color$sf <- data.frame(row.names=c("lightenBase","county","nhd","roads","citypts","rsegs","region","shadow","tidal", "unmodeled"),
                               color=c("honeydew","#0033337F","deepskyblue3","black","black","sienna1","black", "#4040408F","#5F5C87","#828284")
                              )
styles$custom$color$metrics <- data.frame(row.names=c("Surface Water","Groundwater","Nondistinctive"),
                                    color=c("#F7FF00","#FF00FF","#FF9851")
                                    )
styles$custom$color$text <- data.frame(row.names=c("interstate","staterte","ushwy_cities","nhd","county","rsegs"),
                                 color=c("white","white","black","deepskyblue4","#003333", "sienna3")
                                 )
styles$custom$color$fill <- data.frame(row.names=c("interstate","staterte","ushwy"),
                                 color=c("blue","#0B5F14","white")
                                 )


styles$custom$a <- data.frame(class="I", #label type identifier; should match up with the labels df so aesthetics can be joined to it
                               fontface="plain", #plain, bold, italic, bold.italic
                               fontfam="TT Times New Roman",
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
                               fontfam="TT Times New Roman",
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
                               fontfam="TT Times New Roman",
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
                               fontfam="Sans",
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
                               fontfam="Sans",
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
                               fontfam="Serif",
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
                              fontfam="Serif",
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
                              fontfam="Serif",
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
                               fontfam="Serif",
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
                               fontfam="Serif",
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
                              fontfam="Sans",
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

#----Colorblind Aesthetics:----
  #based on Okabe and Ito colorblind scheme mentioned in:
    #https://www.nceas.ucsb.edu/sites/default/files/2022-06/Colorblind%20Safe%20Color%20Schemes.pdf
styles$colorblind$color$sf <- data.frame(row.names=c("lightenBase","county","nhd","roads","citypts","rsegs","region","shadow"),
                                     color=c("honeydew","#009E73","#56B4E9","black","black","#D55E00","black", "#4040408F")
)
styles$colorblind$color$metrics <- data.frame(row.names=c("Surface Water","Groundwater","Nondistinctive"),
                                          color=c("#F0E442","#CC79A7","#E69F00")
)
styles$colorblind$color$text <- data.frame(row.names=c("interstate","staterte","ushwy_cities","nhd","county","rsegs"),
                                       color=c("white","white","black","#56B4E9","#009E73", "#D55E00")
)
styles$colorblind$color$fill <- data.frame(row.names=c("interstate","staterte","ushwy"),
                                       color=c("#0072B2","#009E73","white")
)


styles$colorblind$a <- data.frame(class="I", #label type identifier; should match up with the labels df so aesthetics can be joined to it
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
styles$colorblind$b <- data.frame(class="S",
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
styles$colorblind$c <- data.frame(class="U",
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
styles$colorblind$d <- data.frame(class="town",
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
styles$colorblind$e <- data.frame(class="city",
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
styles$colorblind$f <- data.frame(class="waterbody_lg",
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
styles$colorblind$g <- data.frame(class="waterbody_med",
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
styles$colorblind$h <- data.frame(class="waterbody_sm",
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
styles$colorblind$i <- data.frame(class="stream",
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
styles$colorblind$j <- data.frame(class="majorRiver",
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
styles$colorblind$k <- data.frame(class="county",
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
styles$colorblind$l <- data.frame(class="smallTown",
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

#----Creating Final List of Map Styles Data Frames: (NOT for User Editing)----
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
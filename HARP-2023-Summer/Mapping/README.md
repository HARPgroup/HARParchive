# Mapping For Water Supply Planning

## Overview

This set of files are for the creation of a mapping tool for water supply planners. Includes creation of functions for the main rmd used. There are config files included to allow the user to adjust mapping and table creation to how they desire.

### - Types of Maps:

Each map has an accompanying table to help with analysis

-   State Plan 2020 demand - State Plan 2040 demand (Current stage of development)
-   2020/2040 Demand Scenarios Through Drought Metrics: L90 -- 7Q10
-   Climate: State Plan Dry climate scenario - Climate differential scenario
-   CU: Overall Change in Flow from State Plan
-   Water availability based on instream flow

# Workflow

## Packages

Make sure to have installed all the following packages to ensure all functions and rmds run:

-   data.table
-   hydrotools
-   mgsub
-   sp
-   sf
-   lwgeom
-   nhdplusTools
-   ggmap
-   raster
-   ggplot2
-   ggnewscale
-   ggsn
-   ggspatial
-   ggrepel
-   png
-   flextable
-   geosphere
-   pandoc
-   rapportools
-   arcpullr
-   stringr
-   "memoise"

## Dataframe_Generator

### General Info

The **Dataframe_Generator** is the first step in our document creation process. This document will pull information from the **Foundational_Dataset** and the DEQ Databases. It will filter and process this data as desired by the user and do any additional calculations necessary.

This information will be combined into multiple csv documents which always includes(unless the files are already detected):

-   `_rsegs_sf.csv`: for mapping and filtering river segments
-   `counties_sf.csv`: for mapping and filtering counties
-   `regions_sf.csv`: for mapping and filtering regions
-   `roads_sf.csv`: for mapping and filtering roads
-   `cities.csv`: for mapping and filtering cities

When the user chooses to filter by facility, `_featrs_sf.csv` will be generated. When they choose to filter by source, `_mp_sf.csv` will be created.

### Render

This information can be generated via render or via direct parameter changes in document.

A current working render is as follows:

```         
rmarkdown::render(paste0(github_location,"/HARParchive/HARP-2023-Summer/Mapping/Dataframe_Generator.Rmd"), 
                      params = list(
                        origin = "Chowan_2", 
                        origin_type = "region", 
                        featr_type = "facility", 
                        metric_mod = c("wd_mgd", "unmet1_mgd", "unmet7_mgd", "unmet30_mgd"), 
                        model_version = "vahydro-1.0",
                        metric_feat = "wsp2020_2040_mgy", 
                        rivseg_metric = c("l90_Qout", "l30_Qout", "7q10", "Qout", "WA_90_mgd"), 
                        runid_list = c("runid_11", "runid_13", "runid_17", "runid_1000"), 
                        crs_default = 4326, 
                        limit_featrs_to_origin = FALSE,
                        overwrite_files = TRUE, 
                        base_layer_data = FALSE)
                      )
```

### Parameters

-   **origin**: name of the region, locality, or rivseg, make sure to use correct naming

    -   **rivseg**: river segment ID of basin to be mapped, for map type basin

    -   **locality**: locality/county of interest, for map type locality

    -   **region**: region of interest, must match a region name in <https://github.com/HARPgroup/HARParchive/blob/master/HARP-2023-Summer/Regions_ProposedReg_053122.csv>

    -   **Possible regions** :`BigSandy_UpperTennessee_1`, `BigSandy_UpperTennessee_2`, `Chowan_1`, `Chowan_2`, `Eastern_Shore`, `MiddleJames_1`, `MiddleJames_2`, `MiddleJames_3`, `NewRiver_1`, `NewRiver_2`, `NorthernCoastalPlain_1`, `NorthernCoastalPlain_2`, `NorthernCoastalPlain_3`, `NorthernPiedmont_1`, `NorthernPiedmont_2`, `NorthernVirginia`, `Roanoke_1`, `Roanoke_2`, `Roanoke_3`, `Shenandoah_1`, `Shenandoah_2`, `SoutheastVirginia`, `UpperJames_1`, `UpperJames_2`, `York_James_1`, `York_James_2`

-   **origin_type**: what type of origin the name above is categorized under

    -   *Possible Origins*: `basin`, `locality` or `region`

-   **featr_type**: Which feature type you want to map by

    -   *Possible Input* : `source` or `facility`

-   **metric_mod**: the modeled metric name to be mapped & used in table. Does not need to be set to fiveyr_avg_mgy if mapping source

    -   *Possible Metrics*: `wd_mgd`, `gw_demand_mgd`, `l30_Qintake`, `ps_mgd`, `fiveyr_avg_mgy`, etc.

-   **model_version**: used in pulling facility metrics from vahydro

    -   *Possible Input*: `vahydro-1.0`, etc.

-   **metric_feat**: non-modeled metric name

    -   *Possible Metrics*: `wsp2020_2040_mgy`

-   **rivseg_metric**: drought metric for riverseg maps, can list multiple, use c()

    -   *Possible Metrics*: `190_Qout`,`7q10`, `l30_Qout`, etc.

-   **runid_list**: likely to be modified- return to description

| `runid` List Key |                      Scenario                       |
|:----------------:|:---------------------------------------------------:|
|    `runid_0`     |                    Pre-Condition                    |
|    `runid_1`     |                Historical Condition                 |
|    `runid_3`     |                   Permit Term Max                   |
|    `runid_11`    |                2020 Demand Scenario                 |
|    `runid_12`    |                2030 Demand Scenario                 |
|    `runid_13`    |                2040 Demand Scenario                 |
|    `runid_14`    | Median Climate Change Scenario (50/50)- 2020 Demand |
|    `runid_15`    |  Dry Climate Change Scenario (10/10) - 2020 Demand  |
|    `runid_16`    |  Wet Climate Change Scenario (90/90) - 2020 Demand  |
|    `runid_17`    |  Dry Climate Change Scenario (10/20) - 2040 Demand  |
|    `runid_18`    |                2020 Exempt User Runs                |
|    `runid_19`    | Median Climate Change Scenario (50/50)- 2040 Demand |
|    `runid_20`    |  Wet Climate Change Scenario (90/90) - 2040 Demand  |
|    `runid_21`    |                  2015 Demand 2010                   |
|    `runid_22`    |                  2015 Demand 2040                   |

-   **crs_default**: usually `4326`
-   **limit_featrs_to_origin**: if `TRUE` then featrs will be cutoff at the region/locality specified, if `FALSE` then all featrs in the associated basins are plotted
-   **overwrite_files**: if `FALSE` then the document will stop execution if rivseg and feature files already exist
-   **base_later_data**: if `FALSE` then the document will only generate the origin/metric-dependent data for mapping (rsegs, featrs), if `TRUE` then the document will also re-generate map base-layer data (regions, counties, cities, roads)

### Related Documents/Functions

-   *fn_download_read* : used for files that need to be downloaded prior to reading in the data; typically necessary for downloading & unpacking less-straightforward filetypes like .zip files, .shp files, etc.


## WSP_Regional_Summaries.RMD

### General Info

This RMD pulls in the csv files created in `Dataframe_Generator.R` and forms them into the desired maps and tables. The final output of this document is the desired Markdown file for the users input.

### Parameters

When completing these, it is imperative everything aligns with input parameters for the `Dataframe_Generator.R` and with the document titles created by it.

-   **Params that need to match the `Dataframe_Generator` params**: origin, origin_type, featr_type, runid_list, crs_default
-   **featrs_file**: input the location and name for the `featrs_sf` or `mp_sf` file created by `Dataframe_Generator.R`
-   **featrs_file_map_bubble_column** : the desired runid and metric that will be mapped as a bubble
    -   *Possible input examples*: all by runid_metric like- `runid_11_wd_mgd`, except for- `five_yr_avg`
-   **featrs_file_table_column** : the desired runid and metric for the table documentation
    -   *Possible input examples*: `runid_11_wd_mgd`,`runid_13_wd_mgd`,`five_yr_avg`,`wsp2020_2040_mgy`
-   **rsegs_file** : input the location and name for the `rsegs_sf` file created by `Dataframe_Generator.R`
-   **run_set** : looks for metric and data column names to map and show in tables, works with the config, need to make sure requested runid and metric are available
    -   *Possible input* : generally `wsp_2020_2040` but for case studies `permit_dev`
-   **map_style** : determining map aesthetics like colors, fonts, font size
    -   *Possible Input* : `colorblind`, `default`, `custom`
-   **bbox_type** : either auto or custom. Custom pulls bounds from a preset list, auto sets a buffer around origin of interest
    -   *Possible Input* : `auto`, `custom`
-   **show_map** : generaton of map in the rmd or not, `FALSE` for no generation

### Related Functions


-   *fns_spatial* : Defines all HARP-analyst-written functions for dealing with spatial data, see above section for details
    - *fn_geoCol*: Determines the geometry column of a dataframe 
    - *fn_sqldf_sf*: Allows sqldf on spatial dataframes. Drops the spatial column, performs the query, then merges it with the original object to return the geometry
    - *fn_centroid_coords*: Gets the coordinates of the centroid of an sf dataframe and adds an X and Y column for those coordinates
-   *fn_mapgen*: contains the mapping process used to create all map types (facils/sources only and rivserseg maps). Broken down into several steps to build each map, applying one supporting function at a time to build each layer. The supporting functions, all contained within `functions/fns_mapgen.R`, are described below
    -   Uses **fn_filter_map** (in dunxriona.fn_filter_map.R): contains process of filtering flowlines, labels etc. to be mapped based on size of boundary box, only called within mapping function
    - *fn_catchMapErrors*: Catch common errors encountered during development and allow the function to proceed. Each step of map generation was wrapped in this function
    - *fn_labelsAndFilters*: Takes all the map labels and determines what to include based on the size and extents of the map. Sets text sizes and filters the labels based on total extent of the map.
    - *fn_shadow*: Shades the map outside of the area of interest, the river segments intersecting the origin
    - *fn_mp_bubbles*: Adds the feature bubbles, sized to their metric (default is wsp2020_2040_mgy)
    - *fn_borders*: Puts in different colored borders for the origin, localities within the origin, and riversegments
    - *fn_polygonFill*: Colors in the river segments based on the `rivmap_ramp` and sets up the associated legend
    - *fn_nhdLines*: Plots the NHD flowlines and water bodies within the origin
    - *fn_roadsAndCityPoints*: Creates the roads and cities layer of the map
    - *fn_textRepe*: Puts in labels for roads, cities, counties, river segments, and water bodies. There is a lot of potential labels, especially adding in the bubble layers. This function attempts to fit everything, and will cut out labels in a very crowded area of the map
-   *fn_gw_mapgen*: contains the mapping process for critical cell maps in the GWMA. Same base code as `fn_mapgen` but with an extra input to add in aquifer geometry and no rsegs.
-   *fn_tablegen*: creates a flextable from a provided dataframe, or the columns selected from within that dataframe (default is "all")
-   *fn_labelprep* : general actions for label processing, run on nhd labels and road labels after each of their respective functions
-   *fn_get_memo_nhdplus.R*: Wraps nhd functions (namely `plot_nhdplus`) in the memoise function, so that it caches the result. This does not change the inputs of the `plot_nhdplus` function.
-   *fn_nhd_labs* : contains nhd-specific actions for labeling (flowlines and waterbodies); want to do away with this function since it only filters NHD data (this can be done with the rest of the data processing)
-   *mapstyle_config* : contains aesthetic customization for mapping (all map types), including unique styling for each label type (rivers/streams, counties, cities etc.)
-   *riversegmaps_config* : contains customizations specific for rivseg drought maps, including values and colors for % difference ranges


### Text/Narrative Files

These files are where narrative for the individual sections can edited. The files are located at <https://github.com/HARPgroup/HARParchive/blob/master/HARP-2023-Summer/Section_Narratives> , one folder above this one. They are separate from the main RMD so that it only pulls in narrative for the desired sections when generating a document. Also this can make it easier to be able to edit text without digging through the code. The names of these files line up with the list of riversg metrics in Mapstyle_config, so be sure to update any of those names in both places, otherwise this text will not be pulled in. They are all inserted as plain text, so formatting such as bold, italics, are not read through.

### Config Files

There are three locations planners will be editing code, the parameters in `Dataframe_Generator` and `WSP_Regional_Summaries`, as well as any config files. The editing of config files is if a specific aesthetic change is desired. The editing of parameters is where the location intended for analysis is inputted.

### Mapstyle_config

-   **Human-Readable Metric Names**: this section is added so runids and metrics have a understandable name -`read_metric_name`: this is for input of metrics or runids that need to be renamed -`new_metric_name`: this is for input of new names, keep everything in the same order as `read_metric_name`

-   **Run_Sets**: this section allows the user to input various pieces of information for use in dataframe, mapping and table gen

```         
format:
 wsp_run_set <- list(
    riverseg_metrics=list( 
      list([parameter style name], [column in CSV file]) )
```

-   *Current params in use*:

    -   metric: input the specific metric wanted to be used, such as 'l30_Qout',
    -   column_name: input the percent diff long column name, such as 'percentDiff_l30_Qout_runid_11_runid_13',
    -   run_label: this is used for naming of tables and maps, such as '2040 WSP L30 %',
    -   tables_cols: this is what columns are desired in the tables presented, such as c('name', 'riverseg', 'Metric', 'runid_11_l30_Qout', 'runid_13_l30_Qout', 'percentDiff_l30_Qout_runid_11_runid_13'),
    -   sort_col: this is what column will be sorted by in the table, such as 'percentDiff_l30_Qout_runid_11_runid_13',
    -   sort_decreasing: this is TRUE if sorting by decreasing and FALSE if sorting by increasing

-   Everything in custom aesthetics can be adjusted, if not changed, then defaults (below custom) will be used

-   **Current Possible Text Layers: Require MANY font/color/size aesthetics**

    -   **Roads**: Interstate(I), US Hwy(U), State Rte(S)
    -   **Cities**: Major Cities, Towns
    -   **Water**: Major Rivers (nhd class 5,6), Streams (nhd class 4), Waterbodies (nhd network and off network wtbd's)
    -   **Polygons**: Counties, Potentially Riversegs/Watersheds

-   **Current Possible Point Layers: Require colors & point sizes**

    -   **Cities**: Major Cities, Towns
    -   **Facilities**: Metric-scaled bubbles of either:
        -   surface water for model data at the facility-level (default color 1)
        -   distinct surface water (default color 1) and groundwater (default color 2) for data at MP-level
        -   neither purely surface water nor groundwater at the facility level (default color 3)

-   **Current Possible sf Layers: Require colors & line widths**

    -   **Roads**: Interstate(I), US Hwy(U), State Rte(S)
    -   **NHD**: Whichever flowlines were kept for the extent, scaled by streamOrde ; Waterbodies
    -   **Polygons**: Counties, Reverse fill of basin, Riverseg borders

-   **Overrides for Custom Bbox**: Allows user to input custom bounding boxes to override the auto-calculated bounding box for certain map origins.

```         
format: 
custom_bboxes <- list(
  '[origin name]'= cbind(
          c(xmin=[min longitude], xmax=[max longitude]),
          c(ymin=[min latitude], ymax=[max latitude])
          ),
  '[origin name 2]'= cbind(
          c(xmin=[min longitude], xmax=[max longitude]),
          c(ymin=[min latitude], ymax=[max latitude])
          )
)
```

### Riversegmaps_config

to edit later(might end up merging into mapstyle)

# Example Renders

## For a working Basin example:

### Dataframe_Generator Render:

```         
rmarkdown::render(paste0(github_location, "/HARParchive/HARP-2023-Summer/Mapping/Dataframe_Generator.Rmd"), 
                  params = list(
                      origin = "JL6_7430_7320", 
                      origin_type = "basin", 
                      featr_type = "facility", 
                      metric_mod = "wd_mgd", 
                      model_version = "vahydro-1.0",
                      metric_feat = "wsp2020_2040_mgy", 
                      rivseg_metric = c("l30_Qout", "7q10", "Smin_L30_mg"), 
                      runid_list = c("runid_11", "runid_13", "runid_17", "runid_19"), 
                      crs_default = 4326, 
                      limit_featrs_to_origin = FALSE,
                      overwrite_files = TRUE, 
                      base_layer_data = FALSE))
```

### WSP_Regional_Summaries Render:

```         
rmarkdown::render(paste0(github_location, "/HARParchive/HARP-2023-Summer/Mapping/WSP_Regional_Summaries.Rmd"), 
                  output_file = paste0(export_path, "/JL6_7430_7320test"),
                  output_format = "word_document",
                  params = list(
                      origin = "JL6_7430_7320", 
                      origin_type = "basin", 
                      featr_type = "facility", 
                      featrs_file = paste0(export_path, "/JL6_7430_7320_featrs_sf.csv"), 
                      featrs_file_map_bubble_column = "wsp2020_2040_mgy", 
                      featrs_file_table_column = c("runid_11_wd_mgd","runid_13_wd_mgd","five_yr_avg","wsp2020_2040_mgy"), 
                      rsegs_file = paste0(export_path, "/JL6_7430_7320_rsegs_sf.csv"), 
                      run_set = "wsp_2020_2040", 
                      runid_list = c("runid_11", "runid_13", "runid_17" ,"runid_19"), 
                      crs_default = 4326, 
                      map_style = "custom", 
                      bbox_type = "auto",
                      show_map = TRUE))
```

## For working Region example:

### Dataframe_Generator Render:

```         
rmarkdown::render(paste0(github_location, "/HARParchive/HARP-2023-Summer/Mapping/Dataframe_Generator.Rmd"), 
                  params = list(
                      origin = "Roanoke_1", 
                      origin_type = "region", 
                      featr_type = "facility", 
                      metric_mod = "wd_mgd", 
                      model_version = "vahydro-1.0",
                      metric_feat = "wsp2020_2040_mgy", 
                      rivseg_metric = c("l30_Qout", "7q10", "Smin_L30_mg"), 
                      runid_list = c("runid_11", "runid_13", "runid_17", "runid_19"), 
                      crs_default = 4326, 
                      limit_featrs_to_origin = FALSE,
                      overwrite_files = TRUE, 
                      base_layer_data = FALSE))
```

### WSP_Regional_Summaries Render:

```         
rmarkdown::render(paste0(github_location, "/HARParchive/HARP-2023-Summer/Mapping/WSP_Regional_Summaries.Rmd"), 
                  output_file = paste0(export_path, "/roanoke1test"),
                  output_format = "word_document",
                  params = list(
                      origin = "Roanoke_1", 
                      origin_type = "region", 
                      featr_type = "facility", 
                      featrs_file = paste0(export_path, "/Roanoke_1_featrs_sf.csv"), 
                      featrs_file_map_bubble_column = "wsp2020_2040_mgy", 
                      featrs_file_table_column = c("runid_11_wd_mgd","runid_13_wd_mgd","five_yr_avg","wsp2020_2040_mgy"), 
                      rsegs_file = paste0(export_path, "/Roanoke_1_rsegs_sf.csv"), 
                      run_set = "wsp_2020_2040", 
                      runid_list = c("runid_11", "runid_13", "runid_17" ,"runid_19"), 
                      crs_default = 4326, 
                      map_style = "custom", 
                      bbox_type = "auto",
                      show_map = TRUE))
```

## For working Locality example:

### Dataframe_Generator Render:

```         
rmarkdown::render(paste0(github_location, "/HARParchive/HARP-2023-Summer/Mapping/Dataframe_Generator.Rmd"), 
                  params = list(
                      origin = "Giles", 
                      origin_type = "locality", 
                      featr_type = "facility", 
                      metric_mod = "wd_mgd", 
                      model_version = "vahydro-1.0",
                      metric_feat = "wsp2020_2040_mgy", 
                      rivseg_metric = c("l30_Qout", "7q10", "Smin_L30_mg"), 
                      runid_list = c("runid_11", "runid_13", "runid_17", "runid_19"), 
                      crs_default = 4326, 
                      limit_featrs_to_origin = FALSE,
                      overwrite_files = TRUE, 
                      base_layer_data = FALSE))
```

### WSP_Regional_Summaries Render:

```         
rmarkdown::render(paste0(github_location, "/HARParchive/HARP-2023-Summer/Mapping/WSP_Regional_Summaries.Rmd"), 
                  output_file = paste0(export_path, "/Gilestest"),
                  output_format = "word_document",
                  params = list(
                      origin = "Giles", 
                      origin_type = "locality", 
                      featr_type = "facility", 
                      featrs_file = paste0(export_path, "/Giles_featrs_sf.csv"), 
                      featrs_file_map_bubble_column = "wsp2020_2040_mgy", 
                      featrs_file_table_column = c("runid_11_wd_mgd","runid_13_wd_mgd","five_yr_avg","wsp2020_2040_mgy"), 
                      rsegs_file = paste0(export_path, "/Giles_1_rsegs_sf.csv"), 
                      run_set = "wsp_2020_2040", 
                      runid_list = c("runid_11", "runid_13", "runid_17" ,"runid_19"), 
                      crs_default = 4326, 
                      map_style = "custom", 
                      bbox_type = "auto",
                      show_map = TRUE))
```

# Code breakdown

This section will go through the blocks of codes in both rmds to explain their purpose and what they are doing.

## Datafrane_Generator.Rmd

### Setup

Loads in required packages, sets up ds connection, calls in custom functions. It also assigns objects to the input parameters. Foundation location is read in from the config file, HARP students set it to GitHub. 

### Loading_data

Checks if the files to be generated already exist in the desired location. If they do AND overwrite_files is TRUE, then the code stops here. Otherwise it removes the files if they exist, and continues. 

Reads in the foundation data, facility modeling data (from the server), mapping geometries (counties, regions, rsegs)

### Base_layers

This section only runs if base_layer_data is TRUE. By default, it is false and this section will not run. The purpose of this section is to create the background map layers, such as roads and cities. These files already exist, so there is no need to run this section unless something happens to them.

### Creating_featrs_rsegs

The foundation is read in at an MP level. This section groups it by facility then joins it with the modelling data. It then adds on what river segment the facility is in. Since some river segments overlap, it looks for the smallest one it is in. `featrs` is the object that contains the facilities (or whatever level the map is being generated at), `rsegs` is the object containing the river segments.

### Filtering_featrs_rsegs

This filters the `featrs` and `rsegs` object to the origin, so the specific region of interest if origin_type = 'region'. Only includes rsegs that are at least 1% contained within the origin. This ensures river segments with the slightest bit of overlap on the border are not included.

If you are trying to get a list of all features and river segments (not filtered), then skip/comment out this section.

### Getting_rseg_modelling

Adds the modelling data onto the `rsegs` object. Created % difference columns based on runid for 2020.

### Saving FIles

This is the last code block, which just saves the `featrs` and `rsegs` objects into csvs. The `rsegs` csv is too large to open in excel due to the detailed geometries, but it can be read into the Summaries RMD. This is also where the base layers are saved if base_layer_data were TRUE. 

## WSP_Regional_Summaries.Rmd

Sections in an rmd can make debugging a bit easier when there is an error while rendering. The summaries rmd does much more work than the dataframe generator, so was much more prone to errors. As a result, there are many sections, some of which do very little work to make it clear where an event affecting the document is or where an error would occur in a common place.

### Setup

Reads in packages.

### UserInputs

Assigns objects to all parameters defined for the RMD.

### TOC

Creates the table of contents using `block_toc()` from the officedown package. This section is only a single line, but it needs to be its own section because of how the package works.

### Load Functions & Configs

Reads in custom function files and the config files from GitHub. Tries to pull everything from your local git if the `HARParchive_location` (which is defined in our personal config files) exists, otherwise it pulls it from the GitHub website. Also defines a function for titling the legend based on the metric and runid to be used in the mapping later.

### Data_Configuration

Setting up the runsets based on the config file. `riverseg_metric` is a vector of all the sections to be included in the final document. `run_config$riverseg_metrics` is the list of all the settings for each of the sections, such as the legend scale, title, column names for the table, all the aesthetics of the map and table.

### Load Data

Reads in the data from the df_generator output. Also reads in the base files, whether they were generated or not. `rsegs` is still the object for river segments but now `facils` is the name of the features object.

### Bbox

Creates the bounding box for the maps (same one is used for all maps). If bbox_type is TRUE (which it is by default), it looks for the origin name (i.e. Chowan_2) in the list of custom defined bboxes. If its not there, then it sets it automatically as it would if `bbox_type` were FALSE. This is why its kept TRUE by default, because it only applies when a custom box has been defined.

To create it automatically, it sets a 0.02 latitude/longitude buffer around `rsegs`. Since `rsegs` contains all segments that intersect the origin, this is buffer is more than enough to capture the whole region.

### Get NHD in Bbox

Pulls in the NHD lines and data within the bounding box. Pulling NHD lines generally takes a while, so this uses `memo_plot_nhdplus`, which is a custom `memoize` function that tries to cache the result. This generally works to save a lot of time, but sometimes it can cache a `NULL` result, causing the maps to fail. There is an `if` statement here to forget the cached result if its pulling a `NULL` and try again.

This and many other sections are wrapped entirely in an `if show_maps == TRUE` statement, where basically anything that sets up the maps will only run if the `show_maps` paremeter is `TRUE`, The summary can be run without generating any maps (just tables) and all sections like this are skipped, causing it to run much faster.

### Filter NHD

Filters nhd to leave only major rivers and water bodies, and does some slight renaming. Although this saves the filtered result as `ndh2`, which I am not sure if this is actually used...

### Prep Text Labels

Another 1 line section. Puts together the labels for the maps, combining the nhd names, cities, roads, and counties.

### Create Map 1

This section creates the first map in the document, showing all the features on the map with differently sized and labelled bubbles. Unlike the rest of the maps, there is no rseg coloring. First it creates a clean looking title based on the name, so BigSanyUpperTennesse_1 becomes 'Big Sandy Upper Tennessee 1'. Then it orders the `facils` data frame so the number column can be added (this is the number shown in the table and map). The data is also sorted into bins based on the legend (this is what is used to determine what size to make each bubble). It looks for the column named in the parameter `featrs_file_map_bubble_column`. 


Everything is fed into `fn_mapgen`. This is a large custom function in the *fns_mapgen.R* that stitches everything up to this point into a map. There are many inputsdescribed below. This function can take up to a minute to run, which makes the map generation the slowest part of this RMD (mostly the river segment maps since there are more of them). Once generated, the map is saved to a file in the `export_path` folder, set in our config files.


- bbox: The bounding box, created in the bbox section
- crs_default: Defined in the parameter `crs_default` parameter. Default is 4326
- mp_layer: This is the ordered `facils` df. Like `facils`, the object name is independent of the type of feature being mapped. If `featr_type` is facilities, mp_layer still shows facilities/
- metric_unit: Detmerines the scale of the legend for the feature bubbles (mgy vs. mgd)
- featr_type: Parameter `feater_type`
- maptitle: This is set at the beginning of this section
- mapnum: This differentiates the initial map from the rseg maps. This is set to 1, meaning there should be no color filling of the river segments
- rseg_leg_title: Riversegment legend title (does not apply to Map 1)
- map_server: The URL to pull the basemap from VDEM. This argument is deprecated since there is no need for a basemap to start from.
- base_Layer: The URL of which layer to pull from the VDEM  map. Also deprecated
- maplabs: The labels to put on the map, as defined in the **Prop Text Labels** section
- nhd: The nhd lines pulled in the **NHD** section
- roads: Pulls in the road layer from the **Load Data** section
- rsegs: The geometries and data of the river segments, will always be `rsegs`
- map_style_set: This is based on the type of style of map set in the parameter `map-style`. This determines aesthetic things like text font and color, fill color, and border colors.
- rivmap_ramp: The scale to use for the river segment map legends. This does not apply to Map 1.

### View_Map_1

This section includes Map 1 into the document. For each table and map, if there are expected flaws (i.e. the data does not contain a `featrs_file_map_bubble_column` column) a message is generated and stored in the `messages` data frame. This is compiled and saved in the **Generate Errors Summary** at the end to show any problems encountered.

### Create Table 1

This section created the feature table after Map 1, showing the 2020 vs 2040 projected uses, feature info like name, number corresponding to the map, and rseg name/id. This section was set upo based on heavy testing of facilities, not sources.

First, the names in `facils` are adjusted based on status, adding an asterisk to indicate proposed/inactive features. Then the `table` dataframe is established pulling  out the specific columns needed and naming them appropriately. The order they are added to `table` is the order they appear in the document. Like many other places in the document, a for loop was set up to add the parameter `featrs_file_table_column` to `table` so it can take any number of columns. Within the for loop, it detects a modeling column and replaces NAs with 'No Model' for GW facilities (where GW fraction = 1).

Column names, label names, and titles are often retrieved from the dataframe `readable`, which is a table of computer friendlu names and their readable counterparts (gw_frac -> GW Fraction). The `table` dataframe is then converted to a flextable (an HTML table that looks nice in the final cdocument) with the `fn_tablegen` custom function. It takes the following inputs

- table: The `table` object
- columns: What columns should be selected. "all" indicates all columns from `table` are to be displayed
- tabletitle: Title of the table
- num = Unlike with the maps, table 1 does not behave differently than the others. This argument is just to set the title of the table (1.1 for this section).
- highlight_col/highlight_limit: This determines if a column should e conditionaly highlighted yellow (used to indicate very negative values for river segments in the section maps). Not used for table 1.1

### GW_Maps

This section creates the aquifer critical cell maps for regions/origins that intersect the Groundwater Management Area. It reads in shapefiles for the critical cells from the OWS onedrive, as well as for the GWMA. The critical cell shaoefiles are the actual geometries of the critical cells, as in a series of rectangles and their coordinates rather than a raster. This is what was received from Aquaveo in 2023. 

For regions within the GWMA, the critical cell map for each aquifer is overlain on the region, even if there are no critical cells within the bounding box. There is an exception built in for the Eastern Shore, which was purposefully avoided for the rest of the document. The only map that matters for the Eastern shore is the Yorktown Eastover aquifer, and this does not affect any other region. So a manual exception as built in to include only that map for that region, and not include it in any others.

Otherwise, if the origin/region is inside the GWMA, a map for the Potomac, Aquia, and Piney point aquifer is created. This section saves these pictures and includes them in the document all within this `if` statement. The  maps are generated using the `fn_gw_mapgen` custom function. This function is very similar to `fn_mapgen`, but a bit simplified since not as many layers are required for the GW maps. Namely, the river segments and NHD lines were removed. Two arguments were added **aquifer_shp**, which uses the shapefiles for the critical cell maps, and **origin_shape**, which uses the geometry of the origin/region to shade anything outside its borders and make it obvious which cells are contained. This section is after the **Create Map 1** section since it reuses most of the same arguments, with the exception of the title, which is defined based on the aquifer being mapped.

### View Table 1

Puts table 1 into the document (this is done by just letting `ft1`, the flextable object, print which puts it in the document). At the beginning and end of this section, there is a `block_section()` function call from the `officer` package. This is what controls this section only to be landscape, while the rest of the document is in portrait. 

### Create Map 2

While this section is called Map 2, it refers to all other maps. These are all the section maps, each river segment metric and shading them to show their relative values based on the `rivmap_ramp`. Since it is one section for all the different metrics (defined in `run_config$riverseg_metrics`), it is all ran within a large `for` loop. The beginning of each iteration determines if the map is supposed to be displayed (the `show_map` parameter defined in the config for each metric).

The `run_config$riverseg_metrics` determines how the map will be set up, determining which dataset to use (all use the `rseg` dataset, with the exception of unmet demand), what column to look for (generally one of the % difference columns calculated in dataframe_generator), what to title the graph, what to title the table columns, the legend ramp, and how to sort the table.

Similair to Map 1, the rsegs are binned based on the ramp defined in `run_config$riverseg_metrics`. `fn_mapgen` is used to create the maps again, with mostly the same inputs. The differences are called out below. These maps are then saved as files.

- mapnum is set to 2, which means it will run `fn_polygonFill`, which colors in the rsegs based on the metric
- rsegs: Uses the `rsegs_sf` object, which is `rsegs` that now includes the bins for the metric
- rivmap_ramp: Pulls the legend ramp defined in `run_config$riverseg_metrics`

### Create Table 2 UPDATED

This section creates the tables that go under the rseg maps in each section. As stated bef6re, the config file dictates what columns to include from what dataframe. This section pulls those listed columns and applies some formatting (round numbers to 1 decimal point, replace blanks with NAs, sort data, filter any above/below specified values, limit how many rows to display). The options in the config runset determine how to apply this formatting, i.e. whether to include NAs, which way to sort, if any filtering should be applied.

Most of this formatting is done with the custom function `fn_filter_table`, which takes the data table and the metric config as arguments and applies the specified formatting described above. `fn_tablegen` then creates the flextable, this time using the highlight_col and limit arguments on the percent difference column displayed in the respective map.

### View Rivseg Maps and Tables

This section fills out the rest of the document after Table 1. For each metric, it starts with a section header. Then it pulls in the relevant narrative textfiles in HARP-2023-Summer/Section_Narratives (tries to pull a text file with the same name as the `run_config$riverseg_metrics[[k]]$run_label`. If this does not exist it just moves on. If it does it includes the contents as plain text). 

The figure file is pulled in if the `show_map` parameter of the config metric is `TRUE` (which generally all are except for Unmet Demand), and includes the table if it exists (which all should have run except for Water Availability). This is once again a loop for each metric, so it repeats for each metric resulting in different sections of the final report.

### Generate Errors Summary

This section compiles all the messages put together for each graph and table section into an HTML document. The purpose of this is often maps showed weird behaviors that did not cause the script to break but did not run how it should have. This document will show any common problems that occurred.
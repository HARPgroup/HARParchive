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
-   rgeos
-   sf
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

-   **foundation_path**: use to access the foundation files in your local machine if needed

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
-   *fns_spatial* : Defines all HARP-analyst-written functions for dealing with spatial data
-   *fn_centroid_coords*: for adding centroid coordinates to a data frame based on the geometry contained, used mostly for labeling purposes

## WSP_Regional_Summaries.RMD

### General Info

This RMD pulls in the csv files created in `Dataframe_Generator.R` and forms them into the desired maps and tables. The final output of this document is the desired Markdown file for the users input. This document also heavily uses mapstyle_config.

### Render

This information can be generated via render or via direct parameter changes in document.

A current working render is as follows:

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
-   **bbox_type** : either auto or vahydro(pulls box from vahydro values), the vahydro type only functional for river segments
    -   *Possible Input* : `auto`, `vahydro`
-   **show_map** : generaton of map in the rmd or not, `FALSE` for no generation

### Related Documents/Functions

-   *fns_spatial* : Defines all HARP-analyst-written functions for dealing with spatial data, see above section for details
-   *fn_mapgen*: contains the mapping process used to create all map types (facils/sources only and rivserseg maps)gen
    -   Uses **fn_filter_map** : contains process of filtering flowlines, labels etc. to be mapped based on size of boundary box, only called within mapping function
-   *fn_tablegen*: creates a flextable from data frame provided (and soon the columns specified within that data)
-   *fn_labelprep* : general actions for label processing, run on nhd labels and road labels after each of their respective functions
-   *fn_nhd_labs* : contains nhd-specific actions for labeling (flowlines and waterbodies); want to do away with this function since it only filters NHD data (this can be done with the rest of the data processing)
-   *mapstyle_config* : contains aesthetic customization for mapping (all map types), including unique styling for each label type (rivers/streams, counties, cities etc.)
-   *riversegmaps_config* : contains customizations specific for rivseg drought maps, including values and colors for % difference ranges
-   *fn_get_memo_nhdplus*: for the memoise function

## Config Files

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
    -   data_set = use 'rseg_no_geom' for metrics that use rseg data. Use 'facils_nogeom' for metrics that use facil data, like unmet demand. 
    -   show_map = 'TRUE' when mapping for rseg data, 'FALSE' when mapping for facil data or unmet demand
    -   digit = the decimals you want to round that table to 

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

### Riversegmaps_config

Creates the coloring for different percentages on the map as well as the highlight level for the tables. 

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

### Example Images

to edit later

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

### Example Images

to edit later

## For working Locality example: (Not Currently Running)

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

### Example Images

to edit later

## Other Documents In Use:

to edit later

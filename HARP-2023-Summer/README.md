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

## Documents

### -Functions:

-   **fn_mapgen** : contains the mapping process used to create all map types (facils/sources only and rivserseg maps)

-   **fn_filter_map** : contains process of filtering flowlines, labels etc. to be mapped based on size of boundary box, only called within mapping function

-   **fn_tablegen** : creates a flextable from data frame provided (and soon the columns specified within that data)

-   **fn_ndh_labs** : contains nhd-specific actions for labeling (flowlines and waterbodies)

-   **fn_road_labs** : contains roads-specific actions for labeling

-   **fn_labelprep** : general actions for label processing, run on nhd labels and road labels after each of their respective functions

-   **fn_download_read** : used for downloading & unpacking less-straightforward filetypes like .zip files

-   **fn_process_geom** : for creating spatial objects by specifying which data column contains geometry information

-   **fn_centroid_coords** : for adding centroid coordinates to a data frame based on the geometry contained, used mostly for labeling purposes

-   **fn_get_pd_min** :

### -Confg Files: Usage explained in Usage section

-   **mapstyle_config** : contains aesthetic customizations for mapping (all map types), including unique styling for each label type (rivers/streams, counties, cities etc.)

-   **riversegmaps_config** : contains customizations specific for rivseg drought maps, including values and colors for % difference ranges

### -Others:

-   **get_impoundment_Smin** :

-   **hydroImpoundment** :

-   **region_maps_batch** : Allows user to run all regions in render format to create files

-   **update_SminCPL_batch** :

-   **waterSupplyElement** :

-   **waterSupplyModelNode** :

-   **WSP_basemap** :

-   **WSP_Regional_Summaries** : The center RMD document where all maps and tables are created using functions

## Usage

There are two locations planners will be editing code, the parameters and any config files. The editing of config files is if a specific aesthetic change is desired. The editing of parameters is where the location intended for analysis is inputted.

### - Config Files

Explain here how to use config files, what gets changed

### - Parameters

-   **rivseg**: river segment ID of basin to be mapped, for map type basin

-   **locality**: locality/county of interest, for map type locality

-   **region**: region of interest, must match a region name in <https://github.com/HARPgroup/HARParchive/blob/master/HARP-2023-Summer/Regions_ProposedReg_053122.csv>

    -   *Possible regions* :`BigSandy_UpperTennessee_1`, `BigSandy_UpperTennessee_2`, `Chowan_1`, `Chowan_2`, `Eastern_Shore`, `MiddleJames_1`, `MiddleJames_2`, `MiddleJames_3`, `NewRiver_1`, `NewRiver_2`, `NorthernCoastalPlain_1`, `NorthernCoastalPlain_2`, `NorthernCoastalPlain_3`, `NorthernPiedmont_1`, `NorthernPiedmont_2`, `NorthernVirginia`, `Roanoke_1`, `Roanoke_2`, `Roanoke_3`, `Shenandoah_1`, `Shenandoah_2`, `SoutheastVirginia`, `UpperJames_1`, `UpperJames_2`, `York_James_1`, `York_James_2`,

-   **type**: either facility(map by facility) or source(map by point source)

    -   *Possible Input*: `facility`, `source`

-   **model_version**: used in pulling facility metrics from vahydro

    -   *Possible Input*: `vahydro-1.0`, etc.

-   **runid_list_facilities**: possible runids for modeled metrics, specific for creation of map 1, can choose multiple, use c()

-   **runid_list_riversegs**:possible runids for modeled metrics, specific for creation of riverseg maps, can choose multiple, use c()

    -   *Possible Input*: Please note these are only some of the typical options, there are other potential runids on VAHydro

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

-   **metric_mod** : the modeled metric name to be mapped & used in table. Does not need to be set to fiveyr_avg_mgy if mapping source
    -   *Possible Metrics*: `wd_mgd`, `gw_demand_mgd`, `l30_Qintake`, `ps_mgd`, `fiveyr_avg_mgy`, etc.
-   **metric_feat** : non-modeled metric name
    -   *Possible Metrics*: `wsp2020_2040_mgy`, `fiveyr_avg_mgy`, etc.
-   **rivseg_metric** : drought metric for riverseg maps, can list multiple, use c()
    -   *Possible Metrics*: `190_Qout`,`7q10`, `l30_Qout`, etc.
-   **map_type**: either basin, locality, or region , to control the extent of the map & table
    -   *Possible Input* : `basin`, `locality`, or `region`
-   **map_styles** : determining map aesthetics like colors, fonts, font size
    -   *Possible Input* : `colorblind`, `default`, `custom`
-   **map_by**: metric(s) to map and scale points/bubbles by. Must include both the runid and metric name for modeled metrics formatted like `runid_11_wd_mgd`
    -   *Possible Input* : \``runid_11_wd_mgd`, `runid_13_wd_mgd`, etc.
-   **limit**: either basins, for including all facils/sources from intersecting basins in the map/table, or boundary to only include those within the locality or regional boundary
    -   *Possible Input* : `basins`,`boundary`
-   **table_col**: specific metrics to include in the table. Must include both the runid and metric name for modeled metrics (formatted like map_by var), can list multiple, use c()
    -   *Possible Input* : c("`runid_11_wd_mgd`", "`runid_13_wd_mgd`", etc.)
-   **bbox_type**: either auto or vahydro(pulls box from vahydro values)
    -   *Possible Input* : `auto`, `vahydro`
-   **output_format** : determines the format of the rendered and saved file
    -   *Possible Input* : `pdf_document`, `word_document`, `html_document`, etc.

### - Rendering

The following is an example render command for the rmd done for measuring points, mapping a region

```         

rmarkdown::render(paste0(getwd(),"/wsp_regional_summaries.Rmd"), 
                  output_file = paste0(export_path,"mappingRMD_knitTestRegion"),
                  output_format = "word_document",
                  params = list(
                    rivseg = "MN0_7870_0000", 
                    locality = "Stafford", 
                    region = "BigSandy_UpperTennessee_1", 
                    type = "source", 
                    model_version = "vahydro-1.0", 
                    runid_list_facilities = c("runid_11","runid_13"), 
                    runid_list_riversegs = c("runid_11","runid_13"),
                    metric_mod = "wd_mgd",
                    metric_feat = "wsp2020_2040_mgy",
                    rivseg_metric = c("l30_Qout", "7q10"),
                    map_type = "region",
                    map_style = "custom",
                    map_by = "fiveyr_avg_mgy",
                    limit = "basins",
                    table_col = c("runid_11_wd_mgd","runid_13_wd_mgd","fiveyr_avg_mgy","wsp2020_2040_mgy"),
                    bbox_type = "auto"))
```

The following is an example render command for the rmd done for facilities, mapping a locality

```         

rmarkdown::render(paste0(getwd(),"/wsp_regional_summaries.Rmd"), 
                  output_file = paste0(export_path,"mappingRMD_knitTestRegion"),
                  output_format = "word_document",
                  params = list(
                    rivseg = "MN0_7870_0000", 
                    locality = "Stafford", 
                    region = "BigSandy_UpperTennessee_1", 
                    type = "facility
                    model_version = "vahydro-1.0", 
                    runid_list_facilities = c("runid_11","runid_13"), 
                    runid_list_riversegs = c("runid_11","runid_13"),
                    metric_mod = "wd_mgd",
                    metric_feat = "wsp2020_2040_mgy",
                    rivseg_metric = c("l30_Qout", "7q10"),
                    map_type = "locality
                    map_style = "custom",
                    map_by = "runid_13_wd_mgd",
                    limit = "basins",
                    table_col = c("runid_11_wd_mgd","runid_13_wd_mgd","fiveyr_avg_mgy","wsp2020_2040_mgy"),
                    bbox_type = "auto"))
```

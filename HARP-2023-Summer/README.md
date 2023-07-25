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

## Usage

There are two locations planners will be editing code, the parameters and any config files. The editing of config files is if a specific aesthetic change is desired. The editing of parameters is where the location intended for analysis is inputted.

### - Config Files

Explain here how to use config files, what gets changed

### - Parameters

-   **rivseg**: river segment ID of basin to be mapped, for map type basin
-   **localit**y: locality/county of interest, for map type locality
-   **region**: region of interest, must match a region name in <https://github.com/HARPgroup/HARParchive/blob/master/HARP-2023-Summer/Regions_ProposedReg_053122.csv>
    -   *Possible regions*: `BigSandy_UpperTennessee_1`, `BigSandy_UpperTennessee_2`, `Chowan_1`, `Chowan_2`, `Eastern_Shore`, `MiddleJames_1`, `MiddleJames_2`, `MiddleJames_3`, `NewRiver_1`, `NewRiver_2`, `NorthernCoastalPlain_1`, `NorthernCoastalPlain_2`, `NorthernCoastalPlain_3`, `NorthernPiedmont_1`, `NorthernPiedmont_2`, `NorthernVirginia`, `Roanoke_1`, `Roanoke_2`, `Roanoke_3`, `Shenandoah_1`, `Shenandoah_2`, `SoutheastVirginia`, `UpperJames_1`, `UpperJames_2`, `York_James_1`, `York_James_2`,
-   **type**: either facility(map by facility) or source(map by point source)
-   **model_version**: used in pulling facility metrics from vahydro
-   **runid_list**: runids for modeled metrics

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

-   **metric**: the modeled metric name to be mapped & used in table. Does not need to be set to fiver_avg_mgy
    -   *Possible Metrics*: `wd_mgd`, `gw_demand_mgd`, `l30_Qintake`, `ps_mgd`, `fiver_avg_mgy`
-   **map_type**: either `basin`, `locality`, or `region` , to control the extent of the map & table
-   **map_by**: metric(s) to map and scale points/bubbles by. Must include both the runid and metric name for modeled metrics formatted like `runid_11_wd_mgd`
-   **limit**: either basins, for including all facils/sources from intersecting basins in the map/table, or boundary to only include those within the locality or regional - boundary
-   **table_col**: specific metrics to include in the flextable. Must include both the runid and metric name for modeled metrics (formatted like map_by var)
-   **bbox_type**: either `auto` or `vahydro`(pulls box from vahydro values)

### - Rendering

The following is an example render command for the rmd

```         

rmarkdown::render("~/Desktop/GitHub/HARParchive/HARP-2023-Summer/mapping_codeReview.Rmd",           
output_file = "~/Desktop/HARPteam23/mappingRMD_knit",
params = list(
rivseg = "JL6_7430_7320",                      
locality = "Culpeper",                      
region = "Shenandoah_2",                      
type = "facility",                      
model_version = "vahydro-1.0",                      
runid_list = c("runid_11","runid_13"),                      
metric = "wd_mgd",                      
map_type = "region",                     
map_by = "runid_11_wd_mgd",                     
limit = "basins",                     
table_col = c("runid_11_wd_mgd","runid_13_wd_mgd","fiveyr_avg_mgy"),                    
bbox_type = "auto"
)
)
```

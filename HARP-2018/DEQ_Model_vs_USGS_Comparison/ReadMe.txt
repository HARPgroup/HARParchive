DEQ_Model_vs_USGS_Comparison
v1.2

Daniel Hildebrand
Kelsey Reitz
Hailey Alspaugh

Users should run the "master_RUN_ALL.R" script in the "code" folder.
Required inputs: 
Location of the "DEQ_Model_vs_USGS_Comparison" directory (include the "DEQ_Model_vs_USGS_Comparison" in the filepath)
USGS gage: either a number ("02077500") or "all"
Specification of whether "new" or "original" data should be used.

If "new" data is specified, data will be downloaded, prepped, and stored within the "data" folder.
If "original" data is specified, pre-downloaded data included within the data directory will be used.
If "all" gages are run, spatial analysis will be conducted and stored in the "spatial_analysis" folder.
No matter which options are selected, all metrics will be calculated and exported as tables in the results folder, all plots will be generated and stored in the results folder, and a dashboard .pdf file will be created and stored in the results folder.

If new gage/segment combos are desired to be run, required inputs include:
Updating "Gage_To_Segment.csv" within the "data" folder to include a row containing the new gage number, associated river segment, and gage drainage area / model river segment area.  Gage drainage areas can be found using the readNWISsite() function, while model river segment areas can be obtained from GIS analysis.
Manually creating a site description and storing it in the "gage_descriptions" directory
Manually creating a map of the river segment and gage location and storing it in the "gage_locations" directory


Updates: 
v1.1: Fixed issue resulting from single gage being run through master script.
v1.2: Fixed Sept. 10% calculation -- prior to this version, Model: Sep.10% was actually Gage: Sep.10% and vice versa.


Session Info from which HARP analysts' results were generated:

R version 3.4.4 (2018-03-15)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows >= 8 x64 (build 9200)

Matrix products: default

locale:
[1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252    LC_MONETARY=English_United States.1252 LC_NUMERIC=C                           LC_TIME=English_United States.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] raster_2.6-7        rgdal_1.3-3         sp_1.3-1            magick_1.9          readr_1.1.1         knitr_1.20          scales_0.5.0        ggplot2_2.2.1       lfstat_0.9.4        lattice_0.20-35     lmom_2.6            xts_0.10-2          PearsonDS_1.1       IHA_0.2-41          zoo_1.8-2           bindrcpp_0.2.2      plyr_1.8.4          lubridate_1.7.4     dataRetrieval_2.7.3 rmarkdown_1.10     

loaded via a namespace (and not attached):
 [1] tinytex_0.5         tidyselect_0.2.4    reshape2_1.4.3      purrr_0.2.4         colorspace_1.3-2    htmltools_0.3.6     yaml_2.1.19         rlang_0.2.0         pillar_1.2.2        glue_1.2.0          RColorBrewer_1.1-2  bindr_0.1.1         stringr_1.3.1       munsell_0.4.3       gtable_0.2.0        caTools_1.17.1      htmlwidgets_1.2     evaluate_0.10.1     labeling_0.3        latticeExtra_0.6-28 curl_3.2            lmomRFA_3.1         highr_0.6           Rcpp_0.12.17        backports_1.1.2     jsonlite_1.5        hms_0.4.2           digest_0.6.15       stringi_1.1.7       dplyr_0.7.5         grid_3.4.4          rprojroot_1.3-2     tools_3.4.4         bitops_1.0-6        magrittr_1.5        lazyeval_0.2.1      tibble_1.4.2        pkgconfig_2.0.1     xml2_1.2.0          dygraphs_1.1.1.4    assertthat_0.2.0    httr_1.3.1          R6_2.2.2            compiler_3.4.4
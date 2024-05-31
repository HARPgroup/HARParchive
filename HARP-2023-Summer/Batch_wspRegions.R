# This script is to do a batch run of all localities and/or regions for the WSP Regional Summary HARP products

#load github harddrive locations from harddrive if you have github repositories
library("sqldf")
basepath='/var/www/R'
source('/var/www/R/config.R')

#define sets to loop through
region_set <- c("BigSandy_UpperTennessee_1", "BigSandy_UpperTennessee_2", "Chowan_1", "Chowan_2", "Eastern_Shore", "MiddleJames_1", "MiddleJames_2", "MiddleJames_3", "NewRiver_1", "NewRiver_2", "NorthernCoastalPlain_1", "NorthernCoastalPlain_2", "NorthernCoastalPlain_3", "NorthernPiedmont_1", "NorthernPiedmont_2", "NorthernVirginia", "Roanoke_1", "Roanoke_2", "Roanoke_3", "Shenandoah_1", "Shenandoah_2", "SoutheastVirginia", "UpperJames_1", "UpperJames_2", "York_James_1", "York_James_2")


#if you want to use the render statements withing the loop below to run one region
#origin_name <- "Chowan_1" 

#run all regions
i <- 0
for (x in region_set) {
  
  i <- i+1
  origin_name <-region_set[i]
  
  rmarkdown::render(paste0(github_location,"/HARParchive/HARP-2023-Summer/Mapping/Dataframe_Generator.Rmd"), 
                    params = list(
                      origin = paste0(origin_name), 
                      origin_type = "region", 
                      featr_type = "facility", 
                      metric_mod = c("wd_mgd", "unmet1_mgd", "unmet7_mgd", "unmet30_mgd"), 
                      model_version = "vahydro-1.0",
                      metric_feat = "wsp2020_2040_mgy", 
                      rivseg_metric = c("l90_Qout", "l30_Qout", "7q10", "Smin_L30_mg"), 
                      runid_list = c("runid_11", "runid_13", "runid_17"), 
                      crs_default = 4326, 
                      limit_featrs_to_origin = FALSE,
                      overwrite_files = TRUE, 
                      base_layer_data = FALSE
                    )
  )
  
  rmarkdown::render(paste0(github_location,"/HARParchive/HARP-2023-Summer/Mapping/WSP_Regional_Summaries.Rmd"), 
                    output_file = paste0(export_path, origin_name, "_wsp"),
                    output_format = "word_document",
                    params = list(
                      origin = paste0(origin_name), 
                      origin_type = "region", 
                      featr_type = "facility", 
                      featrs_file = paste0(export_path, "/", origin_name, "_featrs_sf.csv"), 
                      featrs_file_map_bubble_column = "wsp2020_2040_mgy", 
                      featrs_file_table_column = c("runid_11_wd_mgd","runid_13_wd_mgd","five_yr_avg","wsp2020_2040_mgy"), 
                      rsegs_file = paste0(export_path, "/", origin_name,"_rsegs_sf.csv"), 
                      run_set = "wsp_2020_2040", 
                      runid_list = c("runid_11", "runid_13", "runid_17"), 
                      crs_default = 4326, 
                      map_style = "custom", 
                      bbox_type = "auto",
                      show_map = TRUE))
  
  
}

# #Run all localities - section in progress
# 
# lg_all <- read.csv(paste0(github_location,"/Foundational_Data/data/foundation_dataset_mgy.csv"))
# locality_set <- sqldf('SELECT fips_code FROM lg_all GROUP BY fips_code')
# locality_set <- as.list(locality_set$fips_code)
# #locality_set <- t(locality_set)
# 
# i <- 0
# for (x in locality_set) {
#   i <- i+1
#   origin_name <-locality_set[i]
#   
#   rmarkdown::render(paste0(github_location,"/HARParchive/HARP-2023-Summer/Mapping/Dataframe_Generator.Rmd"), 
#                     params = list(
#                       origin = paste0(origin_name), 
#                       origin_type = "region", 
#                       featr_type = "facility", 
#                       metric_mod = c("wd_mgd", "unmet1_mgd", "unmet7_mgd", "unmet30_mgd"), 
#                       model_version = "vahydro-1.0",
#                       metric_feat = "wsp2020_2040_mgy", 
#                       rivseg_metric = c("l90_Qout", "l30_Qout", "7q10", "Smin_L30_mg"), 
#                       runid_list = c("runid_11", "runid_13", "runid_17"), 
#                       crs_default = 4326, 
#                       limit_featrs_to_origin = FALSE,
#                       overwrite_files = TRUE, 
#                       base_layer_data = FALSE
#                     )
#   )
#   
#   rmarkdown::render(paste0(github_location,"/HARParchive/HARP-2023-Summer/Mapping/WSP_Regional_Summaries.Rmd"), 
#                     output_file = paste0(export_path, origin_name, "_wsp"),
#                     output_format = "word_document",
#                     params = list(
#                       origin = paste0(origin_name), 
#                       origin_type = "region", 
#                       featr_type = "facility", 
#                       featrs_file = paste0(export_path, "/", origin_name, "_featrs_sf.csv"), 
#                       featrs_file_map_bubble_column = "wsp2020_2040_mgy", 
#                       featrs_file_table_column = c("runid_11_wd_mgd","runid_13_wd_mgd","five_yr_avg","wsp2020_2040_mgy"), 
#                       rsegs_file = paste0(export_path, "/", origin_name,"_rsegs_sf.csv"), 
#                       run_set = "wsp_2020_2040", 
#                       runid_list = c("runid_11", "runid_13", "runid_17"), 
#                       crs_default = 4326, 
#                       map_style = "custom", 
#                       bbox_type = "auto",
#                       show_map = TRUE))
#   
# }

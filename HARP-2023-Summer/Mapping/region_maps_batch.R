#This script will loop through regional planning groups and create a map and table for each, exporting each as a rendered document 
#RMD responsible for map & table creation is mapping_codeReview.R
library(data.table) #needed for fread
library(sqldf)
library(pandoc)
basepath='/var/www/R'
source('/var/www/R/config.R')

#get regional planning groups 
regions <- fread('https://github.com/HARPgroup/HARParchive/raw/master/HARP-2023-Summer/Mapping/DataFiles/Regions_ProposedReg_053122.csv')

#get the region names 
region_names <- sqldf("SELECT DISTINCT VMDWA_Reg2 from regions")

#for loop for each of the region names 
for (i in (1:nrow(region_names))) {
  region <- region_names[i,1] #single region name
  #render command, with each doc named with the region
  rmarkdown::render(paste0(getwd(),"/mapping_codeReview.Rmd"), 
                    output_file = paste0(export_path,"mappingRMD_knit_", region), #unique name for each doc 
                    output_format = "word_document",
                    params = list(
                      rivseg = "JL7_7070_0001", 
                      locality = "Stafford", 
                      region = region, 
                      type = "facility", 
                      model_version = "vahydro-1.0", 
                      runid_list_facilities = c("runid_11","runid_13"), 
                      runid_list_riversegs = c("runid_11","runid_13"),
                      metric_mod = "wd_mgd",
                      metric_feat = "wsp2020_2040_mgy",
                      rivseg_metric = "l30_Qout",
                      map_type = "region",
                      map_style = "custom",
                      map_by = "fiveyr_avg_mgy",
                      limit = "basins",
                      table_col = c("runid_11_wd_mgd","runid_13_wd_mgd","fiveyr_avg_mgy","wsp2020_2040_mgy"),
                      bbox_type = "auto"))
  
}



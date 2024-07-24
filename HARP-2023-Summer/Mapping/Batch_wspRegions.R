# This script is to do a batch run of all localities and/or regions for the dataframe generator and WSP Regional Summaries

#load github harddrive locations from harddrive if you have github repositories
library("sqldf")
basepath='/var/www/R'
source('/var/www/R/config.R')

#define sets to loop through
region_set <-
  c(
    "BigSandy_UpperTennessee_1", "BigSandy_UpperTennessee_2",
    "Chowan_1", "Chowan_2", 
    "Eastern_Shore",
    "MiddleJames_1", "MiddleJames_2",
    "MiddleJames_3",#ERRORS HERE!
    "NewRiver_1", "NewRiver_2",
    "NorthernCoastalPlain_1", "NorthernCoastalPlain_2",
    "NorthernCoastalPlain_3", "NorthernPiedmont_1", "NorthernPiedmont_2",
    "NorthernVirginia", "Roanoke_1", 
    #CONNOR STOPPED HERE. Roanoke_2 onward still need testing. Error in Roanoke 2, 685-787 in if(columns == "all")...
    "Roanoke_2", "Roanoke_3",
    "Shenandoah_1", "Shenandoah_2", "SoutheastVirginia",
    "UpperJames_1", "UpperJames_2", "York_James_1", "York_James_2"
  )

#lg_all <- read.csv(paste0(github_location,"/Foundational_Data/data/foundation_dataset_mgy.csv"))
#locality_set <- sqldf('SELECT fips_code FROM lg_all GROUP BY fips_code')
#locality_set <- as.list(locality_set$fips_code)
#locality_set <- t(locality_set)
locality_set <- c("51001",	"51003",	"51005",	"51007",	"51009",	"51011",	"51013",	"51015",	"51017",	"51019",	"51021",	"51023",	"51025",	"51027",	"51029",	"51031",	"51033",	"51035",	"51036",	"51037",	"51041",	"51043",	"51045",	"51047",	"51049",	"51051",	"51053",	"51057",	"51059",	"51061",	"51063",	"51065",	"51067",	"51069",	"51071",	"51073",	"51075",	"51077",	"51079",	"51081",	"51083",	"51085",	"51087",	"51089",	"51091",	"51093",	"51095",	"51097",	"51099",	"51101",	"51103",	"51105",	"51107",	"51109",	"51111",	"51113",	"51115",	"51117",	"51119",	"51121",	"51125",	"51127",	"51131",	"51133",	"51135",	"51137",	"51139",	"51141",	"51143",	"51145",	"51147",	"51149",	"51153",	"51155",	"51157",	"51159",	"51161",	"51163",	"51165",	"51167",	"51169",	"51171",	"51173",	"51175",	"51177",	"51179",	"51181",	"51183",	"51185",	"51187",	"51191",	"51193",	"51195",	"51197",	"51199",	"51510",	"51520",	"51530",	"51540",	"51550",	"51580",	"51590",	"51595",	"51600",	"51620",	"51630",	"51640",	"51650",	"51660",	"51670",	"51678",	"51680",	"51683",	"51685",	"51690",	"51700",	"51710",	"51720",	"51730",	"51740",	"51750",	"51760",	"51770",	"51775",	"51800",	"51810",	"51820",	"51830",	"51840")


#to run a single render statement within the loop, define region or locality name here
origin_name <- "Shenandoah_1" 
#East shore fails as does MiddleJames_3
### Run all regions ##########################
for (x in 18:length(region_set)) {
  origin_name <- region_set[x]
  print(paste0("Rendering ",origin_name))
  
  #RENDER DATAFRAME GENERATOR
  rmarkdown::render(paste0(github_location,"/HARParchive/HARP-2023-Summer/Mapping/Dataframe_Generator.Rmd"), 
                    params = list(
                      origin = paste0(origin_name), 
                      origin_type = "region", 
                      featr_type = "facility", 
                      metric_mod = c("wd_mgd", "unmet1_mgd", "unmet7_mgd", "unmet30_mgd"), 
                      model_version = "vahydro-1.0",
                      metric_feat = "wsp2020_2040_mgy", 
                      rivseg_metric = c("l90_Qout", "l30_Qout", "7q10", "consumptive_use_frac", "Smin_L30_mg"), 
                      runid_list = c("runid_11", "runid_13", "runid_17"), 
                      crs_default = 4326, 
                      limit_featrs_to_origin = FALSE,
                      overwrite_files = TRUE, 
                      base_layer_data = FALSE
                    )
  )
  
  # #check if file exists and may be open, which will cause document render to fail after generating doc successfully
   fileName <- paste0(export_path, origin_name, "_wsp")
  # if(file.exists(paste0(export_path, origin_name, "_wsp.docx"))){
  #   print("Reminder, close any open version of the regional summary doc before rendering it.")
  #   #fileName <- paste0(export_path, origin_name, "_wsp2")
  # }
  
  #RENDER WSP REGIONAL SUMMARY DOC
  rmarkdown::render(paste0(github_location,"/HARParchive/HARP-2023-Summer/Mapping/WSP_Regional_Summaries.Rmd"), 
                    output_file = fileName,
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
  
  # #clear environment and reload config, so that prior maps don't interfere with next region
  # rm(list = ls())
  # library("sqldf")
  # basepath='/var/www/R'
  # source('/var/www/R/config.R')
}

#### Run all localities ##########################
for (x in locality_set) {
  origin_name <- x

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

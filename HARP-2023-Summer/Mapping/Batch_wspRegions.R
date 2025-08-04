# This script is to do a batch run of all localities and/or regions for the dataframe generator and WSP Regional Summaries

#load github harddrive locations from harddrive if you have github repositories
library("sqldf")
basepath='/var/www/R'
source('/var/www/R/config.R')

#define sets to loop through
region_set <-
  c(
    "BigSandy_UpperTennessee_1",  #1
    "BigSandy_UpperTennessee_2", #2
    "Chowan_1", #3
    "Chowan_2", #4
    "Eastern_Shore", #5
    "MiddleJames_1", #6
    "MiddleJames_2", #7
    "MiddleJames_3", #8
    "NewRiver_1", #9
    "NewRiver_2", #10
    "NorthernCoastalPlain_1", #11
    "NorthernCoastalPlain_2", #12
    "NorthernCoastalPlain_3", #13
    "NorthernPiedmont_1", #14
    "NorthernPiedmont_2", #15
    "NorthernVirginia", #16
    "Roanoke_1", #17
    "Roanoke_2", #18
    "Roanoke_3", #19
    "Shenandoah_1", #20
    "Shenandoah_2", #21
    "SoutheastVirginia", #22
    "UpperJames_1", #23
    "UpperJames_2", #24
    "York_James_1", #25
    "York_James_2" #26
  )




locality_set <- 
  c(
    "51001",	"51003",	"51005",	"51007",	"51009",	"51011",	"51013",	"51015",	"51017",	"51019",
    "51021",	"51023",	"51025",	"51027",	"51029",	"51031",	"51033",	"51035",	"51036",	"51037",
    "51041",	"51043",	"51045",	"51047",	"51049",	"51051",	"51053",	"51057",	"51059",	"51061",
    "51063",	"51065",	"51067",	"51069",	"51071",	"51073",	"51075",	"51077",	"51079",	"51081",
    "51083",	"51085",	"51087",	"51089",	"51091",	"51093",	"51095",	"51097",	"51099",	"51101",
    "51103",	"51105",	"51107",	"51109",	"51111",
    "51113",	"51115",	"51117",	"51119",	"51121",
    "51125",	"51127",	"51131",	"51133",	"51135",	"51137",	"51139",	"51141",	"51143",	"51145",
    "51147",	"51149",	"51153",	"51155",	"51157",	"51159",	"51161",	"51163",	"51165",	"51167",
    "51169",	"51171",	"51173",	"51175",	"51177",	"51179",	"51181",	"51183",	"51185",	"51187",
    "51191",	"51193",	"51195",	"51197",	"51199",	"51510",	"51520",	"51530",	"51540",	"51550",
    "51580",	"51590",	"51595",	"51600",	"51620",  "51630",	"51640",	"51650",	"51660",	"51670",
    "51678",	"51680",	"51683",	"51685",  "51690",	"51700",	"51710",	"51720",	"51730",	"51740",
    "51750",	"51760",	"51770",	"51775",	"51800",	"51810",	"51820",	"51830",	"51840")



#to run a single render statement within the loop, define region or locality name here

origin_name <- "Chowan_2" 

### Run all regions ##########################
for (x in 1:length(region_set)) {
  origin_name <- region_set[x]
  print(paste0("Rendering ",origin_name))
  #RENDER DATAFRAME GENERATOR - REGION
  
  ## Had some spotty connection problems where it took 2 or 3 times to run successfully, so now it runs 5 times after an error
  for (k in 1:5) {
    tryCatch({
      rmarkdown::render(paste0(github_location,"/HARParchive/HARP-2023-Summer/Mapping/Dataframe_Generator.Rmd"), 
                        params = list(
                          origin = paste0(origin_name), 
                          origin_type = "region", 
                          featr_type = "facility", 
                          metric_mod = c("wd_mgd", "unmet1_mgd", "unmet7_mgd", "unmet30_mgd"), 
                          model_version = "vahydro-1.0",
                          metric_feat = "wsp2020_2040_mgy", 
                          rivseg_metric = c("l90_Qout", "l30_Qout", "7q10", "Qout", "WA_90_mgd","l90_cc_Qout","l30_cc_Qout"), 
                          runid_list = c("runid_11", "runid_13", "runid_17", "runid_1000"), 
                          crs_default = 4326, 
                          limit_featrs_to_origin = FALSE,
                          overwrite_files = TRUE, 
                          base_layer_data = FALSE)
                        )
        break
    }, error =function(repeatit) {
      print(paste0('DF gen ',origin_name,'Fail Attempt: ',k))
    })
  }


  
  # #check if file exists and may be open, which will cause document render to fail after generating doc successfully
   fileName <- paste0(export_path, origin_name, "_wsp")
  if(file.exists(paste0(export_path, origin_name, "_wsp.docx"))){
    print("Reminder, close any open version of the regional summary doc before rendering it.")
    #fileName <- paste0(export_path, origin_name, "_wsp2")
  }

   for (k in 1:5) {
     tryCatch({
       #RENDER WSP REGIONAL SUMMARY DOC - REGION
       rmarkdown::render(paste0(github_location,"/HARParchive/HARP-2023-Summer/Mapping/WSP_Regional_Summaries.Rmd"), 
                         output_file =  paste0(export_path, origin_name, "_wsp"),
                         # output_format = "word_document",
                         params = list(
                           origin = paste0(origin_name), 
                           origin_type = "region", 
                           featr_type = "facility", 
                           featrs_file = paste0(export_path, "/", origin_name, "_featrs_sf.csv"), 
                           featrs_file_map_bubble_column = "wsp2020_2040_mgy", 
                           featrs_file_table_column = c("Use_Type","runid_11_wd_mgd","runid_13_wd_mgd","wsp2020_2040_mgy"), 
                           rsegs_file = paste0(export_path, "/", origin_name,"_rsegs_sf.csv"), 
                           run_set = "wsp_2020_2040", 
                           runid_list = c("runid_11", "runid_13", "runid_17", "runid_1000"), 
                           crs_default = 4326, 
                           map_style = "custom", 
                           bbox_type = "custom",
                           show_map = TRUE))
       break
     }, error = function(repeatit) {
       print(paste0('WSP gen ',origin_name,'Fail Attempt: ',k))
     })
   }
       

  # #clear environment and reload config, so that prior maps don't interfere with next region
  # rm(list = ls())
  # library("sqldf")
  # basepath='/var/www/R'
  # source('/var/www/R/config.R')
}



origin_name <- '51011'
basepath='/var/www/R'
source('/var/www/R/config.R')

#### Run all localities ##########################
for (x in locality_set) {
  origin_name <- x
  print(paste0("Rendering ",origin_name))
  ## DF GENERATOR - LOCALITY
  for (k in 1:5) {
    tryCatch({
      rmarkdown::render(paste0(github_location,"/HARParchive/HARP-2023-Summer/Mapping/Dataframe_Generator.Rmd"),
                        params = list(
                          origin = origin_name,
                          origin_type = "locality",
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
      break
    }, error = function(repeatit) {
      print(paste0('df gen ',origin_name,'Fail Attempt: ',k))
    })
  }
  ## SUMMARY - LOCALITY
  for (k in 1:5) {
    tryCatch({
      rmarkdown::render(paste0(github_location,"/HARParchive/HARP-2023-Summer/Mapping/WSP_Regional_Summaries.Rmd"),
                        output_file = paste0(export_path, origin_name, "_wsp"),
                        # output_format = "word_document",
                        params = list(
                          origin = paste0(origin_name),
                          origin_type = "locality",
                          featr_type = "facility",
                          featrs_file = paste0(export_path, "/", origin_name, "_featrs_sf.csv"),
                          featrs_file_map_bubble_column = "wsp2020_2040_mgy",
                          featrs_file_table_column = c("Use_Type","runid_11_wd_mgd","runid_13_wd_mgd","wsp2020_2040_mgy"),
                          rsegs_file = paste0(export_path, "/", origin_name,"_rsegs_sf.csv"),
                          run_set = "wsp_2020_2040",
                          runid_list = c("runid_11", "runid_13", "runid_17", "runid_1000"),
                          crs_default = 4326,
                          map_style = "custom",
                          bbox_type = "custom",
                          show_map = TRUE))
      
      
      break
    }, error = function(repeatit) {
      print(paste0('WSP gen ',origin_name,'Fail Attempt: ',k))
    })
  }
  
}

ex <- dbGetQuery(conn = ds$connection, "
SELECT f.hydroid, f.name, wsp.propvalue AS fac2040, mp.propcode AS mpiput2040,
TO_TIMESTAMP(wsp.modified)::date AS fac, TO_TIMESTAMP(mp.modified)::date AS modelinput, m.pid,m.propname,
mpr.propcode AS rseg
FROM dh_feature f 
INNER JOIN dh_properties wsp
  ON f.hydroid = wsp.featureid
  AND f.bundle = 'facility'
  AND wsp.propname = 'wsp2020_2040_mgy'
LEFT JOIN dh_properties gw
  ON f.hydroid = gw.featureid
  AND gw.propname = 'gw_frac'
LEFT JOIN dh_properties m
  ON f.hydroid = m.featureid
  AND m.propcode = 'vahydro-1.0'
LEFT JOIN dh_properties mp
  ON mp.featureid = m.pid
  AND mp.propname = 'wsp2020_2040_mgy'
LEFT JOIN dh_properties mpr
  ON mpr.featureid = m.pid
  AND mpr.propname = 'riverseg'

WHERE wsp.propvalue != mp.propcode::numeric
  AND (gw.propvalue < 1 OR gw.propvalue IS NULL)
  AND ftype NOT ILIKE '%wsp%'
")

write.csv(ex, row.names = F, "C:\\Users\\ejp42531\\OneDrive - Commonwealth of Virginia\\OWS\\WSPA\\10_Yr Resubmission_2023/WSP2040_projecton_discrepencies.csv")

## Based on all rsegs (df genrator without the filtering chunk)
rsegs$tidal <- substr(rsegs$riverseg, nchar(rsegs$riverseg) - 3, nchar(rsegs$riverseg)) == '0000'

nawarsegs <- rsegs[!is.na(rsegs$pid) & !is.na(rsegs$runid_13_l90_Qout) & !rsegs$tidal &
                     (is.na(rsegs$runid_13_WA_90_mgd) | is.na((rsegs$runid_11_WA_90_mgd))) ,]

write.csv(nawarsegs, row.names = F, "Rsegs no WA.csv")

nomodels <- rsegs[!rsegs$tidal & (is.na(rsegs$runid_13_l90_Qout) | is.na(rsegs$runid_11_l90_Qout)), ]
write.csv(nawarsegs, row.names = F, "C:\\Users\\ejp42531\\OneDrive - Commonwealth of Virginia\\OWS\\WSPA\\10_Yr Resubmission_2023/rsegs no rund13.csv")

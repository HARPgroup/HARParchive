


rmarkdown::render("C:/Users/nrf46657/Desktop/GitHub/HARParchive/HARP-2023-Summer/WSP_Regional_Summaries.Rmd", 
                  output_file = "C:/Users/nrf46657/Desktop/GitHub/plots/JU4_7330_7000_fac",
                  output_format = "word_document",
                  params = list(
                    rivseg = "JU4_7330_7000", 
                    locality = "Stafford", 
                    region = "BigSandy_UpperTennessee_1", 
                    type = "facility", 
                    model_version = "vahydro-1.0", 
                    runid_list_facilities = c("runid_11","runid_13"), 
                    runid_list_riversegs = c("runid_11","runid_13"),
                    metric_mod = "wd_mgd",
                    metric_feat = "wsp2020_2040_mgy",
                    rivseg_metric = c("l30_Qout", "7q10"),
                    map_type = "basin",
                    map_style = "custom",
                    map_by = "fiveyr_avg_mgy",
                    limit = "basins",
                    table_col = c("runid_11_wd_mgd","runid_13_wd_mgd","fiveyr_avg_mgy","wsp2020_2040_mgy"),
                    bbox_type = "auto"))

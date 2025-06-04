## Updating the region and counties sf
mapping <- read.csv(paste0(github_location,"/HARParchive/HARP-2023-Summer/Mapping/Data/Regions_ProposedReg_050625.csv"))

counties_file <- paste0(github_location, '/HARParchive/HARP-2023-Summer/Mapping/Data/counties2_sf.csv')
counties <- sf::st_as_sf(read.csv(counties_file), wkt = "WKT", crs=crs_default, remove=FALSE)

## Cant use sqldf for sf dfs, so just writing with a for loop
for (i in 1:nrow(counties)) {
  
  counties$Region[i] <- mapping$CPU[mapping$County == counties$name[i]]
  
}

regions <- data.frame(region = unique(counties$Region), WKT = NA)

for (i in 1:nrow(regions)) {
  
  regions$WKT[i] <- st_as_text(st_union(counties[counties$Region == regions$region[i],]))
  
}


write.csv(regions, paste0(github_location,"/HARParchive/HARP-2023-Summer/Mapping/Data/regions_sf.csv"), row.names = F)

counties <-  st_drop_geometry(counties)
write.csv(counties, paste0(github_location, '/HARParchive/HARP-2023-Summer/Mapping/Data/counties_sf.csv'), row.names = F)

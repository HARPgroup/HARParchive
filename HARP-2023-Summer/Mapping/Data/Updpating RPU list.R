
## Eport form Andrew
rpu_list <- read.csv(paste0(github_location,'/HARParchive/HARP-2023-Summer/Mapping/Data/Locality_RPU.csv'))
## Mapping of friendly names to CPU names for regions
namekey  <- read.csv(paste0(github_location,'/HARParchive/HARP-2023-Summer/Mapping/Data/RPA Name key.csv'))
rpus <- read.csv(paste0(github_location,'/HARParchive/HARP-2023-Summer/Mapping/Data/Regions_ProposedReg_050625.csv'))

## Updating with newest matching from Andrew (or any other WSPA planner)
rpu <- sqldf('
SELECT rpus.County, key.CPU
FROM rpus
LEFT JOIN rpu_list rlist
  ON rpus.County = rlist.Name
LEFT JOIN namekey key
  ON key.WSPA = rlist.RPA
')

write.csv(rpu, paste0(github_location,'/HARParchive/HARP-2023-Summer/Mapping/Data/Regions_ProposedReg_050625'), row.names = F)

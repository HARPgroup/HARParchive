## Updating RPU Process: Received Locality_RPU.csv from Andrew (name changed when he sent it over), saved
## to Data directory. The namekey turns the RPU names with spaces into the underscored names expected from Summaries
## The Regions_ProposedReg_050625.csv file is the file to be updated that the sf is created based on
## Then run Updating regionsf.R

## Export form Andrew
rpu_list <- read.csv(paste0(github_location,'/HARParchive/HARP-2023-Summer/Mapping/Data/Locality_RPU.csv'))
## Mapping of friendly names to CPU names for regions
namekey  <- read.csv(paste0(github_location,'/HARParchive/HARP-2023-Summer/Mapping/Data/RPA Name key.csv'))
rpus <- read.csv(paste0(github_location,'/HARParchive/HARP-2023-Summer/Mapping/Data/Regions_ProposedReg_060925.csv'))

## Updating with newest matching from Andrew (or any other WSPA planner)
rpu <- sqldf::sqldf('
SELECT rpus.County, key.CPU
FROM rpus
LEFT JOIN rpu_list rlist
  ON rpus.County = rlist.Name
LEFT JOIN namekey key
  ON key.WSPA = rlist.RPA
')

write.csv(rpu, paste0(github_location,'/HARParchive/HARP-2023-Summer/Mapping/Data/Regions_ProposedReg_060925.csv'), row.names = F)

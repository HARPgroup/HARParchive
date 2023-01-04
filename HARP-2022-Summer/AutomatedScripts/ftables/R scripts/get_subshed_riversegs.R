
# Setup
suppressPackageStartupMessages(library("hydrotools")) #needed to pull values from VAHydro 
# Link data source (ds) to VAHydro
basepath='/var/www/R';
source("/var/www/R/config.R") #will need file in same folder/directory
ds <- RomDataSource$new(site, rest_uname = rest_uname)
ds$get_token(rest_pw)

#commands
argst <- commandArgs(trailingOnly = T)
path <- argst[1]
#testing ! comment out later:
path <- '/aa_HARP/aa_GitHub/HARParchive/HARP-2022-Summer/AutomatedScripts/ftables/'

#identify subsheds by a name longer than 29 characters
rsegs <- RomFeature$new(ds,list(ftype='vahydro',bundle='watershed'),TRUE)
subshed_hydrocodes <- subset(rsegs$hydrocode, nchar(rsegs$hydrocode)>29)
subshed_hydrocodes <- data.frame(subshed_hydrocodes) #collect hydrocodes into data frame

#make hydrocodes into riverseg's only (for ftable_creation purposes)
subshed_riversegs <- gsub("vahydrosw_wshed_","",as.character(subshed_hydrocodes$subshed_hydrocodes))
subshed_riversegs <- data.frame(subshed_riversegs)

#write to csv
file <- paste(path, 'subshed_riversegs.txt', sep='')
write.table(subshed_riversegs, file = file, quote = FALSE, row.names = FALSE, col.names = FALSE)

#subsheds <- read.csv('/aa_HARP/aa_GitHub/HARParchive/HARP-2022-Summer/AutomatedScripts/ftables/subshed_riversegs.csv', sep='')

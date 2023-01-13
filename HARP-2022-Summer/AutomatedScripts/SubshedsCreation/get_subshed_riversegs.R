

#commands
argst <- commandArgs(trailingOnly = T)
path <- argst[1]
#testing ! comment out later:
#path <- '/aa_HARP/aa_GitHub/HARParchive/HARP-2022-Summer/AutomatedScripts/SubshedsCreation/'

#retrieve subshed da----
site <- "http://deq1.bse.vt.edu/d.dh"    #Specify the site of interest, either d.bet OR d.dh

# Load Libraries
suppressPackageStartupMessages(library("hydrotools")) #needed to pull values from VAHydro 
basepath='/var/www/R';
source(paste(basepath,'config.R',sep='/'))
ds = RomDataSource$new(site, rest_uname)
ds$get_token(rest_pw)

# get the DA, need to grab a model output first in order to ensure segments with a channel subcomp
# are included

# GET DA
df_area <- data.frame(
  'model_version' = c('vahydro-1.0',  'vahydro-1.0',  'vahydro-1.0'),
  'runid' = c('runid_11', '0.%20River%20Channel', 'local_channel'),
  'runlabel' = c('QBaseline_2020', 'comp_da', 'subcomp_da'),
  'metric' = c('Qbaseline', 'drainage_area', 'drainage_area')
)
da_data <- om_vahydro_metric_grid(
  metric = metric, runids = df_area,
  base_url = paste(site,'entity-model-prop-level-export',sep="/"),
  ds = ds
)
da_data <- sqldf(
  "select pid, comp_da, subcomp_da, riverseg,
   CASE
    WHEN comp_da is null then subcomp_da
    ELSE comp_da
    END as da
   from da_data
  ")

#isolate da for our subshed:
vahydro_subs <- subset(da_data, nchar(da_data$riverseg)>13)
vahydro_subs <- vahydro_subs[,-2:-3]

file <- paste(path, 'subshed_list.csv', sep='')
write.table(vahydro_subs, file = file, quote = FALSE, row.names = FALSE, col.names = TRUE, sep=",")

#-old----
#identify subsheds by a name longer than 29 characters
#rsegs <- RomFeature$new(ds,list(ftype='vahydro',bundle='watershed'),TRUE)
#subshed_hydrocodes <- subset(rsegs$hydrocode, nchar(rsegs$hydrocode)>29)
#subshed_hydrocodes <- data.frame(subshed_hydrocodes) #collect hydrocodes into data frame

#make hydrocodes into riverseg's only (for ftable_creation purposes)
#subshed_riversegs <- gsub("vahydrosw_wshed_","",as.character(subshed_hydrocodes$subshed_hydrocodes))
#subshed_riversegs <- data.frame(subshed_riversegs)

#write to csv
#file <- paste(path, 'subshed_riversegs.txt', sep='')
#write.table(subshed_riversegs, file = file, quote = FALSE, row.names = FALSE, col.names = FALSE)
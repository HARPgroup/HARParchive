
# Script for exporting run data from an h5 to CSV
# Script will have 3 inputs: h5_file_path, output_file_path, and data_source_table 

library(rhdf5)
library(R.utils)

# Accepting command arguments:
argst <- commandArgs(trailingOnly = T)
h5_file_path <- argst[1] # arguments default as class 'character' ; name of the .h5
output_file_path <- argst[2] #name of csv being created
data_source_table <- argst[3] # Path to data table within h5 that begins with /[groupname]

# Reading in table from h5
fid = H5Fopen(h5_file_path) # Opens the h5 file, fid is a h5 identifier
did = H5Dopen(fid, data_source_table) # Opens the data table of interest using the path provided
pwater <- H5Dread(did, bit64conversion = "double")
origin <- "1970-01-01"
pwater$index <- as.POSIXct((pwater$index)/10^9, origin = origin, tz = "UTC")

# Exporting to a csv
write.table(pwater,file = output_file_path, sep = ",", row.names = FALSE) # Maybe .csv should be added to the end of file argument?


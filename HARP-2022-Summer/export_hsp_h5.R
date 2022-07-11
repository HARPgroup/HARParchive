#Script for exporting run data from an h5 to CSV
# Script will have 3 inputs: h5_file_path, output_file_path, and data_source_table 

library(rhdf5)
library(R.utils)

# Accepting command arguments:
argst <- commandArgs(trailingOnly = T)
h5_file_path <- argst[1]
output_file_path <- argst[2]
data_source_table <- argst[3]

# Reading in table from h5
fid = H5Fopen(h5_file_path)
did = H5Dopen(fid, data_source_table)
pwater <- H5Dread(did, bit64conversion = "double")
origin <- "1970-01-01"
pwater$index <- as.POSIXct((pwater$index)/1000000000, origin = origin, tz = "UTC")

#Exporting to a csv
write.table(pwater,file = output_file_path, sep = ",", row.names = FALSE)









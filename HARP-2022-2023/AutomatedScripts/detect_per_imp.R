#Script to determine if an h5 is from a pervious or impervious landuse 

library(rhdf5)
library(R.utils)
library(stringr)

argst <- commandArgs(trailingOnly = T)
h5_file_path <- argst[1]
#h5_file_path <- '/opt/model/p53/p532c-sova/output/hspf/land/out/for/hsp2_2022/forA51037.h5'   #comment out

h5_ls <- paste(h5ls(h5_file_path, recursive = FALSE))
ls_string <- toString(h5_ls[2])
#ls_string <- "c(\"CONTROL\", \"PERLND\", \"RESULTS\", \"RUN_INFO\", \"TIMESERIES\")"  #comment out 
if (str_detect(ls_string, 'PERLND')== TRUE) {
  var1=1
  }
if (str_detect(ls_string, 'IMPLND')== TRUE) {
  var1=0 
}

cat(var1)
#prints 1 for pwater table, prints 0 for iwater table
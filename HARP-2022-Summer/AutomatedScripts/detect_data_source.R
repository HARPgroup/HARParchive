#Script will detect if a data table path is present in an h5 file

library(rhdf5)
library(R.utils)
library(stringr)

argst <- commandArgs(trailingOnly = T)
h5_file_path <- argst[1]
data_source_path <- argst[2] #path should begin with / and not contain 'table' at the end
#data_source_path <- '/RESULTS/PERLND_P001/PWATER'   #comment out
#h5_file_path <- '/media/model/p532/out/land/h5/for/hsp2_2022/forA51037.h5'   #comment out

h5_ls <- h5ls(h5_file_path) #h5_ls becomes a data frame
group_string <- toString(h5_ls[,1])
#ls_string <- "c(\"CONTROL\", \"PERLND\", \"RESULTS\", \"RUN_INFO\", \"TIMESERIES\")"  #comment out 
if (str_detect(group_string, data_source_path)== TRUE) {
  var1=1
}
if (str_detect(group_string, data_source_path)== FALSE) {
  var1=0 
}
cat(var1)
#Testing is successful for pwater and iwater paths 
# hydr path in a river seg h5?
#Ouputs 1 if data source is present, 0 if not 
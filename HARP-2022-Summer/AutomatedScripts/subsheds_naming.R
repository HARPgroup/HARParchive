# This script generates new names for subwatersheds, and checks that the name is unique

argst <- commandArgs(trailingOnly = T)
subshed <- argst[1]
path <- argst[2]
#testing:
# subshed <- 'PS2_5560_5100_linville_creek'
# path <- '/aa_HARP/aa_GitHub/HARParchive/HARP-2022-Summer/AutomatedScripts/Subsheds'

splits <- strsplit(subshed, "_") #creates a list of 1
ds <- as.numeric(splits[[1:2]])

names <- read.csv('http://deq1.bse.vt.edu:81/p6/vadeq/config/catalog/geo/p600/rivernames.csv', sep=',')
names <- names[1]

ids <- t(data.frame(strsplit(names$river, "_")))
rownames(ids) <- NULL
unique_ids <- ids[,2]

repeat {
  if(ds %in% unique_ids) {
    ds <- ds + 1
   } else {
      break
  }
}

new_name <- paste(splits[[1]][[1]],ds,ds-1, sep = "_")

write(new_name, file = paste0(path,"/", new_name,".txt"), sep = ",")

#proportioning function & script based on area weight
#So far, works for landuse proportioning and water area proportioning

suppressPackageStartupMessages(library("sqldf"))

argst <- commandArgs(trailingOnly = T)
file <- argst[1]
subshed <- argst[2]
main_seg <- argst[3]
sub_da <- as.numeric(argst[4])

#TESTING
#main_seg <- 'PS2_5560_5100'
#subshed <- 'PS2_5568_5560'
#da <- 46.04
#file <- 'HARParchive/HARP-2022-Summer/AutomatedScripts/SubshedsCreation/land_water_area.csv'
#file <- 'HARParchive/HARP-2022-Summer/AutomatedScripts/SubshedsCreation/land_use_2013VAHYDRO2018615.csv'
#-- -- 

file <- read.csv(file, sep=',')

area_propor <- function(
    subshed,
    main_seg,
    sub_da, #subshed drainage area
    file
){
# -- -- -- 
  # remove end row(s) and save for later, if they exist
  end_row <-
    as.numeric(rownames(file[grep('end', file[, 1]),])) #the row number of 'end'
  
  if (length(end_row) != 0) {
    #there exists an 'end' and potentially NOTES
    last_row <- as.numeric(length(file[, 1]))
    file_end <- file[(end_row - 1):last_row, ]
    file <-
      file[-(end_row - 1):-last_row, ] #just the data of the file
  }
# -- -- --
  # isolate data we want to work with
  main_segs <- sqldf(paste0("select * from file where riverseg = '",main_seg,"'"))
  subsheds <- sqldf(paste0("select * from file where riverseg = '",subshed,"'"))
  
  if (length(subsheds[, 1]) == 0) {
    subsheds <- data.frame(matrix(0, 1, length(colnames(main_segs))))
    colnames(subsheds) <- colnames(main_segs)
  }
  # sum subshed area
  sub_area <- sum(subsheds[-1:-2]) / 640
  
# -- -- --
  # if subshed area has values, have to add them back to main_seg
  if (sub_area != 0) {
    
    if (colnames(file)[2] == "landseg") { #means it's a 1 entry per lrseg file
      
      for (i in 1:length(main_segs[-1:-2])) {
        #for each landuse in subshed, find the corresponding data in main_segs
        sub_lsegs <- subsheds[grep(main_segs[i, 2], subsheds[, 2]), ]
        
        main_lsegs <- main_segs[grep(main_segs[i, 2], main_segs[, 2]), ]
        
        if (length(sub_lsegs[, 1]) != 0) {
          main_segs[i, -1:-2] <- sub_lsegs[-1:-2] + main_lsegs[-1:-2]
        } # else: main_segs remains the same
      }
      
    } else { #means it's a 1 entry per rseg file
      main_segs[-1:-2] <- subsheds[-1:-2] + main_segs[-1:-2]
    }
  }
# -- -- --
  mainws_area <- sum(main_segs[-1:-2])/640 #calculate the main ws total area
  propor <- sub_da/mainws_area
  
  # do the proportioning
  new_sub <- main_segs
  new_sub[-1:-2] <- main_segs[-1:-2] * propor
  new_sub[,1] <- subshed
  
  main_segs[-1:-2] <- main_segs[-1:-2] * (1-propor)
  
  remove <- subset(file, file[,1] != main_seg) #remove old land use values
  remove <- subset(remove, remove[,1] != subshed) # remove any pre-existing subshed values
  
  file <- rbind(remove, new_sub, main_segs)
  
  # order by lrseg or just rseg
  if (colnames(file)[2] == "landseg") { #means it's a 1 entry per lrseg file
    file <- file[order(file$landseg, file[,1]),]
  } else { #means it's a 1 entry per rseg file
    file <- file[order(file[,1]),]
  }
  
  # add end row(s) back if exists
  if (length(end_row) != 0) {
    file <- rbind(file,file_end) #add the end section back on, if it exists
  }
  
  # return formatted file
  return(file)
  
}
  
file_new <- area_propor(subshed, main_seg, sub_sub_da, file)
  
write.table(file,
            file=file,
            append = FALSE,
            quote = FALSE,
            sep = ",",
            row.names = FALSE,
            col.names = TRUE)

message('proportioned the river segment and subshed')

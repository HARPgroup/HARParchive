#proportioning function & script based on area weight
#So far, works for landuse proportioning and water area proportioning

argst <- commandArgs(trailingOnly = T)
file <- argst[1]
subshed <- argst[2]
main_seg <- argst[3]
da <- as.numeric(argst[4])
cols <- argst[5] #this is a maybe ; if usage is only for lrseg files, cols != an argument anymore

#TESTING
#main_seg <- 'PS2_5560_5100'
#subshed <- 'PS2_5568_5560'
#da <- 46.04
#file <- 'HARParchive/HARP-2022-Summer/AutomatedScripts/SubshedsCreation/land_water_area.csv'
#file <- 'HARParchive/HARP-2022-Summer/AutomatedScripts/SubshedsCreation/SCRORG.csv'
#file <- 'HARParchive/HARP-2022-Summer/AutomatedScripts/SubshedsCreation/land_use_2013VAHYDRO2018615.csv'
#cols <- -1:-2 
#-- -- 

table <- read.csv(file, sep=',')

area_propor <- function(
    subshed,
    main_seg,
    sub_da, #subshed drainage area
    table, 
    cols #col(s) to be proportioned --> table[,cols] to call
){
# -- -- -- 
  # remove end row(s) and save for later, if they exist
  end_row <-
    as.numeric(rownames(table[grep('end', table[, 1]),])) #the row number of 'end'
  
  if (length(end_row) != 0) {
    #there exists an 'end' and potentially NOTES
    last_row <- as.numeric(length(table[, 1]))
    table_end <- table[(end_row - 1):last_row, ]
    table <-
      table[-(end_row - 1):-last_row, ] #just the data of the table
  }
# -- -- --
  # isolate data we want to work with
  main_segs <- table[grep(main_seg, table[, 1]),]
  subsheds <- table[grep(subshed, table[, 1]),]
  
  if (length(subsheds[, 1]) == 0) {
    subsheds <- data.frame(matrix(0, 1, length(colnames(main_segs))))
    colnames(subsheds) <- colnames(main_segs)
  }
  # sum subshed area
  sub_area <- sum(subsheds[cols]) / 640
  
# -- -- --
  # if subshed area has values, have to add them back to main_seg
  if (sub_area != 0) {
    
    if (colnames(table)[2] == "landseg") { #means it's a 1 entry per lrseg file
      
      for (i in 1:length(main_segs[-cols])) {
        #for each landuse in subshed, find the corresponding data in main_segs
        sub_lsegs <- subsheds[grep(main_segs[i, 2], subsheds[, 2]), ]
        
        main_lsegs <- main_segs[grep(main_segs[i, 2], main_segs[, 2]), ]
        
        if (length(sub_lsegs[, 1]) != 0) {
          main_segs[i, cols] <- sub_lsegs[cols] + main_lsegs[cols]
        } # else: main_segs remains the same
      }
      
    } else { #means it's a 1 entry per rseg file
      main_segs[cols] <- subsheds[cols] + main_segs[cols]
    }
  }
# -- -- --
  mainws_area <- sum(main_segs[cols])/640 #calculate the main ws total area
  propor <- da/mainws_area
  
  # do the proportioning
  new_sub <- main_segs
  new_sub[cols] <- main_segs[cols] * propor
  new_sub[,1] <- subshed
  
  main_segs[cols] <- main_segs[cols] * (1-propor)
  
  remove <- subset(table, table[,1] != main_seg) #remove old land use values
  remove <- subset(remove, remove[,1] != subshed) # remove any pre-existing subshed values
  
  table <- rbind(remove, new_sub, main_segs)
  
  # order by lrseg or just rseg
  if (colnames(table)[2] == "landseg") { #means it's a 1 entry per lrseg file
    table <- table[order(table$landseg, table[,1]),]
  } else { #means it's a 1 entry per rseg file
    table <- table[order(table[,1]),]
  }
  
  # add end row(s) back if exists
  if (length(end_row) != 0) {
    table <- rbind(table,table_end) #add the end section back on, if it exists
  }
  
  # return formatted table
  return(table)
  
}
  
table_new <- area_propor(subshed, main_seg, sub_da, table, cols)
  
write.table(table,
            file=file,
            append = FALSE,
            quote = FALSE,
            sep = ",",
            row.names = FALSE,
            col.names = TRUE)

message('proportioned the river segment and subshed')

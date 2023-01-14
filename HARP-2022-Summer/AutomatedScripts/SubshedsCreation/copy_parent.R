# creates values for subshed within a dataset by copying parent watershed's values
# works for both 1 entry per rseg and 1 entry per lrseg

#-arguments----
argst <- commandArgs(trailingOnly = T)
file <- argst[1]
subshed <- argst[2]
main_seg <- argst[3]

#-TESTING-
#--1 entry per lrseg e.g. = the first transport file:
#file <- 'HARParchive/HARP-2022-Summer/AutomatedScripts/SubshedsCreation/wF180615RXAPXXXW_l2w.csv'
#--1 entry per rseg: 
#file <- 'HARParchive/HARP-2022-Summer/AutomatedScripts/SubshedsCreation/gen_info_rseg.csv'
#file <- 'HARParchive/HARP-2022-Summer/AutomatedScripts/SubshedsCreation/river_met_wdm.csv'
#file <- 'HARParchive/HARP-2022-Summer/AutomatedScripts/SubshedsCreation/river_prad_wdm.csv'
#--
subshed <- 'PS2_5568_5560'
main_seg <- 'PS2_5560_5100'

#-load file & manipulate----
table <- read.csv(file, sep=',') 
table <- subset(table, table[,1] != subshed) #delete any pre-existing subshed rows

#sometimes there is an 'end' or NOTES at the bottom which we want to keep there
end_row <- as.numeric(rownames(table[grep('end', table[,1]), ])) #the row number of 'end'

if (length(end_row) != 0) { #there exists an 'end' and potentially NOTES
  last_row <- as.numeric(length(table[,1]))
  table_end <- table[(end_row-1):last_row,] #this didn't work quite right with prad for some reason
  table <- table[-(end_row-1):-last_row,] #just the data of the table
}

new_sub <- subset(table, table[,1] == main_seg) #copy parent wshed values
new_sub[1] <- subshed #rename them w/ subshed name

table <- rbind(table, new_sub) #add subshed to list

if (colnames(table)[2] == "landseg") { #means it's a 1 entry per lrseg file
  table <- table[order(table$landseg, table[,1]),]
} else { #means it's a 1 entry per rseg file
  table <- table[order(table[,1]),]
}

if (length(end_row) != 0) {
  table <- rbind(table,table_end) #add the end section back on, if it exists
}

#-save----
write.table(table,
            file=file,
            append = FALSE,
            quote = FALSE,
            sep = ",",
            row.names = FALSE,
            col.names = TRUE)

message('created subshed containing parent values')
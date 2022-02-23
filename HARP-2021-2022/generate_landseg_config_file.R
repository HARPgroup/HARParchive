## Script that creates land segment congfig file for all southern rivers
## Last Updated 2/18/2022

library(sqldf)
library(data.table)
library(utils)

#reading in land seg grid data
dat <-  read.table("C:/Users/alexw/Documents/R/HARP/Summer 2021/text files/p5_Lsegs.txt", header = TRUE, sep = " ")
#creating a list of just the land segments
landseg_list <- unique(dat$land_seg)

i <- 1
landseg_config_file <- c()
while (i <= length(landseg_list)){
  #combining all
  dat$landseg <- landseg_list[i]
  config_row_list <- t(sqldf("SELECT grid_ID
                      FROM dat
                      WHERE land_seg = landseg"))
  config_row <- as.data.frame(t(c(landseg_list[i], length(config_row_list), config_row_list)))
  landseg_config_file <- rbindlist(list(landseg_config_file,config_row),fill=TRUE)
  i <- i+1
  print(paste0(landseg_list[i]," config line created"))
}

#writing land segment config text file
write.table(landseg_config_file, "C:/Users/alexw/Documents/R/HARP/Summer 2021/text files/p5_landseg_config_file.txt", sep = " ", row.names = FALSE, col.names = FALSE, quote = FALSE)

#writing land segment list text file
write.table(t(landseg_list), "C:/Users/alexw/Documents/R/HARP/Summer 2021/text files/p5_landseg_list.txt", sep = " ", row.names = FALSE, col.names = FALSE)

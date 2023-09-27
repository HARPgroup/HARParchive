#THIS SCRIPT RETRIEVES AND PLOTS USGS STEAMFLOW DATA 
#----------------------------------------------------------------------------------------------------------
#Initialize variables and packages
rm(list = ls())  #clear variables
library(dataRetrieval) #needed for pulling in gage data
library(ggplot2) #used for plotting

#User inputs 
USGS_GAGE_ID <- '02075500'  #Specify gage of interest
date_lower <- '2005-01-01'  #Specify date range of interest
date_upper <- '2005-12-01'
save_directory <- "C:\\Users\\nrf46657\\Desktop\\So. Rvs. Summer 2017\\analysis_code\\plots"  #Specify location for storing plot images locally

#Use USGS webservice to retrieve historic gage data
print(paste("USGS_GAGE_ID: ", USGS_GAGE_ID, sep='')); 
gage_info <- readNWISsite(USGS_GAGE_ID)
name <- gage_info$station_nm
drainage_area <- gage_info$drain_area_va
gage_data <- readNWISdv(USGS_GAGE_ID,'00060') #parameter code for Discharge, cubic feet per second
gage_data <- renameNWISColumns(gage_data)
gage_data <- gage_data[(gage_data$Flow > 0),] #Remove any negative values from gage record
gage_data <- gage_data[(gage_data$Date > date_lower & gage_data$Date < date_upper),] #subsets dataframe to specified date range

print (paste("Plotting ELF"));
plot <- ggplot(gage_data, aes(x=Date,y=Flow))+
  geom_point(data = gage_data,aes(colour="black")) +
  geom_line(data = gage_data,aes(colour="blue")) +
  #geom_smooth(data = gage_data,aes(colour="blue")) + # Add a loess smoothed fit curve with confidence region (instead of geom_line above)
  ggtitle(paste(name,": ",USGS_GAGE_ID,"\n(",date_lower," - ",date_upper,") \n\nDrainage Area = ",drainage_area," mi^2",sep="")) + 
  theme(plot.title = element_text(size = 12, face = "bold"),axis.text = element_text(colour = "blue")) +
  labs(x="\nDate",y="Flow (cfs)") + 
  theme(legend.key=element_rect(fill='white')) +
  scale_color_manual("Legend",values=c("black","blue"),labels=c("Mean Daily Flow","Trendline")) + 
  guides(colour = guide_legend(override.aes = list(linetype=c(0,1),shape=c(16,NA),fill=NA),label.position = "right")
  ); 

#END plotting function - save file to local directory
filename <- paste("gage",USGS_GAGE_ID,date_lower,date_upper,".png", sep="_")
ggsave(file=filename, path = save_directory, width=12, height=6)
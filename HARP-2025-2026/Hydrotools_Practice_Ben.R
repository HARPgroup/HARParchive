library(hydrotools)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)



#Group 1
 flows <- dataRetrieval::readNWISdv("01633000",parameterCd = "00060")
 flows <- dataRetrieval::renameNWISColumns(flows)
 #Convert flows to zoo
 flows_zoo <- zoo::as.zoo(x = flows$Flow)
 zoo::index(flows_zoo) <- flows$Date
 #Use group 1 to get the minimum monthly flows:
 G1_Mount_Jackson <- hydrotools::group1(flows_zoo,"water",FUN = min)
 
 flows2 <- dataRetrieval::readNWISdv("01632000",parameterCd = "00060")
 flows2 <- dataRetrieval::renameNWISColumns(flows2)
 #Convert flows to zoo
 flows_zoo2 <- zoo::as.zoo(x = flows2$Flow)
 zoo::index(flows_zoo2) <- flows2$Date
 #Use group 1 to get the minimum monthly flows:
 G1_Cootes_Store <- hydrotools::group1(flows_zoo2,"water",FUN = min)
 
 flows3 <- dataRetrieval::readNWISdv("01634000",parameterCd = "00060")
 flows3 <- dataRetrieval::renameNWISColumns(flows3)
 #Convert flows to zoo
 flows_zoo3 <- zoo::as.zoo(x = flows3$Flow)
 zoo::index(flows_zoo3) <- flows3$Date
 #Use group 1 to get the minimum monthly flows:
 G1_Strasburg <- hydrotools::group1(flows_zoo3,"water",FUN = min)
 
#Group 2
  flows4 <- dataRetrieval::readNWISdv("01633000",parameterCd = "00060")
  flows4 <- dataRetrieval::renameNWISColumns(flows4)
  #Convert flows to zoo
 flows_zoo4 <- zoo::as.zoo(x = flows4$Flow)
  zoo::index(flows_zoo4) <- flows4$Date
  #Use group 2 to get critical period flows and stats:
 G2_Mount_Jackson<- hydrotools::group2(flows_zoo4,"water",mimic.tnc = TRUE)
 
 flows5 <- dataRetrieval::readNWISdv("01632000",parameterCd = "00060")
 flows5 <- dataRetrieval::renameNWISColumns(flows5)
 #Convert flows to zoo
 flows_zoo5 <- zoo::as.zoo(x = flows5$Flow)
 zoo::index(flows_zoo5) <- flows5$Date
 #Use group 2 to get critical period flows and stats:
 G2_Cootes_Store<- hydrotools::group2(flows_zoo5,"water",mimic.tnc = TRUE)
 
 flows6 <- dataRetrieval::readNWISdv("01634000",parameterCd = "00060")
 flows6 <- dataRetrieval::renameNWISColumns(flows6)
 #Convert flows to zoo
 flows_zoo6 <- zoo::as.zoo(x = flows6$Flow)
 zoo::index(flows_zoo6) <- flows6$Date
 #Use group 2 to get critical period flows and stats:
 G2_Strasburg<- hydrotools::group2(flows_zoo6,"water",mimic.tnc = TRUE)
 

 max_data <- data.frame(
   Location = c(rep("Cootes Store", nrow(G2_Cootes_Store)),
                rep("Mount Jackson", nrow(G2_Mount_Jackson)),
                rep("Strasburg", nrow(G2_Strasburg))),
   Max_7Day = c(G2_Cootes_Store$"7 Day Max",
                G2_Mount_Jackson$"7 Day Max",
                G2_Strasburg$"7 Day Max"),
   Year    =  c(G2_Cootes_Store$"year",
                G2_Mount_Jackson$"year",
                G2_Strasburg$"year")
   
 )
 
A<- ggplot(max_data, aes(x = Location, y = Max_7Day, fill=Location)) +
   geom_boxplot() +
   labs(title = "7-Day Max Flow (cfs)",
        x = "Location",
        y = "Flow (cfs)") +
   theme_minimal()
 
 
 min_data <- data.frame(
   Location = c(rep("Cootes Store", nrow(G2_Cootes_Store)),
                rep("Mount Jackson", nrow(G2_Mount_Jackson)),
                rep("Strasburg", nrow(G2_Strasburg))),
   Min_7Day = c(G2_Cootes_Store$"7 Day Min",
                G2_Mount_Jackson$"7 Day Min",
                G2_Strasburg$"7 Day Min"),
   Year    =  c(G2_Cootes_Store$"year",
                G2_Mount_Jackson$"year",
                G2_Strasburg$"year")
 )
   
 
 
 B<-ggplot(min_data, aes(x = Location, y = Min_7Day, fill=Location)) +
   geom_boxplot() +
   labs(title = "7-Day Min Flow (cfs)",
        x = "Location",
        y = "Flow (cfs)") +
   theme_minimal()
 
 grid.arrange(A,B, ncol=2, top="North Fork Shenandoah Max and Mins")
 
C<- ggplot() + 
   geom_line(data = min_data, aes(x = Year, y = Min_7Day, color=Location)) +
   geom_line(data = max_data, aes(x = Year, y = Max_7Day, color=Location)) +
   xlab('Year') + 
   ylab('Flow cfs')+
   ggtitle(('Line graph of min and max cfs 1925-2025'))+
   theme_minimal()
C
 
D<- ggplot() + 
   geom_point(data = min_data, aes(x = Year, y = Min_7Day, color=Location)) +
   geom_point(data = max_data, aes(x = Year, y = Max_7Day, color=Location)) +
   xlab('Year') + 
   ylab('Flow cfs')+
   ggtitle('scatter plot of min and max cfs 1925-2025')+
   theme_minimal()
 D
E<- ggplot() + 
   geom_point(data = min_data, aes(x = Year, y = Min_7Day, color=Location)) +
   xlab('Year') + 
   ylab('Flow cfs')+
   ggtitle('Scatter plot of min cfs 1925-2025')+
   theme_minimal()
E

temp1 <- dataRetrieval::readNWISdv("01633000",parameterCd = "00010")
temp1 <- dataRetrieval::renameNWISColumns(temp1)
temp2 <- dataRetrieval::readNWISdv("01632000",parameterCd = "00010")
temp2 <- dataRetrieval::renameNWISColumns(temp2)

flow_temp1 <- merge(flows, temp1, by = "Date")
flow_temp2 <- merge(flows2, temp2, by = "Date")

temp_location <- data.frame(
  Location = c(rep("Cootes Store", nrow(flow_temp1)),
               rep("Mount Jackson", nrow(flow_temp2))),
  Temp = c(flow_temp1$Wtemp,
           flow_temp2$Wtemp),
  Date = c(flow_temp1$Date,
           flow_temp2$Date)
)

f<- ggplot() + 
  geom_line(data =temp_location, aes(x = Date, y = Temp, color=Location)) +
  xlab('Date') + 
  ylab('Water Temperature')+
  ggtitle(('Line graph of water temperature cfs 2007-2009'))+
  theme_minimal()
f


flows$Location <- "Cootes Store"
temp1$Location <- "Cootes Store"

flows2$Location <- "Mount Jackson"
temp2$Location <- "Mount Jackson"

flow_temp1 <- merge(flows, temp1, by = c("Date", "Location"))
flow_temp2 <- merge(flows2, temp2, by = c("Date", "Location"))



flows$Date <- as.Date(flows$Date)
temp1$Date <- as.Date(temp1$Date)

temp_flow_combined <- rbind(flow_temp1, flow_temp2)

ggplot(temp_flow_combined, aes(x = Wtemp, y = Flow, color = Location)) +
  geom_point(alpha = 0.6) +
  xlab("Water Temperature (°C))") +
  ylab("Discharge (CFS)") +
  ggtitle("Water Temperature vs. Discharge (CFS), 2007–2009") +
  theme_minimal()

ggplot(temp_flow_combined, aes(x = Wtemp, y = Flow, color = Location)) +
  ylim(0, 1000)+
  geom_point(alpha = 0.6) +
  xlab("Water Temperature (°C)") +
  ylab("Discharge (CFS))") +
  ggtitle("Water Temperature vs. Discharge (CFS), 2007–2009") +
  theme_minimal()




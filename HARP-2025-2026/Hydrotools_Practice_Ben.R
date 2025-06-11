library(hydrotools)
library(dplyr)

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
 
 
 #Averaging May
 mean(G1_Cootes_Store$May, na.rm = TRUE)
 
 summarise(G1_Cootes_Store, Average = mean(January, na.rm = T))
 
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

 
 
 
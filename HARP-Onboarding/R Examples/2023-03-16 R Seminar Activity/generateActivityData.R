#Be sure to call the dataRetrieval library!
library(dataRetrieval)
#We will use the dataRetrieval function readNWISdv to get mean daily streamflow at our gage
gageData_all <- readNWISdv("02064000",parameterCd = "00060")
#We will set some names for the data frame to make it easier to work with
names(gageData_all) <- c("agency","site_no","Date","Flow","QC")
#Filter for instances where QC is not "P" (Provisional)
gageData_QC <- gageData_all %>% 
  filter(QC != "P")
#Get random dates and flows above Jan 01, 2000
datesToSample <- sample(gageData_QC$Date[gageData_QC$Date>"2000-01-01"],100)
datesToSample <- datesToSample[order(datesToSample)]
flowToSample <- gageData_QC$Flow[gageData_QC$Date%in%datesToSample]

#Whats the lognormal distribution of all flows at gage?
mean(log(gageData_QC$Flow));var(log(gageData_QC$Flow))^0.5

#Fake facility flows by subtracting a random lognormal value based on distribution at gage and adjusting via uniform distributions
flowFacility <- exp((log(flowToSample) - rnorm(100,4.55*0.25,0.8572) * runif(100,0.9,1.1)) * runif(100,0.75,1.25))
#Set negative flows to 0 and cap at 300 to exclude big storms
flowFacility[flowFacility<0] <- 0
tooHigh <- which(flowFacility>=300)

#Remove storm flows above 300
streamFlow <- flowToSample[-tooHigh]
datesOut <- datesToSample[-tooHigh]
flowFacilityFinal <- flowFacility[-tooHigh]
#If the facility flow is above the downstream gage, adjust it
flowFacilityFinal[flowFacilityFinal>streamFlow] <- streamFlow[flowFacilityFinal>streamFlow] * runif(length(flowFacilityFinal[flowFacilityFinal>streamFlow]),0.75,0.95)
#Plot to make sure data makes sense
plot(streamFlow,flowFacilityFinal)

#Convert to MGD and write out to a csv file:
flowFacilityFinal <- flowFacilityFinal * 3600 * 24 * 12*12*12 / 231 /1000000
out <- data.frame(Date=datesOut,FlowMGD=round(flowFacilityFinal,2),stringsAsFactors = F)
write.csv(out,"data/facilityFlowinMGD.csv",row.names = F)


#Now, create facility discharge based on 0.1 MGD max:
dischargeFacility <- exp(rnorm(length(flowFacilityFinal),log(1),0.25))
dischargeFacility[dischargeFacility>3] <- 2
#Should not exceed flowFacilityFinal
dischargeFacility[dischargeFacility>flowFacilityFinal] <- 0.95 * flowFacilityFinal[dischargeFacility>flowFacilityFinal]
plot(dischargeFacility)

#Generate pH and hardness to output in a wide format with flow
pHData <- rnorm(length(flowFacilityFinal),7,0.25)
hardnessData <- rnorm(length(flowFacilityFinal),80,20)
out <- data.frame(Date=rep(datesOut,3),
                  value=c(signif(dischargeFacility,2),
                          round(hardnessData,0),
                          round(pHData,2)),
                  measType=c(
                    rep("Flow",length(flowFacilityFinal)),
                    rep("Hardness",length(flowFacilityFinal)),
                    rep("pH",length(flowFacilityFinal))
                    ),
                  stringsAsFactors = F)
#Change the format of the dates to a non-standard value
out$Date <- format.Date(out$Date,"%d-%b-%y")

#Make a wide dataset
out <- pivot_wider(out,id_cols=measType,names_from=Date)

write.csv(out,"data/facilityDischargeinMGD.csv",row.names = F)

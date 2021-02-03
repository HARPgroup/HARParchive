rm(list=ls())
#Load libraries
library(jsonlite)
library(dplyr)
library(lubridate)
library(dataRetrieval)

#Load in lat/long as well as impoundment attributes from external spreadsheet
#Note: Data is formatted with freq below, but is checked in the for loop. An error is output is column names exported from StreamStat don't match freq
Flows<-read.csv("C:/Users/connorb5/Desktop/GitHub/hydro-tools/HARP-2018/Delineation and Impoundment Creation/Input_example2.csv")
Flows$USGSDA<-NA
freq<-c("PK2","PK2_33","PK5","PK10","PK25","PK50","PK100","PK200")
Flows[freq]<-NA
Flows$StageWithFreeboard<-as.numeric(Flows$StageWithFreeboard)
Flows$AvailableStage<-as.numeric(Flows$AvailableStage)
Flows$NormalStage<-as.numeric(Flows$NormalStage)
Flows$MaximumStage<-as.numeric(Flows$MaximumStage)

#Given geographic inputs.
state<-"VA"

#This loop iterates through every location in the "Flows" data frame and appends them with StreamStat desing storms and drainage area
for (i in 1:length(Flows$Lat)){
  print(paste0(i," in ",length(Flows$Lat)))
  lat<-Flows$Lat[i]
  long<-Flows$Long[i]
  #Develop peak flow statistics based on a delineation on StreamStat servers and maps based on location
  ########METHOD USING STREAMSTAT WEB SERVICES########
  #Delineate the watershed online to develop a workspace QID and the drainage area. GIS layers available here as well
  json_file<-paste0("https://streamstats.usgs.gov/streamstatsservices/watershed.json?rcode=",state,"&xlocation=",long,"&ylocation=",lat,"&crs=4326&includeparameters=true&includeflowtypes=false&includefeatures=true&simplify=true")
  json_data<-fromJSON(txt=json_file)
  QID<-json_data$workspaceID
  da<-json_data$parameters$value[3]#Drainage area
  #Estimate flow based on drainage area using the StreamStat Regressions and the workspace ID (QID)
  json_file<-paste0("https://streamstats.usgs.gov/streamstatsservices/flowstatistics.json?rcode=",state,"&workspaceID=",QID,"&includeflowtypes=true")
  json_data<-fromJSON(txt=json_file)
  data<-json_data$RegressionRegions
  data<-data[[3]]#peak flows are list 3, low flows list 2, urban peak flows list 1
  data<-data$Results
  data<-data[[1]]
  mag<-data$Value
  name<-data$code
  if(sum(as.character(name)!=freq)>0){
   warning("Error, check flow output format. Column names in 'Flows' does not match StreamStat output in 'data'")
  }
  #####ALTERNATIVE METHOD####
  #json_file<-paste0("https://streamstats.usgs.gov/streamstatsservices/watershed.json?rcode=",state,"&xlocation=",long,"&ylocation=",lat,"&crs=4326&includeparameters=true&includeflowtypes=false&includefeatures=true&simplify=true")
  #json_data<-fromJSON(txt=json_file)
  #URBAN<-json_data$parameters$value[9]
  #da<-json_data$parameters$value[3]
  #Flow equations are ONLY for VA (https://pubs.usgs.gov/sir/2014/5090/pdf/sir2014-5090.pdf)
  ##mag<-c(2.027+(URBAN-40.290)*((log10(da)-1.216)*-0.00414)+(0.00468*URBAN)+(-0.366*log10(da)),
  #       #2.072+(URBAN-40.227)*((log10(da)-1.205)*-0.00403)+(0.00464*URBAN)+(-0.362*log10(da)),
  #       #2.229+(URBAN-39.370)*((log10(da)-1.139)*-0.00346)+(0.00487*URBAN)+(-0.338*log10(da)),
  #       #2.373+(URBAN-38.706)*((log10(da)-1.103)*-0.00313)+(0.00470*URBAN)+(-0.334*log10(da)),
  #       #2.557+(URBAN-39.168)*((log10(da)-1.083)*-0.00224)+(0.00434*URBAN)+(-0.332*log10(da)),
  #       #2.697+(URBAN-39.168)*((log10(da)-1.083)*-0.00219)+(0.00390*URBAN)+(-0.343*log10(da)),
  #       #2.776+(URBAN-38.765)*((log10(da)-1.070)*-0.00242)+(0.00434*URBAN)+(-0.342*log10(da)),
  #       #2.863+(URBAN-39.063)*((log10(da)-1.057)*-0.00223)+(0.00465*URBAN)+(-0.329*log10(da)))
  ##mag<-da*10^mag
  #mag<-c(2.197+00593*log10(da),
  #       2.287+0.576*log10(da),
  #       2.540+0.551*log10(da),
  #       2.719+0.534*log10(da),
  #       2.916+0.514*log10(da),
  #       3.043+0.501*log10(da),
  #       3.157+0.490*log10(da),
  #       3.263+0.480*log10(da))
  #mag<-10^mag
  ########################
  #Store design flows and drainage area into the Flows dataframe
  Flows[i,freq]<-mag
  Flows$USGSDA[i]<-da
}

#Load in USGS gauge data at the difficult run watershed, using enough data to establish long-term peak data analysis
siteNo<-"01646000"
pCode<-'00060'
stat<-'00001'#'00003'
start.date<-'1984-01-01'
end.date<-'2005-12-31'
USGS<-readNWISdv(siteNo,pCode,start.date,end.date)
USGS<-USGS[order(USGS$Date),]
#Calculate and append USGS data with water year
USGS$WY<-numeric(length(USGS$Date))
b<-year(USGS$Date[1])
for (i in 1:length(USGS$Date)){
  USGS$WY[i]<-b-1
  if(USGS$Date[i]>=as.Date(paste0(b,"-10-01"))){
    b<-b+1
    USGS$WY[i]<-b-1
  }
}
rm(b,i)
#Find peak flows from daily flow values (rather than the instaneous peak in readNWISpeak that are not available in Chesapeake Bay Model calibration)
Obs<-as.data.frame(summarize(group_by(USGS,WY),MaxFlow=max(X_00060_00003)))

#Find design storms at the outlet from observed gauge data
quantile(Obs$MaxFlow,c(0.5,1-3/7,0.8,0.9,0.96,.98,0.99,0.995))

#Find the design storm at each impoundment, scale to catchmetn area, and then back-calculate orifice dimensions
Flows$DAScaled10<-as.numeric(quantile(peakUSGS$peak_va,0.9))*Flows$USGSDA/Flows$USGSDA[Flows$Trib=='Outlet']
Flows$DAoutletWidth<-Flows$DAScaled10/(0.6*sqrt(2*32.2*(Flows$StageWithFreeboard-0.5*Flows$OutletHeight))*Flows$OutletHeight)


#write.csv(Flows,"G:\\My Drive\\Impoundment Modeling\\Model Runs\\Documentation\\OutletStructuresVAallregression.csv")

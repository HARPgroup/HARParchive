#Purpose: Plot hydrology outputs from the Chesapeake Bay Watershed Model-------------

#input data from model
uri_output_model_OD5_8780_8660 <- 'https://docs.google.com/spreadsheets/d/e/2PACX-1vQm4Au29pTc15uga59pvo96de-x_TJFsiW0eFTe_tZh4kDLiwrEwuOwtnQu_Q40Yhw8Th5xiMVXXLx3/pub?output=csv'
output_model_OD5_8780_8660 <- read.csv(uri_output_model_OD5_8780_8660, header = TRUE, sep = ",", stringsAsFactors = FALSE);

#input data from USGS
uri_USGS <- 'https://docs.google.com/spreadsheets/d/1D3PomN_dgByVHfhhBUlUwLHB6dSud3MarUl7U8LlsUE/pub?output=csv';
USGS = read.csv(uri_USGS, header = TRUE, sep = ",", stringsAsFactors = FALSE);

#name variables for model data
year_model_OD5 <- output_model_OD5_8780_8660$year
ovol <- output_model_OD5_8780_8660$ovol

#name variables for USGS gage data
year_USGS <- USGS$year
discharge <- USGS$discharge


#Plot and compare data from model and USGS gages
plot(ovol ~ year_model_OD5,main="Comparison between CBWM OD5_8780_8660 output and USGS data (Paces, VA)",xlab="Year",
     ylab="Outflow/Discharge (ft^3/s)",type="o",col="red",lwd=2, label = ovol)
lines(discharge ~ year_USGS, type="o", col="blue", lwd=2, label = discharge)
legend("topleft", legend = c("Model Output", "USGS Data"), col = c("red", "blue"), lty=1:1, 
       cex=0.8,lwd=2,bg="gray85",text.font=2)
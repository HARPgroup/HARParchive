# Calculates yearly average for flow data from daily flow data

#Loads Data
URI_model_daily <- 'https://docs.google.com/spreadsheets/d/e/2PACX-1vRSt9LQQHtGK8kwfiOriEsPolDt_77JoXYW-KVMebP2GKptI8tzWsacshjqEWv0oWgR0J2zYYUx6ZSv/pub?output=csv'
model_daily = read.csv(URI_model_daily, header = TRUE, sep = ",", stringsAsFactors = FALSE);

annual_flow <- data.frame(matrix(nrow = 1, ncol = 2))
colnames(annual_flow) <- c("Year", "Flow")


j <- 1
k <- 365
for (i in 1:22) {
  start_year <- model_daily[j,2]
  end_year_normal <- model_daily[k,2]
  end_year_leap <- model_daily[(k+1),2]
  if (start_year == end_year_leap){
  annual_flow[i,1] <- model_daily[j, 2]
  annual_flow[i,2] <- mean(model_daily$ovol[j:(k+1)])
  j <- j +366
  k <- k+366} else if (start_year != end_year_leap){
    annual_flow[i,1] <- model_daily[j, 2]
    annual_flow[i,2] <- mean(model_daily$ovol[j:k])
    j <- j+365
    k <- k+365
  }
}
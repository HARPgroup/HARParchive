# Automating August Low Flows

library('zoo')
library('IHA')
library(PearsonDS)

fn_iha_7q10 <- function(zoots) {
  g2 <- group2(zoots) 
  #print("Group 2, 7-day low flow results ")
  #print(g2["7 Day Min"])
  x <- as.vector(as.matrix(g2["7 Day Min"]))
  # fudge 0 values
  # correct for zeroes?? If so, use this loop:
  # This is not an "approved" method - we need to see how the GS/other authorities handles this
  for (k in 1:length(x)) {
    if (x[k] <= 0) {
      x[k] <- 0.00000001
      print (paste("Found 0.0 average in year", g2["year"], sep = " "))
    }
  }
  x <- log(x)
  pars <- PearsonDS:::pearsonIIIfitML(x)
  x7q10 <- exp(qpearsonIII(0.1, params = pars$par))
  return(x7q10);
}

fn_iha_mlf <- function(zoots, targetmo) {
  modat <- group1(zoots,'water','min')  # IHA function that calculates minimum monthly statistics for our data by water year	 
  print(paste("Grabbing ", targetmo, " values ", sep=''))
  g1vec <- as.vector(as.matrix(modat[,targetmo]))  # gives only August statistics
  
  # calculates the 50th percentile - this is the August Low Flow
  # August Low Flow = median flow of the annual minimum flows in August for a chosen time period
  print("Performing quantile analysis")
  x <- quantile(g1vec, 0.5, na.rm = TRUE);
  return(as.numeric(x));
}

fn_iha_mhf <- function(zoots, targetmo) {
  modat <- group1(zoots,'water','max')  # IHA function that calculates maximum monthly statistics for our data by water year	 
  print(paste("Grabbing ", targetmo, " values ", sep=''))
  g1vec <- as.vector(as.matrix(modat[,targetmo]))  # gives only August statistics, if 8
  
  # calculates the 50th percentile - this is the August High Flow
  # August High Flow = median flow of the annual maximum flows in August for a chosen time period
  print("Performing quantile analysis")
  x <- quantile(g1vec, 0.5, na.rm = TRUE);
  return(as.numeric(x));
}

#Calculates the Drought of record Flow
fn_iha_DOR <- function(flows){
  loflows <- group2(flows);
  l90 <- loflows["90 Day Min"];
  ndx = which.min(as.numeric(l90[,"90 Day Min"]));
  dor_flow = round(loflows[ndx,]$"90 Day Min",1);
  #dor_year = loflows[ndx,]$"year";
}

#Returns the Drought of Record Year
fn_iha_DOR_Year <- function(flows){
  loflows <- group2(flows);
  l90 <- loflows["90 Day Min"];
  ndx = which.min(as.numeric(l90[,"90 Day Min"]));
  #}dor_flow = round(loflows[ndx,]$"90 Day Min",1);
  dor_year = loflows[ndx,]$"year";
}

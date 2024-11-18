## Set working directory
setwd("C:/Users/natef/OneDrive - Virginia Tech/HARP")

#get packages
library(ggplot2)
library(sqldf)
library(tidyr)

#set varibales for stream gage id
gage_id <- "02026000"


#download stormeventflow and rating values
stormflow <- read.csv(paste("http://deq1.bse.vt.edu:81/met/stormVol_nldas2/flow/usgs_ws_",gage_id,"-stormevent-flow.csv",sep = ""))
ratings0 <- read.csv(paste("http://deq1.bse.vt.edu:81/met/nldas2/out/usgs_ws_",gage_id,"-nldas2-lm_simple-rating-ts.csv",sep = ""))

#clean up ratings dates
ratings <- ratings0 %>%
  separate(start_date, into = c("timestamp","start_time"), sep = " ")

#sql join
# Perform the join
result <- sqldf("SELECT stormflow.timestamp, stormflow.stormID, ratings.rating
                 FROM stormflow
                 INNER JOIN ratings ON stormflow.timestamp = ratings.timestamp")
is.na(result$stormID)

plot <- ggplot(data = result, 
       mapping = aes(x = !is.na(stormID), y = rating))+
  geom_boxplot()+
  xlab("Storm Event?")+
  ggtitle("Comparison of Ratings During Storm Events and Outside of Storm Events")+
  theme_bw()+
  ylim(c(0,1))

plot




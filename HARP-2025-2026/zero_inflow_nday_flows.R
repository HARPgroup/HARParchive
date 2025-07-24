# Calculating flow for n days without any inflow of rain

# Matched USGS Gages and Land Segments
# Cootes Store (01632000) = N51165
# Mount Jackson (01633000) = N51171
# Strasburg (01634000) = N51187

# Libraries
library(dataRetrieval)
library(lubridate)
library(ggplot2)

land_type <- "for"
land_code <- "N51187"
site_num <- "01634000"

model_data <- read.csv(paste0("http://deq1.bse.vt.edu:81/p6/out/land/subsheds2/pwater/", land_type, land_code,"_pwater.csv"))

# Make model data daily using func from landseg_groundwater_IH.R
model_data <- make.model.daily(model_data, "date")

# creating AGWRC and dAGWRC column
model_data$AGWRC <- model_data$AGWO / c(NA, head(model_data$AGWO, -1))

model_data$d_AGWRC <- model_data$AGWRC / c(NA, head(model_data$AGWRC, -1))

# creating KGW column
model_data$KGW <- 1 - model_data$AGWRC


model_data$AGWO_30 <- NA
model_data$AGWO_60 <- NA
model_data$AGWO_90 <- NA
model_data$AGWO_120 <- NA
model_data$AGWO_240 <- NA
model_data$AGWO_480 <- NA



for(i in 1:nrow(model_data)) {
  AGWS_future <- model_data$AGWS[i]
  KGW <- model_data$KGW[i]
  AGWET <- model_data$AGWET[i]
  
  # predictigng 480 days forward
  AGWS_30 <- AGWS_future
  AGWS_60 <- AGWS_future
  AGWS_90 <- AGWS_future
  AGWS_120 <- AGWS_future
  AGWS_240 <- AGWS_future
  AGWS_480 <- AGWS_future
  
  for(day in 1:480) {
    AGWO_day <- KGW * AGWS_future
    AGWS_future <- AGWS_future - AGWO_day - AGWET
    
    if(day == 30) AGWS_30 <- AGWS_future
    if(day == 60) AGWS_60 <- AGWS_future
    if(day == 90) AGWS_90 <- AGWS_future
    if(day == 120) AGWS_120 <- AGWS_future
    if(day == 240) AGWS_240 <- AGWS_future
    if(day == 480) AGWS_480 <- AGWS_future
  }
  
  # Calculating AGWO values using future storsge
  model_data$AGWO_30[i] <- KGW * AGWS_30
  model_data$AGWO_60[i] <- KGW * AGWS_60
  model_data$AGWO_90[i] <- KGW * AGWS_90
  model_data$AGWO_120[i] <- KGW * AGWS_120
  model_data$AGWO_240[i] <- KGW * AGWS_240
  model_data$AGWO_480[i] <- KGW * AGWS_480
}


# Set all values less than 0 to zero for new outflows
for(i in 1:nrow(model_data)){
  if (!is.na(model_data$AGWO_30[i]) && model_data$AGWO_30[i] < 0) {
    model_data$AGWO_30[i] <- 0
  }
  
  if (!is.na(model_data$AGWO_60[i]) && model_data$AGWO_60[i] < 0) {
    model_data$AGWO_60[i] <- 0
  } 
  
  if (!is.na(model_data$AGWO_90[i]) && model_data$AGWO_90[i] < 0) {
    model_data$AGWO_90[i] <- 0
  } 
  
  if (!is.na(model_data$AGWO_120[i]) && model_data$AGWO_120[i] < 0) {
    model_data$AGWO_120[i] <- 0
  }
  
  if (!is.na(model_data$AGWO_240[i]) && model_data$AGWO_240[i] < 0) {
    model_data$AGWO_240[i] <- 0
  }
  
  if (!is.na(model_data$AGWO_480[i]) && model_data$AGWO_480[i] < 0) {
    model_data$AGWO_480[i] <- 0
  }
  
}


model_valid <- sqldf(
  " select * from model_data
  where 0.97 <= d_AGWRC and d_AGWRC <= 1.03 
  and AGWRC <= AGWRC
  and AGWO_30 >= AGWO_480
  "
) 


# Finding Area

library(sf) 
library(dplyr)
library(ggplot2)
library(hydrotools)
library(DBI)
library(terra)

basepath <- '/var/www/R'
source(paste(basepath, 'config.R', sep = '/'))

##GET CBP6 LAND SEGMENTS (FULL)##
landsegs <- dbGetQuery(ds$connection,"
  SELECT * FROM dh_feature_fielded
  WHERE bundle = 'landunit' AND ftype = 'cbp6_landseg'")

landsegs_geo <- st_as_sf(landsegs, wkt = "dh_geofield", crs = 4326)
landsegs_geo <- st_make_valid(landsegs_geo)

landsegs_study <- filter(landsegs_geo, landsegs_geo$name == land_code)

####TRY TO CALCULATE AREA####
#project Albers
landsegs_study_proj <- st_transform(landsegs_study, crs = 5070)
#m^2 area
landsegs_study_proj$area_m2 <- st_area(landsegs_study_proj)
#ft^2 conversion
landsegs_study_proj$area_ft2 <- as.numeric(landsegs_study_proj$area_m2) * 10.7639
#view
landsegs_study_proj %>% 
  select(name, area_m2, area_ft2)

area_sqft <- landsegs_study_proj$area_ft2

# Using land segment drainage area of sqft for Cootes Store
inpd_cfs <- (1/86400) * (1/12) * (area_sqft)

model_valid$AGWO <- model_valid$AGWO *inpd_cfs
model_valid$AGWO_30 <- model_valid$AGWO_30 *inpd_cfs
model_valid$AGWO_60 <- model_valid$AGWO_60 *inpd_cfs
model_valid$AGWO_90 <- model_valid$AGWO_90 *inpd_cfs
model_valid$AGWO_120 <- model_valid$AGWO_120 *inpd_cfs
model_valid$AGWO_240 <- model_valid$AGWO_240 *inpd_cfs
model_valid$AGWO_480 <- model_valid$AGWO_480 *inpd_cfs



row_num <- 421

temp_rows <- c(0,30, 60, 90, 120, 240, 480)
temp_vals <- c(model_valid$AGWO[row_num],
               model_valid$AGWO_30[row_num],
               model_valid$AGWO_60[row_num],
               model_valid$AGWO_90[row_num],
               model_valid$AGWO_120[row_num],
               model_valid$AGWO_240[row_num],
               model_valid$AGWO_480[row_num])

ex_plot <- data.frame(temp_rows, temp_vals)

ggplot(data = ex_plot, mapping = aes(x = temp_rows, y = temp_vals))+
  geom_point(size = 2, color = "dodgerblue4")+
  geom_line(color = "dodgerblue3")+
  geom_text(aes(label = paste0(round(temp_vals, digits = 3), " cfs")), vjust = -0.1, hjust = -0.5, size = 3.5)+
  theme_bw()+
  ggtitle(paste0("Predicted Future Outflow Values for ", model_valid$Date[row_num]))+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Days Without Inflow")+
  ylab("AGWO (cfs)")


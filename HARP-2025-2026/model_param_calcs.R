# Matched USGS Gages and Land Segments
# Cootes Store (01632000) = N51165
# Mount Jackson (01633000) = N51171
# Strasburg (01634000) = N51187

# Libraries
library(dataRetrieval)
library(lubridate)
library(ggplot2)

land_code <- "pasN51187"
site_num <- "01634000"

model_data <- read.csv(paste0("http://deq1.bse.vt.edu:81/p6/out/land/subsheds2/pwater/", land_code,"_pwater.csv"))

# Get data timescale limits
startDate <- min(model_data$index)
endDate <- max(model_data$index)

# Download USGS Data
# Cootes Store
usgs_data <- readNWISdata(site = site_num, parameterCd = "00060",
                        startDate = as_date(startDate),
                        endDate = as_date(endDate))
usgs_data <- renameNWISColumns(usgs_data)
usgs_data$dateTime <- as_date(usgs_data$dateTime)

# Make model data daily using func from landseg_groundwater_IH.R
model_data <- make.model.daily(model_data, "date")

# creating AGWRC column
model_data$AGWRC <- model_data$AGWO / c(NA, head(model_data$AGWO, -1))

usgs_data$AGWRC <- usgs_data$Flow / c(NA, head(usgs_data$Flow, -1))

model_data_valid <- subset(model_data, AGWI == 0)

usgs_data_valid <- sqldf(
  "select a.* from usgs_data as a
   inner join model_data_valid as b
   on( 
    b.Date = a.dateTime
   )
  ")

comp_data <- sqldf(
  "select Date, AGWRC, 'MODEL' as Data from model_data_valid
  union all
  select dateTime as Date, AGWRC, 'USGS' as Data from usgs_data_valid
  "
)

ggplot(comp_data, mapping = aes(x = AGWRC, color = Data, fill = Data))+
  geom_density( alpha =0.2)+
  xlim(0,1.5)+
  theme_bw()+
  scale_color_manual(values = c("firebrick","dodgerblue3"))+
  scale_fill_manual(values = c("firebrick","dodgerblue3"))+
  ylab("Density")+
  ggtitle(paste0("Calculated AGWRCs for days with 0 AGWI (", land_code, ")"))+
  theme(plot.title = element_text(hjust = 0.5))
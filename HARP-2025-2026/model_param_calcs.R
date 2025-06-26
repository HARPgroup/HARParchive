# Matched USGS Gages and Land Segments
# Cootes Store (01632000) = N51165
# Mount Jackson (01633000) = N51171
# Strasburg (01634000) = N51187

# Libraries
library(dataRetrieval)
library(lubridate)
library(ggplot2)

# Download Model Data (FORESTED)
cs_model <- read.csv("http://deq1.bse.vt.edu:81/p6/out/land/subsheds2/pwater/forN51165_pwater.csv")
mtj_model <- read.csv("http://deq1.bse.vt.edu:81/p6/out/land/subsheds2/pwater/forN51171_pwater.csv")
sb_model <- read.csv("http://deq1.bse.vt.edu:81/p6/out/land/subsheds2/pwater/forN51187_pwater.csv")

# Get data timescale limits
startDate <- min(cs_model$index)
endDate <- max(cs_model$index)

# Download USGS Data
# Cootes Store
cs_usgs <- readNWISdata(site = "01632000", parameterCd = "00060",
                        startDate = as_date(startDate),
                        endDate = as_date(endDate))
cs_usgs <- renameNWISColumns(cs_usgs)
cs_usgs$dateTime <- as_date(cs_usgs$dateTime)

# Mount Jackson
mtj_usgs <- readNWISdata(site = "01633000", parameterCd = "00060",
                        startDate = as_date(startDate),
                        endDate = as_date(endDate))
mtj_usgs <- renameNWISColumns(mtj_usgs)

# Strasburg
sb_usgs <- readNWISdata(site = "01634000", parameterCd = "00060",
                        startDate = as_date(startDate),
                        endDate = as_date(endDate))
sb_usgs <- renameNWISColumns(sb_usgs)


# Make model data daily using func from landseg_groundwater_IH.R
cs_model <- make.model.daily(cs_model, "date")
mtj_model <- make.model.daily(mtj_model, "date")
sb_model <- make.model.daily(sb_model, "date")


# comp_QO <- sqldf(
#   "select a.Date, a.AGWO, b.Flow from cs_model as a
#   left outer join cs_usgs as b 
#   on (
#     a.Date = b.dateTime
#   )
#   ")


## Looking into cs only for now
cs_model$AGWRC <- cs_model$AGWO / c(NA, head(cs_model$AGWO, -1))

cs_usgs$AGWRC <- cs_usgs$Flow / c(NA, head(cs_usgs$Flow, -1))

cs_model_valid <- subset(cs_model, AGWI == 0)

cs_usgs_valid <- sqldf(
  "select a.* from cs_usgs as a
   inner join cs_model_valid as b
   on( 
    b.Date = a.dateTime
   )
  ")

cs_comp <- sqldf(
  "select Date, AGWRC, 'MODEL' as Data from cs_model_valid
  union all
  select dateTime as Date, AGWRC, 'USGS' as Data from cs_usgs_valid
  "
)

ggplot(cs_comp, mapping = aes(x = AGWRC, color = Data, fill = Data))+
  geom_histogram(bins = 25, alpha =0.2)+
  xlim(0,1.5)+
  theme_bw()+
  scale_color_manual(values = c("firebrick","dodgerblue3"))+
  scale_fill_manual(values = c("firebrick","dodgerblue3"))+
  ylab("Count")+
  ggtitle("Calculated AGWRCs for days with 0 AGWI (Forested)")+
  theme(plot.title = element_text(hjust = 0.5))

  











# testing datasets for ws_model_summary.rmd - both files in the AutomatedScripts folder
library(readr)

rseg.data.1 <- data.table::fread('JA4_7280_7340_hydrd_wy.csv')
rseg.data.2 <- data.table::fread('JA4_7280_7340_hydrd_wy_sub.csv')

sdate <- as.Date(min(rseg.data.1$date))         # set as 1 - since we want the same date range for all models(?)
edate <- as.Date(max(rseg.data.1$date))

rseg.cols.1 <- names(rseg.data.1)
#rseg.flow.1 <- zoo(as.numeric(as.character( rseg.data.1$Qout )), order.by = rseg.data.1$index)
#rseg.flow.1 <- window(rseg.flow.1, start = sdate, end = edate)
rseg.data.1 <- zoo(rseg.data.1, order.by = rseg.data.1$index)
rseg.data.1 <- window(rseg.data.1, start = sdate, end = edate)
mode(rseg.data.1) <- 'numeric'

rseg.cols.2 <- names(rseg.data.2)
#rseg.flow.2 <- zoo(as.numeric(as.character( rseg.data.2$Qout )), order.by = rseg.data.2$date)
#rseg.flow.2 <- window(rseg.flow.2, start = sdate, end = edate)
rseg.data.2 <- zoo(rseg.data.2, order.by = rseg.data.2$index)
rseg.data.2 <- window(rseg.data.2, start = sdate, end = edate)
mode(rseg.data.2) <- 'numeric'

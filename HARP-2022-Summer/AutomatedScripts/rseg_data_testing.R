# testing datasets for ws_model_summary.rmd - both files in the AutomatedScripts folder
library(readr)

rseg.data.1 <- data.table::fread('JA4_7280_7340_hydrd_wy.csv')
rseg.data.2 <- data.table::fread('JA4_7280_7340_hydrd_wy_sub.csv')

data.match <- match(rseg.data.1$date, rseg.data.2$date)
sdate <- as.Date(min(data.match))
edate <- as.Date(max(data.match))

rseg.flow.1 <- zoo(as.numeric(as.character(rseg.data.1$Qout)), order.by = rseg.data.1$index)
rseg.flow.1 <- window(rseg.flow.1, start = sdate, end = edate)
mode(rseg.flow.1) <- 'numeric'

rseg.flow.2 <- zoo(as.numeric(as.character(rseg.data.2$Qout)), order.by = rseg.data.2$index)
rseg.flow.2 <- window(rseg.flow.2, start = sdate, end = edate)
mode(rseg.flow.2) <- 'numeric'



# rseg.file.path <- c("/media/model/p6/out/river/hsp2_2022/hydr/JA4_7280_7340_hydrd_wy.csv", "/media/model/p6/out/river/subsheds/hydr/JA4_7280_7340_hydrd_wy.csv" )
# rseg.hydrocode <- c("JA4_7280_7340","vahydrosw_wshed_JA4_7280_7340" )
# rseg.ftype <- c("cbp60","vahydro" )
# rseg.model.version <- c("cbp-6.0","cbp-6.1")
# runid.list <- c("hsp2_2022","subsheds")
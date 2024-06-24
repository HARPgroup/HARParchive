
# How to use SQL in R in a more flexible way than sqldf will permit
# https://dbi.r-dbi.org/reference/dbAppendTable.html
con <- dbConnect(RSQLite::SQLite(), ":memory:")
dbCreateTable(con, "dh_timeseries", ds$tsvalues)
dbWriteTable(con, "dh_timeseries", ds$tsvalues, overwrite = TRUE)
dbExecute(con, "insert into dh_timeseries(tid, tsvalue, featureid) values (2,4.5,10)")
dbExecute(con, "select * from dh_timeseries")
dbReadTable(con, "dh_timeseries")

# Function for a percent difference calculation between 2 columns of a dataframe using sqldf 

#fn_sqldf_sf already loaded w/ fns_spatial.R, which enables sqldf to work w/ spatial dataframes
# explicity load within this function also?

# data = rsegs; column1 = 'runid_11_l30_Qout'; column2 = 'runid_13_l30_Qout'; new_col = 'percentDiff_l30_Qout_11_13' #for testing

fn_pct_diff <- function(data, column1, column2, new_col) {
  
  statemt <- paste("SELECT data.*,
                  CASE WHEN (",column2," - ",column1,")==0
                    THEN 0 ", # 0/0 is NA so when difference is 0, %diff is 0
                   "ELSE ( (",column2," - ",column1,") / ",column1," * 100) ", #calculate %diff as usual
                   "END as ",new_col, #creates % diff. column
                   " FROM data
                 ",sep="") #!! need a case for when colname1 is zero but colname2 isn't ?
  
  data <- fn_sqldf_sf(statemt, geomback="data")
  
  return(data)
  
}
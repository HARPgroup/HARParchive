# Function for a percent difference calculation between 2 columns of a dataframe using sqldf 

library(sqldf)
source(paste0(github_location,"/HARParchive/HARP-2023-Summer/fns_spatial.R"),local = TRUE)
#fn_sqldf_sf loaded within fns_spatial.R, which enables sqldf to work w/ spatial dataframes

#data = rsegs; column1 = 'runid_11_l30_Qout'; column2 = 'runid_13_l30_Qout'; new_col = 'percentDiff_l30_Qout_11_13' #for testing

fn_pct_diff <- function(data, column1, column2, new_col) {
  
  df <- data
  
  statemt <- paste("SELECT df.*,
                  CASE WHEN (",column2," - ",column1,")==0
                    THEN 0 ", # 0/0 is NA so when difference is 0, %diff is 0
                   "ELSE ( (",column2," - ",column1,") / ",column1," * 100) ", #calculate %diff as usual
                   "END as ",new_col, #creates % diff. column
                   " FROM df
                 ",sep="") #!! need a case for when colname1 is zero but colname2 isn't ?
  
  df <- fn_sqldf_sf(statemt, geomback="data")
  
  return(df)
  
}
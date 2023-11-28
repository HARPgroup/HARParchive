# Function for a percent difference calculation between 2 columns of a dataframe using sqldf 

fn_pct_diff <- function(data, column1, column2, new_col) {
  
  ## from dataframe generator 
  
  statemt <- paste("SELECT data.*,
                  CASE WHEN (",column2," - ",column1,")==0
                    THEN 0 ", # 0/0 is NA so when difference is 0, %diff is 0
                   "ELSE ( (",column2," - ",column1,") / ",column1," * 100) ", #calculate %diff as usual
                   "END as ",new_col, #creates % diff. column
                   " FROM data
                 ",sep="") #!! need a case for when colname1 is zero but colname2 isn't ?
  
  rsegs <- fn_sqldf_sf(statemt, geomback="data")
  
  
}
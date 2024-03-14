# Function for a percent difference calculation between 2 columns of a dataframe using sqldf 

source(paste0(github_uri,"/HARP-2023-Summer/Mapping/Functions/fns_spatial.R"),local = TRUE)
#fn_sqldf_sf loaded within fns_spatial.R, which enables sqldf to work w/ spatial dataframes

#data = rsegs; column1 = 'runid_11_l30_Qout'; column2 = 'runid_13_l30_Qout'; new_col = 'percentDiff_l30_Qout_11_13' #for testing

#data: the dataframe, spatial or regular, which the % difference column will be added to 
#column1: the first col used in the % diff calculation (char)
#column1: the first col used in the % diff calculation (char)
#new_col: name of new % diff column added to data (char)
#geom: either TRUE or FALSE; whether the data is spatial or not (class = sf)
fn_pct_diff <- function(data, column1, column2, new_col, geom) {
  
  df <- as.data.frame(data)
  
  statemt <- paste("SELECT df.*,
                  CASE WHEN (",column2," - ",column1,")==0
                    THEN 0 ", # 0/0 is NA so when difference is 0, % diff is 0
                   "ELSE ( (",column2," - ",column1,") / ",column1," * 100) ", #calculate %diff as usual
                   "END as ",new_col, #creates % diff. column
                   " FROM df
                 ",sep="") #!! need a case for when colname1 is zero but colname2 isn't ?
  
  if (geom == TRUE) { #if data is spatial 
    df <- fn_sqldf_sf(statemt, geomback="df") #gives error when no geometry column present 
  } else {
    df <- sqldf(statemt)
  }
  
  
  
  return(df)
  
}
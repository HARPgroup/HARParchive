##Establishes function to streamline important table data for WSP_Regional_Summaries.Rmd
##Sorts & filters tables based on information provided in Run Sets (from mapstyle_config.R) 

fn_filter_table <- function(table_data, run_set_k){
  #table_data = rivTables[[k]]
  #run_set_k = run_config$riverseg_metrics[[k]]

  if(run_set_k[["exlude_NAs"]] == TRUE){ #remove NAs from table
    #note that all blank entries are replaced with NA prior to this function (WSP Summaries ~line 735)
    table_data <- table_data[!is.na(table_data[[run_set_k$sort_col]]), ]
  }
  if(!is.null(run_set_k$floor)){ #min value displayed will be > run_set$floor
    table_data <- table_data[table_data[[run_set_k$sort_col]] > run_set_k$floor |
                               is.na(table_data[[run_set_k$sort_col]]), ] #must include NAs in case exlude_NAs==FALSE
  }
  if(!is.null(run_set_k$ceiling)){ #max value displayed will be < run_set$ceiling
    table_data <- table_data[table_data[[run_set_k$sort_col]] < run_set_k$ceiling |
                               is.na(table_data[[run_set_k$sort_col]]), ]
  }
  table_data <- table_data[order(as.vector(table_data[[run_set_k$sort_col]]), 
                                 decreasing = run_set_k$sort_decreasing),] #sorts decreasing if sort_decreasing==TRUE
  if(!is.null(run_set_k$n_entries)){#limit num. rows displayed
    table_data <- head(table_data, n=run_set_k$n_entries)
  }
  return(table_data)
}
## Establishing a function to generate table when given data 
# Loading required libraries for table
library(flextable)

#create function
fn_tablegen <- function(featr_type, table, columns, alignment, origin_type, metric, origin, tabletitle, num) { 
  #featr_type: either 'facility' (map 1) or 'riverseg' (map 2)
  #table: data frame to be transformed into a flextable 
  #alignment: text alignment in flextable, either 'left', 'center', 'right', or 'justify'
  #columns: a list of columns to display from the dataframe supplied, in the order the columns will be 
  
  #set up rivseg table titles/same as rivseg map titles loop
    
  #set table defaults of theme and font and make sure theres no blanks from NAs, can be edited if desired
  set_flextable_defaults(
    font.size = 8, background.color = "white", 
    na_str = "NA", nan_str = "NA", #padding = 1, 
    table.layout = 'fixed', tabcolsep = 1.5,
    fonts_ignore = T, big.mark = "", decimal.mark= ".") #fonts_ignore applies when knitting as pdf 
  
  if (columns == "all") { #columns param in function is all meaning keep columns as normal
    ft <- flextable(table)
  } else { # want specific columns in table 
    tablecols <- table[, paste0(columns)] 
    ft <- flextable(tablecols)
  }
  
  ft <- theme_vanilla(ft)
  #autonum1 <- run_autonum(seq_id = "tab", pre_label = "Table ", bkm = "anytable")
  #autonum1 <- run_autonum(seq_id = "table", pre_label="Table", post_label = ': ') # number the table, bkm (bookmark) is important as the cross-referencing is done using the bookmark
  #ft <- set_caption(ft, caption= "Test", autonum =autonum1)
  
  
  if (featr_type == 'facility') { #facility tables need more editing then riverseg at the moment, this is subject to change 
    
    ft <- void(ft, j=1, part = "header") #remove name of 1st column in facil/source tables, which will always be the # for the facil/source
    ft <- width(ft, j= 'River Segment ID', width = 1) #making sure rseg ID isn't cut off in facil/source tables
    ft <- width(ft, j=1, width = 0.5)
    ft <- width(ft, j=2, width = 0.8)
    ft <- align(ft, align = alignment, part = "all")
    ft <- add_header_lines(ft, values= "Table 1.1") #add subtite
    ft <- add_header_lines(ft, values= tabletitle) #add title 
    ft <- fontsize(ft, i=1, size=14, part = "header") #inc size of title
    
  } else if (featr_type == 'riverseg') {
    
    ft <- autofit(ft)
    ft <- width(ft, j=1, width = 1.5)
    ft <- width(ft, j=2, width = 1)
    ft <- width(ft, j=4, width = 1)
    ft <- width(ft, j=5, width = 1)
    ft <- align(ft, align = alignment, part = "all")
    
    #highlight when precent diff is below highlight limit(defined in config)
    
    if (data_set == 'rseg_no_geom')  { #only do this for rseg maps not facil maps 
    ft <- flextable::bg(ft, i = ft$body$dataset$`percentDiff` < as.numeric(rseg_highlight_limit), bg = "yellow") #background color for flextable
    }
    
   
    #num <- grep(metric, rivseg_metric, value=FALSE)
    ft <- add_header_lines(ft, values= paste0("Table 2.", num)) #add subtite
    
    #ft <- add_header_lines(ft, values= numname) #add subtite
    ft <- add_header_lines(ft, values= tabletitle) #add title 
    ft <- fontsize(ft, i=1, size=14, part = "header") #inc size of title
  }
  return(ft)
}


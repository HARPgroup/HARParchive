## Establishing a function to generate table when given data 
# Loading required libraries for table
library(flextable)

#create function
fn_tablegen <- function(
    featr_type, table, columns, alignment, 
    origin_type, metric, origin, tabletitle, num,
    highlight_col = FALSE, highlight_limit = -10
  ) { 
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
    fonts_ignore = T, big.mark = "", decimal.mark = ".", digits = 4 ) #fonts_ignore applies when knitting as pdf 
  
  if (columns == "all") { #columns param in function is all meaning keep columns as normal
    ft <- flextable(table)
  } else { # want specific columns in table 
    tablecols <- table[, paste0(columns)] 
    ft <- flextable(tablecols)
  }
  
  ft <- theme_vanilla(ft)
  ft <- autofit(ft)
  ft <- width(ft, j=1, width = 1.5)
  ft <- width(ft, width = 1)
  ft <- align(ft, align = alignment, part = "all")
  
  #highlight when precent diff is below highlight limit(defined in config)
  # this should be removed in favor of the same type of styling used in maps whereby
  # we create a color ramp (regular, and yellow) and set a column with the style to use
  # in the input table.  But for now we keep this.
  if (highlight_col %in% names(ft$body$dataset)) {
    ft <- flextable::bg(ft, i = ft$body$dataset[,highlight_col] < as.numeric(highlight_limit), bg = "yellow") #background color for flextable
  }
  
  #num <- grep(metric, rivseg_metric, value=FALSE)
  ft <- add_header_lines(ft, values= paste0("Table ", num)) #add subtite
  
  #ft <- add_header_lines(ft, values= numname) #add subtite
  ft <- add_header_lines(ft, values= tabletitle) #add title 
  ft <- fontsize(ft, i=1, size=14, part = "header") #inc size of title
  return(ft)
}


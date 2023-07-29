## Establishing a function to generate table when given data 
# Loading required libraries for table
library(flextable)

#create function
fn_tablegen <- function(type, table, alignment) { 
#type: either 'facility' (map 1) or 'riverseg' (map 2)
#table: data frame to be transformed into a flextable 
#alignment: text alignment in flextable, either 'left', 'center', 'right', or 'justify'
  

#set table defaults of theme and font and make sure theres no blanks from NAs, can be edited if desired
set_flextable_defaults(
  font.size = 8, background.color = "white", 
  padding = 1, na_str = "NA", nan_str = "NA", 
  table.layout = 'fixed', tabcolsep = 1.5,
  fonts_ignore = T, big.mark = "", decimal.mark= ".") #fonts_ignore applies when knitting as pdf 

# specify aesthetics for spacing in the table, can be edited if desired
ft <- flextable(table)
ft <- theme_vanilla(ft)

if (type == 'facility') {
  ft <- void(ft, j=1, part = "header") #remove name of 1st column in facil/source tables, which will always be the # for the facil/source
  ft <- width(ft, j= 'River Segment ID', width = 1) #making sure rseg ID isn't cut off in facil/source tables
  ft <- line_spacing(ft, space = 1.25)
  ft <- width(ft, j=1, width = 0.25)
  ft <- width(ft, j=2, width = 0.8)
  ft <- align(ft, align = alignment, part = "all")
} else if (type == 'riverseg') {
  ft <- autofit(ft)
  x <- -10 # change percent desired -- to be moved to config 
  ft <- flextable::highlight(ft, i = ft$body$dataset$`% Difference` < x ,color = "yellow")  #highlight row when percent difference is below x
  }
assign('ft', ft, envir = globalenv())
}


## Establishing a function to generate table when given data 
# Loading required libraries for table
library(flextable)

#create function
fn_tablegen <- function(table) { 
  
#set table defaults of theme and font and make sure theres no blanks from nas, can be edited if desired
set_flextable_defaults(
  font.size = 8, background.color = "white", 
  padding = 1, na_str = "NA", nan_str = "NA", 
  table.layout = 'fixed', tabcolsep = 1.5,
  fonts_ignore = T) #fonts_ignore applies when knitting as pdf 

# specify aesthetics for spacing in the table, can be edited if desired
ft <- flextable(table)
ft <- void(ft, j=1, part = "header")
ft <- line_spacing(ft, space = 1.25)
ft <- theme_vanilla(ft)
ft <- width(ft, j=1, width = 0.25)
ft <- width(ft, j=2, width = 0.8)
ft <- width(ft, j=(3:length(table)), width = 1) #making sure rseg ID isn't cut off 
ft <- align(ft, align = "left", part = "all")
  

assign('ft', ft, envir = globalenv())

  


}

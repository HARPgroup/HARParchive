## Establishing a function to generate table when given data 
# Loading required libraries for table
library(flextable)

#create function
fn_tablegen <- function(table) { 
  
#set table defaults of theme and font and make sure theres no blanks from nas 
set_flextable_defaults(
  font.size = 8, background.color = "white", 
  padding = 1, na_str = "NA", nan_str = "NA", 
  table.layout = 'fixed', tabcolsep = 1.5,
  fonts_ignore = T) #fonts_ignore applies when knitting as pdf 

ft <- flextable(table)
ft <- void(ft, j=1, part = "header")
ft <- line_spacing(ft, space = 1.25)
ft <- theme_vanilla(ft)
ft <- width(ft, j=1, width = 0.25)
ft <- width(ft, j=2, width = 0.8)
ft <- width(ft, j=(3:7), width = 1) #making sure rseg ID isn't cut off 
ft <- align(ft, align = "left", part = "all")
  
#tabledf<- autofit(tabledf)
#ft <- flextable::align(tabledf, align = "center", part = "all") 
assign('ft', ft, envir = globalenv())
#print('Flextable as variable: ft')
  
#alignment of table
 #table %>% 
  # autofit() %>%
  # flextable::align(align = "center", j = c(1:9), part = "all") 

}

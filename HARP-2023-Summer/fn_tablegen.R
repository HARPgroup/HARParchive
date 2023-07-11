## Establishing a function to generate table when given data 
# Loading required libraries for table
library(flextable)

#create function
fn_tablegen <- function(table) { 
  
#set table defaults of theme and font and make sure theres no blanks from nas 
set_flextable_defaults(
  font.size = 10, background.color = "white", 
  padding = 6, 
  na_str = "NA", nan_str = "NA")

ft <- flextable(table)
ft <- theme_vanilla(ft)
  
#tabledf<- autofit(tabledf)
#ft <- flextable::align(tabledf, align = "center", part = "all") 
assign('ft', ft, envir = globalenv())
print('Flextable as variable: ft')
  
  
#alignment of table
 #table %>% 
  # autofit() %>%
  # flextable::align(align = "center", j = c(1:9), part = "all") 

#change titles of columns in table
#not needed if declared in data frame
# table <- set_header_labels(table,
#                            Facility = "Facility Name", 
#                            NUM = "Facility Number", 
#                            Source_Type = "Source Type",
#                            Rivseg= "River Segment", 
#                            rivsegName = " River Segment Names",
#                            Locality = "Location(County)",
#                            Upstream_Downstream = "Upstream Vs. Downstream", 
#                            vwp_max_mgy = "Permitted Capacity(mgy)",
#                            fiveYr = "5 Year Average Use - Source(mgy)"
                           
#)
}

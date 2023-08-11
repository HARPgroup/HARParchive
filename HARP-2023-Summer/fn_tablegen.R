## Establishing a function to generate table when given data 
# Loading required libraries for table
library(flextable)

#create function
fn_tablegen <- function(type, table, columns, alignment, map_type, metric, rivseg, title) { 
#type: either 'facility' (map 1) or 'riverseg' (map 2)
#table: data frame to be transformed into a flextable 
#alignment: text alignment in flextable, either 'left', 'center', 'right', or 'justify'
#columns: a list of columns to display from the dataframe supplied, in the order the columns will be 

#set up rivseg table titles/same as rivseg map titles loop
if (title == "default"){
    if (map_type == "basin") {
      title <- (paste("Basin Upstream of", rsegs$name[rsegs$riverseg==rivseg] , rivseg, metric, sep=" ") )
    } else if (map_type == "locality") {
      title <- paste0(locality, " Locality, ", metric)
    }  else if (map_type == "region") {
      title <- paste0(region, " Region, ", metric)
    } 
  }
  else {
    if (map_type == "basin") {
      title <- ( paste("Basin Upstream of", rsegs$name[rsegs$riverseg==rivseg] , rivseg, title, sep=" ") )
    } else if (map_type == "locality") {
      title <- paste0(locality, " Locality, ", title)
    }  else if (map_type == "region") {
      title <- paste0(region, " Region, " ,title)
    } 
  }
  
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



if (type == 'facility') { #facility tables need more editing then riverseg at the moment, this is subject to change 
  ft <- void(ft, j=1, part = "header") #remove name of 1st column in facil/source tables, which will always be the # for the facil/source
  ft <- width(ft, j= 'River Segment ID', width = 1) #making sure rseg ID isn't cut off in facil/source tables
  ft <- width(ft, j=1, width = 0.5)
  ft <- width(ft, j=2, width = 0.8)
  ft <- align(ft, align = alignment, part = "all")
  ft <- add_header_lines(ft, values= "Table 1.1") #add subtite
  ft <- add_header_lines(ft, values= "Map 1 Table") #add title 
  ft <- fontsize(ft, i=1, size=14, part = "header") #inc size of title
} else if (type == 'riverseg') {
  ft <- autofit(ft)
#  x <- -10 # change percent desired -- to moved to config
#  ft <- flextable::highlight(ft, i = ft$body$dataset$`% Difference` < as.numeric(rseg_highlight_limit),color = "yellow")  #highlight row when percent difference is below limit
  ft <- flextable::bg(ft, i = ft$body$dataset$`% Difference` < as.numeric(rseg_highlight_limit), bg = "yellow") #background color for flextable
  #for (k in 1:length(metric)){
    #numname[[k]]<- paste0("Table 2.", k)
    #ft <- add_header_lines(ft, values= numname[[k]] ) #add subtite
  #}
  ft <- add_header_lines(ft, values= title) #add title 
  ft <- fontsize(ft, i=1, size=14, part = "header") #inc size of title
}
return(ft)
#assign('ft', ft, envir = globalenv())
}


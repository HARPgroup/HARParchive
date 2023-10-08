## Establishing a function to generate table when given data 
# Loading required libraries for table
library(flextable)

#create function
fn_tablegen <- function(featr_type, table, columns, alignment, origin_type, metric, origin, title) { 
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
  
  if (title == "default"){
    if (origin_type == "basin") {
      title <- (paste("Basins Above", rsegs$name[rsegs$riverseg==origin], origin, ",", metric, sep=" ") )
    } else if (origin_type == "locality") {
      title <- paste0(origin, " Locality, ", metric)
    }  else if (origin_type == "region") {
      title <- paste0(region, " Region, ", metric)
    } 
  }
  else {
    if (origin_type == "basin") {
      title <- ( paste("Basins Above", rsegs$name[rsegs$riverseg==origin] , origin, ",", title, sep=" ") )
    } else if (origin_type == "locality") {
      title <- paste0(origin, " Locality, ", title)
    }  else if (origin_type == "region") {
      title <- paste0(region, " Region, " ,title)
    } 
  }
  
  ft <- void(ft, j=1, part = "header") #remove name of 1st column in facil/source tables, which will always be the # for the facil/source
  ft <- width(ft, j= 'River Segment ID', width = 1) #making sure rseg ID isn't cut off in facil/source tables
  ft <- width(ft, j=1, width = 0.5)
  ft <- width(ft, j=2, width = 0.8)
  ft <- align(ft, align = alignment, part = "all")
  ft <- add_header_lines(ft, values= "Table 1.1") #add subtite
  ft <- add_header_lines(ft, values= title) #add title 
  ft <- fontsize(ft, i=1, size=14, part = "header") #inc size of title
} else if (featr_type == 'riverseg') {
  ft <- autofit(ft)
  #highlight when precent diff is below highlight limit(defined in config)
  ft <- flextable::bg(ft, i = ft$body$dataset$`% Difference` < as.numeric(rseg_highlight_limit), bg = "yellow") #background color for flextable
 
   if (title == "default"){
    if (origin_type == "basin") {
      title <- (paste("Basins Above" , origin, ",", metric, sep=" ") )
    } else if (origin_type == "locality") {
      title <- paste0(origin, " Locality, ", metric)
    }  else if (origin_type == "region") {
      title <- paste0(origin, " Region, ", metric)
    } 
  }
  else {
    if (origin_type == "basin") {
      title <- ( paste("Basins Above", origin, ",", title, sep=" ") )
    } else if (origin_type == "locality") {
      title <- paste0(origin, " Locality, ", title)
    }  else if (origin_type == "region") {
      title <- paste0(origin, " Region, " ,title)
    } 
  }
  
  num <- grep(metric, rivseg_metric, value=FALSE)
  ft <- add_header_lines(ft, values= paste0("Table 2.", num)) #add subtite
  
  #ft <- add_header_lines(ft, values= numname) #add subtite
  ft <- add_header_lines(ft, values= title) #add title 
  ft <- fontsize(ft, i=1, size=14, part = "header") #inc size of title
}
return(ft)
#assign('ft', ft, envir = globalenv())
}


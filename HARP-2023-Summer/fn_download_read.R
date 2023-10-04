## Load data that requires file download from the web before reading in data
## inputs:
# url : file's download url
# filetype : currently takes either 'csv' or 'shp'
# zip : set to TRUE if downloading a zip file

fn_download_read <- function(url, filetype, zip=FALSE) { #creating function
  localpath <- tempdir()
  filename <- basename(url)
  filepath <- paste(localpath,"\\", filename, sep="")
  
  download.file(url, filepath)
  
  #unzip the file if required
  if(zip==TRUE){
    folder <- utils::unzip(filepath, exdir=localpath)
    filepath <- grep(".*.csv.*", folder, value=TRUE)
  }
  #read csv type and make data frame
  if(filetype=="csv"){
    df <- read.csv(file=filepath, header=TRUE, sep=",")
  }
  # read shp type and make data frame
  if(filetype=="shp"){
    layer <- gsub("\\.zip", "", filename)
    df <- sf::read_sf(dsn=localpath, layer=layer)
  } 
  #only download csv or shp files
  if(filetype!="csv" & filetype!="shp"){
    message(paste("Error in download_read(): filetype must be 'csv' or 'shp'"))
  }
  return(df)
}
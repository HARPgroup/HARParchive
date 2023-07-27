# Read data that requires file download from the web

fn_download_read <- function(url, filetype, zip) { #creating function
  localpath <- tempdir()
  filename <- basename(url)
  filepath <- paste(localpath,"\\", filename, sep="")
  
  download.file(url, filepath)
  
  #unzip the file if required
  if(zip==TRUE){
    folder <- unzip(filepath, exdir=localpath)
    filepath <- grep(".*.csv.*", folder, value=TRUE)
  }
  #read csv type and make data frame
  if(filetype=="csv"){
    df <- read.csv(file=filepath, header=TRUE, sep=",")
  }
  # read shp type and make data frame
  if(filetype=="shp"){
    layer <- gsub("\\.zip", "", filename)
    df <- read_sf(dsn=localpath, layer=layer)
  } 
  #only download csv or shp files
  if(filetype!="csv" & filetype!="shp"){
    message(paste("Error in download_read(): filetype must be 'csv' or 'shp'"))
  }
  return(df)
}
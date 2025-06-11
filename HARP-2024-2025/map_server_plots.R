args <- commandArgs(trailingOnly = TRUE)
suppressPackageStartupMessages(library('sqldf'))
suppressPackageStartupMessages(library('nanotime'))
suppressPackageStartupMessages(library('RCurl'))
suppressPackageStartupMessages(library('png'))
suppressPackageStartupMessages(library('recolorize'))
suppressPackageStartupMessages(library('terra'))
suppressPackageStartupMessages(library('rasterVis'))
suppressPackageStartupMessages(library('magick'))
if (length(args) != 8) {
  message("Usage: Rscript map_server.R Day Month Year Hour hydrocode ftype varkey plot_location")
  q()
}

day <- args[1]
month <- args[2]
year <- args[3]
hour <- args[4]
hydrocode <- args[5]
ftype <- args[6]
varkey <- args[7]
png_location <- args[8]

#finds our Extents that we use in our URL to create the box for our image
extent <- fn$sqldf("select St_Xmin(dh_geofield_geom) as xmin, St_Xmax(dh_geofield_geom) as xmax, St_Ymin(dh_geofield_geom) as ymin, St_Ymax(dh_geofield_geom) as ymax 
                from dh_feature_fielded 
                where ftype = '$ftype' 
                and hydrocode = '$hydrocode'", 
                connection=ds$connection)
#finds the tid for our image using the date and varkey
varid <- fn$sqldf("select data.varid
             from dh_timeseries_weather as data
             left join dh_variabledefinition as variable
             on(
             data.varid = variable.hydroid
             )
             where variable.varkey = '$varkey'
             AND extract(year from to_timestamp(tsendtime)) = '$year'
             AND extract(month from to_timestamp(tsendtime)) = '$month'
            AND extract(day from to_timestamp(tsendtime)) = '$day'
            AND extract(hour from to_timestamp(tsendtime)) = '$hour'",
             connection=ds$connection)

#This is really ugly, but it creates our URL using the variables we made earlier
image_url <- paste0("deq1.bse.vt.edu:81/cgi-bin/mapserv?map=/media/model/met/mapserv/raster_weather.map&layers=major_basins_va_only&layers=met_raster&mapext=",(extent$xmin),"%20",(extent$ymin),"%20",(extent$xmax),"%20",(extent$ymax),"&varid=",(as.integer(varid$varid)),"&year=",(year),"&month=",(month),"&day=",(day),"&hour=",(hour),"&mode=map")
#this is supposed to save our png to a given location
png(paste0("png_location",".png"))
image_data <- getBinaryURL(image_url) 
image <- image_read(image_data)
plot(image)
dev.off()


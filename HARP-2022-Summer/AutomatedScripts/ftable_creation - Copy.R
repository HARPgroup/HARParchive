
# script that generates FTABLEs from existing river segments in VAHydro

#setup
basepath='/var/www/R';
source("/var/www/R/config.R") #will need file in same folder/directory
library("hydrotools")
library('zoo')
library('lubridate')

# Accepting command arguments:
argst <- commandArgs(trailingOnly = T)
hydroid <- argst[1]
riverseg <- argst[2] 
channel <- argst[3]

#Testing: comment these out later
hydroid <- "68308"
riverseg <- "JL2_6440_6441_moormans_sugar_hollow"
channel<- "local_channel"

# Set up our data source
ds <- RomDataSource$new(site, rest_uname = rest_uname)
ds$get_token(rest_pw)
#----------------------------------------------------------------------------

# Pulling from VAHydro
rseg<- RomFeature$new(
  ds,
  list(
    hydrocode= paste("vahydrosw_wshed",riverseg,sep = "_"), 
    ftype='vahydro',
    bundle='watershed'
  ), 
  TRUE
)

model <- RomProperty$new(
  ds,
  list(
    varkey="om_water_model_node",
    propname=rseg$name,
    featureid=rseg$hydroid, 
    entity_type="dh_feature", 
    propcode="vahydro-1.0"
  ), 
  TRUE
)
#model$save(TRUE)

channel_prop <- RomProperty$new(
  ds,
  list(
    varkey="om_USGSChannelGeomObject_sub",
    featureid=model$pid,
    entity_type='dh_properties',
    propname = channel
  ),
  TRUE
)
#local_channel$save(TRUE)

drainage_area <- RomProperty$new(
  channel_prop[["datasource"]],
  list(
    varkey="om_class_Constant",
    featureid=channel_prop$pid,
    entity_type='dh_properties',
    propname='drainage_area'
  ),
  TRUE
)

province <- RomProperty$new(
  channel_prop[["datasource"]],
  list(
    varkey="om_class_Constant",
    featureid=channel_prop$pid,
    entity_type='dh_properties',
    propname='province'
  ),
  TRUE
)

length <- RomProperty$new(
  channel_prop[["datasource"]],
  list(
    varkey="om_class_Constant",
    featureid=channel_prop$pid,
    entity_type='dh_properties',
    propname='length'
  ),
  TRUE
)

slope <- RomProperty$new(
  channel_prop[["datasource"]],
  list(
    varkey="om_class_Constant",
    featureid=channel_prop$pid,
    entity_type='dh_properties',
    propname='slope'
  ),
  TRUE
)
#----------------------------------------------------------------------------

# Calculating Channel Geometry
if (province$propvalue == 1){
  #Appalachian Plateau
  hc = 2.030 # "c" = coefficient for regional regression eqn
  he = 0.2310 # "e" = exponent for regional regression eqn
  bfc = 12.175
  bfe = 0.04711
  bc = 5.389
  be = 0.5349
  n = 0.036 # Manning's n
}

if (province$propvalue ==2){
  #Valley and Ridge
  hc = 1.435
  he = 0.2830
  bfc = 13.216
  bfe = 0.4532
  bc = 4.667
  be = 0.5489
  n = 0.038
}

if (province$propvalue ==3){
  #Piedmont
  hc = 2.137
  he = 0.2561
  bfc = 14.135
  bfe = 0.4111
  bc = 6.393
  be = 0.4604
  n = 0.095
}

if (province$propvalue ==4){
  #Coastal Plain
  hc = 2.820
  he = 0.2000
  bfc = 15.791
  bfe = 0.3758
  bc = 6.440
  be = 0.4442
  n = 0.040
}

# Regional Regression Eqn's:
#bank full stage "max height before floodplain":
h = hc * (drainage_area$propvalue**he)
#bank full width "max width before floodplain":
bf = bfc * (drainage_area$propvalue**bfe)
#base width "width @ lowest stage":
b = bc * (drainage_area$propvalue**be)
#side slope:
z = 0.5 * (bf - b ) / h

#----------------------------------------------------------------------------

# Calculating FTABLE
# Depth
depth <- seq(0,h,length=10) #sequence between zero and bank full stage

# Surface Area
    # water surface width * length of channel
area <- (b + 2*z*depth) * length$propvalue

# Volume
    # length * (b*depth + side_slope*depth^2)
vol <- length$propvalue * (b*depth + z*depth**2)

# Discharge
    # Q = V * A
    # Manning's: V = (1/n) * R^(2/3) * S^(1/2)
disch <- (1/n) * (depth/2)**(2/3) * slope$propvalue**0.5 * sqrt(3)*depth**2

# Compile
ftable <- data.frame(depth, area, vol, disch)
#----------------------------------------------------------------------------

# Exporting to VAHydro

exp_ftable<- RomProperty$new(
  ds, list(
    varkey="om_class_Constant", # what do we say when it's a table??
    featureid=local_channel$pid,
    entity_type='dh_properties',
    propname = 'ftable'
  ),
  TRUE
)
exp_ftable$propvalue <- ftable
exp_ftable$save(TRUE)



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
hydroid <- "68298"
riverseg <- "OR1_7700_7980"
channel<- "0. River Channel"

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
    propname="Upper Club Creek", #previously rseg$name
    featureid=rseg$hydroid,
    entity_type="dh_feature", 
    propcode="vahydro-1.0"
  ), 
  TRUE
)

channel_prop <- RomProperty$new(
  ds,
  list(
    varkey="om_USGSChannelGeomObject", #for local_channel it needs _sub added to end
    featureid=model$pid,
    entity_type='dh_properties',
    propname = channel
  ),
  TRUE
)

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

da <- drainage_area$propvalue #106.052

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

province <- province$propvalue

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

clength <- length$propvalue

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
cslope <- slope$propvalue #0.0016

#----------------------------------------------------------------------------

# Calculating Channel Geometry
if (province == 1){
  #Appalachian Plateau
  hc = 2.030 # "c" = coefficient for regional regression eqn
  he = 0.2310 # "e" = exponent for regional regression eqn
  bfc = 12.175
  bfe = 0.04711
  bc = 5.389
  be = 0.5349
  n = 0.036 # Manning's n
}

if (province ==2){
  #Valley and Ridge
  hc = 1.435
  he = 0.2830
  bfc = 13.216
  bfe = 0.4532
  bc = 4.667
  be = 0.5489
  n = 0.038
}

if (province ==3){
  #Piedmont
  hc = 2.137
  he = 0.2561
  bfc = 14.135
  bfe = 0.4111
  bc = 6.393
  be = 0.4604
  n = 0.095
}

if (province ==4){
  #Coastal Plain
  hc = 2.820
  he = 0.2000
  bfc = 15.791
  bfe = 0.3758
  bc = 6.440
  be = 0.4442
  n = 0.040
}

if (province == -1){
  #Single/General Equation
  hc = 2.177
  he = 0.2293
  bfc = 13.128
  bfe = 0.4432
  bc = 5.471
  be = 0.5103
  n = 0.05225 #avg of above n values
}
  
# Regional Regression Eqn's:
#bank full stage "max height before floodplain":
h = hc * (da**he)
#bank full width "max width before floodplain":
bf = bfc * (da**bfe)
#base width "width @ lowest stage":
b = bc * (da**be)
#side slope:
z = 0.5 * (bf - b ) / h

#----------------------------------------------------------------------------

# Calculating FTABLE

# Depth
#depth <- seq(0,h,length=19) #sequence between zero and bank full stage
depth<- c(0.586,1.172,1.758, 2.343, 2.929, 3.515)

# Surface Area
    # water surface width * length of channel
sw <- b+2*z*depth
area <- (sw *clength)/43560 #converting from ft^2 to acres

# Volume
    # length * cross sectional area
vol <- (clength * (0.5*(sw+b)*depth))/43560 #converting to ft-acre

# Discharge
    # Q = V * A , A = cross sectional area / flow area
    # Manning's: V = (1.49/n) * R^(2/3) * S^(1/2) 
    # ^^ (1.49/n) is English ; (1/n) is metric
    # Hydraulic radius = (depth*(b+z*depth))/(b+2*depth*sqrt(1+z^2))
    # or Hydraulic R = (depth*(b + sw)/2)/(b + 2*(((sw-b)/2)**2 + depth**2)**0.5)
disch <- (1.49/n) * ((depth*(b+z*depth))/(b+2*depth*sqrt(1+z^2)))**(2/3) * cslope**0.5 * 0.5*(sw+b)*depth

# Compile 
ftable <- data.frame(depth, area, vol, disch)
#colnames(ftable) <- ("Depth (ft)", "Area (acres)", "Volume (ac-ft)", "Discharge (cfs)")
#----------------------------------------------------------------------------

# Exporting to VAHydro

#exp_ftable<- RomProperty$new(
#  ds, list(
#    varkey="om_class_Constant", # what do we say when it's a table??
#    featureid=local_channel$pid,
#    entity_type='dh_properties',
#    propname = 'ftable'
#  ),
#  TRUE
#)
#exp_ftable$propvalue <- ftable
#exp_ftable$save(TRUE)


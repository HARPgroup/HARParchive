
# script that generates FTABLEs from existing river segments in VAHydro

# Setup
library("hydrotools") #needed to pull values from VAHydro 

# Link data source (ds) to VAHydro
basepath='/var/www/R';
source("/var/www/R/config.R") #will need file in same folder/directory
ds <- RomDataSource$new(site, rest_uname = rest_uname)
ds$get_token(rest_pw)

# Accepting command arguments:
argst <- commandArgs(trailingOnly = T)
riverseg <- argst[1] 
channel <- argst[2]

#Testing: comment these out later
#riverseg <- "OR1_7700_7980"
riverseg <- "JL2_6850_6890"
channel<- '0. River Channel'

#----Pulling from VAHydro----
rseg<- RomFeature$new(ds,list(
    hydrocode= paste("vahydrosw_wshed",riverseg,sep = "_"), 
    ftype='vahydro',
    bundle='watershed'), 
  TRUE)

model <- RomProperty$new(ds,list(
    varkey="om_water_model_node",
    featureid=rseg$hydroid,
    entity_type="dh_feature", 
    propcode="vahydro-1.0"), 
  TRUE)

channel_prop <- RomProperty$new(ds,list(
    varkey="om_USGSChannelGeomObject", #for local_channel it needs _sub added to end
    featureid=model$pid,
    entity_type='dh_properties',
    propname = channel),
  TRUE)

drainage_area <- RomProperty$new(channel_prop[["datasource"]],list(
    varkey="om_class_Constant",
    featureid=channel_prop$pid,
    entity_type='dh_properties',
    propname='drainage_area'),
  TRUE)
da <- drainage_area$propvalue


province <- RomProperty$new(channel_prop[["datasource"]],list(
    varkey="om_class_Constant",
    featureid=channel_prop$pid,
    entity_type='dh_properties',
    propname='province'),
  TRUE)
prov <- province$propvalue


length <- RomProperty$new(channel_prop[["datasource"]],list(
    varkey="om_class_Constant",
    featureid=channel_prop$pid,
    entity_type='dh_properties',
    propname='length'),
  TRUE)
clength <- length$propvalue #channel length


slope <- RomProperty$new(channel_prop[["datasource"]],list(
    varkey="om_class_Constant",
    featureid=channel_prop$pid,
    entity_type='dh_properties',
    propname='slope'),
  TRUE)
cslope <-  slope$propvalue #longitudinal channel slope


#----Provincial Channel Geometry----
if (prov == 1){
  #Appalachian Plateau
  hc = 2.030 # "c" = coefficient for regional regression eqn
  he = 0.2310 # "e" = exponent for regional regression eqn
  bfc = 12.175
  bfe = 0.04711
  bc = 5.389
  be = 0.5349
  n = 0.036 # Manning's n
  nf = 0.055 # Floodplain n
}

if (prov == 2){
  #Valley and Ridge
  hc = 1.435
  he = 0.2830
  bfc = 13.216
  bfe = 0.4532
  bc = 4.667
  be = 0.5489
  n = 0.038
  nf = 0.048
}

if (prov ==3){
  #Piedmont
  hc = 2.137
  he = 0.2561
  bfc = 14.135
  bfe = 0.4111
  bc = 6.393
  be = 0.4604
  n = 0.04
  nf = 0.063
}

if (prov ==4){
  #Coastal Plain
  hc = 2.820
  he = 0.2000
  bfc = 15.791
  bfe = 0.3758
  bc = 6.440
  be = 0.4442
  n = 0.033
  nf = 0.06
}

# Regional Regression Eqn's:
#bank full stage "max height before floodplain":
h = hc * (da**he)
#bank full width "max width before floodplain":
bf = bfc * (da**bfe)
#base width "width @ lowest stage":
b = bc * (da**be)
#side slope of channel:
z = 0.5 * (bf - b ) / h


#----Calculating FTABLE----
#--additional parameters:
#depth
cdepth <- seq(0,h,length=10) #channel
fdepth <- seq(h+1, h*4 ,length=9) #floodplain
depth <- c(cdepth, fdepth)

#floodplain side slope where base = bankfull width & top width = 5x that:
#zf <- 0.5 * (5*bf - bf)/(h) 
zf <- z

#--function:
fn_make_trap_ftable <- function(depth, clength, cslope, b, z, n) { 
  sw <- b + 2*z*depth #surface width
  sw[depth == 0] <- 0
  area <- (sw * clength)/43560 #surface area
  vol <- (clength * (0.5*(sw+b)*depth))/43560
  disch <- (1.49/n) * ((depth*(b+z*depth))/(b+2*depth*sqrt(1+z^2)))**(2/3) * cslope**0.5 * 0.5*(sw+b)*depth
  ftable <- data.frame(depth, area, vol, disch)
  return(ftable)
}

cftab <- fn_make_trap_ftable(cdepth, clength, cslope, b, z, n) #in-channel
fptab <- fn_make_trap_ftable(fdepth-h, clength, cslope, 5*bf, zf, nf) #floodplain

# add values from below floodplain to fptab
fptab$depth <- fptab$depth + h
fptab$vol <- fptab$vol + max(cftab$vol)
fptab$disch <- fptab$disch + max(cftab$disch)

ftable_specific <- rbind(cftab, fptab)

#----Exporting----


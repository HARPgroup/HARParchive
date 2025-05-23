
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
riverseg <- "JL2_6850_6890"
channel<- "0. River Channel"

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
prov <- -1 #province$propvalue


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
clength <- length$propvalue #channel length


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
cslope <- slope$propvalue #longitudinal channel slope

#----------------------------------------------------------------------------
# Calculating Channel Geometry
if (prov == 1){
  #Appalachian Plateau
  hc = 2.030 # "c" = coefficient for regional regression eqn
  he = 0.2310 # "e" = exponent for regional regression eqn
  bfc = 12.175
  bfe = 0.04711
  bc = 5.389
  be = 0.5349
  n = 0.036 # Manning's n
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
}

if (prov ==3){
  #Piedmont
  hc = 2.137
  he = 0.2561
  bfc = 14.135
  bfe = 0.4111
  bc = 6.393
  be = 0.4604
  n = 0.095
}

if (prov ==4){
  #Coastal Plain
  hc = 2.820
  he = 0.2000
  bfc = 15.791
  bfe = 0.3758
  bc = 6.440
  be = 0.4442
  n = 0.040
}

if (prov == -1){
  #Single/General Equation
  hc = 2.177
  he = 0.2293
  bfc = 13.128
  bfe = 0.4432
  bc = 5.471
  be = 0.5103
  n = 0.05225 #avg of ^above n values
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

#----------------------------------------------------------------------------
# Calculating FTABLE

# Depth
#depth <- seq(0,h,length=19) #sequence between zero and bank full stage
depth<- c(.000, 0.757, 1.513, 2.27, 3.027, 3.783, 4.540, 6.053, 7.567, 9.080, 12.107, 15.133, 
            18.160, 21.187, 24.213, 27.240, 30.267, 33.293, 36.320)

# Surface Area
# water surface width * length of channel
sw <- b + 2*z*depth
sw[depth == 0] <- 0 # zero depth = zero surface water width
area = (sw * clength)/43560 #converting to acres

# Volume
# length * cross sectional area
vol <- (clength * (0.5*(sw+b)*depth))/43560 #converting to ft-acre

# Discharge
# Q = V * A ; where A = cross sectional area / flow area
# Manning's Eqn: V = (1.49/n) * R^(2/3) * S^(1/2) 
# (1.49/n) is English ; (1/n) is metric
# Hydraulic Radius = (depth*(b+z*depth))/(b+2*depth*sqrt(1+z^2))
# OR Hydraulic R = (depth*(b + sw)/2)/(b + 2*(((sw-b)/2)**2 + depth**2)**0.5)

disch <- (1.49/n) * ((depth*(b+z*depth))/(b+2*depth*sqrt(1+z^2)))**(2/3) * 
  cslope**0.5 * 0.5*(sw+b)*depth

# Compile 
ftable <- data.frame(depth, area, vol, disch)
colnames(ftable) <- c('Depth (ft)', 'Area (acres)', 'Volume (ac-ft)', 'Discharge (cfs)')

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

#----------------------------------------------------------------------------
# Trying Some Plotting

# Run w/ Province = 3:
ftable_specific <- data.frame(depth, area, vol, disch)
# Run w/ Province = -1:
ftable_generic <- data.frame(depth, area, vol, disch)

# Loading UCI Table
library(RCurl)
ftable_uci <- read.csv(paste('http://deq1.bse.vt.edu:81/p532/input/param/river/hsp2_2022/ftables/',
                         riverseg, '.ftable', sep=''), 
                sep='', 
                header=FALSE,
                col.names = c('depth','area','vol', 'disch', 'flo-thru'),
                nrows = 19, skip= 5,
                comment.char="*")


# Zoomed Out:
# Area
par(mfrow = c(2,3))
plot(ftable_uci$depth, ftable_uci$area, type='l', col = 'red', ylim=c(0, max(ftable_uci$area)), xlab = "Depth (ft)", ylab = "Water Surface Area (acres)")
lines(ftable_specific$depth, ftable_specific$area, type='l', col='dark green')
lines(ftable_generic$depth, ftable_generic$area, type='l', col='blue')
title(main = 'Ftable Area')
legend(x=0 , y=max(ftable_uci$area) , legend= c("uci", "specific equations", "generic equations"), col=c('red', 'dark green', 'blue'),bty='n',lty=1, cex=0.85)

# Volume
plot(ftable_uci$depth, ftable_uci$vol, type='l', col = 'red', ylim=c(0, max(ftable_uci$vol)), xlab = "Depth (ft)", ylab = "Volume (acre-ft)")
lines(ftable_specific$depth, ftable_specific$vol, type='l', col='dark green')
lines(ftable_generic$depth, ftable_generic$vol, type='l', col='blue')
title(main = 'Ftable Volume')
legend(x=0 , y=max(ftable_uci$vol) , legend= c("uci", "specific equations", "generic equations"), col=c('red', 'dark green', 'blue'),bty='n',lty=1, cex = 0.85)

# Discharge
plot(ftable_uci$depth, ftable_uci$disch, type='l', col = 'red', ylim=c(0, max(ftable_uci$disch)), xlab = "Depth (ft)", ylab = "Discharge (cfs)")
lines(ftable_specific$depth, ftable_specific$disch, type='l', col='dark green')
lines(ftable_generic$depth, ftable_generic$disch, type='l', col='blue')
title(main = 'Ftable Discharge')
legend(x=0 , y=max(ftable_uci$disch) , legend= c("uci", "specific equations", "generic equations"), col=c('red', 'dark green', 'blue'),bty='n',lty=1, cex=0.85)

# Zooming In:
# Area
plot(ftable_uci$depth, ftable_uci$area, type='l', col = 'red', xlim=c(0,h+3), ylim=c(0,median(ftable_uci$area)), xlab = "Depth (ft)", ylab = "Water Surface Area (acres)")
lines(ftable_specific$depth, ftable_specific$area, type='l', col='dark green')
lines(ftable_generic$depth, ftable_generic$area, type='l', col='blue')
title(main = 'Area Just Past h')
legend(x=h-5 , y=ftable_uci$area[2] , legend= c("uci", "specific equations", "generic equations"), col=c('red', 'dark green', 'blue'),bty='n',lty=1, cex=0.85)

# Volume
plot(ftable_uci$depth, ftable_uci$vol, type='l', col = 'red', xlim=c(0,h+3), ylim=c(0, median(ftable_uci$vol)), xlab = "Depth (ft)", ylab = "Volume (acre-ft)")
lines(ftable_specific$depth, ftable_specific$vol, type='l', col='dark green')
lines(ftable_generic$depth, ftable_generic$vol, type='l', col='blue')
title(main = 'Volume Just Past h')
legend(x=0 , y=median(ftable_uci$vol), legend= c("uci", "specific equations", "generic equations"), col=c('red', 'dark green', 'blue'),bty='n',lty=1, cex = 0.85)

# Discharge
plot(ftable_uci$depth, ftable_uci$disch, type='l', col = 'red', xlim=c(0,h+3), ylim=c(0, median(ftable_uci$disch)), xlab = "Depth (ft)", ylab = "Discharge (cfs)")
lines(ftable_specific$depth, ftable_specific$disch, type='l', col='dark green')
lines(ftable_generic$depth, ftable_generic$disch, type='l', col='blue')
title(main = 'Discharge Just Past h')
legend(x=0 , y=median(ftable_uci$disch) , legend= c("uci", "specific equations", "generic equations"), col=c('red', 'dark green', 'blue'),bty='n',lty=1, cex=0.85)

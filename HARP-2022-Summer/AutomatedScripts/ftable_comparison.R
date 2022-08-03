
# Comparing Our Ftables to UCI w/ plotting

# Setup
library("hydrotools") #needed to pull values from VAHydro 

# Link data source (ds) to VAHydro
basepath='/var/www/R';
source("/var/www/R/config.R") #will need file in same folder/directory
ds <- RomDataSource$new(site, rest_uname = rest_uname)
ds$get_token(rest_pw)

# Arguments:
riverseg <- "OR1_7700_7980"
#riverseg <- "JL2_6850_6890"
channel<- "0. River Channel"

# Load the UCI Table:
library(RCurl)
ftable_uci <- read.csv(paste('http://deq1.bse.vt.edu:81/p532/input/param/river/hsp2_2022/ftables/',
                             riverseg, '.ftable', sep=''), 
                       sep='', 
                       header=FALSE,
                       col.names = c('depth','area','vol', 'disch', 'flo-thru'),
                       nrows = 19, skip= 5,
                       comment.char="*")


#----Pulling from VAHydro----
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
    #propname="Upper Club Creek", #previously rseg$name
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
prov <- province$propvalue


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


#----Calculating Regional-Specific Ftable----
# Geometry:
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
  n = 0.04
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

# - - - - Ftable: - - - - 
# Depth
depth <- c(ftable_uci$depth)

# Surface Area
# water surface width * length of channel
sw <- b + 2*z*depth
sw[depth == 0] <- 0 # zero depth = zero surface water width
area = (sw * clength)/43560 #converting to acres

# Volume
# length * cross sectional area
vol <- (clength * (0.5*(sw+b)*depth))/43560 #converting to ft-acre

# Discharge
disch <- (1.49/n) * ((depth*(b+z*depth))/(b+2*depth*sqrt(1+z^2)))**(2/3) * 
  cslope**0.5 * 0.5*(sw+b)*depth

# Compile
ftable_specific <- data.frame(depth, area, vol, disch)



#----Generic Ftable----
prov <- -1
if (prov == -1){
  #Single/General Equation
  hc = 2.177
  he = 0.2293
  bfc = 13.128
  bfe = 0.4432
  bc = 5.471
  be = 0.5103
  n = 0.037 #avg of other n values
}
# Regional Regression Eqn's:
#bank full stage "max height before floodplain":
h_g = hc * (da**he)
#bank full width "max width before floodplain":
bf = bfc * (da**bfe)
#base width "width @ lowest stage":
b = bc * (da**be)
#side slope of channel:
z = 0.5 * (bf - b ) / h_g

# - - - - Ftable: - - - - 
# Surface Area
# water surface width * length of channel
sw <- b + 2*z*depth
sw[depth == 0] <- 0 # zero depth = zero surface water width
area = (sw * clength)/43560 #converting to acres

# Volume
# length * cross sectional area
vol <- (clength * (0.5*(sw+b)*depth))/43560 #converting to ft-acre

# Discharge
disch <- (1.49/n) * ((depth*(b+z*depth))/(b+2*depth*sqrt(1+z^2)))**(2/3) * 
  cslope**0.5 * 0.5*(sw+b)*depth

# Compile
ftable_generic <- data.frame(depth, area, vol, disch)




#----Plotting----
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
plot(ftable_uci$depth, ftable_uci$area, type='l', col = 'red', xlim=c(0,h+3), ylim=c(0,ftable_uci$area[11]), xlab = "Depth (ft)", ylab = "Water Surface Area (acres)")
lines(ftable_specific$depth, ftable_specific$area, type='l', col='dark green')
lines(ftable_generic$depth, ftable_generic$area, type='l', col='blue')
title(main = 'Area Just Past h')
#legend(x=h-5 , y=ftable_uci$area[2] , legend= c("uci", "specific equations", "generic equations"), col=c('red', 'dark green', 'blue'),bty='n',lty=1, cex=0.85)

  # Volume
plot(ftable_uci$depth, ftable_uci$vol, type='l', col = 'red', xlim=c(0,h+3), ylim=c(0,ftable_uci$vol[11]), xlab = "Depth (ft)", ylab = "Volume (acre-ft)")
lines(ftable_specific$depth, ftable_specific$vol, type='l', col='dark green')
lines(ftable_generic$depth, ftable_generic$vol, type='l', col='blue')
title(main = 'Volume Just Past h')
#legend(x=0 , y=median(ftable_uci$vol), legend= c("uci", "specific equations", "generic equations"), col=c('red', 'dark green', 'blue'),bty='n',lty=1, cex = 0.85)

  # Discharge
plot(ftable_uci$depth, ftable_uci$disch, type='l', col = 'red', xlim=c(0,h+3), ylim=c(0,ftable_uci$disch[11]), xlab = "Depth (ft)", ylab = "Discharge (cfs)")
lines(ftable_specific$depth, ftable_specific$disch, type='l', col='dark green')
lines(ftable_generic$depth, ftable_generic$disch, type='l', col='blue')
title(main = 'Discharge Just Past h')
#legend(x=0 , y=median(ftable_uci$disch) , legend= c("uci", "specific equations", "generic equations"), col=c('red', 'dark green', 'blue'),bty='n',lty=1, cex=0.85)



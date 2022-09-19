
# Comparing Our Ftables to UCI w/ plotting

# Setup
library("hydrotools") #needed to pull values from VAHydro 

# Link data source (ds) to VAHydro
basepath='/var/www/R';
source("/var/www/R/config.R") #will need file in same folder/directory
ds <- RomDataSource$new(site, rest_uname = rest_uname)
ds$get_token(rest_pw)

# Arguments:
#riverseg <- "OR1_7700_7980"
riverseg <- "JL2_6850_6890"
channel<- "0. River Channel"

# Load the UCI Table:
ftable_uci <- read.csv(paste('http://deq1.bse.vt.edu:81/p532/input/param/river/hsp2_2022/ftables/',
                             riverseg, '.ftable', sep=''), 
                       sep='', 
                       header=FALSE,
                       col.names = c('depth','area','vol', 'disch', 'flo-thru'),
                       nrows = 19, skip= 5)

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
clength <- length$propvalue #122284.8  channel length


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
cslope <-  slope$propvalue #0.0016 longitudinal channel slope


#----Regional-Specific Channel Geometry----
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

#- - - Regional Regression Eqn's:- - -
#bank full stage "max height before floodplain":
h = hc * (da**he)
#bank full width "max width before floodplain":
bf = bfc * (da**bfe)
#base width "width @ lowest stage":
b = bc * (da**be)
#side slope of channel:
z = 0.5 * (bf - b ) / h

#----Ftable Parameters---- 
# Depth
cdepth <- c(ftable_uci$depth[1:10])
fdepth <- c(ftable_uci$depth[11:19])
depth <- c(cdepth, fdepth)

# Floodplain Parameters
ym <- h/1.25 # mean channel depth
wm <- b + 2*z*ym # mean channel width
fw <- bf #2*wm + bf # "the flood plain width, on each side of the reach, is equal to the mean channel width" -BASINS tech note 1
zf <- 6*z #for Rockfish
#zf <- 9.25*z #for Roanoke

#----Replacing Calculations w/ Function----
fn_make_trap_ftable <- function(depth, clength, cslope, b, z, n) { 
  sw <- b + 2*z*depth
  sw[depth == 0] <- 0
  area <- (sw * clength)/43560
  vol <- (clength * (0.5*(sw+b)*depth))/43560
  disch <- (1.49/n) * ((depth*(b+z*depth))/(b+2*depth*sqrt(1+z^2)))**(2/3) * cslope**0.5 * 0.5*(sw+b)*depth
  ftable <- data.frame(depth, area, vol, disch)
  return(ftable)
}

cftab <- fn_make_trap_ftable(cdepth, clength, cslope, b, z, n)

fptab <- fn_make_trap_ftable(fdepth-h, clength, cslope, bf, zf, nf)

# add values from below floodplain to floodplain ftab
fptab$depth <- fptab$depth + h
fptab$vol <- fptab$vol + max(cftab$vol)
fptab$disch <- fptab$disch + max(cftab$disch)

ftable_specific <- rbind(cftab, fptab)

#----Pulling from NHDplus----
#install.packages("nhdplusTools")
library(nhdplusTools)

#outdir <- "C:/aa_HARP"
#download_nhdplusv2(
#  outdir,
#  url = paste0("https://edap-ow-data-commons.s3.amazonaws.com/NHDPlusV21/",
#               "Data/NationalData/NHDPlusV21_NationalData_Seamless", "_Geodatabase_Lower48_07.7z"),
#  progress = TRUE
#)

COMID <- c(8545673,8548041,8545773,8545771,8545765,8545713,8547533,8545537,8545493,8547479,8545397,8545399,8545389,8545381,8545369,8545377,8545351,8545321,8545293,8545181,8545165,8545111,8545109,8545055,8547471,8545069,8545079,8545089,8545091,8545093,8545083,8545009,8544987,8544985,8544943,8544925,8544859,8544833,8547459,8544805,8544751,8544683,8544663,8544639,8544631)
rockfish_data_flow <- get_nhdplus(comid=COMID, realization="flowline")

#----
#----Generic Ftable----
prov_g <- -1
if (prov_g == -1){
  #Single/General Equation
  hc = 2.177
  he = 0.2293
  bfc = 13.128
  bfe = 0.4432
  bc = 5.471
  be = 0.5103
  n_g = 0.037 #avg of other n values
}
# Regional Regression Eqn's:
#bank full stage "max height before floodplain":
h_g = hc * (da**he)
#bank full width "max width before floodplain":
bf_g = bfc * (da**bfe)
#base width "width @ lowest stage":
b_g = bc * (da**be)
#side slope of channel:
z_g = 0.5 * (bf - b ) / h_g


ftable_generic <- fn_make_trap_ftable(depth, clength, cslope, b_g, z_g, n_g)

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






#Old or Misc Stuff----
#----Saving to UCI----
writeLines(sprintf("% 16s", as.list(round(ftab, 2))))

model_features <- c(68210, 68123, 68183)
sprintf("% 12s", model_features)

#----Original Ftable:----
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
A <- ((sw+b)/2)*depth
P <- b + 2*depth*sqrt(z**2 +1)
disch <- (1.49/n) * (A/P)**(2/3) * cslope**0.5 * A

# Compile
ftable_specific <- data.frame(depth, area, vol, disch)

#----Original FP Calculations----
# Surface Area
csw <- b + 2*z*cdepth
fsw <- fw + 2*zf*(fdepth-h)
sw <- c(csw, fsw)
sw[depth == 0] <- 0 # zero depth = zero surface water width
area = (sw * clength)/43560 #converting to acres

# Volume
cvol <- (clength * (0.5*(csw+b)*cdepth))/43560
hvol <- (clength * (0.5*((b + 2*z*h)+b)*h))/43560
fvol <- (clength * (0.5*(fsw+fw)*(fdepth-h)))/43560 + hvol

vol <- c(cvol,fvol)

# Discharge
cdisch <- (1.49/n) * ((cdepth*(b+z*cdepth))/(b+2*cdepth*sqrt(1+z^2)))**(2/3) * 
  cslope**0.5 * 0.5*(csw+b)*cdepth
hdisch <- (1.49/n) * ((h*(b+z*h))/(b+2*h*sqrt(1+z^2)))**(2/3) * 
  cslope**0.5 * 0.5*((b + 2*z*h)+b)*h
fdisch <- (1.49/nf) * (((fdepth-h)*(fw+zf*(fdepth-h)))/(fw+2*(fdepth-h)*sqrt(1+zf^2)))**(2/3) * 
  cslope**0.5 * 0.5*(fsw+fw)*(fdepth-h) + hdisch

disch <- c(cdisch, fdisch)

# Compile
ftable_specific <- data.frame(depth, area, vol, disch)

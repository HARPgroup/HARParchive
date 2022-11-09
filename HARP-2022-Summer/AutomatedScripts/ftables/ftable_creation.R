
# script that generates FTABLEs from existing river segments in VAHydro

# Setup
suppressPackageStartupMessages(library("hydrotools")) #needed to pull values from VAHydro 

# Link data source (ds) to VAHydro
basepath='/var/www/R';
source("/var/www/R/config.R") #will need file in same folder/directory
ds <- RomDataSource$new(site, rest_uname = rest_uname)
ds$get_token(rest_pw)

# Accepting command arguments:
argst <- commandArgs(trailingOnly = T)
riverseg <- as.character(argst[1]) 
channel <- as.character(argst[2])
path <- as.character(argst[3])

#Testing: comment these out later
#riverseg <- "OR1_7700_7980"
#riverseg <- "JL2_6850_6890"
#channel<- '0. River Channel'
#path <- '/aa_HARP/aa_GitHub/HARParchive/HARP-2022-Summer/AutomatedScripts/ftables/'

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
    #varkey="om_USGSChannelGeomObject", #for local_channel it needs _sub added to end
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
#depth
cdepth <- seq(0,h,length=10) #channel
fdepth <- seq(h+1, h*4 ,length=9) #floodplain

<<<<<<< HEAD
Abf <- ((b + 2*z*depth+b)/2)*h #channel cross-sect area @ bankfull
=======
Abf <- ((b + 2*z*h+b)/2)*h #channel cross-sect area @ bankfull
>>>>>>> ffd5512e0b73acc1353620af4c3d92520d7381c6
Pbf <- b + 2*h*sqrt(z**2 +1) #channel wetted perimeter @ bankfull

#--Discharge Calculation Info:
# Q = V * A ; where A = cross sectional area (aka flow area)
# Manning's Eqn: V = (1.49/n) * R^(2/3) * S^(1/2) 
# (1.49/n) is English ; (1/n) is metric
# Hydraulic Radius = A/P , cross-sect.area/wetted perim.

fn_make_trap_ftable <- function(depth, clength, cslope, b, z, n, h, bf, fp, Abf, Pbf) { 
  sw <- b + 2*z*depth #surface width
  sw[depth == 0] <- 0
  A <- ((sw+b)/2)*depth #cross-sect. area (trapezoid)
  P <- b + 2*depth*sqrt(z**2 +1) #wetted perimeter (trapezoid)
  
  if (fp == TRUE) {
    A <- A + Abf #combine fp and channel@bankfull trapezoidal areas
    P <- P + Pbf - bf #fp + wetted perim.@bankfull - bf width of channel
    depth <- depth + h
  }
  
  area <- (sw * clength)/43560 #surface area
  vol <- (clength * A)/43560 # /43560 to convert to ft-acres
  disch <- (1.49/n) * (A/P)**(2/3) * cslope**0.5 * A
  ftable <- data.frame(depth, area, vol, disch)
  return(ftable)
}

cftab <- fn_make_trap_ftable(cdepth, clength, cslope, b, z, n, h, bf, FALSE, Abf, Pbf) #in-channel
fptab <- fn_make_trap_ftable(fdepth-h, clength, cslope, 5*bf, z, nf, h, bf, TRUE, Abf, Pbf) #floodplain 
# ^base of floodplain = 5x bankfull width

ftable <- rbind(cftab, fptab)

#----Exporting----
#format data:
DEPTH <- sprintf("%10.3f", ftable$depth) # %Space_per_num.num_of_decimal_placesf
AREA <- sprintf("%9.3f", ftable$area)
VOLUME <- sprintf("%9.2f", ftable$vol)
DISCH <- sprintf("%9.2f", ftable$disch)

ftable_formatted <- data.frame(DEPTH,AREA,VOLUME,DISCH)

#format header:
file <- paste(path, riverseg, '.ftable', sep='')
riverseg_pieces <- str_split(riverseg, "_", n = Inf, simplify = TRUE)
header1 <- paste(sprintf("%-8s %4s","FTABLE",riverseg_pieces[2])) # "-" = left-aligned text
header2 <- paste("NOTE: FLOODPLAIN BASE = 5*BANKFULL WIDTH ***")
header3 <- paste(sprintf("%5s %s","", "FLOODPLAIN SIDE-SLOPE = SAME AS CHANNEL'S ***"))
header4 <- paste(" ROWS COLS ***")
header5 <- paste("   19    4")
header6 <- paste(sprintf("%10s %9s %9s %9s %4s","DEPTH","AREA","VOLUME","DISCH","***"))
header7 <- paste(sprintf("%10s %9s %9s %9s %4s","(FT)","(ACRES)","(AC-FT)","(CFS)","***"),"\n")
cat(paste(header1,header2,header3,header4,header5,header6,header7, sep="\n"), file = file, append=FALSE)

#make table:
uci_form <- write.table(ftable_formatted,
                        file=file,
                        append = TRUE,
                        quote = FALSE,
                        dec = ".", #decimals indicated by "."
                        row.names = FALSE,
                        col.names = FALSE)
#footer
cat(paste("  END FTABLE"), file=file, append=TRUE)

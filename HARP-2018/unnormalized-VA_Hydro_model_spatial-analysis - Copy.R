# DOCUMENTATION -----------------------------------------------------------

# Creates an outputted table containing every metric from
# every model river segment, pulled from VA Hydro. It also is used to 
# create quick maps for spatial analysis of metrics associated with river segments.

# LIBRARIES -----
library(rgdal)
library(raster)
library(rgeos)
library(ggmap)
library(ggsn)
library(sp)

# INPUTS ------------------------------------------------------------------
output_location <- "C:\\Users\\Daniel\\Downloads"

site <- "http://deq2.bse.vt.edu/d.bet"    #Specify the site of interest, either d.bet OR d.dh

basepath='C:\\Users\\Daniel\\Documents\\HARP\\GitHub\\hydro-tools';

# SETUP

source(paste(basepath,'config.local.private',sep='/'));
source(paste(hydro_tools,"VAHydro-2.0/rest_functions.R", sep = "/")); 
source(paste(hydro_tools,"VAHydro-1.0/fn_vahydro-1.0.R", sep = "/"));  
source(paste(hydro_tools,"LowFlow/fn_iha.R", sep = "/"));
#retrieve rest token
source(paste(hydro_tools,"auth.private", sep = "/"));#load rest username and password, contained in auth.private file
token <- rest_token(site, token, rest_uname, rest_pw);
options(timeout=120); # set timeout to twice default level to avoid abort due to high traffic



# CREATING LIST OF RIVER SEGS ---------------------------------------------------

all.riv.segs <- c('OR5_7980_8200', 'OR2_8020_8130', 'OR2_8070_8120', 'OR4_8120_7890',
                  'OR2_8130_7900', 'OR5_8200_8370', 'OR4_8271_8120', 'TU3_8480_8680',
                  'TU1_8570_8680', 'TU3_8650_8800', 'TU4_8680_8810', 'TU2_8790_9070',
                  'TU4_8800_9290', 'TU4_8810_9000', 'BS4_8540_8441', 'BS3_8580_8440',
                  'BS2_8590_8440', 'BS1_8730_8540', 'MN2_8250_8190', 'MN4_8260_8400',
                  'MN0_8300_0001', 'MN4_8400_8380', 'MN4_8510_8380', 'MN2_8530_8510',
                  'NR6_7820_7960', 'NR1_7880_8050', 'BS3_8350_8330', 'BS4_8440_8441',
                  'MN3_7540_7680', 'MN1_7590_7860', 'MN1_7620_7710', 'MN3_7680_7860',
                  'MN4_7710_8161', 'MN2_7720_7830', 'MN1_7730_8160', 'MN3_7770_7930',
                  'MN4_7810_8080', 'MN2_7830_7950', 'MN3_7860_8080', 'MN3_7930_8010',
                  'MN4_7950_7710', 'MN1_7990_8100', 'MN3_8010_7950', 'MN4_8080_8110',
                  'MN2_8100_8190', 'MN5_8161_8160', 'MN3_8190_8260', 'MN5_8230_8161',
                  'NR6_7960_8050', 'NR1_8030_8051', 'NR6_8050_8051', 'NR6_8051_8000',
                  'NR6_8170_7960', 'NR6_8180_8051', 'NR2_8210_8180', 'NR3_8290_8170',
                  'NR3_8420_8430', 'NR3_8430_7820', 'NR6_8640_8500', 'NR3_8690_8500',
                  'NR5_8700_8640', 'NR3_8740_8500', 'NR5_8760_8640', 'NR1_8820_8760',
                  'NR5_8870_8760', 'NR1_8960_8870', 'NR1_9030_9080', 'NR5_9050_8870',
                  'NR5_9080_9050', 'NR4_9130_9080', 'NR1_9150_9050', 'NR3_9170_9130',
                  'NR3_9190_9170', 'NR3_9240_9130', 'NR2_9250_9170', 'NR3_9310_9240',
                  'OD3_8340_8520', 'OD3_8520_8621', 'OD2_8560_8630', 'OD6_8621_8470',
                  'OD3_8630_8720', 'OD6_8660_8621', 'OD2_8670_8890', 'OD3_8710_8470',
                  'OD3_8720_8900', 'OD5_8770_8780', 'OD5_8780_8660', 'OD2_8830_8710',
                  'OD2_8840_9020', 'OD3_8850_8931', 'OD5_8890_8770', 'OD5_8900_8770',
                  'OD1_8910_8930', 'OD2_8920_8830', 'OD3_8930_8931', 'OD3_8931_9140',
                  'OD5_8940_8780', 'OD4_8990_8900', 'OD3_9020_9110', 'OD4_9110_9140',
                  'OD4_9140_8990', 'OD1_9270_9110', 'OR2_7610_7780', 'OR2_7650_8070',
                  'OR2_7670_7840', 'OR1_7700_7980', 'OR3_7740_8271', 'OR2_7780_7890',
                  'OR2_7840_7970', 'OR5_7890_7970', 'OR2_7900_7740', 'OR5_7910_8410',
                  'OR5_7970_8200', 'OR1_8280_8020', 'OR1_8320_8271', 'OR5_8370_8410',
                  'OR5_8410_8470', 'OR2_8450_8490', 'OR2_8460_8271', 'OR7_8470_8490',
                  'TU2_8860_9000', 'TU3_8880_9230', 'TU2_8950_9040', 'TU2_8970_9280',
                  'TU5_9000_9280', 'TU1_9010_9290', 'TU3_9040_9180', 'TU3_9060_9230',
                  'TU2_9070_9090', 'TU2_9100_9200', 'TU3_9180_9090', 'TU2_9200_9180',
                  'TU1_9220_9200', 'TU3_9230_9260', 'NR2_8600_8700', 'NR6_8500_7820')

# COUNTS NUMBER OF RIVER SEGMENTS ------------------------------------------------
num.segs <- as.numeric(length(all.riv.segs))
seg.names <- all.riv.segs

# CALCULATES TOTAL NUMBER OF METRICS IN ALL METRICS ------------------------------
metrics.names <- c('Overall Mean Flow', 'Jan. Low Flow', 'Feb. Low Flow',
                   'Mar. Low Flow', 'Apr. Low Flow', 'May Low Flow',
                   'June Low Flow', 'July Low Flow', 'Aug. Low Flow',
                   'Sep. Low Flow', 'Oct. Low Flow', 'Nov. Low Flow',
                   'Dec. Low Flow', 'Jan. Mean Flow', 'Feb. Mean Flow',
                   'Mar. Mean Flow', 'Apr. Mean Flow', 'May Mean Flow',
                   'June Mean Flow', 'July Mean Flow', 'Aug. Mean Flow',
                   'Sep. Mean Flow', 'Oct. Mean Flow', 'Nov. Mean Flow',
                   'Dec. Mean Flow', 'Min. 1-Day Low Flow', 'Med. 1-Day Low Flow',
                   'Min. 3-Day Low Flow', 'Med. 3-Day Low Flow', 'Min. 7-Day Low Flow',
                   'Med. 7-Day Low Flow', 'Min. 30-Day Low Flow', 'Med. 30-Day Low Flow',
                   'Min. 90-Day Low Flow', 'Med. 90-Day Low Flow', '7q10',
                   '1pct Non-Exceedance', '5pct Non-Exceedance',
                   '50pct Non-Exceedance', '95pct Non-Exceedance', '99pct Non-Exceedance',
                   'Sep. 10pct Non-Exceedance', 'Mean Baseflow', 'Jan. High Flow',
                   'Feb. High Flow', 'Mar. High Flow', 'Apr. High Flow',
                   'May High Flow', 'June High Flow', 'July High Flow',
                   'Aug. High Flow', 'Sep. High Flow', 'Oct. High Flow',
                   'Nov. High Flow', 'Dec. High Flow', 'Max. 1-Day High Flow',
                   'Med. 1-Day High Flow', 'Max. 3-Day High Flow', 'Med. 3-Day High Flow',
                   'Max. 7-Day High Flow', 'Med. 7-Day High Flow', 'Max. 30-Day High Flow',
                   'Med. 30-Day High Flow', 'Max. 90-Day High Flow', 'Med. 90-Day High Flow',
                   "Drought Year Mean", "Contrib. Drainage Area")
num.metrics <- length(metrics.names)

# CREATES EMPTY DATA FRAME WITH DIMENSIONS OF ALL METRICS BY NUM.SEGS ------------
all.errors.all.segments <- data.frame(matrix(NA, nrow = num.segs, ncol = num.metrics))
colnames(all.errors.all.segments) <- metrics.names
rownames(all.errors.all.segments) <- seg.names

# POPULATES DATA FRAME WITH ALL_METRICS PERCENT ERROR DATA -----------------------
all.errors.line.no <- 1

for (i in 1:num.segs) {
  RivSeg <- all.riv.segs[i]
  
  # GETTING MODEL DATA FROM VA HYDRO
  hydrocode = paste("vahydrosw_wshed_",RivSeg,sep="");
  ftype = 'vahydro'; # nhd_huc8, nhd_huc10, vahydro
  inputs <- list (
    hydrocode = hydrocode,
    bundle = 'watershed',
    ftype = 'vahydro'
  )
  #property dataframe returned
  feature = FALSE;
  odata <- getFeature(inputs, token, site, feature);
  hydroid <- odata[1,"hydroid"];
  fname <- as.character(odata[1,]$name );
  print(paste("Retrieved hydroid",hydroid,"for", fname,RivSeg, sep=' '));
  # get the p5.3.2, scenario  model segment attached to this river feature
  inputs <- list(
    varkey = "om_model_element",
    featureid = hydroid,
    entity_type = "dh_feature",
    propcode = "p532cal_062211"
  )
  model <- getProperty(inputs, site, model)
  all.metrics <- data.frame(matrix(NA, nrow = 1, ncol = num.metrics))
  # Getting the contributing drainage area feature
  areainfo <- list(
    varkey = "wshed_drainage_area_sqmi",
    featureid = as.integer(as.character(hydroid)),
    entity_type = "dh_feature"
  )
  contrib.drain.area <- getProperty(areainfo, site, contrib.drain.area)
  all.metrics[1,num.metrics] <- contrib.drain.area$propvalue
  all.metrics[1,num.metrics] <- all.metrics[1,num.metrics]*5280*5280 #Converting from sq. miles to sq.feet
  
  # GETTING MODEL METRICS FROM VA HYDRO
  
  # 00: Overall Mean Flow
  alfinfo <- list(
    varkey = "overall_mean",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,1] <- alfprop$propvalue

  # 01: January Low Flow
  alfinfo <- list(
    varkey = "monthly_low_flow",
    propcode = "ml1",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,2] <- alfprop$propvalue
  
  # 02: February Low Flow
  alfinfo <- list(
    varkey = "monthly_low_flow",
    propcode = "ml2",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,3] <- alfprop$propvalue
  
  # 03: March Low Flow
  alfinfo <- list(
    varkey = "monthly_low_flow",
    propcode = "ml3",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,4] <- alfprop$propvalue
  
  # 04: April Low Flow
  alfinfo <- list(
    varkey = "monthly_low_flow",
    propcode = "ml4",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,5] <- alfprop$propvalue
  
  # 05: May Low Flow
  alfinfo <- list(
    varkey = "monthly_low_flow",
    propcode = "ml5",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,6] <- alfprop$propvalue
  
  # 06: June Low Flow
  alfinfo <- list(
    varkey = "monthly_low_flow",
    propcode = "ml6",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,7] <- alfprop$propvalue
  
  # 07: July Low Flow
  alfinfo <- list(
    varkey = "monthly_low_flow",
    propcode = "ml7",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,8] <- alfprop$propvalue
  
  # 08: August Low Flow
  alfinfo <- list(
    varkey = "monthly_low_flow",
    propcode = "ml8",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,9] <- alfprop$propvalue
  
  # 09: September Low Flow
  alfinfo <- list(
    varkey = "monthly_low_flow",
    propcode = "ml9",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,10] <- alfprop$propvalue
  
  # 10: October Low Flow
  alfinfo <- list(
    varkey = "monthly_low_flow",
    propcode = "ml10",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,11] <- alfprop$propvalue
  
  # 11: November Low Flow
  alfinfo <- list(
    varkey = "monthly_low_flow",
    propcode = "ml11",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,12] <- alfprop$propvalue
  
  # 12: December Low Flow
  alfinfo <- list(
    varkey = "monthly_low_flow",
    propcode = "ml12",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,13] <- alfprop$propvalue
  
  # 13: January Mean Flow
  alfinfo <- list(
    varkey = "monthly_mean_flow",
    propcode = "mm1",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,14] <- alfprop$propvalue
  
  # 14: February Mean Flow
  alfinfo <- list(
    varkey = "monthly_mean_flow",
    propcode = "mm2",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,15] <- alfprop$propvalue
  
  # 15: March Mean Flow
  alfinfo <- list(
    varkey = "monthly_mean_flow",
    propcode = "mm3",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,16] <- alfprop$propvalue
  
  # 16: April Mean Flow
  alfinfo <- list(
    varkey = "monthly_mean_flow",
    propcode = "mm4",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,17] <- alfprop$propvalue
  
  # 17: May Mean Flow
  alfinfo <- list(
    varkey = "monthly_mean_flow",
    propcode = "mm5",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,18] <- alfprop$propvalue
  
  # 18: June Mean Flow
  alfinfo <- list(
    varkey = "monthly_mean_flow",
    propcode = "mm6",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,19] <- alfprop$propvalue
  
  # 19: July Mean Flow
  alfinfo <- list(
    varkey = "monthly_mean_flow",
    propcode = "mm7",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,20] <- alfprop$propvalue
  
  # 20: August Mean Flow
  alfinfo <- list(
    varkey = "monthly_mean_flow",
    propcode = "mm8",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,21] <- alfprop$propvalue
  
  # 21: September Mean Flow
  alfinfo <- list(
    varkey = "monthly_mean_flow",
    propcode = "mm9",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,22] <- alfprop$propvalue
  
  # 22: October Mean Flow
  alfinfo <- list(
    varkey = "monthly_mean_flow",
    propcode = "mm10",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,23] <- alfprop$propvalue
  
  # 23: November Mean Flow
  alfinfo <- list(
    varkey = "monthly_mean_flow",
    propcode = "mm11",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,24] <- alfprop$propvalue
  
  # 24: December Mean Flow
  alfinfo <- list(
    varkey = "monthly_mean_flow",
    propcode = "mm12",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,25] <- alfprop$propvalue
  
  # 25: 1 Day Minimum Low Flow
  alfinfo <- list(
    varkey = "min_low_flow",
    propcode = "min1",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,26] <- alfprop$propvalue
  
  # 26: 1 Day Median Low Flow
  alfinfo <- list(
    varkey = "med_low_flow",
    propcode = "medl1",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,27] <- alfprop$propvalue
  
  # 27: 3 Day Minimum Low Flow
  alfinfo <- list(
    varkey = "min_low_flow",
    propcode = "min3",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,28] <- alfprop$propvalue
  
  # 28: 3 Day Median Low Flow
  alfinfo <- list(
    varkey = "med_low_flow",
    propcode = "medl3",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,29] <- alfprop$propvalue
  
  # 29: 7 Day Minimum Low Flow
  alfinfo <- list(
    varkey = "min_low_flow",
    propcode = "min7",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,30] <- alfprop$propvalue
  
  # 30: 7 Day Median Low Flow
  alfinfo <- list(
    varkey = "med_low_flow",
    propcode = "medl7",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,31] <- alfprop$propvalue
  
  # 31: 30 Day Minimum Low Flow
  alfinfo <- list(
    varkey = "min_low_flow",
    propcode = "min30",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,32] <- alfprop$propvalue
  
  # 32: 30 Day Median Low Flow
  alfinfo <- list(
    varkey = "med_low_flow",
    propcode = "medl30",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,33] <- alfprop$propvalue
  
  # 33: 90 Day Minimum Low Flow
  alfinfo <- list(
    varkey = "min_low_flow",
    propcode = "min90",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,34] <- alfprop$propvalue
  
  # 34: 90 Day Median Low Flow
  alfinfo <- list(
    varkey = "med_low_flow",
    propcode = "medl90",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,35] <- alfprop$propvalue
  
  # 35: 7Q10
  alfinfo <- list(
    varkey = "7q10",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,36] <- alfprop$propvalue
  
  # 38: 1% Non-Exceedance
  alfinfo <- list(
    varkey = "non-exceedance",
    propcode = "ne1",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,37] <- alfprop$propvalue
  
  # 39: 5% Non-Exceedance
  alfinfo <- list(
    varkey = "non-exceedance",
    propcode = "ne5",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,38] <- alfprop$propvalue
  
  # 40: 50% Non-Exceedance
  alfinfo <- list(
    varkey = "non-exceedance",
    propcode = "ne50",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,39] <- alfprop$propvalue
  
  # 41: 95% Non-Exceedance
  alfinfo <- list(
    varkey = "non-exceedance",
    propcode = "ne95",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,40] <- alfprop$propvalue
  
  # 42: 99% Non-Exceedance
  alfinfo <- list(
    varkey = "non-exceedance",
    propcode = "ne99",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,41] <- alfprop$propvalue
  
  # 43: September 10%
  alfinfo <- list(
    varkey = "monthly_non-exceedance",
    propcode = "mne9_10",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,42] <- alfprop$propvalue
  
  # 44: Mean Baseflow
  alfinfo <- list(
    varkey = "baseflow",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,43] <- alfprop$propvalue
  
  # 45: January High Flow
  alfinfo <- list(
    varkey = "monthly_high_flow",
    propcode = "mh1",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,44] <- alfprop$propvalue
  
  # 46: February High Flow
  alfinfo <- list(
    varkey = "monthly_high_flow",
    propcode = "mh2",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,45] <- alfprop$propvalue
  
  # 47: March High Flow
  alfinfo <- list(
    varkey = "monthly_high_flow",
    propcode = "mh3",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,46] <- alfprop$propvalue
  
  # 48: April High Flow
  alfinfo <- list(
    varkey = "monthly_high_flow",
    propcode = "mh4",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,47] <- alfprop$propvalue

  # 49: May High Flow
  alfinfo <- list(
    varkey = "monthly_high_flow",
    propcode = "mh5",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,48] <- alfprop$propvalue
  
  # 50: June High Flow
  alfinfo <- list(
    varkey = "monthly_high_flow",
    propcode = "mh6",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,49] <- alfprop$propvalue
  
  # 51: July High Flow
  alfinfo <- list(
    varkey = "monthly_high_flow",
    propcode = "mh7",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,50] <- alfprop$propvalue
  
  # 52: August High Flow
  alfinfo <- list(
    varkey = "monthly_high_flow",
    propcode = "mh8",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,51] <- alfprop$propvalue
  
  # 53: September High Flow
  alfinfo <- list(
    varkey = "monthly_high_flow",
    propcode = "mh9",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,52] <- alfprop$propvalue
  
  # 54: October High Flow
  alfinfo <- list(
    varkey = "monthly_high_flow",
    propcode = "mh10",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,53] <- alfprop$propvalue
  
  # 55: November High Flow
  alfinfo <- list(
    varkey = "monthly_high_flow",
    propcode = "mh11",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,54] <- alfprop$propvalue
  
  # 56: December High Flow
  alfinfo <- list(
    varkey = "monthly_high_flow",
    propcode = "mh12",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,55] <- alfprop$propvalue
  
  # 57: 1 Day Maximum High Flow
  alfinfo <- list(
    varkey = "max_high_flow",
    propcode = "max1",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,56] <- alfprop$propvalue
  
  # 58: 1 Day Median High Flow
  alfinfo <- list(
    varkey = "med_high_flow",
    propcode = "medh1",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,57] <- alfprop$propvalue
  
  # 59: 3 Day Maximum High Flow
  alfinfo <- list(
    varkey = "max_high_flow",
    propcode = "max3",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,58] <- alfprop$propvalue
  
  # 60: 3 Day Median High Flow
  alfinfo <- list(
    varkey = "med_high_flow",
    propcode = "medh3",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,59] <- alfprop$propvalue
  
  # 61: 7 Day Maximum High Flow
  alfinfo <- list(
    varkey = "max_high_flow",
    propcode = "max7",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,60] <- alfprop$propvalue
  
  # 62: 7 Day Median High Flow
  alfinfo <- list(
    varkey = "med_high_flow",
    propcode = "medh7",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,61] <- alfprop$propvalue
  
  # 63: 30 Day Maximum High Flow
  alfinfo <- list(
    varkey = "max_high_flow",
    propcode = "max30",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,62] <- alfprop$propvalue
  
  # 64: 30 Day Maximum High Flow
  alfinfo <- list(
    varkey = "med_high_flow",
    propcode = "medh30",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,63] <- alfprop$propvalue
  
  # 65: 90 Day Maximum High Flow
  alfinfo <- list(
    varkey = "max_high_flow",
    propcode = "max90",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,64] <- alfprop$propvalue
  
  # 66: 90 Day Median High Flow
  alfinfo <- list(
    varkey = "med_high_flow",
    propcode = "medh90",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,65] <- alfprop$propvalue
  
  # 67: Mean Flow in Year of Drought of Record
  alfinfo <- list(
    varkey = "dor_mean",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,66] <- alfprop$propvalue
  
  all.errors.all.segments[all.errors.line.no,] <- all.metrics[1,1:num.metrics]
  all.errors.line.no <- all.errors.line.no + 1
}

Metrics <- all.errors.all.segments[,-num.metrics]

#--------------------------------------------------------------------------------------------
#LOAD STATE GEOMETRY
#--------------------------------------------------------------------------------------------
STATES <- read.table(file=paste(hydro_tools,"GIS_LAYERS","STATES.tsv",sep="\\"), header=TRUE, sep="\t") #Load state geometries

#specify spatial extent for map  
extent <- data.frame(x = c(-84, -75), 
                     y = c(35, 41))  


bb=readWKT(paste0("POLYGON((",extent$x[1]," ",extent$y[1],",",extent$x[2]," ",extent$y[1],",",extent$x[2]," ",extent$y[2],",",extent$x[1]," ",extent$y[2],",",extent$x[1]," ",extent$y[1],"))",sep=""))
bbProjected <- SpatialPolygonsDataFrame(bb,data.frame("id"), match.ID = FALSE)
bbProjected@data$id <- rownames(bbProjected@data)
bbPoints <- fortify(bbProjected, region = "id")
bbDF <- merge(bbPoints, bbProjected@data, by = "id")

VA <- STATES[which(STATES$state == "VA"),]
VA_geom <- readWKT(VA$geom)
VA_geom_clip <- gIntersection(bb, VA_geom)
VAProjected <- SpatialPolygonsDataFrame(VA_geom_clip,data.frame("id"), match.ID = TRUE)
VAProjected@data$id <- rownames(VAProjected@data)
VAPoints <- fortify( VAProjected, region = "id")
VADF <- merge(VAPoints,  VAProjected@data, by = "id")

TN <- STATES[which(STATES$state == "TN"),]
TN_geom <- readWKT(TN$geom)
TN_geom_clip <- gIntersection(bb, TN_geom)
TNProjected <- SpatialPolygonsDataFrame(TN_geom_clip,data.frame("id"), match.ID = TRUE)
TNProjected@data$id <- rownames(TNProjected@data)
TNPoints <- fortify( TNProjected, region = "id")
TNDF <- merge(TNPoints,  TNProjected@data, by = "id")

NC <- STATES[which(STATES$state == "NC"),]
NC_geom <- readWKT(NC$geom)
NC_geom_clip <- gIntersection(bb, NC_geom)
NCProjected <- SpatialPolygonsDataFrame(NC_geom_clip,data.frame("id"), match.ID = TRUE)
NCProjected@data$id <- rownames(NCProjected@data)
NCPoints <- fortify( NCProjected, region = "id")
NCDF <- merge(NCPoints,  NCProjected@data, by = "id")

KY <- STATES[which(STATES$state == "KY"),]
KY_geom <- readWKT(KY$geom)
KY_geom_clip <- gIntersection(bb, KY_geom)
KYProjected <- SpatialPolygonsDataFrame(KY_geom_clip,data.frame("id"), match.ID = TRUE)
KYProjected@data$id <- rownames(KYProjected@data)
KYPoints <- fortify( KYProjected, region = "id")
KYDF <- merge(KYPoints,  KYProjected@data, by = "id")

WV <- STATES[which(STATES$state == "WV"),]
WV_geom <- readWKT(WV$geom)
WV_geom_clip <- gIntersection(bb, WV_geom)
WVProjected <- SpatialPolygonsDataFrame(WV_geom_clip,data.frame("id"), match.ID = TRUE)
WVProjected@data$id <- rownames(WVProjected@data)
WVPoints <- fortify( WVProjected, region = "id")
WVDF <- merge(WVPoints,  WVProjected@data, by = "id")

MD <- STATES[which(STATES$state == "MD"),]
MD_geom <- readWKT(MD$geom)
MD_geom_clip <- gIntersection(bb, MD_geom)
MDProjected <- SpatialPolygonsDataFrame(MD_geom_clip,data.frame("id"), match.ID = TRUE)
MDProjected@data$id <- rownames(MDProjected@data)
MDPoints <- fortify( MDProjected, region = "id")
MDDF <- merge(MDPoints,  MDProjected@data, by = "id")

DE <- STATES[which(STATES$state == "DE"),]
DE_geom <- readWKT(DE$geom)
DE_geom_clip <- gIntersection(bb, DE_geom)
DEProjected <- SpatialPolygonsDataFrame(DE_geom_clip,data.frame("id"), match.ID = TRUE)
DEProjected@data$id <- rownames(DEProjected@data)
DEPoints <- fortify( DEProjected, region = "id")
DEDF <- merge(DEPoints,  DEProjected@data, by = "id")

PA <- STATES[which(STATES$state == "PA"),]
PA_geom <- readWKT(PA$geom)
PA_geom_clip <- gIntersection(bb, PA_geom)
PAProjected <- SpatialPolygonsDataFrame(PA_geom_clip,data.frame("id"), match.ID = TRUE)
PAProjected@data$id <- rownames(PAProjected@data)
PAPoints <- fortify( PAProjected, region = "id")
PADF <- merge(PAPoints,  PAProjected@data, by = "id")

NJ <- STATES[which(STATES$state == "NJ"),]
NJ_geom <- readWKT(NJ$geom)
NJ_geom_clip <- gIntersection(bb, NJ_geom)
NJProjected <- SpatialPolygonsDataFrame(NJ_geom_clip,data.frame("id"), match.ID = TRUE)
NJProjected@data$id <- rownames(NJProjected@data)
NJPoints <- fortify( NJProjected, region = "id")
NJDF <- merge(NJPoints,  NJProjected@data, by = "id")

OH <- STATES[which(STATES$state == "OH"),]
OH_geom <- readWKT(OH$geom)
OH_geom_clip <- gIntersection(bb, OH_geom)
OHProjected <- SpatialPolygonsDataFrame(OH_geom_clip,data.frame("id"), match.ID = TRUE)
OHProjected@data$id <- rownames(OHProjected@data)
OHPoints <- fortify( OHProjected, region = "id")
OHDF <- merge(OHPoints,  OHProjected@data, by = "id")

SC <- STATES[which(STATES$state == "SC"),]
SC_geom <- readWKT(SC$geom)
SC_geom_clip <- gIntersection(bb, SC_geom)
SCProjected <- SpatialPolygonsDataFrame(SC_geom_clip,data.frame("id"), match.ID = TRUE)
SCProjected@data$id <- rownames(SCProjected@data)
SCPoints <- fortify( SCProjected, region = "id")
SCDF <- merge(SCPoints,  SCProjected@data, by = "id")

DC <- STATES[which(STATES$state == "DC"),]
DC_geom <- readWKT(DC$geom)
DC_geom_clip <- gIntersection(bb, DC_geom)
DCProjected <- SpatialPolygonsDataFrame(DC_geom_clip,data.frame("id"), match.ID = TRUE)
DCProjected@data$id <- rownames(DCProjected@data)
DCPoints <- fortify( DCProjected, region = "id")
DCDF <- merge(DCPoints,  DCProjected@data, by = "id")

for (i in 1:num.segs) {
  RivSeg <- all.riv.segs[i]
  namer <- paste0("watershedDF", i)
  
  # GETTING MODEL DATA FROM VA HYDRO
  hydrocode = paste("vahydrosw_wshed_",RivSeg,sep="");
  ftype = 'vahydro'; # nhd_huc8, nhd_huc10, vahydro
  inputs <- list (
    hydrocode = hydrocode,
    bundle = 'watershed',
    ftype = 'vahydro'
  )
  odata <- getFeature(inputs, token, site, feature);
  geom <- odata$geom
  
  # CLIP WATERSHED GEOMETRY TO BOUNDING BOX
  watershed_geom <- readWKT(geom)
  watershed_geom_clip <- gIntersection(bb, watershed_geom)
  if (is.null(watershed_geom_clip)) {
    watershed_geom_clip = watershed_geom
  }
  wsdataProjected <- SpatialPolygonsDataFrame(watershed_geom_clip,data.frame("id"), match.ID = FALSE)
  wsdataProjected@data$id <- rownames(wsdataProjected@data)
  watershedPoints <- fortify(wsdataProjected, region = "id")
  watershedDF <- merge(watershedPoints, wsdataProjected@data, by = "id")
  assign(namer, watershedDF)
}

dir.create(paste0(output_location,"\\SouthernRivers_p532_SpatialAnalysis-unnormalized"), showWarnings = FALSE);
new.output_location <- paste0(output_location, "\\SouthernRivers_p532_SpatialAnalysis-unnormalized")

# Initiating counter
ctr <- 1

# Determining metric column names
metric.names <- metrics.names
num.metrics <- length(metrics.names)-1
cols <- rainbow(6)

for (ctr in 1:num.metrics) {
  
  DesiredMetric<- metric.names[ctr]
  
  Metrics[,paste0("DesiredMetric")] <- Metrics[DesiredMetric]
  
  # GRAPHING -----

  map <- ggplot(data = VADF, aes(x=long, y=lat, group = group))+
    geom_polygon(data = bbDF, color="black", fill = "powderblue",lwd=0.5)+
    geom_polygon(data = VADF, color="gray46", fill = "gray")+
    geom_polygon(data = TNDF, color="gray46", fill = "gray", lwd=0.5)+
    geom_polygon(data = NCDF, color="gray46", fill = "gray", lwd=0.5)+
    geom_polygon(data = SCDF, color="gray46", fill = "gray", lwd=0.5)+
    geom_polygon(data = KYDF, color="gray46", fill = "gray", lwd=0.5)+
    geom_polygon(data = WVDF, color="gray46", fill = "gray", lwd=0.5)+
    geom_polygon(data = MDDF, color="gray46", fill = "gray", lwd=0.5)+
    geom_polygon(data = DEDF, color="gray46", fill = "gray", lwd=0.5)+
    geom_polygon(data = PADF, color="gray46", fill = "gray", lwd=0.5)+
    geom_polygon(data = NJDF, color="gray46", fill = "gray", lwd=0.5)+
    geom_polygon(data = OHDF, color="gray46", fill = "gray", lwd=0.5)+
    geom_polygon(data = DCDF, color="gray46", fill = "gray", lwd=0.5)
  for (i in 1:num.segs) {
      namer <- paste0('watershedDF', i)
      if (Metrics$DesiredMetric[i] < 25) {
        map <- map + geom_polygon(data = eval(parse(text = namer)), color='black', aes(fill='a'), lwd = 0.1)
      } else if (Metrics$DesiredMetric[i] < 100) {
        map <- map + geom_polygon(data = eval(parse(text = namer)), color='black', aes(fill='b'), lwd = 0.1)
      } else if (Metrics$DesiredMetric[i] < 500) {
        map <- map + geom_polygon(data = eval(parse(text = namer)), color='black', aes(fill='c'), lwd = 0.1)
      } else if (Metrics$DesiredMetric[i] < 1000) {
        map <- map + geom_polygon(data = eval(parse(text = namer)), color='black', aes(fill='d'), lwd = 0.1)
      } else if (Metrics$DesiredMetric[i] < 5000) {
        map <- map + geom_polygon(data = eval(parse(text = namer)), color='black', aes(fill='e'), lwd = 0.1)
      } else {
        map <- map + geom_polygon(data = eval(parse(text = namer)), color='black', aes(fill='f'), lwd = 0.1)
      }
  }
#Add legend, add title, ggsave
  map <- map + ggtitle(DesiredMetric)
  map <- map + scale_fill_manual(breaks = c('a', 'b', 'c', 'd', 'e', 'f'), limits=c('a', 'b', 'c', 'd', 'e', 'f'), labels=c("Less than 25", "25 to 100", "100 to 500", "500 to 1000", "1000 to 5000", "Greater than 5000"), values=cols, name = "Flow (cfs)")
  #ADD NORTH ARROW AND SCALE BAR
  map<-map+north(bbDF, location = 'topleft', symbol = 12, scale=0.1)+
    scalebar(bbDF, location = 'bottomleft', dist = 100, dd2km = TRUE, model = 'WGS84',st.bottom=FALSE, st.size = 3.5,
             anchor = c(
               x = (((extent$x[2] - extent$x[1])/2)+extent$x[1])-1.1,
               y = extent$y[1]+(extent$y[1])*0.001
             ))+
    scale_x_continuous(limits = c(extent$x))+
    scale_y_continuous(limits = c(extent$y))+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          #axis.text =element_text(size=rel(2)),    #Uncomment to display lat/long on plot
          #axis.title = element_text(size=rel(2)),  #Uncomment to display lat/long on plot
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          legend.text = element_text(size=rel(1)),
          legend.title = element_text(size= rel(1.2)))
  
  # Incrementing counter
  ctr <- ctr + 1
  ggsave(filename = paste0(DesiredMetric, ".jpeg"), device = "jpeg", path = new.output_location, width = 7, height = 4.5)
}

# DOCUMENTATION -----------------------------------------------------------

# Creates an outputted table containing every metric from
# every model river segment, pulled from VA Hydro. It also is used to 
# create quick maps for spatial analysis of metrics associated with river segments.

# LIBRARIES -----
library(rgdal)
library(raster)

# INPUTS ------------------------------------------------------------------

# Address of "DEQ_Model_vs_USGS_Comparison" folder
# Include "DEQ_Model_vs_USGS_Comparison" in address!
container <- "C:\\Users\\Daniel\\Documents\\HARP\\DEQ_Model_ONLY_v1.0"

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
                   'Year of 90-Day Low Flow', '1pct Non-Exceedance', '5pct Non-Exceedance',
                   '50pct Non-Exceedance', '95pct Non-Exceedance', '99pct Non-Exceedance',
                   'Sep. 10pct Non-Exceedance', 'Mean Baseflow', 'Jan. High Flow',
                   'Feb. High Flow', 'Mar. High Flow', 'Apr. High Flow',
                   'May High Flow', 'June High Flow', 'July High Flow',
                   'Aug. High Flow', 'Sep. High Flow', 'Oct. High Flow',
                   'Nov. High Flow', 'Dec. High Flow', 'Max. 1-Day High Flow',
                   'Med. 1-Day High Flow', 'Max. 3-Day High Flow', 'Med. 3-Day High Flow',
                   'Max. 7-Day High Flow', 'Med. 7-Day High Flow', 'Max. 30-Day High Flow',
                   'Med. 30-Day High Flow', 'Max. 90-Day High Flow', 'Med. 90-Day High Flow',
                   "Drought Year Mean")
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
  
  # 37: Drought of Record Year
  alfinfo <- list(
    varkey = "dor_year",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,37] <- alfprop$propvalue
  
  # 38: 1% Non-Exceedance
  alfinfo <- list(
    varkey = "non-exceedance",
    propcode = "ne1",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,38] <- alfprop$propvalue
  
  # 39: 5% Non-Exceedance
  alfinfo <- list(
    varkey = "non-exceedance",
    propcode = "ne5",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,39] <- alfprop$propvalue
  
  # 40: 50% Non-Exceedance
  alfinfo <- list(
    varkey = "non-exceedance",
    propcode = "ne50",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,40] <- alfprop$propvalue
  
  # 41: 95% Non-Exceedance
  alfinfo <- list(
    varkey = "non-exceedance",
    propcode = "ne95",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,41] <- alfprop$propvalue
  
  # 42: 99% Non-Exceedance
  alfinfo <- list(
    varkey = "non-exceedance",
    propcode = "ne99",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,42] <- alfprop$propvalue
  
  # 43: September 10%
  alfinfo <- list(
    varkey = "monthly_non-exceedance",
    propcode = "mne9_10",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,43] <- alfprop$propvalue
  
  # 44: Mean Baseflow
  alfinfo <- list(
    varkey = "baseflow",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,44] <- alfprop$propvalue
  
  # 45: January High Flow
  alfinfo <- list(
    varkey = "monthly_high_flow",
    propcode = "mh1",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,45] <- alfprop$propvalue
  
  # 46: February High Flow
  alfinfo <- list(
    varkey = "monthly_high_flow",
    propcode = "mh2",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,46] <- alfprop$propvalue
  
  # 47: March High Flow
  alfinfo <- list(
    varkey = "monthly_high_flow",
    propcode = "mh3",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,47] <- alfprop$propvalue
  
  # 48: April High Flow
  alfinfo <- list(
    varkey = "monthly_high_flow",
    propcode = "mh4",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,48] <- alfprop$propvalue
  
  # 49: May High Flow
  alfinfo <- list(
    varkey = "monthly_high_flow",
    propcode = "mh5",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,49] <- alfprop$propvalue
  
  # 50: June High Flow
  alfinfo <- list(
    varkey = "monthly_high_flow",
    propcode = "mh6",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,50] <- alfprop$propvalue
  
  # 51: July High Flow
  alfinfo <- list(
    varkey = "monthly_high_flow",
    propcode = "mh7",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,51] <- alfprop$propvalue
  
  # 52: August High Flow
  alfinfo <- list(
    varkey = "monthly_high_flow",
    propcode = "mh8",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,52] <- alfprop$propvalue
  
  # 53: September High Flow
  alfinfo <- list(
    varkey = "monthly_high_flow",
    propcode = "mh9",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,53] <- alfprop$propvalue
  
  # 54: October High Flow
  alfinfo <- list(
    varkey = "monthly_high_flow",
    propcode = "mh10",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,54] <- alfprop$propvalue
  
  # 55: November High Flow
  alfinfo <- list(
    varkey = "monthly_high_flow",
    propcode = "mh11",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,55] <- alfprop$propvalue
  
  # 56: December High Flow
  alfinfo <- list(
    varkey = "monthly_high_flow",
    propcode = "mh12",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,56] <- alfprop$propvalue
  
  # 57: 1 Day Maximum High Flow
  alfinfo <- list(
    varkey = "max_high_flow",
    propcode = "max1",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,57] <- alfprop$propvalue
  
  # 58: 1 Day Median High Flow
  alfinfo <- list(
    varkey = "med_high_flow",
    propcode = "medh1",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,58] <- alfprop$propvalue
  
  # 59: 3 Day Maximum High Flow
  alfinfo <- list(
    varkey = "max_high_flow",
    propcode = "max3",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,59] <- alfprop$propvalue
  
  # 60: 3 Day Median High Flow
  alfinfo <- list(
    varkey = "med_high_flow",
    propcode = "medh3",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,60] <- alfprop$propvalue
  
  # 61: 7 Day Maximum High Flow
  alfinfo <- list(
    varkey = "max_high_flow",
    propcode = "max7",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,61] <- alfprop$propvalue
  
  # 62: 7 Day Median High Flow
  alfinfo <- list(
    varkey = "med_high_flow",
    propcode = "medh7",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,62] <- alfprop$propvalue
  
  # 63: 30 Day Maximum High Flow
  alfinfo <- list(
    varkey = "max_high_flow",
    propcode = "max30",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,63] <- alfprop$propvalue
  
  # 64: 30 Day Maximum High Flow
  alfinfo <- list(
    varkey = "med_high_flow",
    propcode = "medh30",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,64] <- alfprop$propvalue
  
  # 65: 90 Day Maximum High Flow
  alfinfo <- list(
    varkey = "max_high_flow",
    propcode = "max90",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,65] <- alfprop$propvalue
  
  # 66: 90 Day Median High Flow
  alfinfo <- list(
    varkey = "med_high_flow",
    propcode = "medh90",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,66] <- alfprop$propvalue
  
  # 67: Mean Flow in Year of Drought of Record
  alfinfo <- list(
    varkey = "dor_mean",
    featureid = as.integer(as.character(model$pid)),
    entity_type = "dh_properties"
  )
  alfprop <- getProperty(alfinfo, site, alfprop)
  all.metrics[1,67] <- alfprop$propvalue
  
  all.errors.all.segments[all.errors.line.no,] <- all.metrics[1,1:num.metrics]
  all.errors.line.no <- all.errors.line.no + 1
}

Metrics <- all.errors.all.segments

# IMPORT DATA -----
#The projection that you want the map to be output in requires a Proj4 code. 
#Search the projection you want (in this case WGS 1984 was used becuase it is compatible with VA Hydro) and choose the link for 'spatialreference.org'
#Once on the website click on the Proj4 link and copy and paste the provided line in quotes after its associated variable name 
Projection<- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs' 

#Importing base map
#The base map for this code is the outline of the states in the US
States<- readOGR(paste0(container, '\\spatial_analysis\\gis_layers\\cb_2017_us_state_5m'),'cb_2017_us_state_5m' )  #Pull the (location of GIS shapefile, desired shapefile)
States<- spTransform(States, CRS=Projection)                                        #Put shapefile in correct projection/coordinated system

#Importing desired area
#The desired area for this code are the river segements associated with the P532 Chesapeake Bay Model
RivSeg<- readOGR(paste0(container, '\\spatial_analysis\\gis_layers\\BaseMap'), 'AlteredRiverSegs')             #Pull the (location of GIS shapefile, desired shapefile)
RivSeg<- spTransform(RivSeg, CRS=Projection)                                     #Put shapefile in correct projection/coordinated system

dir.create(paste0(output_location,"\\SouthernRivers_p532_SpatialAnalysis"), showWarnings = FALSE);

# Initiating counter
ctr <- 1

# Determining metric column names
metric.names <- metrics.names

for (ctr in 1:num.metrics) {
  
  DesiredMetric<- metric.names[ctr]
  
  # SETTING UP FOR LOOP -----
  #Create an empty column for the desired data you would like to be applied to your river segments
  #The metrics will be added to the shapefile for the river segments as seprate columns and the desired metric will be given its own column
  RivSeg@data$RiverSeg<-as.character(RivSeg@data$RiverSeg)
  RivSeg@data$Metric<-NA
  
  Metrics[,paste0("DesiredMetric")] <- Metrics[DesiredMetric]
  
  # LOOP TO ASSOCIATE RIVER SEGMENT AND DATA -----
  #The loop will run and add the desired metrics column to any segment that has a matching river segment ID with that metric
  for (i in 1:length(RivSeg@data$RiverSeg)){
    if (RivSeg@data$RiverSeg[i]%in%row.names(Metrics)){ #if the river segment ID is in the metrics file make it true, if not make it false
      RivSeg@data$Metric[i]<- Metrics$DesiredMetric[row.names(Metrics)==RivSeg@data$RiverSeg[i]]
    }
  }
  
  # GRAPHING -----
  title<-paste0((DesiredMetric),' (cfs)')
  
  colfunc <- colorRampPalette(c("red", "orange", "yellow","springgreen","blue", "blue4"))
  png(filename=paste0(output_location,"\\SouthernRivers_p532_SpatialAnalysis\\", DesiredMetric ,"- Error.png"), 
      width=1400, height=950, units="px")
  
  RivSeg@data$color<- cut(RivSeg@data$Metric,c(-Inf,5,10,25,50,100,250,500,1000,5000,10000,50000,Inf), labels=(colfunc(12)))
  SouthernRivers<- RivSeg[!is.na (RivSeg@data$color),]
  plot(SouthernRivers, col=paste0(SouthernRivers@data$color))
  plot(States, add=TRUE, col='gray')
  lines(States, col='white')
  plot(SouthernRivers, col=paste0(SouthernRivers@data$color), add=T)
  legend("bottom", legend=c('<5', '5 to 10', '10 to 25', '25 to 50', '50 to 100', '100 to 250', '250 to 500', '500 to 1000', '1000 to 5000', '5000 to 10000', '10000 to 50000', '>50000'), col=(colfunc(12)), lty=0, pch=15, pt.cex=7, ncol = 3, bty='n', y.intersp=0.75, x.intersp=0.3, cex=3.5, lwd=2)
  legend("top", legend=c(title), lty=0, pt.cex=3, bty='n', y.intersp=0.75, x.intersp=0.3, cex=5, lwd=2)
  
  dev.off()
  
  # Incrementing counter
  ctr <- ctr + 1
}

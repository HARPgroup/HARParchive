# DOCUMENTATION -----------------------------------------------------------

# Calculates all flow metrics for model and gage data, exports a table
# containing all metrics to the "results" folder

# LIBRARIES ---------------------------------------------------------------

library('IHA')
library('PearsonDS')
library('zoo')
library('lubridate')
library('lfstat')

# INPUTS ------------------------------------------------------------------

# Address of "DEQ_Model_vs_USGS_Comparison" folder
# Include "DEQ_Model_vs_USGS_Comparison" in address!
container <- "C:\\Users\\FujitsuT\\Downloads\\HARP\\GitHub\\hydro-tools\\HARP-2018\\DEQ_Model_vs_USGS_Comparison_Northern"

# USGS Gage number
siteNo <- "01660400"

# Should new or original data be used?
new.or.original <- "new"

# CARRYOVER IF MASTER IS BEING RUN ----------------------------------------
if (exists("container.master") == TRUE) {
  container <- container.master
  siteNo <- siteNo.master
  new.or.original <- new.or.original.master
}

# NEW OR ORIGINAL DATA SWITCH ---------------------------------------------

if (new.or.original == "new") {
  container.cont <- "\\data\\new_(updated)_data"
} else if (new.or.original == "original") {
  container.cont <- "\\data\\original_(reproducible)_data"
} else {
  print("ERROR: neither new or original data specified")
}

# LINKING MODEL SEGMENT ---------------------------------------------------

gage.to.segment <- read.csv(file.path(container, "data", "Gage_To_Segment_Northern.csv"),
                            header = TRUE, sep = ',', stringsAsFactors = FALSE)
gage.to.segment <- subset(gage.to.segment, gage.to.segment$gage_number == as.numeric(siteNo))
RivSeg <- gage.to.segment$river_segment

# SOURCING CALCULATING FUNCTION -------------------------------------------

source(paste0(container, "\\code\\function_holder.R"));

# CREATE FOLDER -----------------------------------------------------------

dir.create(paste0(container,"\\results\\user's_results\\",siteNo, "_vs_", RivSeg), showWarnings = TRUE)

# LOADING DATA ------------------------------------------------------------

data <- read.csv(paste0(container, container.cont, "\\derived_data\\trimmed+area-adjusted_data\\",siteNo,"_vs_",RivSeg, " - Derived Data.csv"))

# ADDING ADDITIONAL DATA COLUMNS ------------------------------------------
data$year <- year(ymd(data$date))
data$month <- month(ymd(data$date))
data$day <- day(ymd(data$date))

# SETUP FOR CALCULATIONS --------------------------------------------------

# Setup for ___ Day Min Calculations
# Running gage calculations
f3_USGS <- zoo(data$gage.flow, order.by = data$date)
g2_USGS <- group2(f3_USGS, year = 'water')
# Running model calculations
f3_model <- zoo(data$model.flow, order.by = data$date)
g2_model <- group2(f3_model, year = 'water')

# Setup for Monthly Means Calculations
USGS_Monthly_Means <- aggregate(data$gage.flow, list(data$month), FUN = mean)
Model_Monthly_Means <- aggregate(data$model.flow, list(data$month), FUN = mean)

# Setup for Montly Mins Calculations
flows_USGS <- zoo(data$gage.flow, order.by = data$date)
flows_model <- zoo(data$model.flow, order.by = data$date)

# Setup for Flow Exceedance Calculations
# Creating vectors of decreasing flow magnitude
dec_flows_USGS <- sort(data$gage.flow, decreasing = TRUE)
dec_flows_model <- sort(data$model.flow, decreasing = TRUE)
# Determining the "rank" (0-1) of the flow value
num_observations <- as.numeric(length(data$date))
rank_vec <- as.numeric(c(1:num_observations))
# Calculating exceedance probability
prob_exceedance <- 100*((rank_vec) / (num_observations + 1))
# Creating vectors of calculated quantiles
model_prob_exceedance <- quantile(dec_flows_model, probs = c(0.01, 0.05, 0.5, 0.95, 0.99))
USGS_prob_exceedance <- quantile(dec_flows_USGS, probs = c(0.01, 0.05, 0.5, 0.95, 0.99))

# Setup for Sept. 10% Flow
sept_flows <- subset(data, month == '9')
sept_quant_USGS <- quantile(sept_flows$gage.flow, 0.10)
sept_quant_model <- quantile(sept_flows$model.flow, 0.10)

# Setup for Baseflow Calculations
gage.data <- data.frame(data$day, data$month, data$year, data$gage.flow)
names(gage.data) <- c('day', 'month', 'year', 'flow')
model.data <- data.frame(data$day, data$month, data$year, data$model.flow)
names(model.data) <- c('day', 'month', 'year', 'flow')
# Creating lowflow objects
USGSriver <- createlfobj(gage.data, hyearstart = 10, baseflow = TRUE, meta = NULL)
modelriver <- createlfobj(model.data, hyearstart = 10, baseflow = TRUE, meta = NULL)
baseflowriver<- data.frame(modelriver, USGSriver);
colnames(baseflowriver) <-c ('mday', 'mmonth', 'myear', 'mflow', 'mHyear', 'mBaseflow',
                             'gday', 'gmonth', 'gyear', 'gflow', 'gHyear', 'gBaseflow')
# removing NA values
baseflowriver<-baseflowriver[complete.cases(baseflowriver)==TRUE,]
USGSriver<- data.frame(baseflowriver$gday, baseflowriver$gmonth, baseflowriver$gyear, 
                       baseflowriver$gflow, baseflowriver$gHyear, baseflowriver$gBaseflow);
modelriver<- data.frame(baseflowriver$mday, baseflowriver$mmonth, baseflowriver$myear, 
                        baseflowriver$mflow, baseflowriver$mHyear, baseflowriver$mBaseflow)
names(USGSriver) <- c('day', 'month', 'year', 'flow', 'hyear', 'baseflow')
names(modelriver) <- c('day', 'month', 'year', 'flow', 'hyear', 'baseflow')
# Adding date vectors
USGSriver$date <- as.Date(paste0(USGSriver$year,"-",USGSriver$month,"-",USGSriver$day))
modelriver$date <- as.Date(paste0(modelriver$year,"-",modelriver$month,"-",modelriver$day))

# Setup for Low Flows
lf_model <- aggregate(modelriver$flow, by = list(modelriver$hyear), FUN = mean)
lf_USGS <- aggregate(USGSriver$flow, by = list(USGSriver$hyear), FUN = mean)
colnames(lf_model) <- c('Water Year', 'Mean Flow')
colnames(lf_USGS) <- c('Water Year', 'Mean Flow')

# Setup for Residuals
resid <- (data$model.flow - data$gage.flow)
resid <- data.frame(data$date, resid)

# CALCULATIONS ------------------------------------------------------------

# OVERALL MEAN FLOW
met00_Gage_MeanFlow <- signif(mean(data$gage.flow), digits=3);
met00_Model_MeanFlow <- signif(mean(data$model.flow), digits=3);
met00_PctError <- (signif(((met00_Model_MeanFlow - met00_Gage_MeanFlow) / met00_Gage_MeanFlow)*100, digits=3));

# JANUARY LOW FLOW
met01_Gage_JanLF <- signif(fn_iha_mlf(flows_USGS,1), digits=3);
met01_Mod_JanLF <- signif(fn_iha_mlf(flows_model,1), digits=3);
met01_PctError <- (signif(((met01_Mod_JanLF - met01_Gage_JanLF) / met01_Gage_JanLF)*100, digits=3));

# FEBRUARY LOW FLOW
met02_Gage_FebLF <- signif(fn_iha_mlf(flows_USGS,2), digits=3);
met02_Mod_FebLF <- signif(fn_iha_mlf(flows_model,2), digits=3);
met02_PctError <- (signif(((met02_Mod_FebLF - met02_Gage_FebLF) / met02_Gage_FebLF)*100, digits=3));

# MARCH LOW FLOW
met03_Gage_MarLF <- signif(fn_iha_mlf(flows_USGS,3), digits=3);
met03_Mod_MarLF <- signif(fn_iha_mlf(flows_model,3), digits=3);
met03_PctError <- (signif(((met03_Mod_MarLF - met03_Gage_MarLF) / met03_Gage_MarLF)*100, digits=3));

# APRIL LOW FLOW
met04_Gage_AprLF <- signif(fn_iha_mlf(flows_USGS,4), digits=3);
met04_Mod_AprLF <- signif(fn_iha_mlf(flows_model,4), digits=3);
met04_PctError <- (signif(((met04_Mod_AprLF - met04_Gage_AprLF) / met04_Gage_AprLF)*100, digits=3));

# MAY LOW FLOW
met05_Gage_MayLF <- signif(fn_iha_mlf(flows_USGS,5), digits=3);
met05_Mod_MayLF <- signif(fn_iha_mlf(flows_model,5), digits=3);
met05_PctError <- (signif(((met05_Mod_MayLF - met05_Gage_MayLF) / met05_Gage_MayLF)*100, digits=3));

# JUNE LOW FLOW
met06_Gage_JunLF <- signif(fn_iha_mlf(flows_USGS,6), digits=3);
met06_Mod_JunLF <- signif(fn_iha_mlf(flows_model,6), digits=3);
met06_PctError <- (signif(((met06_Mod_JunLF - met06_Gage_JunLF) / met06_Gage_JunLF)*100, digits=3));

# JULY LOW FLOW
met07_Gage_JulLF <- signif(fn_iha_mlf(flows_USGS,7), digits=3);
met07_Mod_JulLF <- signif(fn_iha_mlf(flows_model,7), digits=3);
met07_PctError <- (signif(((met07_Mod_JulLF - met07_Gage_JulLF) / met07_Gage_JulLF)*100, digits=3));

# AUGUST LOW FLOW
met08_Gage_AugLF <- signif(fn_iha_mlf(flows_USGS,8), digits=3);
met08_Mod_AugLF <- signif(fn_iha_mlf(flows_model,8), digits=3);
met08_PctError <- (signif(((met08_Mod_AugLF - met08_Gage_AugLF) / met08_Gage_AugLF)*100, digits=3));

# SEPTEMBER LOW FLOW
met09_Gage_SepLF <- signif(fn_iha_mlf(flows_USGS,9), digits=3);
met09_Mod_SepLF <- signif(fn_iha_mlf(flows_model,9), digits=3);
met09_PctError <- (signif(((met09_Mod_SepLF - met09_Gage_SepLF) / met09_Gage_SepLF)*100, digits=3));

# OCTOBER LOW FLOW
met10_Gage_OctLF <- signif(fn_iha_mlf(flows_USGS,10), digits=3);
met10_Mod_OctLF <- signif(fn_iha_mlf(flows_model,10), digits=3);
met10_PctError <- (signif(((met10_Mod_OctLF - met10_Gage_OctLF) / met10_Gage_OctLF)*100, digits=3));

# NOVEMBER LOW FLOW
met11_Gage_NovLF <- signif(fn_iha_mlf(flows_USGS,11), digits=3);
met11_Mod_NovLF <- signif(fn_iha_mlf(flows_model,11), digits=3);
met11_PctError <- (signif(((met11_Mod_NovLF - met11_Gage_NovLF) / met11_Gage_NovLF)*100, digits=3));

# DECEMBER LOW FLOW
met12_Gage_DecLF <- signif(fn_iha_mlf(flows_USGS,12), digits=3);
met12_Mod_DecLF <- signif(fn_iha_mlf(flows_model,12), digits=3);
met12_PctError <- (signif(((met12_Mod_DecLF - met12_Gage_DecLF) / met12_Gage_DecLF)*100, digits=3));

# JANUARY MEAN FLOW
met13_Gage_JanMF <- signif(USGS_Monthly_Means[1,2], digits=3);
met13_Model_JanMF <- signif(Model_Monthly_Means[1,2], digits=3);
met13_PctError <- (signif(((met13_Model_JanMF - met13_Gage_JanMF) / met13_Gage_JanMF)*100, digits=3));

# FEBRUARY MEAN FLOW
met14_Gage_FebMF <- signif(USGS_Monthly_Means[2,2], digits=3);
met14_Model_FebMF <- signif(Model_Monthly_Means[2,2], digits=3);
met14_PctError <- (signif(((met14_Model_FebMF - met14_Gage_FebMF) / met14_Gage_FebMF)*100, digits=3));

# MARCH MEAN FLOW
met15_Gage_MarMF <- signif(USGS_Monthly_Means[3,2], digits=3);
met15_Model_MarMF <- signif(Model_Monthly_Means[3,2], digits=3);
met15_PctError <- (signif(((met15_Model_MarMF - met15_Gage_MarMF) / met15_Gage_MarMF)*100, digits=3));

# APRIL MEAN FLOW
met16_Gage_AprMF <- signif(USGS_Monthly_Means[4,2], digits=3);
met16_Model_AprMF <- signif(Model_Monthly_Means[4,2], digits=3);
met16_PctError <- (signif(((met16_Model_AprMF - met16_Gage_AprMF) / met16_Gage_AprMF)*100, digits=3));

# MAY MEAN FLOW
met17_Gage_MayMF <- signif(USGS_Monthly_Means[5,2], digits=3);
met17_Model_MayMF <- signif(Model_Monthly_Means[5,2], digits=3);
met17_PctError <- (signif(((met17_Model_MayMF - met17_Gage_MayMF) / met17_Gage_MayMF)*100, digits=3));

# JUNE MEAN FLOW
met18_Gage_JunMF <- signif(USGS_Monthly_Means[6,2], digits=3);
met18_Model_JunMF <- signif(Model_Monthly_Means[6,2], digits=3);
met18_PctError <- (signif(((met18_Model_JunMF - met18_Gage_JunMF) / met18_Gage_JunMF)*100, digits=3));

# JULY MEAN FLOW
met19_Gage_JulMF <- signif(USGS_Monthly_Means[7,2], digits=3);
met19_Model_JulMF <- signif(Model_Monthly_Means[7,2], digits=3);
met19_PctError <- (signif(((met19_Model_JulMF - met19_Gage_JulMF) / met19_Gage_JulMF)*100, digits=3));

# AUGUST MEAN FLOW
met20_Gage_AugMF <- signif(USGS_Monthly_Means[8,2], digits=3);
met20_Model_AugMF <- signif(Model_Monthly_Means[8,2], digits=3);
met20_PctError <- (signif(((met20_Model_AugMF - met20_Gage_AugMF) / met20_Gage_AugMF)*100, digits=3));

# SEPTEMBER MEAN FLOW
met21_Gage_SepMF <- signif(USGS_Monthly_Means[9,2], digits=3);
met21_Model_SepMF <- signif(Model_Monthly_Means[9,2], digits=3);
met21_PctError <- (signif(((met21_Model_SepMF - met21_Gage_SepMF) / met21_Gage_SepMF)*100, digits=3));

# OCTOBER MEAN FLOW
met22_Gage_OctMF <- signif(USGS_Monthly_Means[10,2], digits=3);
met22_Model_OctMF <- signif(Model_Monthly_Means[10,2], digits=3);
met22_PctError <- (signif(((met22_Model_OctMF - met22_Gage_OctMF) / met22_Gage_OctMF)*100, digits=3));

# NOVEMBER MEAN FLOW
met23_Gage_NovMF <- signif(USGS_Monthly_Means[11,2], digits=3);
met23_Model_NovMF <- signif(Model_Monthly_Means[11,2], digits=3);
met23_PctError <- (signif(((met23_Model_NovMF - met23_Gage_NovMF) / met23_Gage_NovMF)*100, digits=3));

# DECEMBER MEAN FLOW
met24_Gage_DecMF <- signif(USGS_Monthly_Means[12,2], digits=3);
met24_Model_DecMF <- signif(Model_Monthly_Means[12,2], digits=3);
met24_PctError <- (signif(((met24_Model_DecMF - met24_Gage_DecMF) / met24_Gage_DecMF)*100, digits=3));

# 1 DAY MIN
yearly_Gage_1DayMin <- g2_USGS[,c(1,2)];
met25_Gage_1DayMinMin <- signif(min(yearly_Gage_1DayMin$`1 Day Min`), digits=3);
yearly_Mod_1DayMin <- g2_model[,c(1,2)];
met25_Mod_1DayMinMin <- signif(min(yearly_Mod_1DayMin$`1 Day Min`), digits=3);
met25_PctError <- (signif(((met25_Mod_1DayMinMin - met25_Gage_1DayMinMin) / met25_Gage_1DayMinMin)*100, digits=3));
met26_Gage_1DayMinMed <- signif(median(yearly_Gage_1DayMin$`1 Day Min`), digits=3);
met26_Mod_1DayMinMed <- signif(median(yearly_Mod_1DayMin$`1 Day Min`), digits=3);
met26_PctError <- (signif(((met26_Mod_1DayMinMed - met26_Gage_1DayMinMed) / met26_Gage_1DayMinMed)*100, digits=3));

# 3 DAY MIN
yearly_Gage_3DayMin <- g2_USGS[,c(1,4)];
met27_Gage_3DayMinMin <- signif(min(yearly_Gage_3DayMin$`3 Day Min`), digits=3);
yearly_Mod_3DayMin <- g2_model[,c(1,4)];
met27_Mod_3DayMinMin <- signif(min(yearly_Mod_3DayMin$`3 Day Min`), digits=3);
met27_PctError <- (signif(((met27_Mod_3DayMinMin - met27_Gage_3DayMinMin) / met27_Gage_3DayMinMin)*100, digits=3));
met28_Gage_3DayMinMed <- signif(median(yearly_Gage_3DayMin$`3 Day Min`), digits=3);
met28_Mod_3DayMinMed <- signif(median(yearly_Mod_3DayMin$`3 Day Min`), digits=3);
met28_PctError <- (signif(((met28_Mod_3DayMinMed - met28_Gage_3DayMinMed) / met28_Gage_3DayMinMed)*100, digits=3));

# 7 DAY MIN
yearly_Gage_7DayMin <- g2_USGS[,c(1,6)];
met29_Gage_7DayMinMin <- signif(min(yearly_Gage_7DayMin$`7 Day Min`), digits=3);
yearly_Mod_7DayMin <- g2_model[,c(1,6)];
met29_Mod_7DayMinMin <- signif(min(yearly_Mod_7DayMin$`7 Day Min`), digits=3);
met29_PctError <- (signif(((met29_Mod_7DayMinMin - met29_Gage_7DayMinMin) / met29_Gage_7DayMinMin)*100, digits=3));
met30_Gage_7DayMinMed <- signif(median(yearly_Gage_7DayMin$`7 Day Min`), digits=3);
met30_Mod_7DayMinMed <- signif(median(yearly_Mod_7DayMin$`7 Day Min`), digits=3);
met30_PctError <- (signif(((met30_Mod_7DayMinMed - met30_Gage_7DayMinMed) / met30_Gage_7DayMinMed)*100, digits=3));

# 30 DAY MIN
yearly_Gage_30DayMin <- g2_USGS[,c(1,8)];
met31_Gage_30DayMinMin <- signif(min(yearly_Gage_30DayMin$`30 Day Min`), digits=3);
yearly_Mod_30DayMin <- g2_model[,c(1,8)];
met31_Mod_30DayMinMin <- signif(min(yearly_Mod_30DayMin$`30 Day Min`), digits=3);
met31_PctError <- (signif(((met31_Mod_30DayMinMin - met31_Gage_30DayMinMin) / met31_Gage_30DayMinMin)*100, digits=3));
met32_Gage_30DayMinMed <- signif(median(yearly_Gage_30DayMin$`30 Day Min`), digits=3);
met32_Mod_30DayMinMed <- signif(median(yearly_Mod_30DayMin$`30 Day Min`), digits=3);
met32_PctError <- (signif(((met32_Mod_30DayMinMed - met32_Gage_30DayMinMed) / met32_Gage_30DayMinMed)*100, digits=3));

# 90 DAY MIN
yearly_Gage_90DayMin <- g2_USGS[,c(1,10)];
met33_Gage_90DayMinMin <- signif(min(yearly_Gage_90DayMin$`90 Day Min`), digits=3);
yearly_Mod_90DayMin <- g2_model[,c(1,10)];
met33_Mod_90DayMinMin <- signif(min(yearly_Mod_90DayMin$`90 Day Min`), digits=3);
met33_PctError <- (signif(((met33_Mod_90DayMinMin - met33_Gage_90DayMinMin) / met33_Gage_90DayMinMin)*100, digits=3));
met34_Gage_90DayMinMed <- signif(median(yearly_Gage_90DayMin$`90 Day Min`), digits=3);
met34_Mod_90DayMinMed <- signif(median(yearly_Mod_90DayMin$`90 Day Min`), digits=3);
met34_PctError <- (signif(((met34_Mod_90DayMinMed - met34_Gage_90DayMinMed) / met34_Gage_90DayMinMed)*100, digits=3));

# 7Q10
met35_Gage_7Q10 <- signif(fn_iha_7q10(flows_USGS), digits=3);
met35_Model_7Q10 <- signif(fn_iha_7q10(flows_model), digits=3);
met35_PctError <- (signif(((met35_Model_7Q10 - met35_Gage_7Q10) / met35_Gage_7Q10)*100, digits=3));

# DROUGHT OF RECORD (MIN. 90 DAY MIN.) YEAR
met37_Gage_DoR <- fn_iha_DOR_Year(flows_USGS);
met37_Model_DoR <- fn_iha_DOR_Year(flows_model);
if (met37_Gage_DoR == met37_Model_DoR) {
  met37_PctError <- 0;
} else {
  met37_PctError <- 100;
}

# 1% Non-exceedance Flow
met38_Gage_1NonEx <- signif(USGS_prob_exceedance[["1%"]], digits=3);
met38_Model_1NonEx <- signif(model_prob_exceedance[["1%"]], digits=3);
met38_PctError <- (signif(((met38_Model_1NonEx - met38_Gage_1NonEx) / met38_Gage_1NonEx)*100, digits=3));

# 5% Non-exceedance Flow
met39_Gage_5NonEx <- signif(USGS_prob_exceedance[["5%"]], digits=3);
met39_Model_5NonEx <- signif(model_prob_exceedance[["5%"]], digits=3);
met39_PctError <- (signif(((met39_Model_5NonEx - met39_Gage_5NonEx) / met39_Gage_5NonEx)*100, digits=3));

# 50% Non-exceedance Flow
met40_Gage_50NonEx <- signif(USGS_prob_exceedance[["50%"]], digits=3);
met40_Model_50NonEx <- signif(model_prob_exceedance[["50%"]], digits=3);
met40_PctError <- (signif(((met40_Model_50NonEx - met40_Gage_50NonEx) / met40_Gage_50NonEx)*100, digits=3));

# 95% Non-exceedance Flow
met41_Gage_95NonEx <- signif(USGS_prob_exceedance[["95%"]], digits=3);
met41_Model_95NonEx <- signif(model_prob_exceedance[["95%"]], digits=3);
met41_PctError <- (signif(((met41_Model_95NonEx - met41_Gage_95NonEx) / met41_Gage_95NonEx)*100, digits=3));

# 99% Non-exceedance Flow
met42_Gage_99NonEx <- signif(USGS_prob_exceedance[["99%"]], digits=3);
met42_Model_99NonEx <- signif(model_prob_exceedance[["99%"]], digits=3);
met42_PctError <- (signif(((met42_Model_99NonEx - met42_Gage_99NonEx) / met42_Gage_99NonEx)*100, digits=3));

# Sept. 10% Flow
met43_Gage_Sep10 <- signif(sept_quant_USGS[["10%"]], digits=3);
met43_Model_Sep10 <- signif(sept_quant_model[["10%"]], digits=3);
met43_PctError <- (signif(((met43_Model_Sep10 - met43_Gage_Sep10) / met43_Gage_Sep10)*100, digits=3));

# Baseflow (Average)
met44_Gage_Base <- signif(mean(USGSriver$baseflow), digits=3);
met44_Model_Base <- signif(mean(modelriver$baseflow), digits=3);
met44_PctError <- (signif(((met44_Model_Base - met44_Gage_Base) / met44_Gage_Base)*100, digits=3));

# JANUARY HIGH FLOW
met45_Gage_JanHF <- signif(fn_iha_mhf(flows_USGS,1), digits=3);
met45_Mod_JanHF <- signif(fn_iha_mhf(flows_model,1), digits=3);
met45_PctError <- (signif(((met45_Mod_JanHF - met45_Gage_JanHF) / met45_Gage_JanHF)*100, digits=3));

# FEBRUARY HIGH FLOW
met46_Gage_FebHF <- signif(fn_iha_mhf(flows_USGS,2), digits=3);
met46_Mod_FebHF <- signif(fn_iha_mhf(flows_model,2), digits=3);
met46_PctError <- (signif(((met46_Mod_FebHF - met46_Gage_FebHF) / met46_Gage_FebHF)*100, digits=3));

# MARCH HIGH FLOW
met47_Gage_MarHF <- signif(fn_iha_mhf(flows_USGS,3), digits=3);
met47_Mod_MarHF <- signif(fn_iha_mhf(flows_model,3), digits=3);
met47_PctError <- (signif(((met47_Mod_MarHF - met47_Gage_MarHF) / met47_Gage_MarHF)*100, digits=3));

# APRIL HIGH FLOW
met48_Gage_AprHF <- signif(fn_iha_mhf(flows_USGS,4), digits=3);
met48_Mod_AprHF <- signif(fn_iha_mhf(flows_model,4), digits=3);
met48_PctError <- (signif(((met48_Mod_AprHF - met48_Gage_AprHF) / met48_Gage_AprHF)*100, digits=3));

# MAY HIGH FLOW
met49_Gage_MayHF <- signif(fn_iha_mhf(flows_USGS,5), digits=3);
met49_Mod_MayHF <- signif(fn_iha_mhf(flows_model,5), digits=3);
met49_PctError <- (signif(((met49_Mod_MayHF - met49_Gage_MayHF) / met49_Gage_MayHF)*100, digits=3));

# JUNE HIGH FLOW
met50_Gage_JunHF <- signif(fn_iha_mhf(flows_USGS,6), digits=3);
met50_Mod_JunHF <- signif(fn_iha_mhf(flows_model,6), digits=3);
met50_PctError <- (signif(((met50_Mod_JunHF - met50_Gage_JunHF) / met50_Gage_JunHF)*100, digits=3));

# JULY HIGH FLOW
met51_Gage_JulHF <- signif(fn_iha_mhf(flows_USGS,7), digits=3);
met51_Mod_JulHF <- signif(fn_iha_mhf(flows_model,7), digits=3);
met51_PctError <- (signif(((met51_Mod_JulHF - met51_Gage_JulHF) / met51_Gage_JulHF)*100, digits=3));

# AUGUST HIGH FLOW
met52_Gage_AugHF <- signif(fn_iha_mhf(flows_USGS,8), digits=3);
met52_Mod_AugHF <- signif(fn_iha_mhf(flows_model,8), digits=3);
met52_PctError <- (signif(((met52_Mod_AugHF - met52_Gage_AugHF) / met52_Gage_AugHF)*100, digits=3));

# SEPTEMBER HIGH FLOW
met53_Gage_SepHF <- signif(fn_iha_mhf(flows_USGS,9), digits=3);
met53_Mod_SepHF <- signif(fn_iha_mhf(flows_model,9), digits=3);
met53_PctError <- (signif(((met53_Mod_SepHF - met53_Gage_SepHF) / met53_Gage_SepHF)*100, digits=3));

# OCTOBER HIGH FLOW
met54_Gage_OctHF <- signif(fn_iha_mhf(flows_USGS,10), digits=3);
met54_Mod_OctHF <- signif(fn_iha_mhf(flows_model,10), digits=3);
met54_PctError <- (signif(((met54_Mod_OctHF - met54_Gage_OctHF) / met54_Gage_OctHF)*100, digits=3));

# NOVEMBER HIGH FLOW
met55_Gage_NovHF <- signif(fn_iha_mhf(flows_USGS,11), digits=3);
met55_Mod_NovHF <- signif(fn_iha_mhf(flows_model,11), digits=3);
met55_PctError <- (signif(((met55_Mod_NovHF - met55_Gage_NovHF) / met55_Gage_NovHF)*100, digits=3));

# DECEMBER HIGH FLOW
met56_Gage_DecHF <- signif(fn_iha_mhf(flows_USGS,12), digits=3);
met56_Mod_DecHF <- signif(fn_iha_mhf(flows_model,12), digits=3);
met56_PctError <- (signif(((met56_Mod_DecHF - met56_Gage_DecHF) / met56_Gage_DecHF)*100, digits=3));

# 1 DAY MAX
yearly_Gage_1DayMax <- g2_USGS[,c(1,3)];
met57_Gage_1DayMaxMax <- signif(max(yearly_Gage_1DayMax$`1 Day Max`), digits=3);
yearly_Mod_1DayMax <- g2_model[,c(1,3)];
met57_Mod_1DayMaxMax <- signif(max(yearly_Mod_1DayMax$`1 Day Max`), digits=3);
met57_PctError <- (signif(((met57_Mod_1DayMaxMax - met57_Gage_1DayMaxMax) / met57_Gage_1DayMaxMax)*100, digits=3));
met58_Gage_1DayMaxMed <- signif(median(yearly_Gage_1DayMax$`1 Day Max`), digits=3);
met58_Mod_1DayMaxMed <- signif(median(yearly_Mod_1DayMax$`1 Day Max`), digits=3);
met58_PctError <- (signif(((met58_Mod_1DayMaxMed - met58_Gage_1DayMaxMed) / met58_Gage_1DayMaxMed)*100, digits=3));

# 3 DAY MAX
yearly_Gage_3DayMax <- g2_USGS[,c(1,5)];
met59_Gage_3DayMaxMax <- signif(max(yearly_Gage_3DayMax$`3 Day Max`), digits=3);
yearly_Mod_3DayMax <- g2_model[,c(1,5)];
met59_Mod_3DayMaxMax <- signif(max(yearly_Mod_3DayMax$`3 Day Max`), digits=3);
met59_PctError <- (signif(((met59_Mod_3DayMaxMax - met59_Gage_3DayMaxMax) / met59_Gage_3DayMaxMax)*100, digits=3));
met60_Gage_3DayMaxMed <- signif(median(yearly_Gage_3DayMax$`3 Day Max`), digits=3);
met60_Mod_3DayMaxMed <- signif(median(yearly_Mod_3DayMax$`3 Day Max`), digits=3);
met60_PctError <- (signif(((met60_Mod_3DayMaxMed - met60_Gage_3DayMaxMed) / met60_Gage_3DayMaxMed)*100, digits=3));

# 7 DAY MAX
yearly_Gage_7DayMax <- g2_USGS[,c(1,7)];
met61_Gage_7DayMaxMax <- signif(max(yearly_Gage_7DayMax$`7 Day Max`), digits=3);
yearly_Mod_7DayMax <- g2_model[,c(1,7)];
met61_Mod_7DayMaxMax <- signif(max(yearly_Mod_7DayMax$`7 Day Max`), digits=3);
met61_PctError <- (signif(((met61_Mod_7DayMaxMax - met61_Gage_7DayMaxMax) / met61_Gage_7DayMaxMax)*100, digits=3));
met62_Gage_7DayMaxMed <- signif(median(yearly_Gage_7DayMax$`7 Day Max`), digits=3);
met62_Mod_7DayMaxMed <- signif(median(yearly_Mod_7DayMax$`7 Day Max`), digits=3);
met62_PctError <- (signif(((met62_Mod_7DayMaxMed - met62_Gage_7DayMaxMed) / met62_Gage_7DayMaxMed)*100, digits=3));

# 30 DAY MAX
yearly_Gage_30DayMax <- g2_USGS[,c(1,9)];
met63_Gage_30DayMaxMax <- signif(max(yearly_Gage_30DayMax$`30 Day Max`), digits=3);
yearly_Mod_30DayMax <- g2_model[,c(1,9)];
met63_Mod_30DayMaxMax <- signif(max(yearly_Mod_30DayMax$`30 Day Max`), digits=3);
met63_PctError <- (signif(((met63_Mod_30DayMaxMax - met63_Gage_30DayMaxMax) / met63_Gage_30DayMaxMax)*100, digits=3));
met64_Gage_30DayMaxMed <- signif(median(yearly_Gage_30DayMax$`30 Day Max`), digits=3);
met64_Mod_30DayMaxMed <- signif(median(yearly_Mod_30DayMax$`30 Day Max`), digits=3);
met64_PctError <- (signif(((met64_Mod_30DayMaxMed - met64_Gage_30DayMaxMed) / met64_Gage_30DayMaxMed)*100, digits=3));

# 90 DAY MAX
yearly_Gage_90DayMax <- g2_USGS[,c(1,11)];
met65_Gage_90DayMaxMax <- signif(max(yearly_Gage_90DayMax$`90 Day Max`), digits=3);
yearly_Mod_90DayMax <- g2_model[,c(1,11)];
met65_Mod_90DayMaxMax <- signif(max(yearly_Mod_90DayMax$`90 Day Max`), digits=3);
met65_PctError <- (signif(((met65_Mod_90DayMaxMax - met65_Gage_90DayMaxMax) / met65_Gage_90DayMaxMax)*100, digits=3));
met66_Gage_90DayMaxMed <- signif(median(yearly_Gage_90DayMax$`90 Day Max`), digits=3);
met66_Mod_90DayMaxMed <- signif(median(yearly_Mod_90DayMax$`90 Day Max`), digits=3);
met66_PctError <- (signif(((met66_Mod_90DayMaxMed - met66_Gage_90DayMaxMed) / met66_Gage_90DayMaxMed)*100, digits=3));

# LOW YEARLY MEAN
lf_USGS_flow <- which(lf_USGS$`Water Year` == met37_Gage_DoR)
lf_USGS_flow <- lf_USGS$`Mean Flow`[lf_USGS_flow]
lf_model_flow <- which(lf_model$`Water Year`== met37_Gage_DoR)
lf_model_flow <- lf_model$`Mean Flow`[lf_model_flow]
met67_model_lfmin <- lf_model_flow
met67_USGS_lfmin <- lf_USGS_flow
met67_PctError <- (signif(((met67_model_lfmin - met67_USGS_lfmin) / met67_USGS_lfmin)*100, digits=3));

# OUTPUTTING MATRICES -----
# Creating directory to store output tables in
# CREATE FOLDER -----------------------------------------------------------
dir.create(paste0(container,"\\results\\user's_results\\",siteNo, "_vs_", RivSeg,"\\Tables"), showWarnings = TRUE)

# All metrics, in a row
ALL_METRICS <- matrix(c(met00_Gage_MeanFlow, met00_Model_MeanFlow, met00_PctError,
                        met01_Gage_JanLF, met01_Mod_JanLF, met01_PctError,
                        met02_Gage_FebLF, met02_Mod_FebLF, met02_PctError,
                        met03_Gage_MarLF, met03_Mod_MarLF, met03_PctError,
                        met04_Gage_AprLF, met04_Mod_AprLF, met04_PctError,
                        met05_Gage_MayLF, met05_Mod_MayLF, met05_PctError,
                        met06_Gage_JunLF, met06_Mod_JunLF, met06_PctError,
                        met07_Gage_JulLF, met07_Mod_JulLF, met07_PctError,
                        met08_Gage_AugLF, met08_Mod_AugLF, met08_PctError,
                        met09_Gage_SepLF, met09_Mod_SepLF, met09_PctError,
                        met10_Gage_OctLF, met10_Mod_OctLF, met10_PctError,
                        met11_Gage_NovLF, met11_Mod_NovLF, met11_PctError,
                        met12_Gage_DecLF, met12_Mod_DecLF, met12_PctError,
                        met13_Gage_JanMF, met13_Model_JanMF, met13_PctError,
                        met14_Gage_FebMF, met14_Model_FebMF, met14_PctError,
                        met15_Gage_MarMF, met15_Model_MarMF, met15_PctError,
                        met16_Gage_AprMF, met16_Model_AprMF, met16_PctError,
                        met17_Gage_MayMF, met17_Model_MayMF, met17_PctError,
                        met18_Gage_JunMF, met18_Model_JunMF, met18_PctError,
                        met19_Gage_JulMF, met19_Model_JulMF, met19_PctError,
                        met20_Gage_AugMF, met20_Model_AugMF, met20_PctError,
                        met21_Gage_SepMF, met21_Model_SepMF, met21_PctError,
                        met22_Gage_OctMF, met22_Model_OctMF, met22_PctError,
                        met23_Gage_NovMF, met23_Model_NovMF, met23_PctError,
                        met24_Gage_DecMF, met24_Model_DecMF, met24_PctError,
                        met25_Gage_1DayMinMin, met25_Mod_1DayMinMin, met25_PctError,
                        met26_Gage_1DayMinMed, met26_Mod_1DayMinMed, met26_PctError,
                        met27_Gage_3DayMinMin, met27_Mod_3DayMinMin, met27_PctError,
                        met28_Gage_3DayMinMed, met28_Mod_3DayMinMed, met28_PctError,
                        met29_Gage_7DayMinMin, met29_Mod_7DayMinMin, met29_PctError,
                        met30_Gage_7DayMinMed, met30_Mod_7DayMinMed, met30_PctError,
                        met31_Gage_30DayMinMin, met31_Mod_30DayMinMin, met31_PctError,
                        met32_Gage_30DayMinMed, met32_Mod_30DayMinMed, met32_PctError,
                        met33_Gage_90DayMinMin, met33_Mod_90DayMinMin, met33_PctError,
                        met34_Gage_90DayMinMed, met34_Mod_90DayMinMed, met34_PctError,
                        met35_Gage_7Q10, met35_Model_7Q10, met35_PctError,
                        met37_Gage_DoR, met37_Model_DoR, met37_PctError,
                        met38_Gage_1NonEx, met38_Model_1NonEx, met38_PctError,
                        met39_Gage_5NonEx, met39_Model_5NonEx, met39_PctError,
                        met40_Gage_50NonEx, met40_Model_50NonEx, met40_PctError,
                        met41_Gage_95NonEx, met41_Model_95NonEx, met41_PctError,
                        met42_Gage_99NonEx, met42_Model_99NonEx, met42_PctError,
                        met43_Gage_Sep10, met43_Model_Sep10, met43_PctError,
                        met44_Gage_Base, met44_Model_Base, met44_PctError,
                        met45_Gage_JanHF, met45_Mod_JanHF, met45_PctError,
                        met46_Gage_FebHF, met46_Mod_FebHF, met46_PctError,
                        met47_Gage_MarHF, met47_Mod_MarHF, met47_PctError,
                        met48_Gage_AprHF, met48_Mod_AprHF, met48_PctError,
                        met49_Gage_MayHF, met49_Mod_MayHF, met49_PctError,
                        met50_Gage_JunHF, met50_Mod_JunHF, met50_PctError,
                        met51_Gage_JulHF, met51_Mod_JulHF, met51_PctError,
                        met52_Gage_AugHF, met52_Mod_AugHF, met52_PctError,
                        met53_Gage_SepHF, met53_Mod_SepHF, met53_PctError,
                        met54_Gage_OctHF, met54_Mod_OctHF, met54_PctError,
                        met55_Gage_NovHF, met55_Mod_NovHF, met55_PctError,
                        met56_Gage_DecHF, met56_Mod_DecHF, met56_PctError,
                        met57_Gage_1DayMaxMax, met57_Mod_1DayMaxMax, met57_PctError,
                        met58_Gage_1DayMaxMed, met58_Mod_1DayMaxMed, met58_PctError,
                        met59_Gage_3DayMaxMax, met59_Mod_3DayMaxMax, met59_PctError,
                        met60_Gage_3DayMaxMed, met60_Mod_3DayMaxMed, met60_PctError,
                        met61_Gage_7DayMaxMax, met61_Mod_7DayMaxMax, met61_PctError,
                        met62_Gage_7DayMaxMed, met62_Mod_7DayMaxMed, met62_PctError,
                        met63_Gage_30DayMaxMax, met63_Mod_30DayMaxMax, met63_PctError,
                        met64_Gage_30DayMaxMed, met64_Mod_30DayMaxMed, met64_PctError,
                        met65_Gage_90DayMaxMax, met65_Mod_90DayMaxMax, met65_PctError,
                        met66_Gage_90DayMaxMed, met66_Mod_90DayMaxMed, met66_PctError,
                        met67_USGS_lfmin, met67_model_lfmin, met67_PctError),
                      nrow = 3, ncol = 67);
rownames(ALL_METRICS) = c("USGS Gage", "Model", "Pct. Error");
colnames(ALL_METRICS) = c("Overall Mean Flow", "Jan. Low Flow", 
                          "Feb. Low Flow",
                          "Mar. Low Flow", "Apr. Low Flow",
                          "May Low Flow", "Jun. Low Flow",
                          "Jul. Low Flow", "Aug. Low Flow",
                          "Sep. Low Flow", "Oct. Low Flow",
                          "Nov. Low Flow", "Dec. Low Flow",
                          "Jan. Mean Flow", "Feb. Mean Flow",
                          "Mar. Mean Flow", "Apr. Mean Flow",
                          "May Mean Flow", "Jun. Mean Flow",
                          "Jul. Mean Flow", "Aug. Mean Flow",
                          "Sep. Mean Flow", "Oct. Mean Flow",
                          "Nov. Mean Flow", "Dec. Mean Flow",
                          "Min. 1 Day Min", "Med. 1 Day Min.", 
                          "Min. 3 Day Min", "Med. 3 Day Min.",
                          "Min. 7 Day Min", "Med. 7 Day Min.",
                          "Min. 30 Day Min", "Med. 30 Day Min.",
                          "Min. 90 Day Min", "Med. 90 Day Min.", 
                          "7Q10", "Year of 90-Day Low Flow",
                          "1% Non-Exceedance", "5% Non-Exceedance",
                          "50% Non-Exceedance", "95% Non-Exceedance",
                          "99% Non-Exceedance", "Sept. 10% Non-Exceedance",
                          "Mean Baseflow", "Jan. High Flow", 
                          "Feb. High Flow",
                          "Mar. High Flow", "Apr. High Flow",
                          "May High Flow", "Jun. High Flow",
                          "Jul. High Flow", "Aug. High Flow",
                          "Sep. High Flow", "Oct. High Flow",
                          "Nov. High Flow", "Dec. High Flow",
                          "Max. 1 Day Max", "Med. 1 Day Max.", 
                          "Max. 3 Day Max", "Med. 3 Day Max.",
                          "Max. 7 Day Max", "Med. 7 Day Max.",
                          "Max. 30 Day Max", "Med. 30 Day Max.",
                          "Max. 90 Day Max", "Med. 90 Day Max.",
                          "Drought Year Mean");
write.csv(ALL_METRICS, paste0(container,"\\results\\user's_results\\",siteNo, "_vs_", RivSeg,"\\Tables",
                              "\\All_Metrics.csv"));

# Table 1: Monthly Average Flow
Table1 <- matrix(c(met00_Gage_MeanFlow, met13_Gage_JanMF, met14_Gage_FebMF,
                   met15_Gage_MarMF, met16_Gage_AprMF, met17_Gage_MayMF,
                   met18_Gage_JunMF, met19_Gage_JulMF, met20_Gage_AugMF,
                   met21_Gage_SepMF, met22_Gage_OctMF, met23_Gage_NovMF,
                   met24_Gage_DecMF, met00_Model_MeanFlow, met13_Model_JanMF,
                   met14_Model_FebMF, met15_Model_MarMF, met16_Model_AprMF,
                   met17_Model_MayMF, met18_Model_JunMF, met19_Model_JulMF,
                   met20_Model_AugMF, met21_Model_SepMF, met22_Model_OctMF,
                   met23_Model_NovMF, met24_Model_DecMF, met00_PctError,
                   met13_PctError, met14_PctError, met15_PctError,
                   met16_PctError, met17_PctError, met18_PctError,
                   met19_PctError, met20_PctError, met21_PctError,
                   met22_PctError, met23_PctError, met24_PctError),
                 nrow = 13, ncol = 3);
colnames(Table1) = c("USGS Gage", "Model", "Pct. Error");
rownames(Table1) = c("Overall Mean Flow", 
                     "Jan. Mean Flow", "Feb. Mean Flow",
                     "Mar. Mean Flow", "Apr. Mean Flow",
                     "May Mean Flow", "Jun. Mean Flow",
                     "Jul. Mean Flow", "Aug. Mean Flow",
                     "Sep. Mean Flow", "Oct. Mean Flow",
                     "Nov. Mean Flow", "Dec. Mean Flow");
write.csv(Table1, paste0(container,"\\results\\user's_results\\",siteNo, "_vs_", RivSeg,"\\Tables",
                         "\\Tab. 3 - Monthly Mean Flows.csv"));

# Table 2: Monthly Low Flow
Table2 <- matrix(c(met01_Gage_JanLF, met02_Gage_FebLF, met03_Gage_MarLF,
                   met04_Gage_AprLF, met05_Gage_MayLF, met06_Gage_JunLF,
                   met07_Gage_JulLF, met08_Gage_AugLF, met09_Gage_SepLF,
                   met10_Gage_OctLF, met11_Gage_NovLF, met12_Gage_DecLF,
                   met01_Mod_JanLF, met02_Mod_FebLF, met03_Mod_MarLF, 
                   met04_Mod_AprLF, met05_Mod_MayLF, met06_Mod_JunLF,
                   met07_Mod_JulLF, met08_Mod_AugLF, met09_Mod_SepLF,
                   met10_Mod_OctLF, met11_Mod_NovLF, met12_Mod_DecLF,
                   met01_PctError, met02_PctError, met03_PctError,
                   met04_PctError, met05_PctError, met06_PctError,
                   met07_PctError, met08_PctError, met09_PctError,
                   met10_PctError, met11_PctError, met12_PctError),
                 nrow = 12, ncol = 3);
colnames(Table2) = c("USGS Gage", "Model", "Pct. Error");
rownames(Table2) = c("Jan. Low Flow", "Feb. Low Flow",
                     "Mar. Low Flow", "Apr. Low Flow",
                     "May Low Flow", "Jun. Low Flow",
                     "Jul. Low Flow", "Aug. Low Flow",
                     "Sep. Low Flow", "Oct. Low Flow",
                     "Nov. Low Flow", "Dec. Low Flow");
write.csv(Table2, paste0(container,"\\results\\user's_results\\",siteNo, "_vs_", RivSeg,"\\Tables",
                         "\\Tab. 2 - Monthly Low Flows.csv"));

# Table 3: Monthly High Flow
Table3 <- matrix(c(met45_Gage_JanHF, met46_Gage_FebHF, met47_Gage_MarHF,
                   met48_Gage_AprHF, met49_Gage_MayHF, met50_Gage_JunHF,
                   met51_Gage_JulHF, met52_Gage_AugHF, met53_Gage_SepHF,
                   met54_Gage_OctHF, met55_Gage_NovHF, met56_Gage_DecHF,
                   met45_Mod_JanHF, met46_Mod_FebHF, met47_Mod_MarHF,
                   met48_Mod_AprHF, met49_Mod_MayHF, met50_Mod_JunHF,
                   met51_Mod_JulHF, met52_Mod_AugHF, met53_Mod_SepHF,
                   met54_Mod_OctHF, met55_Mod_NovHF, met56_Mod_DecHF,
                   met45_PctError, met46_PctError, met47_PctError,
                   met48_PctError, met49_PctError, met50_PctError,
                   met51_PctError, met52_PctError, met53_PctError,
                   met54_PctError, met55_PctError, met56_PctError),
                 nrow = 12, ncol = 3);
colnames(Table3) = c("USGS Gage", "Model", "Pct. Error");
rownames(Table3) = c("Jan. High Flow", "Feb. High Flow",
                     "Mar. High Flow", "Apr. High Flow",
                     "May High Flow", "Jun. High Flow",
                     "Jul. High Flow", "Aug. High Flow",
                     "Sep. High Flow", "Oct. High Flow",
                     "Nov. High Flow", "Dec. High Flow");
write.csv(Table3, paste0(container,"\\results\\user's_results\\",siteNo, "_vs_", RivSeg,"\\Tables",
                         "\\Tab. 5 - Monthly High Flows.csv"));

# Table 4: Period Low Flows
Table4 <- matrix(c(met25_Gage_1DayMinMin, met26_Gage_1DayMinMed,
                   met27_Gage_3DayMinMin, met28_Gage_3DayMinMed,
                   met29_Gage_7DayMinMin, met30_Gage_7DayMinMed,
                   met31_Gage_30DayMinMin, met32_Gage_30DayMinMed,
                   met33_Gage_90DayMinMin, met34_Gage_90DayMinMed,
                   met35_Gage_7Q10, met37_Gage_DoR, met67_USGS_lfmin,
                   met44_Gage_Base, met25_Mod_1DayMinMin, met26_Mod_1DayMinMed,
                   met27_Mod_3DayMinMin, met28_Mod_3DayMinMed,
                   met29_Mod_7DayMinMin, met30_Mod_7DayMinMed,
                   met31_Mod_30DayMinMin, met32_Mod_30DayMinMed,
                   met33_Mod_90DayMinMin, met34_Mod_90DayMinMed,
                   met35_Model_7Q10, met37_Model_DoR,
                   met67_model_lfmin,  met44_Model_Base,
                   met25_PctError, met26_PctError, met27_PctError,
                   met28_PctError, met29_PctError, met30_PctError,
                   met31_PctError, met32_PctError, met33_PctError,
                   met34_PctError, met35_PctError,
                   met37_PctError, met67_PctError, met44_PctError), nrow = 14, ncol = 3);
colnames(Table4) = c("USGS Gage", "Model", "Pct. Error");
rownames(Table4) = c("Min. 1 Day Min", "Med. 1 Day Min", 
                     "Min. 3 Day Min", "Med. 3 Day Min",
                     "Min. 7 Day Min", "Med. 7 Day Min",
                     "Min. 30 Day Min", "Med. 30 Day Min",
                     "Min. 90 Day Min", "Med. 90 Day Min", 
                     "7Q10", "Year of 90-Day Low Flow", "Drought Year Mean",
                     "Mean Baseflow");
write.csv(Table4, paste0(container,"\\results\\user's_results\\",siteNo, "_vs_", RivSeg,"\\Tables",
                         "\\Tab. 1 - Low Flow Calculations.csv"));

# Table 5: Period High Flows
Table5 <- matrix(c(met57_Gage_1DayMaxMax, met58_Gage_1DayMaxMed,
                   met59_Gage_3DayMaxMax, met60_Gage_3DayMaxMed,
                   met61_Gage_7DayMaxMax, met62_Gage_7DayMaxMed,
                   met63_Gage_30DayMaxMax, met64_Gage_30DayMaxMed,
                   met65_Gage_90DayMaxMax, met66_Gage_90DayMaxMed,
                   met57_Mod_1DayMaxMax, met58_Mod_1DayMaxMed,
                   met59_Mod_3DayMaxMax, met60_Mod_3DayMaxMed,
                   met61_Mod_7DayMaxMax, met62_Mod_7DayMaxMed,
                   met63_Mod_30DayMaxMax, met64_Mod_30DayMaxMed,
                   met65_Mod_90DayMaxMax, met66_Mod_90DayMaxMed,
                   met57_PctError, met58_PctError,
                   met59_PctError, met60_PctError,
                   met61_PctError, met62_PctError,
                   met63_PctError, met64_PctError,
                   met65_PctError, met66_PctError), 
                 nrow = 10, ncol = 3);
colnames(Table5) = c("USGS Gage", "Model", "Pct. Error");
rownames(Table5) = c("Max. 1 Day Max", "Med. 1 Day Max", 
                     "Max. 3 Day Max", "Med. 3 Day Max",
                     "Max. 7 Day Max", "Med. 7 Day Max",
                     "Max. 30 Day Max", "Med. 30 Day Max",
                     "Max. 90 Day Max", "Med. 90 Day Max");
write.csv(Table5, paste0(container,"\\results\\user's_results\\",siteNo, "_vs_", RivSeg,"\\Tables",
                         "\\Tab. 4 - High Flow Calculations.csv"));

# Table 6: Non-Exceedance Flows
Table6 <- matrix(c(met38_Gage_1NonEx, met39_Gage_5NonEx, met40_Gage_50NonEx,
                   met41_Gage_95NonEx, met42_Gage_99NonEx, met43_Gage_Sep10,
                   met38_Model_1NonEx, met39_Model_5NonEx, met40_Model_50NonEx,
                   met41_Model_95NonEx, met42_Model_99NonEx, met43_Model_Sep10,
                   met38_PctError, met39_PctError, met40_PctError,
                   met41_PctError, met42_PctError, met43_PctError), nrow = 6, ncol = 3);
colnames(Table6) = c("USGS Gage", "Model", "Pct. Error");
rownames(Table6) = c("1% Non-Exceedance", "5% Non-Exceedance",
                     "50% Non-Exceedance", "95% Non-Exceedance",
                     "99% Non-Exceedance", "Sept. 10% Non-Exceedance");
write.csv(Table6, paste0(container,"\\results\\user's_results\\",siteNo, "_vs_", RivSeg,"\\Tables",
                         "\\Tab. 6 - Non-Exceedance Flows.csv"))
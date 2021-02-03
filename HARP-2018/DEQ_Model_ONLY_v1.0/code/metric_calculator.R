# DOCUMENTATION -----------------------------------------------------------

# Calculates all flow metrics for model data, exports a table
# containing all metrics to the "results" folder

# LIBRARIES ---------------------------------------------------------------

library('IHA')
library('PearsonDS')
library('zoo')
library('lubridate')
library('lfstat')

# INPUTS ------------------------------------------------------------------

# Address of "DEQ_Model_ONLY_v..." folder
# Include "DEQ_Model_ONLY_v..." in address!
container <- "C:\\Users\\Daniel\\Documents\\HARP\\DEQ_Model_ONLY_v1.0"

# River Segment ID
RivSeg <- "TU3_9230_9260"

# Should new or original data be used?
new.or.original <- "new"

# CARRYOVER IF MASTER IS BEING RUN ----------------------------------------
if (exists("container.master") == TRUE) {
  container <- container.master
  RivSeg <- RivSeg.master
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

# SOURCING CALCULATING FUNCTION -------------------------------------------

source(paste0(container, "\\code\\function_holder.R"));

# CREATE FOLDER -----------------------------------------------------------

dir.create(paste0(container,"\\results\\user's_results\\"), showWarnings = FALSE)

# LOADING DATA ------------------------------------------------------------

data <- read.csv(paste0(container, container.cont, "\\trimmed_data\\",RivSeg, " - Derived Data.csv"))
data <- data[,c('date','mod.flow')]
colnames(data) <- c('date', 'model.flow')


# ADDING ADDITIONAL DATA COLUMNS ------------------------------------------
data$year <- year(ymd(data$date))
data$month <- month(ymd(data$date))
data$day <- day(ymd(data$date))

# SETUP FOR CALCULATIONS --------------------------------------------------

# Setup for ___ Day Min Calculations
# Running model calculations
f3_model <- zoo(data$model.flow, order.by = data$date)
g2_model <- group2(f3_model, year = 'water')

# Setup for Monthly Means Calculations
Model_Monthly_Means <- aggregate(data$model.flow, list(data$month), FUN = mean)

# Setup for Montly Mins Calculations
flows_model <- zoo(data$model.flow, order.by = data$date)

# Setup for Flow Exceedance Calculations
# Creating vectors of decreasing flow magnitude
dec_flows_model <- sort(data$model.flow, decreasing = TRUE)
# Determining the "rank" (0-1) of the flow value
num_observations <- as.numeric(length(data$date))
rank_vec <- as.numeric(c(1:num_observations))
# Calculating exceedance probability
prob_exceedance <- 100*((rank_vec) / (num_observations + 1))
# Creating vectors of calculated quantiles
model_prob_exceedance <- quantile(dec_flows_model, probs = c(0.01, 0.05, 0.5, 0.95, 0.99))

# Setup for Sept. 10% Flow
sept_flows <- subset(data, month == '9')
sept_quant_model <- quantile(sept_flows$model.flow, 0.10)

# Setup for Baseflow Calculations
model.data <- data.frame(data$day, data$month, data$year, data$model.flow)
names(model.data) <- c('day', 'month', 'year', 'flow')
# Creating lowflow objects
modelriver <- createlfobj(model.data, hyearstart = 10, baseflow = TRUE, meta = NULL)
baseflowriver<- data.frame(modelriver);
colnames(baseflowriver) <-c ('mday', 'mmonth', 'myear', 'mflow', 'mHyear', 'mBaseflow')

# removing NA values
baseflowriver<-baseflowriver[complete.cases(baseflowriver)==TRUE,]
modelriver<- data.frame(baseflowriver$mday, baseflowriver$mmonth, baseflowriver$myear, 
                        baseflowriver$mflow, baseflowriver$mHyear, baseflowriver$mBaseflow)
names(modelriver) <- c('day', 'month', 'year', 'flow', 'hyear', 'baseflow')
# Adding date vectors
modelriver$date <- as.Date(paste0(modelriver$year,"-",modelriver$month,"-",modelriver$day))

# Setup for Low Flows
lf_model <- aggregate(modelriver$flow, by = list(modelriver$hyear), FUN = mean)
colnames(lf_model) <- c('Water Year', 'Mean Flow')

# CALCULATIONS ------------------------------------------------------------

# OVERALL MEAN FLOW
met00_Model_MeanFlow <- signif(mean(data$model.flow), digits=3);

# JANUARY LOW FLOW
met01_Mod_JanLF <- signif(fn_iha_mlf(flows_model,1), digits=3);

# FEBRUARY LOW FLOW
met02_Mod_FebLF <- signif(fn_iha_mlf(flows_model,2), digits=3);

# MARCH LOW FLOW
met03_Mod_MarLF <- signif(fn_iha_mlf(flows_model,3), digits=3);

# APRIL LOW FLOW
met04_Mod_AprLF <- signif(fn_iha_mlf(flows_model,4), digits=3);

# MAY LOW FLOW
met05_Mod_MayLF <- signif(fn_iha_mlf(flows_model,5), digits=3);

# JUNE LOW FLOW
met06_Mod_JunLF <- signif(fn_iha_mlf(flows_model,6), digits=3);

# JULY LOW FLOW
met07_Mod_JulLF <- signif(fn_iha_mlf(flows_model,7), digits=3);

# AUGUST LOW FLOW
met08_Mod_AugLF <- signif(fn_iha_mlf(flows_model,8), digits=3);

# SEPTEMBER LOW FLOW
met09_Mod_SepLF <- signif(fn_iha_mlf(flows_model,9), digits=3);

# OCTOBER LOW FLOW
met10_Mod_OctLF <- signif(fn_iha_mlf(flows_model,10), digits=3);

# NOVEMBER LOW FLOW
met11_Mod_NovLF <- signif(fn_iha_mlf(flows_model,11), digits=3);

# DECEMBER LOW FLOW
met12_Mod_DecLF <- signif(fn_iha_mlf(flows_model,12), digits=3);

# JANUARY MEAN FLOW
met13_Model_JanMF <- signif(Model_Monthly_Means[1,2], digits=3);

# FEBRUARY MEAN FLOW
met14_Model_FebMF <- signif(Model_Monthly_Means[2,2], digits=3);

# MARCH MEAN FLOW
met15_Model_MarMF <- signif(Model_Monthly_Means[3,2], digits=3);

# APRIL MEAN FLOW
met16_Model_AprMF <- signif(Model_Monthly_Means[4,2], digits=3);

# MAY MEAN FLOW
met17_Model_MayMF <- signif(Model_Monthly_Means[5,2], digits=3);

# JUNE MEAN FLOW
met18_Model_JunMF <- signif(Model_Monthly_Means[6,2], digits=3);

# JULY MEAN FLOW
met19_Model_JulMF <- signif(Model_Monthly_Means[7,2], digits=3);

# AUGUST MEAN FLOW
met20_Model_AugMF <- signif(Model_Monthly_Means[8,2], digits=3);

# SEPTEMBER MEAN FLOW
met21_Model_SepMF <- signif(Model_Monthly_Means[9,2], digits=3);

# OCTOBER MEAN FLOW
met22_Model_OctMF <- signif(Model_Monthly_Means[10,2], digits=3);

# NOVEMBER MEAN FLOW
met23_Model_NovMF <- signif(Model_Monthly_Means[11,2], digits=3);

# DECEMBER MEAN FLOW
met24_Model_DecMF <- signif(Model_Monthly_Means[12,2], digits=3);

# 1 DAY MIN
yearly_Mod_1DayMin <- g2_model[,c(1,2)];
met25_Mod_1DayMinMin <- signif(min(yearly_Mod_1DayMin$`1 Day Min`), digits=3);
met26_Mod_1DayMinMed <- signif(median(yearly_Mod_1DayMin$`1 Day Min`), digits=3);

# 3 DAY MIN
yearly_Mod_3DayMin <- g2_model[,c(1,4)];
met27_Mod_3DayMinMin <- signif(min(yearly_Mod_3DayMin$`3 Day Min`), digits=3);
met28_Mod_3DayMinMed <- signif(median(yearly_Mod_3DayMin$`3 Day Min`), digits=3);

# 7 DAY MIN
yearly_Mod_7DayMin <- g2_model[,c(1,6)];
met29_Mod_7DayMinMin <- signif(min(yearly_Mod_7DayMin$`7 Day Min`), digits=3);
met30_Mod_7DayMinMed <- signif(median(yearly_Mod_7DayMin$`7 Day Min`), digits=3);

# 30 DAY MIN
yearly_Mod_30DayMin <- g2_model[,c(1,8)];
met31_Mod_30DayMinMin <- signif(min(yearly_Mod_30DayMin$`30 Day Min`), digits=3);
met32_Mod_30DayMinMed <- signif(median(yearly_Mod_30DayMin$`30 Day Min`), digits=3);

# 90 DAY MIN
yearly_Mod_90DayMin <- g2_model[,c(1,10)];
met33_Mod_90DayMinMin <- signif(min(yearly_Mod_90DayMin$`90 Day Min`), digits=3);
met34_Mod_90DayMinMed <- signif(median(yearly_Mod_90DayMin$`90 Day Min`), digits=3);

# 7Q10
met35_Model_7Q10 <- signif(fn_iha_7q10(flows_model), digits=3);

# DROUGHT OF RECORD (MIN. 90 DAY MIN.) YEAR
met37_Model_DoR <- fn_iha_DOR_Year(flows_model);

# 1% Non-exceedance Flow
met38_Model_1NonEx <- signif(model_prob_exceedance[["1%"]], digits=3);

# 5% Non-exceedance Flow
met39_Model_5NonEx <- signif(model_prob_exceedance[["5%"]], digits=3);

# 50% Non-exceedance Flow
met40_Model_50NonEx <- signif(model_prob_exceedance[["50%"]], digits=3);

# 95% Non-exceedance Flow
met41_Model_95NonEx <- signif(model_prob_exceedance[["95%"]], digits=3);

# 99% Non-exceedance Flow
met42_Model_99NonEx <- signif(model_prob_exceedance[["99%"]], digits=3);

# Sept. 10% Flow
met43_Model_Sep10 <- signif(sept_quant_model[["10%"]], digits=3);

# Baseflow (Average)
met44_Model_Base <- signif(mean(modelriver$baseflow), digits=3);

# JANUARY HIGH FLOW
met45_Mod_JanHF <- signif(fn_iha_mhf(flows_model,1), digits=3);

# FEBRUARY HIGH FLOW
met46_Mod_FebHF <- signif(fn_iha_mhf(flows_model,2), digits=3);

# MARCH HIGH FLOW
met47_Mod_MarHF <- signif(fn_iha_mhf(flows_model,3), digits=3);

# APRIL HIGH FLOW
met48_Mod_AprHF <- signif(fn_iha_mhf(flows_model,4), digits=3);

# MAY HIGH FLOW
met49_Mod_MayHF <- signif(fn_iha_mhf(flows_model,5), digits=3);

# JUNE HIGH FLOW
met50_Mod_JunHF <- signif(fn_iha_mhf(flows_model,6), digits=3);

# JULY HIGH FLOW
met51_Mod_JulHF <- signif(fn_iha_mhf(flows_model,7), digits=3);

# AUGUST HIGH FLOW
met52_Mod_AugHF <- signif(fn_iha_mhf(flows_model,8), digits=3);

# SEPTEMBER HIGH FLOW
met53_Mod_SepHF <- signif(fn_iha_mhf(flows_model,9), digits=3);

# OCTOBER HIGH FLOW
met54_Mod_OctHF <- signif(fn_iha_mhf(flows_model,10), digits=3);

# NOVEMBER HIGH FLOW
met55_Mod_NovHF <- signif(fn_iha_mhf(flows_model,11), digits=3);

# DECEMBER HIGH FLOW
met56_Mod_DecHF <- signif(fn_iha_mhf(flows_model,12), digits=3);

# 1 DAY MAX
yearly_Mod_1DayMax <- g2_model[,c(1,3)];
met57_Mod_1DayMaxMax <- signif(max(yearly_Mod_1DayMax$`1 Day Max`), digits=3);
met58_Mod_1DayMaxMed <- signif(median(yearly_Mod_1DayMax$`1 Day Max`), digits=3);

# 3 DAY MAX
yearly_Mod_3DayMax <- g2_model[,c(1,5)];
met59_Mod_3DayMaxMax <- signif(max(yearly_Mod_3DayMax$`3 Day Max`), digits=3);
met60_Mod_3DayMaxMed <- signif(median(yearly_Mod_3DayMax$`3 Day Max`), digits=3);

# 7 DAY MAX
yearly_Mod_7DayMax <- g2_model[,c(1,7)];
met61_Mod_7DayMaxMax <- signif(max(yearly_Mod_7DayMax$`7 Day Max`), digits=3);
met62_Mod_7DayMaxMed <- signif(median(yearly_Mod_7DayMax$`7 Day Max`), digits=3);

# 30 DAY MAX
yearly_Mod_30DayMax <- g2_model[,c(1,9)];
met63_Mod_30DayMaxMax <- signif(max(yearly_Mod_30DayMax$`30 Day Max`), digits=3);
met64_Mod_30DayMaxMed <- signif(median(yearly_Mod_30DayMax$`30 Day Max`), digits=3);

# 90 DAY MAX
yearly_Mod_90DayMax <- g2_model[,c(1,11)];
met65_Mod_90DayMaxMax <- signif(max(yearly_Mod_90DayMax$`90 Day Max`), digits=3);
met66_Mod_90DayMaxMed <- signif(median(yearly_Mod_90DayMax$`90 Day Max`), digits=3);

# LOW YEARLY MEAN
lf_model_flow <- which(lf_model$`Water Year`== met37_Model_DoR)
lf_model_flow <- lf_model$`Mean Flow`[lf_model_flow]
met67_Mod_LFMin <- lf_model_flow

# OUTPUTTING MATRICES -----
dir.create(paste0(container,"\\results\\user's_results\\",RivSeg), showWarnings = FALSE)
# All metrics, in a row
ALL_METRICS <- matrix(c(met00_Model_MeanFlow, 
                        met01_Mod_JanLF, 
                        met02_Mod_FebLF,
                        met03_Mod_MarLF,
                        met04_Mod_AprLF, 
                        met05_Mod_MayLF,
                        met06_Mod_JunLF, 
                        met07_Mod_JulLF,
                        met08_Mod_AugLF,
                        met09_Mod_SepLF,
                        met10_Mod_OctLF,
                        met11_Mod_NovLF,
                        met12_Mod_DecLF,
                        met13_Model_JanMF,
                        met14_Model_FebMF,
                        met15_Model_MarMF,
                        met16_Model_AprMF,
                        met17_Model_MayMF,
                        met18_Model_JunMF,
                        met19_Model_JulMF,
                        met20_Model_AugMF,
                        met21_Model_SepMF,
                        met22_Model_OctMF,
                        met23_Model_NovMF,
                        met24_Model_DecMF,
                        met25_Mod_1DayMinMin,
                        met26_Mod_1DayMinMed,
                        met27_Mod_3DayMinMin,
                        met28_Mod_3DayMinMed,
                        met29_Mod_7DayMinMin,
                        met30_Mod_7DayMinMed,
                        met31_Mod_30DayMinMin,
                        met32_Mod_30DayMinMed,
                        met33_Mod_90DayMinMin,
                        met34_Mod_90DayMinMed,
                        met35_Model_7Q10,
                        met37_Model_DoR,
                        met38_Model_1NonEx,
                        met39_Model_5NonEx,
                        met40_Model_50NonEx,
                        met41_Model_95NonEx,
                        met42_Model_99NonEx,
                        met43_Model_Sep10,
                        met44_Model_Base,
                        met45_Mod_JanHF,
                        met46_Mod_FebHF,
                        met47_Mod_MarHF,
                        met48_Mod_AprHF,
                        met49_Mod_MayHF,
                        met50_Mod_JunHF,
                        met51_Mod_JulHF,
                        met52_Mod_AugHF,
                        met53_Mod_SepHF,
                        met54_Mod_OctHF,
                        met55_Mod_NovHF,
                        met56_Mod_DecHF,
                        met57_Mod_1DayMaxMax,
                        met58_Mod_1DayMaxMed,
                        met59_Mod_3DayMaxMax,
                        met60_Mod_3DayMaxMed,
                        met61_Mod_7DayMaxMax,
                        met62_Mod_7DayMaxMed,
                        met63_Mod_30DayMaxMax,
                        met64_Mod_30DayMaxMed,
                        met65_Mod_90DayMaxMax,
                        met66_Mod_90DayMaxMed,
                        met67_Mod_LFMin),
                      nrow = 1, ncol = 67);
rownames(ALL_METRICS) = c("Model");
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
write.csv(ALL_METRICS, paste0(container,"\\results\\user's_results\\",RivSeg,
                              "\\All_Metrics.csv"));

# Table 1: Monthly Average Flow
Table1 <- matrix(c(met00_Model_MeanFlow, met13_Model_JanMF,
                   met14_Model_FebMF, met15_Model_MarMF, met16_Model_AprMF,
                   met17_Model_MayMF, met18_Model_JunMF, met19_Model_JulMF,
                   met20_Model_AugMF, met21_Model_SepMF, met22_Model_OctMF,
                   met23_Model_NovMF, met24_Model_DecMF),
                 nrow = 13, ncol = 1);
colnames(Table1) = c("Model");
rownames(Table1) = c("Overall Mean Flow", 
                     "Jan. Mean Flow", "Feb. Mean Flow",
                     "Mar. Mean Flow", "Apr. Mean Flow",
                     "May Mean Flow", "Jun. Mean Flow",
                     "Jul. Mean Flow", "Aug. Mean Flow",
                     "Sep. Mean Flow", "Oct. Mean Flow",
                     "Nov. Mean Flow", "Dec. Mean Flow");
write.csv(Table1, paste0(container,"\\results\\user's_results\\",RivSeg,
                         "\\Tab. 3 - Monthly Mean Flows.csv"));

# Table 2: Monthly Low Flow
Table2 <- matrix(c(met01_Mod_JanLF, met02_Mod_FebLF, met03_Mod_MarLF, 
                   met04_Mod_AprLF, met05_Mod_MayLF, met06_Mod_JunLF,
                   met07_Mod_JulLF, met08_Mod_AugLF, met09_Mod_SepLF,
                   met10_Mod_OctLF, met11_Mod_NovLF, met12_Mod_DecLF),
                 nrow = 12, ncol = 1);
colnames(Table2) = c("Model");
rownames(Table2) = c("Jan. Low Flow", "Feb. Low Flow",
                     "Mar. Low Flow", "Apr. Low Flow",
                     "May Low Flow", "Jun. Low Flow",
                     "Jul. Low Flow", "Aug. Low Flow",
                     "Sep. Low Flow", "Oct. Low Flow",
                     "Nov. Low Flow", "Dec. Low Flow");
write.csv(Table2, paste0(container,"\\results\\user's_results\\",RivSeg,
                         "\\Tab. 2 - Monthly Low Flows.csv"));

# Table 3: Monthly High Flow
Table3 <- matrix(c(met45_Mod_JanHF, met46_Mod_FebHF, met47_Mod_MarHF,
                   met48_Mod_AprHF, met49_Mod_MayHF, met50_Mod_JunHF,
                   met51_Mod_JulHF, met52_Mod_AugHF, met53_Mod_SepHF,
                   met54_Mod_OctHF, met55_Mod_NovHF, met56_Mod_DecHF),
                 nrow = 12, ncol = 1);
colnames(Table3) = c("Model");
rownames(Table3) = c("Jan. High Flow", "Feb. High Flow",
                     "Mar. High Flow", "Apr. High Flow",
                     "May High Flow", "Jun. High Flow",
                     "Jul. High Flow", "Aug. High Flow",
                     "Sep. High Flow", "Oct. High Flow",
                     "Nov. High Flow", "Dec. High Flow");
write.csv(Table3, paste0(container,"\\results\\user's_results\\",RivSeg,
                         "\\Tab. 5 - Monthly High Flows.csv"));

# Table 4: Period Low Flows
Table4 <- matrix(c(met25_Mod_1DayMinMin, met26_Mod_1DayMinMed,
                   met27_Mod_3DayMinMin, met28_Mod_3DayMinMed,
                   met29_Mod_7DayMinMin, met30_Mod_7DayMinMed,
                   met31_Mod_30DayMinMin, met32_Mod_30DayMinMed,
                   met33_Mod_90DayMinMin, met34_Mod_90DayMinMed,
                   met35_Model_7Q10, met37_Model_DoR,
                   met67_Mod_LFMin,  met44_Model_Base), 
                 nrow = 14, ncol = 1);
colnames(Table4) = c("Model");
rownames(Table4) = c("Min. 1 Day Min", "Med. 1 Day Min", 
                     "Min. 3 Day Min", "Med. 3 Day Min",
                     "Min. 7 Day Min", "Med. 7 Day Min",
                     "Min. 30 Day Min", "Med. 30 Day Min",
                     "Min. 90 Day Min", "Med. 90 Day Min", 
                     "7Q10", "Year of 90-Day Low Flow", "Drought Year Mean",
                     "Mean Baseflow");
write.csv(Table4, paste0(container,"\\results\\user's_results\\",RivSeg,
                         "\\Tab. 1 - Low Flow Calculations.csv"));

# Table 5: Period High Flows
Table5 <- matrix(c(met57_Mod_1DayMaxMax, met58_Mod_1DayMaxMed,
                   met59_Mod_3DayMaxMax, met60_Mod_3DayMaxMed,
                   met61_Mod_7DayMaxMax, met62_Mod_7DayMaxMed,
                   met63_Mod_30DayMaxMax, met64_Mod_30DayMaxMed,
                   met65_Mod_90DayMaxMax, met66_Mod_90DayMaxMed), 
                 nrow = 10, ncol = 1);
colnames(Table5) = c("Model");
rownames(Table5) = c("Max. 1 Day Max", "Med. 1 Day Max", 
                     "Max. 3 Day Max", "Med. 3 Day Max",
                     "Max. 7 Day Max", "Med. 7 Day Max",
                     "Max. 30 Day Max", "Med. 30 Day Max",
                     "Max. 90 Day Max", "Med. 90 Day Max");
write.csv(Table5, paste0(container,"\\results\\user's_results\\",RivSeg,
                         "\\Tab. 4 - High Flow Calculations.csv"));

# Table 6: Non-Exceedance Flows
Table6 <- matrix(c(met38_Model_1NonEx, met39_Model_5NonEx, met40_Model_50NonEx,
                   met41_Model_95NonEx, met42_Model_99NonEx, met43_Model_Sep10), 
                 nrow = 6, ncol = 1);
colnames(Table6) = c("Model");
rownames(Table6) = c("1% Non-Exceedance", "5% Non-Exceedance",
                     "50% Non-Exceedance", "95% Non-Exceedance",
                     "99% Non-Exceedance", "Sept. 10% Non-Exceedance");
write.csv(Table6, paste0(container,"\\results\\user's_results\\",RivSeg,
                         "\\Tab. 6 - Non-Exceedance Flows.csv"))
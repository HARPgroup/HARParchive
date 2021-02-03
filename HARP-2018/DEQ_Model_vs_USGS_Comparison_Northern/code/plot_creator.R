# DOCUMENTATION -----------------------------------------------------------

# Creates plots based on various metrics

# LIBRARIES ---------------------------------------------------------------

library('lfstat')
library('ggplot2')
library('scales')
library('lubridate')
library('IHA')

# INPUTS ------------------------------------------------------------------

# Address of "DEQ_Model_vs_USGS_Comparison" folder
# Include "DEQ_Model_vs_USGS_Comparison" in address!
container <- "C:\\Users\\danie\\Downloads\\Downloads\\HARP\\GitHub\\hydro-tools\\HARP-2018\\DEQ_Model_vs_USGS_Comparison_Northern"

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
gage.to.segment <- subset(gage.to.segment, gage.to.segment$gage == as.numeric(siteNo))
RivSeg <- gage.to.segment$segment

# CREATE FOLDER -----------------------------------------------------------

dir.create(paste0(container,"\\results\\user's_results\\",siteNo, "_vs_", RivSeg, "\\Plots"), showWarnings = TRUE)

# LOADING DATA ------------------------------------------------------------

data <- read.csv(paste0(container, container.cont, "\\derived_data\\trimmed+area-adjusted_data\\",siteNo,"_vs_",RivSeg, " - Derived Data.csv"))

# PLOTTING -----
# Creates directory to store plots and tables
dir.create(paste0(container,"\\results\\user's_results\\",siteNo,"_vs_",RivSeg), showWarnings = FALSE);

# Creating names for plot legends
name_USGS <- paste('Gage', siteNo);
name_model <- paste('Model: River Seg.\n', RivSeg);

# CREATE FUNCTIONS FOR PLOTTING  ------------------------------------------
base_breaks <- function(n = 10){
  function(x) {
    axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
  }
}
scaleFUN <- function(x) sprintf("%.0f", x)


# Basic hydrograph -----
# Max/min for y axis scaling
max <- max(c(max(data$gage.flow), max(data$model.flow)));
min <- min(c(min(data$gage.flow), max(data$model.flow)));
if (max > 10000){
  max <- 100000
}else if (max > 1000){
  max <- 10000
}else if (max > 100){
  max <- 1000
}else if (max > 10){
  max <- 100
}
if (min>100){
  min<-100
}else if (min>10){ 
  min<-10
}else 
  min<-1
if (min==100){
  fixtheyscale<- scale_y_continuous(trans = log_trans(), 
                                    breaks = c(100, 1000, 10000, 100000), 
                                    limits=c(min,max))
}else if (min==10){
  fixtheyscale<- scale_y_continuous(trans = log_trans(), 
                                    breaks = c(10, 100, 1000, 10000), 
                                    limits=c(min,max))
}else if (min==1){
  fixtheyscale<- scale_y_continuous(trans = log_trans(), 
                                    breaks = c(1, 10, 100, 1000, 10000), 
                                    limits=c(min,max))
}else
  fixtheyscale<- scale_y_continuous(trans = log_trans(), breaks = base_breaks(), 
                                    labels=scaleFUN, limits=c(min,max))

# Creating and exporting plot
png(filename=paste0(container,"\\results\\user's_results\\",siteNo,"_vs_",RivSeg,
                    "\\plots\\Fig. 2 - Hydrograph.png"),
    width = 1400, height = 950, units = "px");

df <- data.frame(as.Date(data$date), data$model.flow, data$gage.flow); 
colnames(df) <- c('Date', 'Model', 'USGS')
options(scipen=5, width = 1400, height = 950)
myplot <- ggplot(df, aes(x=Date)) + 
  geom_line(aes(y=USGS, color=name_USGS), size=1) +
  geom_line(aes(y=Model, color=name_model), size=1)+
  fixtheyscale+
  theme_bw()+ 
  theme(legend.position="top", 
        legend.title=element_blank(),
        legend.box = "horizontal", 
        legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid", 
                                         colour ="white"),
        legend.text=element_text(size=30),
        axis.text=element_text(size=36, colour="black"),
        axis.title=element_text(size=36, colour="black"),
        axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid"),
        axis.ticks = element_line(colour="black"),
        panel.grid.major=element_line(colour = "light grey"), 
        panel.grid.minor=element_blank())+
  scale_colour_manual(values=c("black","red"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  labs(y = "Flow (cfs)")
print(myplot)
dev.off()         


# Setup for Residuals
resid <- (data$model.flow - data$gage.flow)
resid <- data.frame(data$date, resid)

# Residuals plot for hydrograph

zeroline <- rep_len(0, length(data$date)) 
quantresid <- data.frame(signif(quantile(resid$resid), digits=3))
min <- min(resid$resid)
max <- max(resid$resid)
names(quantresid) <- c('Percentiles')

png(filename=paste0(container,"\\results\\user's_results\\",siteNo,"_vs_",RivSeg,
                    "\\plots\\Fig. 3 - Residuals.png"),
    width = 1400, height = 950, units = "px");

df <- data.frame(as.Date(resid$data.date), resid$resid, zeroline); 
colnames(df) <- c('Date', 'Residual', 'Zeroline')
options(scipen=5, width = 1400, height = 950)
myplot <- ggplot(df, aes(x=Date)) + 
  geom_point(aes(y=Residual, color=name_USGS), size=3.5) +
  geom_line(aes(y=Zeroline, color=name_model), size=1)+
  scale_y_continuous(limits=c(min,max))+ 
  theme_bw()+ 
  theme(legend.position="top", 
        legend.title=element_blank(),
        legend.box = "horizontal", 
        legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid", 
                                         colour ="white"),
        legend.text=element_text(size=30),
        axis.text=element_text(size=36, colour="black"),
        axis.title=element_text(size=36, colour="black"),
        axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid"),
        axis.ticks = element_line(colour="black"),
        panel.grid.major=element_line(colour = "light grey"), 
        panel.grid.minor=element_blank())+
  scale_colour_manual(values=c("dark green","black"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  labs(y = "Flow Difference(cfs)")
print(myplot)
dev.off()

# text1 <- paste0('Quartiles: ', '0%:',quantresid$Percentiles[1], ',  ',
# '25%:',quantresid$Percentiles[2])
# text2 <- paste0('Quartiles: 50%:',quantresid$Percentiles[3], ',  ', 
# '75%: ',quantresid$Percentiles[4], ',  ', 
# '100%: ',quantresid$Percentiles[5]) 
# mtext(text1, side=1, line=2, outer = FALSE, at = NA,
#       adj = 0, padj = NA, cex = 2, col = 'black', font = NA)
# mtext(text2, side=1, line=3, outer = FALSE, at = NA,
#       adj = 0, padj = NA, cex = 2, col = 'black', font = NA)
#      dev.off();

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

# Flow exceedance plot -----
# Determining max flow value for exceedance plot scale
max <- max(c(max(dec_flows_model), max(dec_flows_USGS)));
min <- min(c(min(dec_flows_model), min(dec_flows_USGS)));

if (max > 10000){
  max <- 100000
}else if (max > 1000){
  max <- 10000
}else if (max > 100){
  max <- 1000
}else if (max > 10){
  max <- 100
}
if (min>100){
  min<-100
}else if (min>10){ 
  min<-10
}else 
  min<-1
if (min==100){
  fixtheyscale<- scale_y_continuous(trans = log_trans(), 
                                    breaks = c(100, 1000, 10000, 100000), 
                                    limits=c(min,max))
}else if (min==10){
  fixtheyscale<- scale_y_continuous(trans = log_trans(), 
                                    breaks = c(10, 100, 1000, 10000), 
                                    limits=c(min,max))
}else if (min==1){
  fixtheyscale<- scale_y_continuous(trans = log_trans(), 
                                    breaks = c(1, 10, 100, 1000, 10000), 
                                    limits=c(min,max))
}else
  fixtheyscale<- scale_y_continuous(trans = log_trans(), breaks = base_breaks(), 
                                    labels=scaleFUN, limits=c(min,max))
# Creating and exporting plot
png(filename=paste0(container,"\\results\\user's_results\\",siteNo,"_vs_",RivSeg,
                    "\\plots\\Fig. 4  - Prob. Exceedance.png"),
    width = 1400, height = 900, units = "px");


df <- data.frame(prob_exceedance, dec_flows_model, dec_flows_USGS); 
colnames(df) <- c('Date', 'Model', 'USGS')
options(scipen=5, width = 1400, height = 950)
myplot <- ggplot(df, aes(x=Date)) + 
  geom_line(aes(y=USGS, color=name_USGS), size=1) +
  geom_line(aes(y=Model, color=name_model), size=1)+
  fixtheyscale+ 
  theme_bw()+ 
  theme(legend.position="top", 
        legend.title=element_blank(),
        legend.box = "horizontal", 
        legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid", 
                                         colour ="white"),
        legend.text=element_text(size=30),
        axis.text=element_text(size=36, colour="black"),
        axis.title=element_text(size=36, colour="black"),
        axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid"),
        axis.ticks = element_line(colour="black"),
        panel.grid.major=element_line(colour = "light grey"), 
        panel.grid.minor=element_blank())+
  scale_colour_manual(values=c("black","red"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  labs(x= "Probability of Exceedance (%)", y = "Flow (cfs)")
print(myplot)
dev.off()

# Setup for Baseflow Calculations
data$year <- year(ymd(data$date))
data$month <- month(ymd(data$date))
data$day <- day(ymd(data$date))
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

# Baseflow Indiviudal Graph -----
# Determining max flow value for plot scale
max <- max(c(max(USGSriver$baseflow), max(modelriver$baseflow), max(USGSriver$flow), max(modelriver$flow)));
min <- min(c(min(USGSriver$baseflow), min(modelriver$baseflow), min(USGSriver$flow), min(modelriver$flow)));

if (max > 10000){
  max <- 100000
}else if (max > 1000){
  max <- 10000
}else if (max > 100){
  max <- 1000
}else if (max > 10){
  max <- 100
}
if (min>100){
  min<-100
}else if (min>10){ 
  min<-10
}else 
  min<-1
if (min==100){
  fixtheyscale<- scale_y_continuous(trans = log_trans(), 
                                    breaks = c(100, 1000, 10000, 100000), 
                                    limits=c(min,max))
}else if (min==10){
  fixtheyscale<- scale_y_continuous(trans = log_trans(), 
                                    breaks = c(10, 100, 1000, 10000), 
                                    limits=c(min,max))
}else if (min==1){
  fixtheyscale<- scale_y_continuous(trans = log_trans(), 
                                    breaks = c(1, 10, 100, 1000, 10000), 
                                    limits=c(min,max))
}else
  fixtheyscale<- scale_y_continuous(trans = log_trans(), breaks = base_breaks(), 
                                    labels=scaleFUN, limits=c(min,max))
# Creating and exporting plot
png(filename=paste0(container,"\\results\\user's_results\\",siteNo,"_vs_",RivSeg,
                    "\\plots\\Fig. 5 - Baseflow.png"),
    width = 1400, height = 900, units = "px");
par(mfrow = c(1,1));

which 
df <- data.frame(as.Date(modelriver$date), modelriver$baseflow, USGSriver$baseflow, modelriver$flow, USGSriver$flow); 
colnames(df) <- c('Date', 'ModelBaseflow', 'USGSBaseflow', 'ModelFlow', 'USGSFlow')
options(scipen=5, width = 1400, height = 950)
myplot <- ggplot(df, aes(x=Date)) + 
  geom_line(aes(y=USGSBaseflow, color=name_USGS), size=1) +
  geom_line(aes(y=ModelBaseflow, color=name_model), size=1)+
  fixtheyscale+ 
  theme_bw()+ 
  theme(legend.position="top", 
        legend.title=element_blank(),
        legend.box = "horizontal", 
        legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid", 
                                         colour ="white"),
        legend.text=element_text(size=30),
        axis.text=element_text(size=36, colour="black"),
        axis.title=element_text(size=36, colour="black"),
        axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid"),
        axis.ticks = element_line(colour="black"),
        panel.grid.major=element_line(colour = "light grey"), 
        panel.grid.minor=element_blank())+
  scale_colour_manual(values=c("black","red"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  labs(y = "Flow (cfs)")
print(myplot)
dev.off()      



# Baseflow Combined Graph -----

# Creating and exporting plot
png(filename=paste0(container,"\\results\\user's_results\\",siteNo,"_vs_",RivSeg,
                    "\\plots\\Fig. 6 - Baseflow and Flow.png"),
    width = 1400, height = 900, units = "px");
par(mfrow = c(1,1));

options(scipen=5, width = 1400, height = 950)
myplot <- ggplot(df, aes(x=Date)) + 
  geom_line(aes(y=USGSFlow, color=paste("Flow:", name_USGS)), size=1)+
  geom_line(aes(y=ModelFlow, color=paste("Flow:", name_model)), size=1)+ 
  geom_line(aes(y=USGSBaseflow, color=paste("Baseflow:", name_USGS)), size=1) +
  geom_line(aes(y=ModelBaseflow, color=paste("Baseflow:", name_model)), size=1)+
  fixtheyscale+ 
  theme_bw()+ 
  theme(legend.position="top", 
        legend.title=element_blank(),
        legend.box = "horizontal", 
        legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid", 
                                         colour ="white"),
        legend.text=element_text(size=20),
        axis.text=element_text(size=36, colour="black"),
        axis.title=element_text(size=36, colour="black"),
        axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid"),
        axis.ticks = element_line(colour="black"),
        panel.grid.major=element_line(colour = "light grey"), 
        panel.grid.minor=element_blank())+
  scale_colour_manual(values=c("black","red","grey", "light pink"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  labs(y = "Flow (cfs)")
print(myplot)
dev.off()


# Zoomed hydrograph in year of lowest 90-year flow -----
# Running gage calculations
f3_USGS <- zoo(data$gage.flow, order.by = data$date)
g2_USGS <- group2(f3_USGS, year = 'water')
# Running model calculations
f3_model <- zoo(data$model.flow, order.by = data$date)
g2_model <- group2(f3_model, year = 'water')

yearly_Gage_90DayMin <- g2_USGS[,c(1,10)];
yearly_Mod_90DayMin <- g2_model[,c(1,10)];
low.year <- subset(yearly_Gage_90DayMin, yearly_Gage_90DayMin$`90 Day Min`==min(yearly_Gage_90DayMin$`90 Day Min`));
low.year <- low.year$year;
low.year <- subset(data, data$year==low.year);

# Scaling using max/min
max <- max(c(max(low.year$gage.flow), max(low.year$model.flow)));
min <- min(c(min(low.year$gage.flow), min(low.year$model.flow)));
if (max > 10000){
  max <- 100000
}else if (max > 1000){
  max <- 10000
}else if (max > 100){
  max <- 1000
}else if (max > 10){
  max <- 100
}
if (min>100){
  min<-100
}else if (min>10){ 
  min<-10
}else 
  min<-1
if (min==100){
  fixtheyscale<- scale_y_continuous(trans = log_trans(), 
                                    breaks = c(100, 1000, 10000, 100000), 
                                    limits=c(min,max))
}else if (min==10){
  fixtheyscale<- scale_y_continuous(trans = log_trans(), 
                                    breaks = c(10, 100, 1000, 10000), 
                                    limits=c(min,max))
}else if (min==1){
  fixtheyscale<- scale_y_continuous(trans = log_trans(), 
                                    breaks = c(1, 10, 100, 1000, 10000), 
                                    limits=c(min,max))
}else
  fixtheyscale<- scale_y_continuous(trans = log_trans(), breaks = base_breaks(), 
                                    labels=scaleFUN, limits=c(min,max))
# Creating and exporting plot
png(filename=paste0(container,"\\results\\user's_results\\",siteNo,"_vs_",RivSeg,
                    "\\plots\\Fig. 7 - Zoomed Hydrograph.png"),
    width = 1400, height = 600, units = "px");

df <- data.frame(as.Date(low.year$date), low.year$model.flow, low.year$gage.flow); 
colnames(df) <- c('Date', 'Model', 'USGS')
options(scipen=5, width = 1400, height = 950)
myplot <- ggplot(df, aes(x=Date)) + 
  geom_line(aes(y=USGS, color=name_USGS), size=1) +
  geom_line(aes(y=Model, color=name_model), size=1)+
  fixtheyscale+ 
  theme_bw()+ 
  theme(legend.position="top", 
        legend.title=element_blank(),
        legend.box = "horizontal", 
        legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid", 
                                         colour ="white"),
        legend.text=element_text(size=30),
        axis.text=element_text(size=28, colour="black"),
        axis.title=element_text(size=28, colour="black"),
        axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid"),
        axis.ticks = element_line(colour="black"),
        panel.grid.major=element_line(colour = "light grey"), 
        panel.grid.minor=element_blank())+
  scale_colour_manual(values=c("black","red"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  labs(y = "Flow (cfs)")
print <- try(print(myplot))
if (class(print)=="try-error") {
  low.year2 <- subset(yearly_Gage_90DayMin, yearly_Gage_90DayMin$`90 Day Min`==sort(yearly_Gage_90DayMin$`90 Day Min`, TRUE)[2]);
  low.year2 <- low.year2$year;
  low.year2 <- subset(data, data$year==low.year2);
  
  # Scaling using max/min
  max2 <- max(c(max(low.year2$gage.flow), max(low.year2$model.flow)));
  min2 <- min(c(min(low.year2$gage.flow), min(low.year2$model.flow)));
  if (max2 > 10000){
    max2 <- 100000
  }else if (max2 > 1000){
    max2 <- 10000
  }else if (max2 > 100){
    max2 <- 1000
  }else if (max2 > 10){
    max2 <- 100
  }
  if (min2>100){
    min2<-100
  }else if (min2>10){ 
    min2<-10
  }else {
    min2<-1
  }
  if (min2==100){
    fixtheyscale2<- scale_y_continuous(trans = log_trans(), 
                                       breaks = c(100, 1000, 10000, 100000), 
                                       limits=c(min2,max2))
  }else if (min2==10){
    fixtheyscale2<- scale_y_continuous(trans = log_trans(), 
                                       breaks = c(10, 100, 1000, 10000), 
                                       limits=c(min2,max2))
  }else if (min2==1){
    fixtheyscale2<- scale_y_continuous(trans = log_trans(), 
                                       breaks = c(1, 10, 100, 1000, 10000), 
                                       limits=c(min2,max2))
  }else {
    fixtheyscale2<- scale_y_continuous(trans = log_trans(), breaks = base_breaks(), 
                                       labels=scaleFUN, limits=c(min2,max2))
  }
  # Creating and exporting plot
  png(filename=paste0(container,"\\results\\user's_results\\",siteNo,"_vs_",RivSeg,
                      "\\plots\\Fig. 7 - Zoomed Hydrograph.png"),
      width = 1400, height = 600, units = "px");
  
  df2 <- data.frame(as.Date(low.year2$date), low.year2$model.flow, low.year2$gage.flow); 
  colnames(df2) <- c('Date', 'Model', 'USGS')
  options(scipen=5, width = 1400, height = 950)
  myplot2 <- ggplot(df2, aes(x=Date)) + 
    geom_line(aes(y=USGS, color=name_USGS), size=1) +
    geom_line(aes(y=Model, color=name_model), size=1)+
    fixtheyscale2+ 
    theme_bw()+ 
    theme(legend.position="top", 
          legend.title=element_blank(),
          legend.box = "horizontal", 
          legend.background = element_rect(fill="white",
                                           size=0.5, linetype="solid", 
                                           colour ="white"),
          legend.text=element_text(size=30),
          axis.text=element_text(size=28, colour="black"),
          axis.title=element_text(size=28, colour="black"),
          axis.line = element_line(colour = "black", 
                                   size = 1, linetype = "solid"),
          axis.ticks = element_line(colour="black"),
          panel.grid.major=element_line(colour = "light grey"), 
          panel.grid.minor=element_blank())+
    scale_colour_manual(values=c("black","red"))+
    guides(colour = guide_legend(override.aes = list(size=5)))+
    labs(y = "Flow (cfs)")
  dev.off()
  print(myplot2)
} else {
  print(myplot)
}
dev.off()

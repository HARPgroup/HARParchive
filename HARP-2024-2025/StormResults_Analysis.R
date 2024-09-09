# Key for Gages:
# 01620500 = North River near Stokesville
# 01634500 = Cedar Creek near Winchester
# 01669000 = Piscataway Creek near Tappahannock
# 02021500 = Maury River at Rockbridge Falls
# 02031000 = Mechums River near White Hall
# 02044500 = Nottoway River near Rawlings
# 03176500 = New River at Glen Lyn
# 03165000 = Chesnut Creek at Galax
# 02054530 = Roanoke River at Glenvar
# 02034000 = Rivanna River at Palmyra

# sql, ggplot2 
library(sqldf)
library(ggplot2)
library(tidyverse)

##############################################################################
# STORM FILE DATA COMPILATION

list_of_files <- list.files(path="C:/Users/ilona/OneDrive - Virginia Tech/HARP/Storm Tests/Ratings/out",
                            recursive = TRUE,
                            pattern = "\\.csv$",
                            full.names = TRUE)
df <- read_csv(list_of_files, id="gageid")

df$gageid <- grep("0", unlist(strsplit(df$gageid, "_")), value = TRUE, perl = TRUE)
df$gageid <- grep("0", unlist(strsplit(df$gageid, "-")), value = TRUE, perl = TRUE)
df$...1 <- NULL

df$season <- ifelse (df$mo %in% c(6:8), 
                               "SUMMER",
                               ifelse (df$mo %in% c(9:11), 
                                       "AUTUMN",
                                       ifelse (df$mo %in% c(12,1,2), 
                                               "WINTER",
                                               ifelse (df$mo %in% c(3:5), 
                                                       "SPRING", NA))))

##############################################################################
# MONTHLY AND SEASONAL R-SQUARED PLOTS

ggplot(data = df)+
  aes(y = season, x=r_squared, color=season)+
  geom_boxplot()+
  xlim(0,1)+
  theme_bw()+
  xlab("R-Squared")+
  ylab("Season")+
  ggtitle("Seasonal R-Squared Values ()")+
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")


ggplot(data = df)+
  aes(y = mo, x=r_squared)+
  geom_boxplot(mapping = aes(group = mo))+
  xlim(0,1)+
  theme_bw()+
  xlab("R-Squared")+
  ylab("Month")+
  ggtitle("Monthly R-Squared Values ()")+
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")+
  scale_y_continuous(breaks = seq(0,12,1))


############################################################################
# Highest and lowest avg R-squared

gage_mean<- aggregate(x= df$r_squared,
                       # Specify group indicator
                       by = list(df$gageid),      
                       # Specify function (i.e. mean)
                       FUN = mean, 
                       na.rm = TRUE )
print(gage_mean)

ggplot(data = gage_mean,
       mapping = aes(x=x))+
  geom_density(color="dodgerblue3")+
  theme_bw()+
  ylab("Density")+
  xlab("R-Squared")+
  ggtitle("Mean R-Squared Value Density for each Gage")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlim(0,1)+
  geom_vline(xintercept = mean(gage_mean$x), lwd=0.5,colour="purple4",linetype = "dashed")

ggplot(data = df,
       mapping = aes(x=r_squared))+
  geom_density(color="indianred4")+
  theme_bw()+
  ylab("Density")+
  xlab("R-Squared")+
  ggtitle("Total R-Squared Value Density")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlim(0,1)+
  geom_vline(xintercept = mean(df$r_squared, na.rm = TRUE), lwd=0.5,colour="deeppink3",linetype = "dashed")


##############################################################################
# Highest R-squared Gage analysis

high_data <- read.csv("C:/Users/ilona/OneDrive - Virginia Tech/HARP/Storm Tests/Ratings/out/usgs_ws_01652500-PRISM-storm_volume-ratings.csv")

ggplot(data = high_data,
       mapping = aes(x=mo,y=r_squared))+
  geom_line(color="magenta4")+
  theme_bw()+
  ylim(0,1)+
  xlab("Month")+
  ylab("R-Squared")+
  ggtitle("Monthly R-Squared, Highest Mean")+
  theme(plot.title = element_text(hjust = 0.5))


#############################################################################
# Lowest R-squared Gage analysis

low_data <- read.csv("C:/Users/ilona/OneDrive - Virginia Tech/HARP/Storm Tests/Ratings/out/usgs_ws_01671025-PRISM-storm_volume-ratings.csv")

ggplot(data = low_data,
       mapping = aes(x=mo,y=r_squared))+
  geom_line(color="darkolivegreen3")+
  theme_bw()+
  ylim(-0.1,1)+
  xlab("Month")+
  ylab("R-Squared")+
  ggtitle("Monthly R-Squared, Lowest Mean")+
  theme(plot.title = element_text(hjust = 0.5))

#############################################################################

precip <- read.csv("C:/Users/ilona/OneDrive - Virginia Tech/HARP/Storm Tests/Peak Diff/usgs_ws_02024000-PRISM-daily.csv")
stormflow <- read.csv("C:/Users/ilona/OneDrive - Virginia Tech/HARP/Storm Tests/Peak Diff/usgs_ws_02024000-stormevent-flow.csv")

precip$obs_date <- as_date(precip$obs_date)
stormflow$timestamp <- as_date(stormflow$timestamp)

stormflow <- subset(stormflow, stormflow$timestamp >= min(precip$obs_date))
stormflow <- subset(stormflow, stormflow$timestamp <= max(precip$obs_date))

compdata <- sqldf(
  "select a.obs_date, a.precip_in,
  b.flow, b.stormID
  from precip as a
  left outer join stormflow as b
  on (
    a.yr = b.year
    and a.obs_date = b.timestamp
  )"
)



############################################################################
# Plot Ratings
ggplot(data=comp_ratings,
       mapping = aes(x=mo))+
  geom_line(aes(y=r_01620500), color="darkred")+
  geom_line(aes(y=r_01629500), color="red3")+
  geom_line(aes(y=r_01634500), color="firebrick1")+
  geom_line(aes(y=r_01643700), color="orangered1")+
  geom_line(aes(y=r_01662800), color="darkorange1")+
  geom_line(aes(y=r_01669000), color="orange1")+
  geom_line(aes(y=r_01669520), color="goldenrod1")+
  geom_line(aes(y=r_02018500), color="yellow2")+
  geom_line(aes(y=r_02021500), color="yellowgreen")+
  geom_line(aes(y=r_02024000), color="palegreen3")+
  geom_line(aes(y=r_02026000), color="darkslategray3")+
  geom_line(aes(y=r_02029000), color="deepskyblue3")+
  geom_line(aes(y=r_02031000), color="dodgerblue4")+
  geom_line(aes(y=r_02032250), color="darkslateblue")+
  geom_line(aes(y=r_02034000), color="purple4")+
  geom_line(aes(y=r_02033500), color="darkorchid4")+
  geom_line(aes(y=r_02051000), color="darkviolet")+
  geom_line(aes(y=r_02054530), color="darkmagenta")+
  geom_line(aes(y=r_03165000), color="deeppink3")+
  geom_line(aes(y=r_03176500), color="hotpink")+
  theme_bw()+
  xlab("Month")+
  ylab("R-Squared")+
  ggtitle("Monthly R-Squared")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(0,1)



ggplot(data=comp_ratings,
       mapping = aes(x=mo))+
  geom_line(aes(y=mean(r_01620500)), color="darkred")+
  geom_line(aes(y=mean(r_01629500)), color="red3")+
  geom_line(aes(y=mean(r_01634500)), color="firebrick1")+
  geom_line(aes(y=mean(r_01643700)), color="orangered1")+
  geom_line(aes(y=mean(r_01662800)), color="darkorange1")+
  geom_line(aes(y=mean(r_01669000)), color="orange1")+
  geom_line(aes(y=mean(r_01669520)), color="goldenrod1")+
  geom_line(aes(y=mean(r_02018500)), color="yellow2")+
  geom_line(aes(y=mean(r_02021500)), color="yellowgreen")+
  geom_line(aes(y=mean(r_02024000)), color="palegreen3")+
  geom_line(aes(y=mean(r_02026000)), color="darkslategray3")+
  geom_line(aes(y=mean(r_02029000)), color="deepskyblue3")+
  geom_line(aes(y=mean(r_02031000)), color="dodgerblue4")+
  geom_line(aes(y=mean(r_02032250)), color="darkslateblue")+
  geom_line(aes(y=mean(r_02034000)), color="purple4")+
  geom_line(aes(y=mean(r_02033500)), color="darkorchid4")+
  geom_line(aes(y=mean(r_02051000)), color="darkviolet")+
  geom_line(aes(y=mean(r_02054530)), color="darkmagenta")+
  geom_line(aes(y=mean(r_03165000)), color="deeppink3")+
  geom_line(aes(y=mean(r_03176500)), color="hotpink")+
  theme_bw()+
  xlab("Month")+
  ylab("R-Squared")+
  ggtitle("Monthly R-Squared")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(0.25,0.75)


ggplot(data=comp_ratings,
       mapping = aes(x=mo))+
  geom_line(aes(y=mean(r_03176500)), color="hotpink")


ggplot(data = comp_ratings,
       mapping = aes(x=mo))+
  geom_line(aes(y=r_01620500), color="red3")+
  geom_line(aes(y=r_01634500), color="orange2")+
  geom_line(aes(y=r_01669000), color="gold1")+
  geom_line(aes(y=r_02021500), color="yellowgreen")+
  geom_line(aes(y=r_02031000), color="darkturquoise")+
  geom_line(aes(y=r_02034000), color="dodgerblue")+
  geom_line(aes(y=r_02044500), color="dodgerblue4")+
  geom_line(aes(y=r_02054530), color="purple3")+
  geom_line(aes(y=r_03165000), color="orchid3")+
  geom_line(aes(y=r_03176500), color="hotpink")+
  theme_bw()+
  xlab("Month")+
  ylab("R-Squared")+
  ggtitle("Monthly R-Squared")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_rect(aes(xmin = 6, xmax = 8, ymin = 0.025, ymax = 0.55),
            fill="transparent", color="red", size=0.75)+
  geom_rect(aes(xmin = 3, xmax = 5, ymin = 0.55, ymax = 0.85),
            fill="transparent", color="red", size=0.75)+
  geom_rect(aes(xmin = 9.25, xmax = 10.75, ymin = 0.4, ymax = 0.85),
          fill="transparent", color="red", size=0.75)+
  geom_rect(aes(xmin = 1.5, xmax = 2.5, ymin = 0.45, ymax = 0.85),
            fill="transparent", color="red", size=0.75)+
  ylim(0,1)


# July Dip (> 5)
ggplot(data = comp_ratings,
       mapping = aes(x=mo))+
  geom_line(aes(y=r_01620500), color="red3")+
  geom_line(aes(y=r_01634500), color="orange2")+
  geom_line(aes(y=r_02021500), color="yellowgreen")+
  geom_line(aes(y=r_02044500), color="dodgerblue4")+
  geom_line(aes(y=r_02054530), color="purple3")+
  geom_line(aes(y=r_03165000), color="orchid3")+
  theme_bw()+
  xlab("Month")+
  ylab("R-Squared")+
  ggtitle("July Decrease in Monthly R-Squared")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlim(5,9)+
  ylim(0,1)

# April Peak (not > 5)
ggplot(data = comp_ratings,
       mapping = aes(x=mo))+
  geom_line(aes(y=r_02021500), color="yellowgreen")+
  geom_line(aes(y=r_02034000), color="dodgerblue")+
  geom_line(aes(y=r_02054530), color="purple3")+
  geom_line(aes(y=r_03165000), color="orchid3")+
  geom_line(aes(y=r_03176500), color="hotpink")+
  theme_bw()+
  xlab("Month")+
  ylab("R-Squared")+
  ggtitle("April Peak Monthly R-Squared")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlim(2,6)+
  ylim(0,1)


# February Peak (not > 5)
ggplot(data = comp_ratings,
       mapping = aes(x=mo))+
  geom_line(aes(y=r_01669000), color="gold1")+
  geom_line(aes(y=r_02031000), color="darkturquoise")+
  geom_line(aes(y=r_02054530), color="purple3")+
  geom_line(aes(y=r_03176500), color="hotpink")+
  theme_bw()+
  xlab("Month")+
  ylab("R-Squared")+
  ggtitle("February Peak Monthly R-Squared")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlim(1,4)+
  ylim(0,1)


# October Peak (not > 5)
ggplot(data = comp_ratings,
       mapping = aes(x=mo))+
  geom_line(aes(y=r_01620500), color="red3")+
  geom_line(aes(y=r_02021500), color="yellowgreen")+
  geom_line(aes(y=r_02031000), color="darkturquoise")+
  geom_line(aes(y=r_02044500), color="dodgerblue4")+
  geom_line(aes(y=r_03165000), color="orchid3")+
  theme_bw()+
  xlab("Month")+
  ylab("R-Squared")+
  ggtitle("October Peak Monthly R-Squared")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlim(8,12)+
  ylim(0,1)


# Data frame with monthly mean r-squared value
monthly_mean <- data.frame(rowMeans(comp_ratings[,2:11]))
monthly_mean$mo <- c(1:12)
monthly_mean$season <- ifelse (monthly_mean$mo %in% c(6:8), 
                        "SUMMER",
                        ifelse (monthly_mean$mo %in% c(9:11), 
                                "AUTUMN",
                                ifelse (monthly_mean$mo %in% c(12,1,2), 
                                        "WINTER",
                                        ifelse (monthly_mean$mo %in% c(3:5), 
                                                "SPRING", NA))))
monthly_mean <- monthly_mean[, c(2,1)]
colnames(monthly_mean)[2] <- "mean_r_squared"




# Add season to comp data
comp_ratings$season <- ifelse (comp_ratings$mo %in% c(6:8), 
                               "SUMMER",
                               ifelse (comp_ratings$mo %in% c(9:11), 
                                       "AUTUMN",
                                       ifelse (comp_ratings$mo %in% c(12,1,2), 
                                               "WINTER",
                                               ifelse (comp_ratings$mo %in% c(3:5), 
                                                       "SPRING", NA))))

library(ggbeeswarm)

ggplot(data = pivot_longer(comp_ratings,2:21))+
  aes(y = season, x=value, color=season)+
  geom_boxplot()+
  xlim(0,1)+
  theme_bw()+
  xlab("R-Squared")+
  ylab("Season")+
  ggtitle("Seasonal R-Squared Values")+
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")


ggplot(data = pivot_longer(comp_ratings,2:21))+
  aes(y = mo, x=value)+
  geom_boxplot(mapping = aes(group = mo))+
  xlim(0,1)+
  theme_bw()+
  xlab("R-Squared")+
  ylab("Month")+
  ggtitle("Monthly R-Squared Values")+
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")+
  scale_y_continuous(breaks = seq(0,12,1))




# Plot monthly average rsquared value
ggplot(data = monthly_mean,
       mapping = aes(x=mo))+
  geom_line(aes(y=mean_r_squared), color="hotpink3")+
  theme_bw()+
  xlab("Month")+
  ylab("Mean R-Squared")+
  ggtitle("Monthly Average R-Squared Values")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(0,1)














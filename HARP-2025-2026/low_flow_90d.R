# Load necessary libraries
suppressPackageStartupMessages(library(hydrotools))
suppressPackageStartupMessages(library(zoo))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(sqldf))
suppressPackageStartupMessages(library(lubridate))


perform.group2 <- function(gageid, col_to_norm){
  # Get data from input site
  flows <- dataRetrieval::readNWISdv(gageid,
                                     parameterCd = "00060")
  flows <- dataRetrieval::renameNWISColumns(flows)
  
  # Get site info
  info <- dataRetrieval::readNWISsite(gageid)
  # Extract Drainage area
  da <- info$drain_area_va
  # convert da to ft2 from mi2
  da <- 5280*5280*da
  
  # Convert flows to zoo
  flows_zoo <- zoo::as.zoo(x=flows$Flow)
  zoo::index(flows_zoo) <- flows$Date
  # Use group 1 to get minimum monthly flows
  flows <- as.data.frame(hydrotools::group2(flows_zoo,"water",mimic.tnc = TRUE))
  
  
  query <- paste0("select year, \"", col_to_norm, "\" as reg_flow from flows")
  
  flows <- sqldf::sqldf(query)
  
  flows$specific_flow <- flows$reg_flow/da
  
  #convert specific flow from ft/s to in/day
  flows$specific_flow <- flows$specific_flow*86400*12
  
  # create column for monthly and yearly values
  flows$monthly <- flows$specific_flow*30
  flows$yearly <- flows$specific_flow*365
  
  
  return(flows)
}

# Boxplots ----
# Get 90 day min data from 3 gages
cs_90_day_min <- perform.group2("01632000", "90 Day Min")

cs_90_day_min$model_yearly <- ifelse(cs_90_day_min$year > 1984,
                                     cs_90_day_min$yearly,
                                     NA)

a <- ggplot(data = cs_90_day_min)+
  geom_boxplot(mapping = aes(x = yearly),
               color="palevioletred2",
               fill="palevioletred1",
               alpha=0.65)+
  coord_cartesian(xlim = c(0,15))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 8))+
  xlab("")+
  ylab("Full Timespan")
  

b <- ggplot(data = cs_90_day_min)+
  geom_boxplot(mapping = aes(x = model_yearly),
               color="olivedrab3",
               fill="olivedrab2",
               alpha=0.65)+
  coord_cartesian(xlim = c(0,15))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 8))+
  xlab("")+
  ylab("Model Timespan")

aa <- gridExtra::grid.arrange(a,b,ncol=1, top="Cootes Store")

mtj_90_day_min <- perform.group2("01633000", "90 Day Min")

mtj_90_day_min$model_yearly <- ifelse(mtj_90_day_min$year > 1984,
                                     mtj_90_day_min$yearly,
                                     NA)

c <- ggplot(data = mtj_90_day_min)+
  geom_boxplot(mapping = aes(x = yearly),
               color="#ffd65f",
               fill="#ffda6f",
               alpha=0.65)+
  coord_cartesian(xlim = c(0,15))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 8))+
  xlab("")+
  ylab("Full Timespan")


d <- ggplot(data = mtj_90_day_min)+
  geom_boxplot(mapping = aes(x = model_yearly),
               color="#55362a",
               fill="#8e5c49",
               alpha=0.65)+
  coord_cartesian(xlim = c(0,15))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 8))+
  xlab("")+
  ylab("Model Timespan")

bb <- gridExtra::grid.arrange(c,d,ncol=1, top="Mount Jackson")


sb_90_day_min <- perform.group2("01634000", "90 Day Min")

sb_90_day_min$model_yearly <- ifelse(sb_90_day_min$year > 1984,
                                      sb_90_day_min$yearly,
                                      NA)

e <- ggplot(data = sb_90_day_min)+
  geom_boxplot(mapping = aes(x = yearly),
               color="#09957c",
               fill="#53c0ad",
               alpha=0.65)+
  coord_cartesian(xlim = c(0,15))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 8))+
  xlab("")+
  ylab("Full Timespan")


f <- ggplot(data = sb_90_day_min)+
  geom_boxplot(mapping = aes(x = model_yearly),
               color="#cc6c2e",
               fill="#ff873a",
               alpha=0.65)+
  coord_cartesian(xlim = c(0,15))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 8))+
  xlab("Flow (in/yr)")+
  ylab("Model Timespan")

cc <- gridExtra::grid.arrange(e,f,ncol=1, top="Strasburg")

gridExtra::grid.arrange(aa,bb,cc, top="Minimum Yearly Flows for Different Timespans")




# Groundwater Needs ----
# Perform Group 2 function
cs_7_day_min <- perform.group2("01632000", "7 Day Min")
mtj_7_day_min <- perform.group2("01633000", "7 Day Min")
sb_7_day_min <- perform.group2("01634000", "7 Day Min")

# Combine data from all three gages
# min_90_day_cfs in cfs
# specific_90_day_min in in/day
# specific_monthly in in/month
# specific_yearly in in/yr
shenandoah_7_day_min <- sqldf::sqldf(
  "select year, reg_flow as min_7_day_cfs, specific_flow as specific_7_day_min,
    monthly as specific_monthly, yearly as specific_yearly,
    'Cootes Store' as Location from cs_7_day_min
    union all
    select year, reg_flow as min_7_day_cfs, specific_flow as specific_7_day_min,
    monthly as specific_monthly, yearly as specific_yearly,
    'Mount Jackson' as Location from mtj_7_day_min
    union all
    select year, reg_flow as min_7_day_cfs, specific_flow as specific_7_day_min,
    monthly as specific_monthly, yearly as specific_yearly,
    'Strasburg' as Location from sb_7_day_min
    "
)


# Plot inches/year required to maintain minimum flow histogram ----
# Cootes Store
CS <- ggplot(data = cs_7_day_min)+
  geom_density(mapping = aes(x=yearly),
               fill="plum2",
               color="plum",
               alpha=0.5)+
  geom_vline(xintercept = mean(cs_7_day_min$yearly), color="plum")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Watershed Flow (in/yr)")+
  ylab("Density")+
  ggtitle("Cootes Store")+
  geom_text(aes(x = 11, y = 0.25,
                label = paste0("Mean = ", round(mean(cs_7_day_min$yearly),2)," in/yr")),
            stat = "unique")+
  coord_cartesian(xlim = c(0,10), ylim = c(0,3))
  
# Mount Jackson
MTJ <- ggplot(data = mtj_7_day_min)+
    geom_density(mapping = aes(x=yearly),
                 fill="olivedrab2",
                 color="olivedrab3",
                 alpha=0.5)+
    geom_vline(xintercept = mean(mtj_7_day_min$yearly), color="olivedrab3")+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5))+
    xlab("Watershed Flow (in/yr)")+
    ylab("Density")+
    ggtitle("Mount Jackson")+
    geom_text(aes(x = 11, y = 0.25,
                label = paste0("Mean = ", round(mean(mtj_7_day_min$yearly),2)," in/yr")),
            stat = "unique")+
  coord_cartesian(xlim = c(0,10), ylim = c(0,3))
  
# Strasburg
SB <- ggplot(data = sb_7_day_min)+
    geom_density(mapping = aes(x=yearly),
                 fill="lightblue2",
                 color="lightblue3",
                 alpha=0.5)+
    geom_vline(xintercept = mean(sb_7_day_min$yearly), color="lightblue3")+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5))+
    xlab("Watershed Flow (in/yr)")+
    ylab("Density")+
    ggtitle("Strasburg")+
    geom_text(aes(x = 11, y = 0.25,
                label = paste0("Mean = ", round(mean(sb_7_day_min$yearly),2)," in/yr")),
            stat = "unique")+
  coord_cartesian(xlim = c(0,10), ylim = c(0,3))

# Entire NF Shenandoah
NFS <- ggplot(data = shenandoah_7_day_min)+
  geom_density(mapping = aes(x=specific_yearly),
                 fill="brown3",
                 color="brown",
                 alpha=0.5)+
  geom_vline(xintercept = mean(shenandoah_7_day_min$specific_yearly), color="brown")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Watershed Flow (in/yr)")+
  ylab("Density")+
  ggtitle("NF Shenandoah")+
  geom_text(aes(x = 11, y = 0.25,
                label = paste0("Mean = ", round(mean(shenandoah_7_day_min$specific_yearly),2)," in/yr")),
            stat = "unique")+
  coord_cartesian(xlim = c(0,10), ylim = c(0,3))

# Yearly 0-15, monthly 0-1.25, daily 0-0.045

gridExtra::grid.arrange(CS,MTJ,SB,NFS, top="Yearly Water Needs to Meet 7 Day Minimum Flow")

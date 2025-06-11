# #Get data for NF Shenandoah Mount Jackson
# flows <- dataRetrieval::readNWISdv("01633000",parameterCd = "00060")
# flows <- dataRetrieval::renameNWISColumns(flows)
# #Convert flows to zoo
# flows_zoo <- zoo::as.zoo(x = flows$Flow)
# zoo::index(flows_zoo) <- flows$Date
# #Use group 2 to get critical period flows and stats:
# hydrotools::group2(flows_zoo,"water",mimic.tnc = TRUE)

# Download Libraries
suppressPackageStartupMessages(library(hydrotools))
suppressPackageStartupMessages(library(zoo))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(sqldf))
suppressPackageStartupMessages(library(lubridate))


# Create perform group 1 function
perform.group1 <- function(gageid){
  # Get data from input site
  flows <- dataRetrieval::readNWISdv(gageid,
                                     parameterCd = "00060")
  flows <- dataRetrieval::renameNWISColumns(flows)
  
  # Get site info
  info <- dataRetrieval::readNWISsite(gageid)
  # Extract Drainage area
  da <- info$drain_area_va
  # convert da to ft2 from mi2
  da<-5280*5280*da
  
  # Convert flows to zoo
  flows_zoo <- zoo::as.zoo(x=flows$Flow)
  zoo::index(flows_zoo) <- flows$Date
  # Use group 1 to get minimum monthly flows
  min_flows <- as.data.frame(hydrotools::group1(flows_zoo, "water", FUN = min))
  min_flows$year <- format(as.Date(rownames(min_flows), format = "%Y"), "%Y")
  
  # Convert to different format
  min_flows <- sqldf::sqldf(
    "select year, 1 as month, January as min_flow from min_flows
    union all
    select year, 2 as month, February as min_flow from min_flows
    union all
    select year, 3 as month, March as min_flow from min_flows
    union all
    select year, 4 as month, April as min_flow from min_flows
    union all
    select year, 5 as month, May as min_flow from min_flows
    union all
    select year, 6 as month, June as min_flow from min_flows
    union all
    select year, 7 as month, July as min_flow from min_flows
    union all
    select year, 8 as month, August as min_flow from min_flows
    union all
    select year, 9 as month, September as min_flow from min_flows
    union all
    select year, 10 as month, October as min_flow from min_flows
    union all
    select year, 11 as month, November as min_flow from min_flows
    union all
    select year, 12 as month, December as min_flow from min_flows
    "
  )
  
  min_flows$date <- lubridate::make_date(min_flows$year, min_flows$month)
  
  # get specific discharge
  min_flows$sp_flow_fps <- min_flows$min_flow/da
  
  return(min_flows)
}

# perform group 1 on 3 Shenandoah gages
cs_flows <- perform.group1("01632000")

mtj_flows <- perform.group1("01633000")

sb_flows <- perform.group1("01634000")

# combien data from 3 gages
shenandoah_mins <- sqldf::sqldf(
    "select date, min_flow, sp_flow_fps, 
    'Cootes Store' as Location from cs_flows
    union all
    select date, min_flow, sp_flow_fps, 
    'Mount Jackson' as Location from mtj_flows
    union all
    select date, min_flow, sp_flow_fps, 
    'Strasburg' as Location from sb_flows
    "
    )

# density plot for minimum flow at each location
# min_plot <- ggplot(shenandoah_mins)+
#   geom_histogram(aes(x=min_flow, 
#                    y=..count..,
#                    fill=Location, 
#                    color=Location),
#                  bins = 150,
#                alpha=0.5)+
#   scale_fill_manual(values=c("plum2", "olivedrab2", "lightblue2"))+
#   scale_color_manual(values=c("plum2", "olivedrab2", "lightblue2"))+
#   theme_bw()+
#   coord_cartesian(xlim = c(0,500))+
#   theme(legend.position = "none")+
#   labs(x="Minimum Flow (cfs)",
#        y="Count")
# 
# # specific discharge plot for each location
# specific_plot <- ggplot(shenandoah_mins)+
#   geom_histogram(aes(x=sp_flow_fps, 
#                    y=..count..,
#                    fill=Location, 
#                    color=Location),
#                  bins = 75,
#                alpha=0.5)+
#   scale_fill_manual(values=c("plum2", "olivedrab2", "lightblue2"))+
#   scale_color_manual(values=c("plum2", "olivedrab2", "lightblue2"))+
#   theme_bw()+
#   coord_cartesian(xlim = c(0,0.00000005))+
#   labs(x="Specific Discharge (ft/s)",
#        y="Count")
# 
#  gridExtra::grid.arrange(min_plot, specific_plot, top="NF Shenandoah Minimum Flows")

# Boxplots of min flow at each location
 min_box <- ggplot(data = shenandoah_mins, mapping = aes(x=Location,y=min_flow))+
  geom_boxplot(aes(color=Location, fill = Location), alpha=0.5)+
  scale_fill_manual(values=c("plum2", "olivedrab2", "lightblue2"))+
  scale_color_manual(values=c("plum3", "olivedrab3", "lightblue3"))+
  theme_bw()+
  theme(legend.position = "none")+
  ylab("Minimum Flow (cfs)")

# Boxplots of specific discharge at each location
 specific_box <- ggplot(data = shenandoah_mins, mapping = aes(x=Location,y=sp_flow_fps))+
   geom_boxplot(aes(color=Location, fill = Location), alpha=0.5)+
   scale_fill_manual(values=c("plum2", "olivedrab2", "lightblue2"))+
   scale_color_manual(values=c("plum3", "olivedrab3", "lightblue3"))+
   theme_bw()+
   theme(legend.position = "none")+
   ylab("Specific Dishcarge (ft/s)")

gridExtra::grid.arrange(min_box, specific_box, ncol=2,top="NF Shenandoah Minimum Flow")



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
  da<-5280*5280*da
  
  # Convert flows to zoo
  flows_zoo <- zoo::as.zoo(x=flows$Flow)
  zoo::index(flows_zoo) <- flows$Date
  # Use group 1 to get minimum monthly flows
  flows <- as.data.frame(hydrotools::group2(flows_zoo,"water",mimic.tnc = TRUE))

  
  query <- paste0("select year, \"", col_to_norm, "\" as reg_flow from flows")
  
  flows <- sqldf::sqldf(query)
  
  flows$specific_flow <- flows$reg_flow/da
  
  return(flows)
}

# Get 90 day max data from each gage
cs_90_day_max <- perform.group2("01632000", "90 Day Max")

mtj_90_day_max <- perform.group2("01633000", "90 Day Max")

sb_90_day_max <- perform.group2("01634000", "90 Day Max")

# combine data from 3 gages
shenandoah_90_day_max <- sqldf::sqldf(
  "select year, reg_flow as max_90_day_cfs, specific_flow as specific_90_day_fps, 
    'Cootes Store' as Location from cs_90_day_max
    union all
    select year, reg_flow as max_90_day_cfs, specific_flow as specific_90_day_fps, 
    'Mount Jackson' as Location from mtj_90_day_max
    union all
    select year, reg_flow as max_90_day_cfs, specific_flow as specific_90_day_fps, 
    'Strasburg' as Location from sb_90_day_max
    "
)


# Get 90 day min data from 3 gages
cs_90_day_min <- perform.group2("01632000", "90 Day Min")

mtj_90_day_min <- perform.group2("01633000", "90 Day Min")

sb_90_day_min <- perform.group2("01634000", "90 Day Min")

shenandoah_90_day_min <- sqldf::sqldf(
  "select year, reg_flow as min_90_day_cfs, specific_flow as specific_90_day_min, 
    'Cootes Store' as Location from cs_90_day_min
    union all
    select year, reg_flow as min_90_day_cfs, specific_flow as specific_90_day_min, 
    'Mount Jackson' as Location from mtj_90_day_min
    union all
    select year, reg_flow as min_90_day_cfs, specific_flow as specific_90_day_min, 
    'Strasburg' as Location from sb_90_day_min
    "
)

# Combine min and max data
shenandoah_90 <- sqldf::sqldf(
  "select a.year, a.max_90_day_cfs, a.specific_90_day_fps, a.Location,
  b.min_90_day_cfs, b.specific_90_day_min
  from shenandoah_90_day_max as a
  left outer join shenandoah_90_day_min as b
  on(
    a.year=b.year and
    a.Location=b.Location
  )
  "
)

max_box <- ggplot(data = shenandoah_90, mapping = aes(x=Location,y=max_90_day_cfs))+
  geom_boxplot(aes(color=Location, fill = Location), alpha=0.5)+
  scale_fill_manual(values=c("plum2", "olivedrab2", "lightblue2"))+
  scale_color_manual(values=c("plum3", "olivedrab3", "lightblue3"))+
  theme_bw()+
  theme(legend.position = "none")+
  ylab("Flow (cfs)")+
  coord_cartesian(ylim = c(0,2500))

# Boxplots of specific discharge at each location
min_box <- ggplot(data = shenandoah_90, mapping = aes(x=Location,y=min_90_day_cfs))+
  geom_boxplot(aes(color=Location, fill = Location), alpha=0.5)+
  scale_fill_manual(values=c("plum2", "olivedrab2", "lightblue2"))+
  scale_color_manual(values=c("plum3", "olivedrab3", "lightblue3"))+
  theme_bw()+
  theme(legend.position = "none")+
  ylab("")+
  coord_cartesian(ylim = c(0,2500))

gridExtra::grid.arrange(max_box, min_box, ncol=2,top="NF Shenandoah 90 Day Max/Min Flow")

# Same plots for specific dishcarge
max_sp_box <- ggplot(data = shenandoah_90, mapping = aes(x=Location,y=specific_90_day_fps))+
  geom_boxplot(aes(color=Location, fill = Location), alpha=0.5)+
  scale_fill_manual(values=c("plum2", "olivedrab2", "lightblue2"))+
  scale_color_manual(values=c("plum3", "olivedrab3", "lightblue3"))+
  theme_bw()+
  theme(legend.position = "none")+
  ylab("Specific Discharge (ft/s)")+
  coord_cartesian(ylim = c(0,.00000015))

# Boxplots of specific discharge at each location
min_sp_box <- ggplot(data = shenandoah_90, mapping = aes(x=Location,y=specific_90_day_min))+
  geom_boxplot(aes(color=Location, fill = Location), alpha=0.5)+
  scale_fill_manual(values=c("plum2", "olivedrab2", "lightblue2"))+
  scale_color_manual(values=c("plum3", "olivedrab3", "lightblue3"))+
  theme_bw()+
  theme(legend.position = "none")+
  ylab("")+
  coord_cartesian(ylim = c(0,.00000015))

gridExtra::grid.arrange(max_sp_box, min_sp_box, ncol=2,top="NF Shenandoah 90 Day Max/Min Specific Discharge")



# Seasonal changes

seasonal.boxplots <- function(gageid, gagename, ymax=0){
  # Download gage data
  flows <- dataRetrieval::readNWISdv(gageid,
                                     parameterCd = "00060")
  flows <- dataRetrieval::renameNWISColumns(flows)
  
  flows$Month <- month(flows$Date)
  # Create season column
  flows$Season <- ifelse(flows$Month>11,'Winter',
                              ifelse(flows$Month>8,'Fall',
                                     ifelse(flows$Month>5,'Summer',
                                            ifelse(flows$Month>2,'Spring','Winter'))))
  flows$Season <- factor(flows$Season, levels=c('Fall','Winter','Spring','Summer'))
  
  if (ymax==0){
    ymax=(max(flows$Flow, na.rm = TRUE)/3)
  }
  
  # Create Boxplot
  season_bp <- ggplot(data = flows, mapping = aes(x=Season,y=Flow))+
    geom_boxplot(aes(color= Season, fill = Season), alpha=0.5)+
    scale_fill_manual(values=c("firebrick3", "lightblue2", "darkolivegreen2", "hotpink"))+
    scale_color_manual(values=c("firebrick", "lightblue3", "darkolivegreen3", "hotpink2"))+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none") +
    ggtitle(paste0("Seasonal Flow at ", gagename) )+
    ylab("Flow (cfs)")+
    coord_cartesian(ylim = c(0,ymax))
  
  return(season_bp)
}

cs_seasonal <- seasonal.boxplots("01632000", "Cootes Store", 1500)

mtj_seasonal <- seasonal.boxplots("01633000", "Mount Jackson", 1500)

sb_seasonal <- seasonal.boxplots("01634000", "Strasburg", 1500)


gridExtra::grid.arrange(cs_seasonal, mtj_seasonal, sb_seasonal, ncol=3)


info <- dataRetrieval::readNWISsite(gageid)
# Extract Drainage area
da <- info$drain_area_va
# convert da to ft2 from mi2
da<-5280*5280*da

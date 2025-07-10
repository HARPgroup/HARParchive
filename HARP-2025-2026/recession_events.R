suppressPackageStartupMessages(library(dataRetrieval))
suppressPackageStartupMessages(library(sqldf))
suppressPackageStartupMessages(library(ggplot2))

gageid <- "01632000"

# Get flow data from USGS
usgs_flows <- readNWISdata(site = gageid,
                         parameterCd = "00060",
                         startDate = "1904-01-01",
                         endDate = "2024-12-31")
usgs_flows <- renameNWISColumns(usgs_flows)

# Select relevant columns to keep df small
usgs_flows <- sqldf(
  "Select dateTime, Flow as flow_cfs from usgs_flows
  ")

# Calculate AGWRC Equivalent
usgs_flows$AGWRC <- usgs_flows$flow_cfs / c(NA, head(usgs_flows$flow_cfs, -1))

# Create column for dAGWRC
usgs_flows$dAGWRC <- usgs_flows$AGWRC / c(NA, head(usgs_flows$AGWRC, -1))

# Determine Recession Periods
usgs_flows <- sqldf(
  "select *, FALSE as recession 
  from usgs_flows where dAGWRC < 0.95
  union all
  select *, TRUE as recession
  from usgs_flows where dAGWRC >= 0.95 and dAGWRC <= 1.05
  order by dateTime
  "
)

usgs_flows$recession <- as.logical(usgs_flows$recession)

# Create Recession Period Summaries
periods <- rle(usgs_flows$recession == TRUE)

# Create  event IDs
event_ids <- cumsum(c(1, diff(which(usgs_flows$recession == TRUE)) != 1))

# Assign drought ids
usgs_flows$event_id <- NA
usgs_flows$event_id[which(usgs_flows$recession == TRUE)] <- event_ids

# get subset with only dorught event data
event_data <- subset(usgs_flows, !is.na(event_id))

event_sum <- data.frame(matrix(,nrow=max(usgs_flows$event_id, na.rm=T), ncol=0))

# create columns from drought data
start_dates <- aggregate(event_data$dateTime ~ event_data$event_id, FUN = min)
end_dates <- aggregate(event_data$dateTime ~ event_data$event_id, FUN = max)
durations <- aggregate(event_data$dateTime ~ event_data$event_id, FUN = length)
mean_flows <- aggregate(event_data$flow_cfs ~ event_data$event_id, FUN = mean)
mean_AGWRC <- aggregate(event_data$AGWRC ~ event_data$event_id, FUN = mean)

# Format aggregates into the dataframe
event_sum$event_id <- start_dates$`event_data$event_id`
event_sum$start_date <- as.Date(start_dates$`event_data$dateTime`)
event_sum$end_date <- as.Date(end_dates$`event_data$dateTime`)
event_sum$ndays <- durations$`event_data$dateTime`
event_sum$mean_flow <- mean_flows$`event_data$flow_cfs`
event_sum$mean_AGWRC <- mean_AGWRC$`event_data$AGWRC`

usgs_flows$dateTime <- as.Date(usgs_flows$dateTime)

event_sum <- subset(event_sum, ndays > 1)

# Plotting A specific storm event ----
i <- 94

s_date <- event_sum$start_date[i] - 30
e_date <- event_sum$end_date[i] + 30
target_event <- event_sum$event_id[i]

ggplot(data = usgs_flows, mapping = aes(x = dateTime)) +
  geom_line(aes(y = flow_cfs)) +
  geom_point(
    data = subset(usgs_flows, event_id == target_event),
    aes(y = flow_cfs),
    color = "firebrick", size = 2) +
  coord_cartesian(xlim = c(s_date, e_date), ylim = c(0, max(event_sum$mean_flow)))+
  theme_bw()+
  xlab("Date")+
  ylab("Flow (cfs)")+
  ggtitle(paste0("Recession Event ", i))+
  theme(plot.title = element_text(hjust = 0.5))

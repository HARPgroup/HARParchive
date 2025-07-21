# Load Packages for plots
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(gridExtra))


# ex. 
DataFilePath <- args[1]
obsColName <- args[2]
predColName <- args[3]
dateColName <- args[4]
ratingColName <- args[5]
outputFilePath <- args[6]

predicted_data <- read.csv(DataFilePath)

# Overlayed scatter plots of flow
flow_scatter <- ggplot(data = predicted_data, mapping = aes(x=.data[[dateColName]]))+
  geom_point(mapping = aes(y = .data[[obsColName]]), size = 0.8, color = "grey40")+
  geom_point(mapping = aes(y = .data[[predColName]]), size = 0.8, color = "green3")+
  theme_bw()+
  xlab("Date")+
  ylab("Flow")

flow_scatter

# make data summary table
sum_table <- data.frame(
  flow_type = c("observed", "predicted"),
  median_flow = c(median(predicted_data[[obsColName]]), median(predicted_data[[predColName]])), 
  avg_rating = c(mean(predicted_data[[ratingColName]]), mean(predicted_data[[ratingColName]]))
)


grid.arrange(flow_scatter, grid.table(sum_table))

rating_plot <- ggplot(data = predicted_data, 
                      mapping = aes(x=.data[[dateColName]], y=.data[[ratingColName]]))+
  geom_point(size = 0.75)+
  geom_smooth(method = "lm")+
  coord_cartesian(ylim = c(-5,1.25))+
  theme_bw()

rating_plot

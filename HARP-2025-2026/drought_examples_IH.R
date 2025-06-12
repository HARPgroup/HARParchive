# Examples using drought_functions_IH.R functions
library(ggplot2)

# Cootes Store 2000-2010 ----
d_opt <- c(1, 3, 7, 30, 90)
colors <- c("tomato","tan1","olivedrab3","steelblue2","mediumpurple1" )

for(j in seq_along(d_opt)){
  i <- d_opt[j]
 # Create unique output names
  df_name <- paste0("cs_droughts_", i)
  plot_name <- paste0("cs_plot_", i)
  
cs_classified <- classify.drought("01632000", i, "2000-01-01", "2010-12-31")

cs_droughts <- create.drought.sum(cs_classified, 7)

plot <- ggplot(data = cs_droughts, mapping = aes(x=ndays))+
  geom_boxplot(color=colors[j], fill=colors[j], alpha=0.65)+
  theme_bw()+
  coord_cartesian(xlim = c(0,100))+
  ylab(paste0(i, " Day"))+
  xlab("")

assign(df_name, cs_droughts)
assign(plot_name,plot )
}
#

cs <- gridExtra::grid.arrange(cs_plot_1, 
                        cs_plot_3, 
                        cs_plot_7, 
                        cs_plot_30, 
                        cs_plot_90, 
                        ncol=1, bottom="Drought Duration (days)", 
                        top = "Cootes Store")

# Mount Jackson 2000-2010 ----
for(j in seq_along(d_opt)){
  i <- d_opt[j]
  # Create unique output names
  df_name <- paste0("mtj_droughts_", i)
  plot_name <- paste0("mtj_plot_", i)
  
  mtj_classified <- classify.drought("01633000", i, "2000-01-01", "2010-12-31")
  
  mtj_droughts <- create.drought.sum(mtj_classified, 7)
  
  plot <- ggplot(data = mtj_droughts, mapping = aes(x=ndays))+
    geom_boxplot(color=colors[j], fill=colors[j], alpha=0.65)+
    theme_bw()+
    coord_cartesian(xlim = c(0,100))+
    ylab(paste0(i, " Day"))+
    xlab("")
  
  assign(df_name, mtj_droughts)
  assign(plot_name,plot )
}
#

mtj <- gridExtra::grid.arrange(mtj_plot_1, 
                        mtj_plot_3, 
                        mtj_plot_7, 
                        mtj_plot_30, 
                        mtj_plot_90, 
                        ncol=1, bottom="Drought Duration (days)", 
                        top = "Mount Jackson")

# Strasburg 2000-2010 ----
for(j in seq_along(d_opt)){
  i <- d_opt[j]
  # Create unique output names
  df_name <- paste0("sb_droughts_", i)
  plot_name <- paste0("sb_plot_", i)
  
  sb_classified <- classify.drought("01634000", i, "2000-01-01", "2010-12-31")
  
  sb_droughts <- create.drought.sum(sb_classified, 7)
  
  plot <- ggplot(data = sb_droughts, mapping = aes(x=ndays))+
    geom_boxplot(color=colors[j], fill=colors[j], alpha=0.65)+
    theme_bw()+
    coord_cartesian(xlim = c(0,100))+
    ylab(paste0(i, " Day"))+
    xlab("")
  
  assign(df_name, sb_droughts)
  assign(plot_name,plot )
}
#

sb <- gridExtra::grid.arrange(sb_plot_1, 
                        sb_plot_3, 
                        sb_plot_7, 
                        sb_plot_30, 
                        sb_plot_90, 
                        ncol=1, bottom="Drought Duration (days)", top = "Strasburg")

gridExtra::grid.arrange(cs,mtj,sb, ncol=3, top="Drought Durations Using n Day Minimums 2000-2010")


# Above tests indicate using 7 Day mins as 
# the IQR varies the least from gage to gage
# Perform group 2 functions of gages using 7 day mins
CS_7_day_min <- perform.group2("01632000", "7 Day Min")
MTJ_7_day_min <- perform.group2("01633000", "7 Day Min")
SB_7_day_min <- perform.group2("01634000", "7 Day Min")

# Combine data from all three gages
# min_90_day_cfs in cfs
# specific_90_day_min in in/day
# specific_monthly in in/month
# specific_yearly in in/yr
NFS_7_day_min <- sqldf::sqldf(
  "select year, reg_flow as min_7_day_cfs, specific_flow as specific_7_day_min,
    monthly, yearly,
    'Cootes Store' as Location from CS_7_day_min
    union all
    select year, reg_flow as min_7_day_cfs, specific_flow as specific_7_day_min,
    monthly, yearly,
    'Mount Jackson' as Location from MTJ_7_day_min
    union all
    select year, reg_flow as min_7_day_cfs, specific_flow as specific_7_day_min,
    monthly, yearly,
    'Strasburg' as Location from SB_7_day_min
    "
)

gage_abbr <- c("CS","MTJ", "SB","NFS")
plots <- list()

for (j in seq_along(gage_abbr)) {
  i<-gage_abbr[j]
  # set variable df name
  df_name <- paste0(i, "_7_day_min")
  df <- get(df_name)
  # get yeraly mean for plots
  yearly_mean <- mean(df$yearly)
  
  # create plot
  plots[[i]] <- ggplot(data=df)+
    geom_histogram((mapping = aes(x=yearly)),
                 fill=colors[j],
                 color=colors[j],
                 alpha=0.5,
                 binwidth = 0.1)+
    geom_vline(xintercept = yearly_mean, color=colors[j])+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5))+
    xlab("Water Needs (in/yr)")+
    ylab("Count")+
    ggtitle(paste0(i))+
    geom_text(x = 3, y = 25, 
              label = paste0("Mean = ", round(yearly_mean, 3), " in/yr"))+
    coord_cartesian(xlim = c(0,4), ylim = c(0,40))
}

# Plot inches/year required to maintain minimum flow histogram ----

plots[[1]] <- plots[[1]]+
  ggtitle("Cootes Store")

plots[[2]] <- plots[[2]]+
  ggtitle("Mount Jackson")

plots[[3]] <- plots[[3]]+
  ggtitle("Strasburg")

plots[[4]] <- plots[[4]]+
  ggtitle("NF Shenandoah")

gridExtra::grid.arrange(plots[[1]],
                        plots[[2]],
                        plots[[3]],
                        plots[[4]],
                        ncol=2,
                        top = "Yearly Water Needs to Meet 7 Day Minimum Flows")





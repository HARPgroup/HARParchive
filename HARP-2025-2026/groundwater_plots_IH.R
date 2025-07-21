# combine data from landseg_groundwater_IH.R
combined_daily <- sqldf(
  "select *, 'H51165' as Segment from H51165_daily
  union all
  select *, 'N51165' as Segment from N51165_daily
  union all
  select *, 'N51171' as Segment from N51171_daily
  union all 
  select *, 'N54031' as Segment from N54031_daily
  "
)

# Compare 4 land segs general ----
ggplot(combined_daily, mapping = aes(x = Date, color = Segment))+
  geom_line(mapping = aes(y = AGWO))+
  scale_x_date(limits = c(as_date("2005-03-01"), as_date("2005-07-31")))+
  theme_bw()+
  coord_cartesian(ylim = c(0,.1))+
  scale_color_manual(values = c("dodgerblue3", "mediumorchid4", "darkorange2", "darkolivegreen4"))

#----

# Plot AGWO vs TGWS for each Land Seg ----


a <- ggplot(H51165_daily, mapping = aes(x = AGWS , y = AGWO))+
  geom_point(size=1, alpha=0.15, color="steelblue2")+
  geom_smooth(method = "lm", color="grey25", size=0.75)+
  stat_regline_equation(label.x = 1.5, label.y =.03)+
  stat_cor(aes(label=after_stat(rr.label)), label.x=1.5, label.y=.015)+
  theme_bw()+
  ggtitle("H51165")+
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Active GW Storage (in)")+
  ylab("Active GW Outflow (in/day)")+
  coord_cartesian(ylim = c(0,0.15), xlim = c(0,4))

b <- ggplot(N51165_daily, mapping = aes(x = AGWS, y = AGWO))+
  geom_point(size=1, alpha=0.15, color="steelblue2")+
  geom_smooth(method = "lm",color="grey25", size=0.75)+
  stat_regline_equation(label.x = 1.5, label.y =.03 )+
  stat_cor(aes(label=after_stat(rr.label)), label.x=1.5, label.y=.015)+
  theme_bw()+
  ggtitle("N51165")+
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Active GW Storage (in)")+
  ylab("Active GW Outflow (in/day)")+
  coord_cartesian(ylim = c(0,0.15), xlim = c(0,4))

c <- ggplot(N51171_daily, mapping = aes(x = AGWS, y = AGWO))+
  geom_point(size=1, alpha=0.15, color="steelblue2")+
  geom_smooth(method = "lm", color="grey25", size=0.75)+
  stat_regline_equation(label.x = 1.5, label.y =.03 )+
  stat_cor(aes(label=after_stat(rr.label)), label.x=1.5, label.y=.015)+
  theme_bw()+
  ggtitle("N51171")+
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Active GW Storage (in)")+
  ylab("Active GW Outflow (in/day)")+
  coord_cartesian(ylim = c(0,0.15), xlim = c(0,4))

d <- ggplot(N54031_daily, mapping = aes(x = AGWS, y = AGWO))+
  geom_point(size=1, alpha=0.15, color="steelblue2")+
  geom_smooth(method = "lm", color="grey25", size =0.75)+
  stat_regline_equation(label.x = 1.5, label.y =.03 )+
  stat_cor(aes(label=after_stat(rr.label)), label.x=1.5, label.y=.015)+
  theme_bw()+
  ggtitle("N54031")+
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Active GW Storage (in)")+
  ylab("Active GW Outflow (in/day)")+
  coord_cartesian(ylim = c(0,0.15), xlim = c(0,4))

grid.arrange(a, b, c, d, ncol=2, top="Groundwater Storage and Outflow in Land Segments (FOR)")


# H51165 Inflow plots (FOR) ----

# UZ
e <- ggplot(H_precalc, mapping = aes(x = index, y = H_precalc$UZI))+
  geom_point(size=0.5, color = "dodgerblue3")+
  theme_bw()+
  ggtitle("Upper Zone")+
  ylab("Inflow (in/hour)")+
  xlab("Date")+
  theme(plot.title = element_text(hjust = 0.45))+
  coord_cartesian(ylim = c(0,0.3))

# ggplot(H_practice, mapping = aes(x = index, y = H_practice$UZI))+
#   geom_point(size=0.5)+
#   ggtitle("Upper Zone Inflow After Calculation")


#Lower Zone
f <- ggplot(H_precalc, mapping = aes(x = index, y = LZI))+
  geom_point(size=0.5, color = "dodgerblue3")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.45)) +
  ggtitle("Lower Zone")+
  ylab("Inflow (in/hour)")+
  xlab("Date")+
  coord_cartesian(ylim = c(0,0.3))

# ggplot(H_practice, mapping = aes(x = index, y = LZI))+
#   geom_point(size=0.5)+
#   ggtitle("Lower Zone Inflow After Calculation")

# AGW

g <- ggplot(H_precalc, mapping = aes(x = index, y = H_precalc$AGWI))+
  geom_point(size=0.5, color="dodgerblue3")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.45))+
  ggtitle("Active Groundwater")+
  ylab("Inflow (in/hour)")+
  xlab("Date")+
  coord_cartesian(ylim = c(0,0.3))

# ggplot(H_practice, mapping = aes(x = index, y = H_practice$AGWI))+
#   geom_point(size=0.5)+
#   ggtitle("AGW Inflow Before Calculation")

grid.arrange(e, f, g, ncol=1, top="Inflows Before Recalculation (H51165, FOR)")
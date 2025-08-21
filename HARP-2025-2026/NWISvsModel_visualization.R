#1985-2013 bar chart for flow
combined_df <- bind_rows(
  results_nwis %>% mutate(Source = "NWIS"),
  results_model %>% mutate(Source = "Model")
)
ggplot(combined_df, aes(x = Site, y = SummaryFlow_cfs, fill = Source)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = round(SummaryFlow_cfs, 1)),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3.5) +
  labs(title = "Q90 (10th percentile) Flow Comparison",
       y = "Summary Flow (cfs)", x = "Site") +
  theme_minimal()


#relative difference plot
comparison_df <- left_join(
  results_nwis %>% select(Site, Storage_inches_nwis = Storage_inches),
  results_model %>% select(Site, Storage_inches_model = Storage_inches),
  by = "Site"
)
comparison_df <- comparison_df %>%
  mutate(
    Pct_Diff = 100 * (Storage_inches_model - Storage_inches_nwis) / Storage_inches_nwis
  )
ggplot(comparison_df, aes(x = Site, y = Pct_Diff)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = paste0(round(Pct_Diff, 1), "%")),
            vjust = ifelse(comparison_df$Pct_Diff >= 0, -0.5, 1.5),
            size = 3.5) +
  labs(title = "Percent Difference in Storage Needs (Model vs NWIS)",
       y = "% Difference", x = "Site") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal()


#use this for producing tables
library(flextable)
flex_table1 <- regulartable(results_model)
flex_table1 <- set_header_labels(
  flex_table1,
  DrainageArea_sqmi = "DrainageArea (sqmi)",
  SummaryFlow_cfs = "SummaryFlow (cfs)",
  Duration_days = "Duration (days)",
  Storage_inches = "Storage (in)"
)
flex_table1


#use this for producing tables
flex_table2 <- regulartable(results_nwis)
flex_table2 <- set_header_labels(
  flex_table2,
  DrainageArea_sqmi = "DrainageArea (sqmi)",
  SummaryFlow_cfs = "SummaryFlow (cfs)",
  Duration_days = "Duration (days)",
  Storage_inches = "Storage (in)"
)
flex_table2



#ratio plot
comparison_df <- left_join(
  results_model %>% select(Site, Model_Q90 = SummaryFlow_cfs),
  results_nwis %>% select(Site, Gage_Q90 = SummaryFlow_cfs),
  by = "Site"
) %>%
  mutate(Ratio = Model_Q90 / Gage_Q90)

ggplot(comparison_df, aes(x = Site, y = Ratio)) +
  geom_col(fill = "darkred") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
  geom_text(
    aes(label = round(Ratio, 2)),
    vjust = -0.5,
    size = 4,
    color = "black"
  ) +
  labs(
    title = "Ratio of Model to Gage mean Flow by Site",
    y = "Model mean / Gage mean",
    x = "Site"
  ) +
  theme_minimal()



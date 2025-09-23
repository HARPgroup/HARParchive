####TESTING INTEGRATING DESCRIPTIVE STATS TO SCATTERPLOT ANALYSIS####
##DEPENDENCIES##
suppressPackageStartupMessages({
  require(ggplot2)
  require(dplyr)
  require(cowplot)
})
if (!requireNamespace("ggrepel", quietly = TRUE)) {
  message("Tip: install.packages('ggrepel') for point labels")
}

##PALETTE##
okabe_ito <- function(n) {
  base <- c("#0072B2", "#E69F00", "#009E73", "#D55E00", "#CC79A7",
            "#56B4E9", "#F0E442", "#000000")
  if (n <= length(base)) base[seq_len(n)] else rep(base, length.out = n)
}

##per-event summary##
#agwr_estimator reduces AGWR within each event (median is robust by default)
build_combined_events <- function(analysis_data, agwr_estimator = median) {
  stopifnot(all(c("GroupID", "Flow", "AGWR", "Season") %in% names(analysis_data)))
  out <- analysis_data %>%
    filter(!is.na(GroupID)) %>%
    group_by(GroupID) %>%
    summarise(
      AGWR   = agwr_estimator(AGWR, na.rm = TRUE),
      Flow   = mean(Flow, na.rm = TRUE),
      Season = dplyr::last(Season),
      .groups = "drop"
    ) %>%
    mutate(GroupID = as.integer(GroupID))
  if (nrow(out) == 0) stop("No events found after summarising analysis_data.")
  out
}

##legend helper##
make_shared_legend <- function(levels_vec, pal, title = "Study events") {
  if (length(levels_vec) == 0) return(NULL)
  dummy <- data.frame(Flow = 0, AGWR = 0,
                      GroupID = factor(levels_vec, levels = levels_vec))
  p_legend <- ggplot(dummy, aes(Flow, AGWR, color = GroupID)) +
    geom_point() +
    scale_color_manual(name = title, values = pal, drop = FALSE) +
    theme_void() + theme(legend.position = "bottom")
  cowplot::get_legend(p_legend)
}

##title helper##
site_title <- function(analysis_data) {
  if ("site_no" %in% names(analysis_data)) {
    paste0(analysis_data$site_no[1])
  } else {
    "Site"
  }
}

##all-events plot with optional stats##
bfd_cfq_all <- function(analysis_data,
                        study_events = integer(0),
                        label_points = FALSE,
                        agwr_estimator = median,
                        show_stats = TRUE,
                        stats_pos = c("right","left")) {
  stats_pos <- match.arg(stats_pos)
  
  #per-event collapse
  df <- build_combined_events(analysis_data, agwr_estimator = agwr_estimator)
  
  #highlight study events
  study_events <- as.integer(study_events)
  present <- intersect(study_events, unique(df$GroupID))
  df_other <- df %>% filter(!(GroupID %in% present))
  df_study <- df %>% filter( GroupID %in% present)
  if (nrow(df_study) > 0) df_study$GroupID <- factor(df_study$GroupID, levels = present)
  pal <- okabe_ito(length(present)); names(pal) <- as.character(present)
  
  #optional regression + counts
  annot_layer <- NULL
  subtitle_layer <- NULL
  if (show_stats && nrow(df) > 1) {
    fit <- lm(AGWR ~ Flow, data = df)
    slope <- unname(coef(fit)[2])
    r2    <- summary(fit)$r.squared
    n_ev  <- nrow(df)
    annot_text <- paste0("Slope = ", round(slope, 7), "  |  R² = ", round(r2, 2))
    xx <- if (stats_pos == "right") max(df$Flow, na.rm = TRUE) else min(df$Flow, na.rm = TRUE)
    hh <- if (stats_pos == "right") 1 else 0
    yy <- max(df$AGWR, na.rm = TRUE)
    annot_layer <- annotate("text", x = xx, y = yy, label = annot_text,
                            hjust = hh, vjust = 1.1, size = 3.2)
    subtitle_layer <- labs(subtitle = paste("Total events:", n_ev))
  }
  
  ggplot(mapping = aes(x = Flow, y = AGWR)) +
    geom_point(data = df_other, color = "grey70", size = 2, alpha = 0.6) +
    geom_point(data = df_study, aes(color = GroupID), size = 3.5) +
    { if (label_points && nrow(df_study) > 0 &&
          requireNamespace("ggrepel", quietly = TRUE))
      ggrepel::geom_text_repel(data = df_study,
                               aes(label = GroupID, color = GroupID),
                               size = 3, show.legend = FALSE) } +
    geom_smooth(data = df, method = "lm", se = FALSE, color = "black") +
    annot_layer +
    scale_color_manual(name = "Study events", values = pal, drop = FALSE) +
    theme_bw() +
    coord_cartesian(xlim = range(df$Flow,  na.rm = TRUE),
                    ylim = range(df$AGWR, na.rm = TRUE)) +
    xlab("Mean Event Flow (cfs)") +
    ylab("Estimated AGWR (per-event)") +
    ggtitle(paste0(site_title(analysis_data), " – All Events")) +
    subtitle_layer +
    theme(plot.title = element_text(hjust = 0.5))
}

##seasonal 4-panel plot with optional per-season stats##
bfd_cfq_seasonal <- function(analysis_data,
                             study_events = integer(0),
                             label_points = FALSE,
                             agwr_estimator = median,
                             show_stats = TRUE,
                             stats_pos = c("right","left")) {
  stats_pos <- match.arg(stats_pos)
  
  df <- build_combined_events(analysis_data, agwr_estimator = agwr_estimator)
  seasons <- c("Spring", "Summer", "Fall", "Winter")
  study_events <- as.integer(study_events)
  present <- intersect(study_events, unique(df$GroupID))
  pal <- okabe_ito(length(present)); names(pal) <- as.character(present)
  
  xr <- range(df$Flow,  na.rm = TRUE)
  yr <- range(df$AGWR, na.rm = TRUE)
  
  make_panel <- function(season) {
    d_season <- df %>% filter(Season == season)
    d_other  <- d_season %>% filter(!(GroupID %in% present))
    d_study  <- d_season %>% filter( GroupID %in% present)
    if (nrow(d_study) > 0) d_study$GroupID <- factor(d_study$GroupID, levels = present)
    
    #per-season regression stats
    annot_layer <- NULL
    if (show_stats && nrow(d_season) > 1) {
      fit <- lm(AGWR ~ Flow, data = d_season)
      slope <- unname(coef(fit)[2])
      r2    <- summary(fit)$r.squared
      txt   <- paste0("Slope=", round(slope, 3), "  |  R²=", round(r2, 2), "  |  n=", nrow(d_season))
      
      xx <- if (stats_pos == "right") max(d_season$Flow, na.rm = TRUE) else min(d_season$Flow, na.rm = TRUE)
      hh <- if (stats_pos == "right") 1 else 0
      yy <- max(d_season$AGWR, na.rm = TRUE)
      
      annot_layer <- annotate("text", x = xx, y = yy, label = txt,
                              hjust = hh, vjust = 1.1, size = 3)
    } else if (show_stats && nrow(d_season) <= 1) {
      xx <- if (stats_pos == "right") max(xr) else min(xr)
      hh <- if (stats_pos == "right") 1 else 0
      yy <- max(yr)
      annot_layer <- annotate("text", x = xx, y = yy, label = "n < 2",
                              hjust = hh, vjust = 1.1, size = 3)
    }
    
    ggplot(mapping = aes(x = Flow, y = AGWR)) +
      geom_point(data = d_other, color = "grey70", size = 2, alpha = 0.6) +
      geom_point(data = d_study, aes(color = GroupID), size = 3.5, show.legend = FALSE) +
      { if (label_points && nrow(d_study) > 0 &&
            requireNamespace("ggrepel", quietly = TRUE))
        ggrepel::geom_text_repel(data = d_study,
                                 aes(label = GroupID, color = GroupID),
                                 size = 3, show.legend = FALSE) } +
      geom_smooth(data = d_season, method = "lm", se = FALSE, color = "black") +
      annot_layer +
      scale_color_manual(values = pal, drop = FALSE) +
      theme_bw() +
      coord_cartesian(xlim = xr, ylim = yr) +
      xlab("Mean Event Flow (cfs)") +
      ylab("Estimated AGWR (per-event)") +
      ggtitle(paste0(season, " Events")) +
      theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
  }
  
  p_spring <- make_panel("Spring")
  p_summer <- make_panel("Summer")
  p_fall   <- make_panel("Fall")
  p_winter <- make_panel("Winter")
  
  legend <- make_shared_legend(levels_vec = as.character(present), pal = pal)
  
  grid_4 <- cowplot::plot_grid(p_spring, p_summer, p_fall, p_winter, ncol = 2, align = "hv")
  title  <- cowplot::ggdraw() +
    cowplot::draw_label(paste0(site_title(analysis_data), " – Seasonal Events"),
                        fontface = "bold", x = 0.5, hjust = 0.5, size = 14)
  
  if (!is.null(legend)) {
    cowplot::plot_grid(title, grid_4, legend, ncol = 1, rel_heights = c(0.08, 1, 0.12))
  } else {
    cowplot::plot_grid(title, grid_4, ncol = 1, rel_heights = c(0.08, 1))
  }
}

##per-event summary table helper##
per_event_summary <- function(analysis_data, agwr_estimator = median) {
  df_ev <- build_combined_events(analysis_data, agwr_estimator = agwr_estimator)
  df_ev %>%
    summarise(
      n         = n(),
      mean_flow = mean(Flow, na.rm = TRUE),
      sd_flow   = sd(Flow, na.rm = TRUE),
      mean_agwr = mean(AGWR, na.rm = TRUE),
      sd_agwr   = sd(AGWR, na.rm = TRUE)
    )
}

###EXAMPLE USE###
#testing_func should already exist in environment with:
#columns: GroupID (int), Flow (numeric), AGWR (numeric), Season (chr/fct), [site_no (optional)]
#example guard:
if (!exists("testing_func")) {
  stop("Please provide `testing_func` with columns GroupID, Flow, AGWR, Season[, site_no].")
}

#plots
p_all <- bfd_cfq_all(
  testing_func,
  study_events = c(7,8,141,142,194,196,203,204),
  label_points = TRUE,
  show_stats   = TRUE,      # slope/R² + n
  stats_pos    = "right"    # or "left"
)
p_sea <- bfd_cfq_seasonal(
  testing_func,
  study_events = c(7,8,141,142,194,196,203,204),
  label_points = TRUE,
  show_stats   = TRUE,
  stats_pos    = "left"
)

print(p_all)
print(p_sea)

#summary table (per-event collapsed, not raw rows)
summary_stats <- per_event_summary(testing_func)
print(summary_stats)

#caption with means on the all-events plot
caption_text <- paste0(
  "Mean Flow = ", round(summary_stats$mean_flow, 1), " cfs; ",
  "Mean AGWR = ", round(summary_stats$mean_agwr, 3),
  " (n = ", summary_stats$n, ")"
)
p_all_with_caption <- p_all + labs(caption = caption_text)
print(p_all_with_caption)
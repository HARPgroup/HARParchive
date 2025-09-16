# --- Dependencies -------------------------------------------------------------
require(ggplot2)
require(dplyr)
require(cowplot)
if (!requireNamespace("ggrepel", quietly = TRUE)) {
  message("Tip: install.packages('ggrepel') for point labels")
}

# --- Palette ------------------------------------------------------------------
okabe_ito <- function(n) {
  base <- c("#0072B2", "#E69F00", "#009E73", "#D55E00", "#CC79A7",
            "#56B4E9", "#F0E442", "#000000")
  if (n <= length(base)) base[seq_len(n)] else rep(base, length.out = n)
}

# --- Build per-event summary (no summarize_event() needed) --------------------
# agwr_estimator: function to reduce AGWR per event (median is robust)
build_combined_events <- function(analysis_data, agwr_estimator = median) {
  stopifnot(all(c("GroupID", "Flow", "AGWR", "Season") %in% names(analysis_data)))
  out <- analysis_data %>%
    filter(!is.na(GroupID)) %>%
    group_by(GroupID) %>%
    summarise(
      AGWR  = agwr_estimator(AGWR, na.rm = TRUE),
      Flow  = mean(Flow, na.rm = TRUE),
      Season = dplyr::last(Season),
      .groups = "drop"
    ) %>%
    mutate(GroupID = as.integer(GroupID))
  if (nrow(out) == 0) stop("No events found after summarising analysis_data.")
  out
}

# --- Legend helper ------------------------------------------------------------
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

# --- Title helper -------------------------------------------------------------
site_title <- function(analysis_data) {
  if ("site_no" %in% names(analysis_data)) {
    paste0(analysis_data$site_no[1])
  } else {
    "Site"
  }
}

# --- All-events plot ----------------------------------------------------------
bfd_cfq_all <- function(analysis_data,
                        study_events = integer(0),
                        label_points = FALSE,
                        agwr_estimator = median) {
  
  df <- build_combined_events(analysis_data, agwr_estimator = agwr_estimator)
  
  study_events <- as.integer(study_events)
  present <- intersect(study_events, unique(df$GroupID))
  
  df_other <- df %>% filter(!(GroupID %in% present))
  df_study <- df %>% filter( GroupID %in% present)
  if (nrow(df_study) > 0) df_study$GroupID <- factor(df_study$GroupID, levels = present)
  
  pal <- okabe_ito(length(present))
  names(pal) <- as.character(present)
  
  ggplot(mapping = aes(x = Flow, y = AGWR)) +
    geom_point(data = df_other, color = "grey70", size = 2, alpha = 0.6) +
    geom_point(data = df_study, aes(color = GroupID), size = 3.5) +
    { if (label_points && nrow(df_study) > 0 &&
          requireNamespace("ggrepel", quietly = TRUE))
      ggrepel::geom_text_repel(data = df_study,
                               aes(label = GroupID, color = GroupID),
                               size = 3, show.legend = FALSE) } +
    geom_smooth(data = df, method = "lm", se = FALSE, color = "black") +
    scale_color_manual(name = "Study events", values = pal, drop = FALSE) +
    theme_bw() +
    coord_cartesian(xlim = range(df$Flow,  na.rm = TRUE),
                    ylim = range(df$AGWR, na.rm = TRUE)) +
    xlab("Mean Event Flow (cfs)") +
    ylab("Estimated AGWR (per-event)") +
    ggtitle(paste0(site_title(analysis_data), " – All Events")) +
    theme(plot.title = element_text(hjust = 0.5))
}

# --- Seasonal 4-panel plot ----------------------------------------------------
bfd_cfq_seasonal <- function(analysis_data,
                             study_events = integer(0),
                             label_points = FALSE,
                             agwr_estimator = median) {
  
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
    
    ggplot(mapping = aes(x = Flow, y = AGWR)) +
      geom_point(data = d_other, color = "grey70", size = 2, alpha = 0.6) +
      geom_point(data = d_study, aes(color = GroupID), size = 3.5, show.legend = FALSE) +
      { if (label_points && nrow(d_study) > 0 &&
            requireNamespace("ggrepel", quietly = TRUE))
        ggrepel::geom_text_repel(data = d_study,
                                 aes(label = GroupID, color = GroupID),
                                 size = 3, show.legend = FALSE) } +
      geom_smooth(data = d_season, method = "lm", se = FALSE, color = "black") +
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


p_all <- bfd_cfq_all(analysis_df,
                     study_events = c(14,31,65,94),
                     label_points = TRUE)
p_sea <- bfd_cfq_seasonal(analysis_df,
                          study_events = c(14,31,65,94),
                          label_points = TRUE)
print(p_all); print(p_sea)
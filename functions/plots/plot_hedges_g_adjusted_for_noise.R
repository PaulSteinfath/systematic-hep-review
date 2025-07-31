plot_hedges_g_adjusted_for_noise <- function(df, 
                                             sigma_s_vals = c(0.5, 1, 2),
                                             sigma_t_vals = c(0.1, 0.2, 0.5),
                                             r_thresh = NULL,
                                             with_clustering=FALSE, 
                                             with_regression=FALSE) {
  
  hedges_column <- compute_effect_columns(df, 
                                          test_type_col = statistics, 
                                          sample_size_col = sample_size, 
                                          groups_col = groups, 
                                          conditions_col = conditions)
  if (!"hedges_g" %in% colnames(df)) {
    df <- bind_cols(df, hedges_column)
  }
  
  if (!with_clustering) {df <- df[df$clustering==0,]}
  if (!with_regression) {df <- df[df$statistics!='Regression',]}
  
  # 0. clean data -----------------------------------------------------------
  df_clean <- df %>% 
    filter(!is.na(hedges_g), !is.na(trials_Mean))
  
  # 1. adjusted estimates for every noise setting --------------------------
  df_adj <- tidyr::expand_grid(df_clean,
                               sigma_s = sigma_s_vals,
                               sigma_t = sigma_t_vals) %>% 
    mutate(
      r     = sigma_s / sigma_t,
      g_adj = hedges_g * sqrt(1 + 1 / (r^2 * trials_Mean)),
      r_lbl = sprintf("σs/σt=%.2f", r)
    )
  
  # 2. apply optional ratio-threshold filter -------------------------------
  if (!is.null(r_thresh)) {
    df_adj <- df_adj %>% filter(r <= r_thresh)
  }
  
  # 3. original (unadjusted) layer -----------------------------------------
  df_raw <- df_clean %>% 
    mutate(
      g_adj = hedges_g,
      r     = NA_real_,
      r_lbl = "original"
    )
  
  # 4. combine & set factor order ------------------------------------------
  r_levels <- c(
    "original",
    paste0("σs/σt=", sprintf("%.2f", sort(unique(df_adj$r))))
  )
  
  df_all <- bind_rows(df_raw, df_adj) %>% 
    mutate(r_lbl = factor(r_lbl, levels = r_levels))
  
  # 5. plot ---------------------------------------------------------------
  p <- ggplot(df_all, aes(x = g_adj, y = r_lbl, fill = ..x..)) +
    geom_density_ridges_gradient(
      scale           = 1.4,
      rel_min_height  = 0.01,
      linewidth            = 0.3,
      bandwidth = 0.1,
      color           = "white"
    ) +
    scale_fill_viridis(name = "Effect size", option = "A") +
    labs(
      x     = "Hedges’ g (adjusted)",
      y     = NULL,
      title = "Original vs. trial-adjusted minimal-detectable g\nacross noise-ratio scenarios"
    ) + plot_theme_default + theme(legend.title = element_text(size = 7))

  return(p)
}
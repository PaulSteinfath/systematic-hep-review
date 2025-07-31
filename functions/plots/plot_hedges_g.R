plot_hedges_g <- function(df, with_clustering=FALSE, with_regression=FALSE){
  
  hedges_column <- compute_effect_columns(df, 
                                          test_type_col = statistics, 
                                          sample_size_col = sample_size, 
                                          groups_col = groups, 
                                          conditions_col = conditions,
                                          sig.level = params$hedges_sig_level,
                                          power.level = params$hedges_power_level)
  df <- bind_cols(df, hedges_column)
  
  if (!with_clustering) {df <- df[df$clustering==0,]}
  if (!with_regression) {df <- df[df$statistics!='Regression',]}
  
  p <- hist_panel(df, col = "hedges_g", force.numeric = T, 
                  title = "Hedges' g", x.label = "Hedges' g", use_proportion = T) +
    geom_point(data = effect_sizes_Coll2020,
               mapping = aes(x = value, y = 0.13, fill = kind), 
               shape = 25, 
               size = 3) +
    scale_fill_manual(name = "Effect sizes from\nColl et al. (2020)", 
                      values = palette_Coll2020) +
    theme(legend.position = c(1, 1),
          legend.justification = c("right", "top"),
          legend.margin = margin(0, 0, 0, 0),
          legend.box.margin = margin(-5, 0, 0, 0),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 7))

  return(p)
  
}

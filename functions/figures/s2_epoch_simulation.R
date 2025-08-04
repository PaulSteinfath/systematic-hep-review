figure_epoch_simulation <- function(df, save_path, ext = "png") {
  
  a <- plot_hedges_g_adjusted_for_noise(df, 
                                        sigma_s_vals = c(1), 
                                        sigma_t_vals = c(0.5, 1, 2, 4, 10), 
                                        r_thresh = 4)
  b <- plot_simulated_effects(d_type = 'g', 
                              plot_type = 'pure', 
                              Ns = seq(10, 300, 10), 
                              ks = seq(10, 300, 10), 
                              sigma_ratio = c(0.1, 0.25, 0.5, 1, 2))
 
  p <- plot_grid(
    a, NULL, b, 
    nrow = 1, ncol = 3, 
    labels = c("A", "", "B"),
    rel_widths = c(0.5, 0.05, 0.5)
  )
  
  ggsave(
    filename = file.path(save_path, paste0("epoch_simulation_plot.", ext)),
    plot = p,
    width = 7,
    height = 5,
    units = "in",
    dpi = 300,
    device = ext,
    bg = "white"
  )
}

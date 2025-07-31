figure_additional_hedges_g <- function(df, save_path, ext = 'svg') {
  fig <- plot_hedges_g(df = df, with_clustering = T, with_regression = T)
  
  ggsave(
    filename = file.path(save_path, paste0("figS1_additional_hedges_g.", ext)),
    plot = fig,
    width = 7,
    height = 5,
    units = "in",
    dpi = 300,
    device = ext,
    bg = "white"
  )
}
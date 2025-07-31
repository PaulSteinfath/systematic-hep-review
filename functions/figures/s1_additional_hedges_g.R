figure_additional_hedges_g <- function(df, save_path, ext = 'svg') {
  fig <- plot_hedges_g(df = df, with_clustering = T, with_regression = T)
  
  ggsave(
    filename = file.path(save_path, paste0("figS1_additional_hedges_g.", ext)),
    plot = fig,
    width = 190,
    height = 127,
    units = "mm",,
    dpi = 300,
    device = ext,
    bg = "white"
  )
}
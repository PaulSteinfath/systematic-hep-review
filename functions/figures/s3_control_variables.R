figure_control_variables <- function(df, save_path, ext = 'svg') {
  fig <- plot_control_variables(df)
  
  ggsave(
    filename = file.path(save_path, paste0("figS3_control_variables.", ext)),
    plot = fig,
    width = 150,
    height = 210,
    units = "mm",
    dpi = 300,
    device = ext,
    bg = "white"
  )
}
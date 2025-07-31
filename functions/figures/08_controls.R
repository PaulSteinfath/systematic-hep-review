figure_controls <- function(df, save_path, ext = 'png') {
  a <- plot_control_categories(df = df)
  b <- plot_ecg_controls(df = df) 
  
  p <- plot_grid(
    a, b,
    ncol = 2, 
    labels = c("A","B"),
    rel_widths = c(1, 1),
    align = "h"
  )
  
  ggsave(
    filename = file.path(save_path, paste0("fig8_controls.", ext)),
    plot = p,
    width = 190,
    height = 88.9,
    units = "mm",
    dpi = 300,
    device = ext,
    bg = "white"
  )
}
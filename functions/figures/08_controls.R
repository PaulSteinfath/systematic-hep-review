figure_controls <- function(df, save_path = NULL, ext = 'png') {
  a <- plot_control_categories(df = df)
  b <- plot_ecg_controls(df = df) 
  
  fig <- plot_grid(
    a, b,
    ncol = 2, 
    labels = c("A","B"),
    rel_widths = c(0.95, 1.05),
    align = "h"
  )
  
  if (!is.null(save_path)) {
    save_figure(fig,
                aspect_ratio = 0.45,  # height / width
                save_path,
                filename = "fig8_controls",
                ext = ext)
  }
  
  return(fig)
}
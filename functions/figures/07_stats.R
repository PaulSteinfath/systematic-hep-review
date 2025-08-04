figure_stats <- function(df, save_path = NULL, ext = 'png') {
  
  a <- hist_panel(df, 
                  col = "Preregistration", 
                  title = "Preregistration", 
                  force.numeric = F, 
                  use_proportion = T, 
                  discrete = T, 
                  custom_labels = c("0" = "No", "1" = "Yes"))
  b <- hist_panel(df = df, 
                  col = "sample_size", 
                  title = "Sample size", 
                  x.label = "Number of Subjects", 
                  use_proportion = T)
  c <- hist_panel(df = df, 
                  col = "groups", 
                  title = "Groups", 
                  x.label = "Number of Groups", 
                  binwidth = 1, 
                  use_proportion = T)  
  d <- hist_panel(df = df,
                  col = "conditions", 
                  title = "Conditions", 
                  x.label = "Number of Conditions", 
                  binwidth = 1, 
                  use_proportion = T)  
  e <- hist_panel(df = df, 
                  col = "trials_Mean", 
                  title = "Averaged epochs", 
                  force.numeric = T, 
                  x.label = "Number of Averaged Epochs", 
                  use_proportion = T)
  
  f <- hist_panel(df = df, 
                  col = "statistics", 
                  title = "Statistical tests",
                  x.label = "", 
                  discrete = T, 
                  tilt_labels = F,
                  decreasing = F,
                  allowed = allowed$statistics) + coord_flip()
  g <- plot_hedges_g(df = df)
  
  first_row <- plot_grid(
    a,b,c,d,e,
    ncol = 5, labels = c("A","B", "C", "D","E"), rel_widths = c(0.8, 0.8, 0.8,0.8,1.2),
    align = "h"
  )
  second_row <- plot_grid(
    f,g,
    ncol = 2, labels = c("F","G"),
    rel_widths = c(1, 1),
    align = "h"
  )
  
  # Add 10% space between rows
  spacer <- plot_grid(NULL)
  
  p <- plot_grid(first_row, spacer, second_row,
                 ncol = 1, rel_heights = c(1, 0.1, 1.4))
  
  if (!is.null(save_path)) {
    ggsave(
      filename = file.path(save_path, paste0("fig7_stats.", ext)),
      plot = p,
      width = 10,
      height = 6.5,
      units = "in",
      dpi = 300,
      device = ext,
      bg = "white"
    )
  }
  
  return(p)
}
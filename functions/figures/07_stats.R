figure_stats <- function(df, save_path = NULL, ext = 'png') {
  
  bins <- 20
  breaks_sample_size <- c(1, 10, 100, 1000)
  breaks_trials <- c(10, 100, 1000, 5000)
  
  df$sample_size_log <- log10(df$sample_size)
  a <- hist_panel(df = df, 
                  col = "sample_size_log", 
                  title = "Sample size", 
                  x.label = "Number of participants",
                  bins = 20,
                  force.numeric = T,
                  use_proportion = T) + 
    scale_x_continuous(breaks = log10(breaks_sample_size), 
                       labels = \(x) round(10^(x), 0)) + 
    expand_limits(x = c(0, 3))

  df$groups_binned <- case_when(
    df$groups <= 3 ~ as.character(df$groups),
    TRUE ~ "3+"
  )
  df$groups_binned <- factor(df$groups_binned,
                             levels = c("1", "2", "3+"))
  b <- hist_panel(df = df, 
                  col = "groups_binned", 
                  title = "Groups", 
                  x.label = "Number of groups", 
                  discrete = T,
                  use_proportion = T)
  
  df$conditions_binned <- case_when(
    df$conditions <= 5 ~ as.character(df$conditions),
    TRUE ~ "5+"
  )
  df$conditions_binned <- factor(df$conditions_binned,
                                 levels = c("1", "2", "3", "4", "5+"))
  c <- hist_panel(df = df,
                  col = "conditions_binned", 
                  title = "Conditions", 
                  x.label = "Number of conditions", 
                  discrete = T, 
                  use_proportion = T,
                  preserve_order = T)
  
  
  df$trials_mean_log <- log10(df$trials_Mean)
  d <- hist_panel(df = df, 
                  col = "trials_mean_log", 
                  title = "Averaged epochs",
                  bins = 20,
                  force.numeric = T, 
                  x.label = "Number of epochs", 
                  use_proportion = T) + 
    scale_x_continuous(breaks = log10(breaks_trials), 
                       labels = \(x) round(10^(x), 0)) + 
    expand_limits(x = c(1, 3))
  
  e <- hist_panel(df = df, 
                  col = "statistics", 
                  title = "Statistical tests",
                  x.label = "", 
                  discrete = T, 
                  tilt_labels = F,
                  decreasing = F,
                  allowed = allowed$statistics) + coord_flip()
  f <- plot_hedges_g(df = df)
  
  first_row <- plot_grid(
    a, b, c, d,
    ncol = 4, 
    labels = c("A", "B", "C", "D"), 
    rel_widths = c(0.9, 0.6, 0.8, 0.9),
    align = "h"
  )
  second_row <- plot_grid(
    e, f,
    ncol = 2, labels = c("E", "F"),
    rel_widths = c(1, 1),
    align = "h"
  )
  
  # Add 3% space between rows
  spacer <- plot_grid(NULL)
  
  fig <- plot_grid(first_row, spacer, second_row,
                   ncol = 1, rel_heights = c(1, 0.1, 1.3))
  
  if (!is.null(save_path)) {
    save_figure(fig,
                aspect_ratio = 0.84,  # height / width
                save_path,
                filename = "fig7_stats",
                ext = ext)
  }
  
  return(fig)
}
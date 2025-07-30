figure_stats <- function(df, save_path, ext = 'png') {
  
  a <- hist_panel(df, 
                  col = "Preregistration", 
                  title = "Preregistration", 
                  force.numeric = F, 
                  use_proportion = T, 
                  discrete = T, 
                  custom_labels = c("No","Yes"))
  b <- hist_panel(df = df, col = "sample_size", title = "Sample Size", x.label = "Number of Subjects", use_proportion = T)
  c <- hist_panel(df = df, col = "groups", title = "Groups", x.label = "Number of Groups", binwidth = 1, use_proportion = T)  
  d <- hist_panel(df = df, col = "conditions", title = "Conditions", x.label = "Number of Conditions", binwidth = 1, use_proportion = T)  
  e <- hist_panel(df = df, col = "trials_Mean", title = "Averaged Epochs", force.numeric = T, x.label = "Number of Averaged Epochs", use_proportion = T)
  
  f <- hist_panel(df = df, 
                  col = "statistics", 
                  title = "Statistical Tests",
                  x.label = "", 
                  discrete = T, 
                  tilt_labels = F,
                  decreasing = F,
                  allowed = c("t-test" = "t-test",
                              "Correlation" = "Correlation",
                              "Regression" = "Regression", 
                              "ANOVA" = "ANOVA",
                              "Non-parametric comparison" = "Non-parametric\ncomparison",
                              "Classification" = "Classification",
                              "F-test" = "F-test")) + coord_flip()
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
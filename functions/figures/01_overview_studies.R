figure_overview_studies <- function(df, save_path = NULL, ext = "svg") {
  p_year <- hist_panel(df, "Year", force.numeric = T, 
                       title = "Publication year", 
                       x.label = "Year", 
                       binwidth = 2, use_proportion = F)
  
  p_modality <- hist_panel(df, "modality", discrete = T,
                           title = "Imaging modality")
  
  p_condition <- hist_panel(df, "study_category", 
                            discrete = T,
                            custom_labels = c("Task" = "Task only", 
                                              "Rest" = "Resting-state\nonly",
                                              "Both" = "Task &\nresting-state"),
                            title = "Experimental setting")
  
  fig <- plot_grid(p_year, p_modality, p_condition,
                   nrow = 1, labels = c("B", "C", "D"), align = "h",
                   axis = "bt")
  
  if (!is.null(save_path)) {
    save_figure(fig,
                aspect_ratio = 0.3,  # height / width
                save_path,
                filename = "fig1_overview_studies",
                ext = ext)
  }
  
  return(fig)
}

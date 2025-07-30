figure_overview_studies <- function(df, save_path, ext = "svg") {
  p_year <- hist_panel(df, "Year", force.numeric = T, 
                       title = "Publication year", 
                       x.label = "Year", 
                       binwidth = 2, use_proportion = F)
  
  p_modality <- hist_panel(df, "modality", discrete = T,
                           title = "Imaging modality")
  
  p_condition <- hist_panel(df, "study_category", 
                            discrete = T,
                            title = "Experimental setting")
  
  fig <- plot_grid(p_year, p_modality, p_condition,
                   nrow = 1, labels = c("B", "C", "D"), align = "h",
                   axis = "bt")
  
  ggsave(
    filename = file.path(save_path, paste0("fig1_overview_studies.", ext)),
    plot = fig,
    width = 10,
    height = 3,
    units = "in",
    dpi = 300,
    device = ext,
    bg = "white"
  )
}

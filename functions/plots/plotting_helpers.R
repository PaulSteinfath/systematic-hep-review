library(dplyr)
library(ggplot2)

plot_theme_default <- theme_classic(base_family = "sans") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 9),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        legend.text = element_text(size = 8), 
        axis.title.x = element_text(size = 9, margin = margin(t = 4)),
        axis.title.y = element_text(size = 9))

plot_fill_default_single <- "#696969"

plot_fill_default <- c("#696969","#A9A9A9","#8a8888")

leads_palette <- c('Lead I' = '#fc8d62', 
                   'Lead II' = '#66c2a5', 
                   'Lead III' = '#8da0cb')

r_t_peak_palette <- c("R-peak" = "#696969", "T-peak" = "#E69F00")


effect_sizes_Coll2020 <- data.frame(
  kind = c("Attention to the heart",
           "Interoceptive performance",
           "Arousal",
           "Patients vs. healthy controls"),
  value = c(0.37, 0.35, 0.72, 0.49)
)
palette_Coll2020 <- c("Attention to the heart" = "#1b9e77",
                      "Interoceptive performance" = "#d95f02",
                      "Arousal" = "#7570b3",
                      "Patients vs. healthy controls" = "#e7298a")

theme_set(plot_theme_default)


prepare_column_plot_data <- function(df, 
                                     column_col, 
                                     value_col, 
                                     method_columns, 
                                     column_mapping_readable, 
                                     pipeline_colors = NULL, 
                                     fixed = FALSE) {
  # Apply readable column names
  df[[column_col]] <- apply_column_mapping(df[[column_col]], column_mapping_readable)
  
  # Add Step column if coloring by pipeline group
  if (!is.null(pipeline_colors)) {
    df$Step <- sapply(df[[column_col]], function(readable) {
      var_name <- column_mapping_readable[readable]
      if (is.na(var_name)) var_name <- readable
      get_pipeline_step(var_name)
    })
    df$Step <- factor(df$Step, levels = names(pipeline_colors))
  }
  
  # Set column factor levels
  if (fixed) {
    fixed_order <- apply_column_mapping(method_columns, column_mapping_readable)
    df[[column_col]] <- factor(df[[column_col]], levels = fixed_order)
  } else {
    if (!is.null(pipeline_colors)) {
      df <- df %>%
        dplyr::arrange(match(Step, c("Statistics", "HER Estimation", "Preprocessing", "Acquisition", "Experiment")), dplyr::desc(.data[[value_col]])) %>%
        dplyr::mutate(!!column_col := factor(.data[[column_col]], levels = unique(.data[[column_col]])))
    } else {
      df <- df %>%
        dplyr::arrange(dplyr::desc(.data[[value_col]])) %>%
        dplyr::mutate(!!column_col := factor(.data[[column_col]], levels = unique(.data[[column_col]])))
    }
  }
  
  return(df)
}

custom_theme <- function() {
  theme(
    plot.title = element_text(size = 11), 
    plot.subtitle = element_text(size = 9)
  )
}

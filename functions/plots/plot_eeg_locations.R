library(eegUtils)
library(ggplot2)
library(tidyr)


get_channel_freq <- function(df, col, ch) {
  in_layout <- sapply(df$meeg_locations, 
                      \(x) grepl(ch, x, fixed = T))
  in_selection <- sapply(df[[col]], 
                         \(x) grepl(ch, x, fixed = T))
  
  n_used <- sum(in_layout)
  if (n_used == 0) {
    return(0)
  }
  
  n_selected <- sum(in_selection)
  n_selected / n_used
}


count_occurrences <- function(df, col, group_col = 'PMID', add.locs = T) {
  layout <- unique(df$meeg_layout)
  if (length(layout) > 1) {
    layout_desc <- paste(layout, collapse = ", ")
    warning(paste("count_occurrences: expected layout to be same",
                  "across the data frame, got:", layout_desc))
  }
  
  df_distinct <- df %>% distinct(meeg_layout, meeg_locations,
                                 !!sym(group_col), !!sym(col))
  freqs <- sapply(ch_names[[layout]], get_channel_freq,
                  df = df_distinct, col = col)
  
  df_freq <- data.frame(meeg_layout = layout,
                        electrode = ch_names[[layout]],
                        freq = freqs)
  
  if (add.locs) {
    montage <- if (layout == "biosemi128") "biosemi128" else NULL
    df_freq <- df_freq %>%
      electrode_locations(montage = montage)
  }
  
  df_freq
}


plot_eeg_locations_separate <- function(df, lim = 1.0) {
  p <- ggplot(df,
              aes(x = x,
                  y = y,
                  fill = freq,
                  z = freq,
                  label = electrode)) +
    geom_topo(grid_res = 200,
              chan_size = rel(0.25), 
              head_size = rel(0.5),
              color = 'black',
              linetype = 'solid',
              linewidth = rel(0.1)) + 
    scale_fill_distiller(palette = "Reds",
                         direction = 1,
                         limits = c(0, lim)) + 
    facet_wrap(. ~ meeg_layout, nrow = 1) +
    theme_void() + 
    coord_equal()
  
  p
}


plot_eeg_locations_combined <- function(df) {
  
}


plot_eeg_locations <- function(df, 
                               col,
                               group_col = 'PMID',
                               layouts = c("standard19", "standard61", "biosemi128"), 
                               combined = T) {
  # Get the number of occurrences for each channel in each layout
  df_sel <- df[df$meeg_layout %in% layouts,]
  counts <- by(df_sel, df_sel$meeg_layout, count_occurrences,
               col = col, group_col = group_col)
  df_counts <- bind_rows(lapply(counts, as.data.frame))
  
  # Plot the results for each layout separately or combined
  if (combined) {
    p <- plot_eeg_locations_combined(df_counts)
  } else {
    p <- plot_eeg_locations_separate(df_counts)
  }
  
  p
}
library(eegUtils)
library(ggplot2)
library(tidyr)

find_channel <- function(ch, ch_list) {
  # Resolve T3-T7, T4-T8, T5-P7, T6-P8
  # ch <- recode(ch, 
  #              "T3" = "T7", "T4" = "T8",
  #              "T5" = "P7", "T6" = "P8")
  grepl(ch, ch_list, fixed = T)
}

layout <- "biosemi128"
montage <- NULL
if (layout == "biosemi128") {
  montage <- "biosemi128"
}
df_channels <- df_included %>%
  mutate(eeg_layout = recode(eeg_layout,
                             "10-10" = "standard61",
                             "10-20" = "standard19")) %>%
  filter(modality == "EEG",
         eeg_layout == layout,
         stats_hypothesis != "none")
for (ch in ch_names[[layout]]) {
  df_channels[[paste0(ch, "_used")]] <- sapply(df_channels$eeg_locations, find_channel, ch = ch)
  df_channels[[paste0(ch, "_selected")]] <- sapply(df_channels$hep_channels_selected, find_channel, ch = ch)
  # df_channels[[paste0(ch, "_significant")]] <- sapply(df_channels$significant_channels, find_channel, ch = ch)
}
df_channels$counter <- 1
summary <- df_channels %>%
  select(-hep_channels_selected) %>%
  group_by(hep_window_type) %>%
  summarise(across(ends_with(c("_used", "_selected", "_significant", "counter")), \(x) sum(x, na.rm = T)))
for (ch in ch_names[[layout]]) {
  # summary[[paste0(ch, "_freq")]] <- summary[[paste0(ch, "_selected")]] / summary$counter
  summary[[paste0(ch, "_freq")]] <- summary[[paste0(ch, "_selected")]] / summary[[paste0(ch, "_used")]]
}
summary_freq <- summary %>%
  select(c(hep_window_type, ends_with("_freq"))) %>%
  pivot_longer(
    cols = !hep_window_type,
    names_to = c("electrode", "dummy"),
    names_sep = "_",
    values_to = "count"
  ) %>%
  electrode_locations(montage = montage)

lim = 1.1 * max(summary_freq$count)
p_window_type <- ggplot(summary_freq,
                        aes(x = x,
                            y = y,
                            fill = count,
                            z = count,
                            label = electrode)) +
  geom_topo(grid_res = 200,
            chan_size = rel(0.25), 
            head_size = rel(0.5),
            color = 'black',
            linetype = 'solid',
            linewidth = rel(0.1)) + 
  scale_fill_distiller(palette = "Reds",
                       direction = 1,
                       limits=c(0, lim)) + 
  facet_wrap(. ~ hep_window_type, nrow = 1) +
  theme_void() + 
  coord_equal()

p_window_type
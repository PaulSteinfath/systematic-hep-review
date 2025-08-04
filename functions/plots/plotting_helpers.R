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

custom_theme <- function() {
  theme(
    plot.title = element_text(size = 11), 
    plot.subtitle = element_text(size = 9)
  )
}

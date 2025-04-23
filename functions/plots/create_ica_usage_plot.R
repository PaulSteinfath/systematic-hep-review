#' Create a bar plot showing ICA usage with epoched/continuous data
create_ica_usage_plot <- function(df) {
    # Colors for the plot
    colors <- c(
        "No ICA" = plot_fill_default_single,
        "Epoched" = plot_fill_default[3],
        "Continuous" = plot_fill_default[2]
    )

    # Count No ICA studies
    no_ica_count <- df %>%
        distinct(PMID, ICA) %>%
        filter(ICA == 0) %>%
        nrow()

    # Put in own df
    no_ica_data <- data.frame(
        ICA = factor(0),
        data_type = "No ICA",
        count = no_ica_count
    )

    # Count and prepare epoched / continuous ICA studies
    ica_data <- df %>%
        distinct(PMID, ICA, ica_on_epochs) %>%
        filter(ICA == 1) %>%
        summarise(
            Epoched = sum(ica_on_epochs == 1),
            Continuous = sum(ica_on_epochs == 0),
            .by = ICA
        ) %>%
        pivot_longer(
            cols = c(Epoched, Continuous),
            names_to = "data_type",
            values_to = "count"
        )

    # Combine datasets
    plot_data <- bind_rows(no_ica_data, ica_data)

    # Create plot
    ggplot(plot_data, aes(x = factor(ICA), y = count, fill = data_type)) +
        # Bars with reduced width
        geom_bar(
            stat = "identity",
            width = 1, # width of the bars
        ) +
        theme(aspect.ratio = 2 / 1) +
        # Labels
        geom_text(
            aes(label = ifelse(data_type == "No ICA",
                sprintf("n=%d", count),
                sprintf("%s\n(n=%d)", data_type, count)
            )),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 2.8
        ) +
        # Scales with adjusted spacing
        scale_fill_manual(values = colors) +
        scale_x_discrete(
            labels = c("No ICA", "ICA"),
            expand = expansion(add = c(0.1, 0.1)) 
        ) +
        scale_y_continuous(expand = expansion(mult = c(0, .1))) +
        # Labels
        labs(
            x = "ICA Application",
            y = "Number of Studies"
        ) +
        # Theme
        theme_classic(base_family = "sans") +
        theme(
            aspect.ratio = 2 / 1, # change aspect ratio to make plot taller /  narrower
            legend.position = "none",
            plot.title = element_text(hjust = 0.1, size = 10),
            axis.title = element_text(size = 10),
            #axis.title.x = element_text(margin = margin(t = 5)), # Specific adjustment for x-axis title
            axis.text = element_text(size = 9),
            axis.text.x = element_text(margin = margin(t = 1)), # Reduced top margin
            panel.grid.major.x = element_blank()
        )
}

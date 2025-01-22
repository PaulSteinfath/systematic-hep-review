library(DT)
library(dplyr)
library(ggplot2)
library(palmerpenguins)
library(shiny)

function(input, output) {
  output$table <- 
    renderDT(
      df_included,
      filter = "top",
      options = list(
        pageLength = 25
      )
    )
  
  output$yearPlot <- renderPlot({
    ggplot(df_included[input$table_rows_all,], aes(x = Year)) +
      geom_histogram(binwidth = 1, color = "white", fill = "lightgray") +
      geom_histogram(data = df_included[input$table_rows_all,], binwidth = 1, color = "white", fill = "blue") +
      theme_classic()
  }, res = 96)
  
  output$filterPlot <- renderPlot({
    create_filter_plots(df_included[input$table_rows_all,])
  }, res = 96)

  output$overviewHistograms <- renderPlot({
    create_overview_panel(df_included[input$table_rows_all,])
  }, res = 96)
}

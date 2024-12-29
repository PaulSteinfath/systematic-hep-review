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
    df_selected <- df_included[input$table_rows_all,]
    ggplot(df_included, aes(x = Year)) +
      geom_histogram(binwidth = 1, color = "white", fill = "lightgray") +
      geom_histogram(data = df_selected, binwidth = 1, color = "white", fill = "blue") +
      theme_classic()
  }, res = 96)
}

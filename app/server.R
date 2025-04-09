library(DT)
library(dplyr)
library(ggplot2)
library(palmerpenguins)
library(shiny)

function(input, output) {

  output$table <- renderDT(
    df_included,
    filter = "top",
    options = list(pageLength = 25)
  )
  
  # Wrap df_selected in a reactive
  df_selected <- reactive({
    # NOTE: use all rows by default
    if (is.null(input$table_rows_all)) {
      df_included
    } else {
      df_included[input$table_rows_all, ] 
    }
  })
  
  output$yearPlot <- renderPlot({
      ggplot(df_selected(), aes(x = Year)) +
        geom_histogram(binwidth = 1, color = "white", fill = "lightgray") +
        geom_histogram(data = df_selected(), binwidth = 1, color = "white", fill = "blue") +
        theme_classic()
    },
    res = 96
  )
    
  output$acquisitionPrepPlot <- renderPlot({
      eeg_acq_prep(df_selected())
    },
    res = 96
  )
  
  output$ecgSummaryPlot <- renderPlot({
      ecg_summary(df_selected())
    }, res = 96
  )
}

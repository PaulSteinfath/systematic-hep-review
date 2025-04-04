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
    df_included[input$table_rows_all, ]
  })
  
  output$yearPlot <- renderPlot({
    ggplot(df_selected(), aes(x = Year)) +
      geom_histogram(binwidth = 1, color = "white", fill = "lightgray") +
      geom_histogram(data = df_selected(), binwidth = 1, color = "white", fill = "blue") +
      theme_classic()
  }, res = 96)
  
  output$acquisitionPrepPlot <- renderPlot({
    eeg_acq_prep(df_selected())
  }, res = 96)
  
  output$cfaRemovalPlot <- renderPlot({
    cfa_removal(df_selected())
  }, res = 96)
  
  output$controlVariablesPlot <- renderPlot({
    create_control_variables_plot(df_selected())
  }, res = 96)
  
  output$timeWindowsPlot <- renderPlot({
    # Show either the averaging or clustering plot based on user selection
    result <- create_time_windows_with_ecg_plot(df_selected(), input$timeWindowType)
    
    # If only one plot is requested, result is the plot itself
    # If both plots are requested, we need to extract the right one
    if (inherits(result, "list")) {
      if (input$timeWindowType == "1") {
        return(result$averaging)
      } else {
        return(result$clustering)
      }
    } else {
      return(result)
    }
  }, res = 96)
  output$ecgSummaryPlot <- renderPlot({
      ecg_summary(df_selected())
    }, res = 96
  )
}

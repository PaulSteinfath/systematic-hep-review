library(DT)
library(dplyr)
library(ggplot2)
library(palmerpenguins)
library(shiny)

# Source required functions and load data
source("../functions/figures.R")
source("../functions/preprocess.R") 
source("../config.R")

# Load the data if not already available
if (!exists("df_included")) {
  df_full <- load_data("../data/HEP - Pubmed Results.csv", "../data/HEP - Manual.csv")
  c(df_screening, df_included) %<-% preprocess(df_full)
}


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
  
  output$ecgSummaryPlot <- renderPlot({
      ecg_summary(df_selected())
    }, res = 96
  )

  output$hepTimeWindowsCombinedPlot <- renderPlot({
    create_hep_time_windows_summary_plot(df_selected())
  }, res = 96)
  
  # EEG Locations by Approach Plot
  output$eegLocationsByApproachPlot <- renderPlot({
    req(input$approach_reference_var)
    
    tryCatch({
      # Check if we have enough data for each reference value
      filtered_data <- df_selected()
      
      # Filter to only EEG data (since EEG locations only work for EEG)
      eeg_data <- filtered_data %>% filter(modality == "EEG")
      
      if(nrow(eeg_data) == 0) {
        ggplot() + 
          geom_text(aes(x = 0, y = 0, label = "No EEG data available in current selection"), size = 4) +
          theme_void()
      } else {
        # Get all available values for the reference variable
        available_values <- unique(eeg_data[[input$approach_reference_var]])
        available_values <- available_values[!is.na(available_values) & available_values != "" & available_values != "unknown"]
        
        if(length(available_values) < 2) {
          ggplot() + 
            geom_text(aes(x = 0, y = 0, 
                         label = paste("Need at least 2 different values for", input$approach_reference_var, "to compare")), 
                     size = 4) +
            theme_void()
        } else {
          data_counts <- table(eeg_data[[input$approach_reference_var]])
          sufficient_values <- names(data_counts)[data_counts >= 3]
          
          if(length(sufficient_values) < 2) {
            ggplot() + 
              geom_text(aes(x = 0, y = 0, 
                           label = "Insufficient EEG data: Need at least 3 studies per group"), 
                       size = 4) +
              theme_void()
          } else {
            # Additional check: ensure we have valid EEG location data
            eeg_data_with_locations <- eeg_data %>%
              filter(!is.na(eeg_locations), eeg_locations != "", eeg_locations != "unknown")
            
            if(nrow(eeg_data_with_locations) < 5) {
              ggplot() + 
                geom_text(aes(x = 0, y = 0, 
                             label = "Insufficient EEG location data for plotting\n(Need at least 5 studies with location info)"), 
                         size = 4) +
                theme_void()
            } else {
              # Use all sufficient values for comparison
              eeg_locations_summary(eeg_data, 
                                  reference_var = input$approach_reference_var,
                                  reference_values = sufficient_values)
            }
          }
        }
      }
    }, error = function(e) {
      ggplot() + 
        geom_text(aes(x = 0, y = 0, label = paste("Error creating plot:", e$message)), size = 4) +
        theme_void()
    })
  }, res = 96)
}

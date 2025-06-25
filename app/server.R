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
  
  # Dynamic UI for approach reference values
  output$approach_reference_values_ui <- renderUI({
    req(input$approach_reference_var)
    
    # Get unique values for the selected reference variable from the full dataset
    unique_vals <- unique(df_included[[input$approach_reference_var]])
    unique_vals <- unique_vals[!is.na(unique_vals) & unique_vals != "" & unique_vals != "unknown"]
    
    # Debug: print available values
    cat("Available values for", input$approach_reference_var, ":", paste(unique_vals, collapse = ", "), "\n")
    
    # Set default selection based on the reference variable
    default_selection <- switch(input$approach_reference_var,
      "hep_approach" = c("Averaging", "Clustering"),
      "hep_window_type" = c("Primary", "Secondary"),
      "hep_relative_to" = c("R-peak", "T-peak"),
      "modality" = c("EEG", "MEG"),
      if(length(unique_vals) >= 2) unique_vals[1:2] else unique_vals
    )
    
    # Only include defaults that actually exist in the data
    default_selection <- intersect(default_selection, unique_vals)
    if(length(default_selection) == 0 && length(unique_vals) >= 2) {
      default_selection <- unique_vals[1:2]
    }
    
    cat("Default selection:", paste(default_selection, collapse = ", "), "\n")
    
    checkboxGroupInput("approach_reference_values",
                      "Select Values to Compare:",
                      choices = unique_vals,
                      selected = default_selection)
  })
  
  # Dynamic UI for window type reference values  
  output$window_reference_values_ui <- renderUI({
    req(input$window_reference_var)
    
    # Get unique values for the selected reference variable from the full dataset
    unique_vals <- unique(df_included[[input$window_reference_var]])
    unique_vals <- unique_vals[!is.na(unique_vals) & unique_vals != "" & unique_vals != "unknown"]
    
    # Set default selection based on the reference variable
    default_selection <- switch(input$window_reference_var,
      "hep_window_type" = c("Primary", "Secondary"),
      "hep_approach" = c("Averaging", "Clustering"),
      "hep_relative_to" = c("R-peak", "T-peak"),
      "modality" = c("EEG", "MEG"),
      if(length(unique_vals) >= 2) unique_vals[1:2] else unique_vals
    )
    
    # Only include defaults that actually exist in the data
    default_selection <- intersect(default_selection, unique_vals)
    if(length(default_selection) == 0 && length(unique_vals) >= 2) {
      default_selection <- unique_vals[1:2]
    }
    
    checkboxGroupInput("window_reference_values",
                      "Select Values to Compare:",
                      choices = unique_vals,
                      selected = default_selection)
  })
  
  # EEG Locations by Approach Plot
  output$eegLocationsByApproachPlot <- renderPlot({
    req(input$approach_reference_var, input$approach_reference_values)
    
    tryCatch({
      if(length(input$approach_reference_values) != 2) {
        ggplot() + 
          geom_text(aes(x = 0, y = 0, label = "Please select exactly 2 values to compare"), size = 4) +
          theme_void()
      } else {
        eeg_locations_summary(df_included, 
                            reference_var = input$approach_reference_var,
                            reference_values = input$approach_reference_values)
      }
    }, error = function(e) {
      ggplot() + 
        geom_text(aes(x = 0, y = 0, label = paste("Error creating plot:", e$message)), size = 4) +
        theme_void()
    })
  }, res = 96)
  
  # EEG Locations by Window Type Plot
  output$eegLocationsByWindowTypePlot <- renderPlot({
    req(input$window_reference_var, input$window_reference_values)
    
    tryCatch({
      if(length(input$window_reference_values) != 2) {
        ggplot() + 
          geom_text(aes(x = 0, y = 0, label = "Please select exactly 2 values to compare"), size = 4) +
          theme_void()
      } else {
        eeg_locations_summary(df_included,
                            reference_var = input$window_reference_var, 
                            reference_values = input$window_reference_values)
      }
    }, error = function(e) {
      ggplot() + 
        geom_text(aes(x = 0, y = 0, label = paste("Error creating plot:", e$message)), size = 4) +
        theme_void()
    })
  }, res = 96)
}

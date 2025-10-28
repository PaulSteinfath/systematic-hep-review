library(DT)
library(dplyr)
library(ggplot2)
library(shiny)

# Source the project initialization
source("init_workspace.R")

# Load the data 
df_included <- read.csv("data/derivatives/included.csv", stringsAsFactors = FALSE)

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
  
  # Overview Studies Plot
  output$overviewStudiesPlot <- renderPlot({
    tryCatch({
      figure_overview_studies(df_selected(), save_path = NULL)
    }, error = function(e) {
      ggplot() + 
        geom_text(aes(x = 0, y = 0, label = paste("Error:", e$message)), size = 4) +
        theme_void()
    })
  }, res = 96)
  
  # Overview Pipelines Plot  
  output$overviewPipelinesPlot <- renderPlot({
    tryCatch({
      figure_overview_pipelines(df_selected(), save_path = NULL)
    }, error = function(e) {
      ggplot() + 
        geom_text(aes(x = 0, y = 0, label = paste("Error:", e$message)), size = 4) +
        theme_void()
    })
  }, res = 96)
  
  # M/EEG Acquisition & Preprocessing Plot
  output$meegAcqPrepPlot <- renderPlot({
    tryCatch({
      figure_meeg_acq_prep(df_selected(), save_path = NULL)
    }, error = function(e) {
      ggplot() + 
        geom_text(aes(x = 0, y = 0, label = paste("Error:", e$message)), size = 4) +
        theme_void()
    })
  }, res = 96)
  
  # ECG Summary Plot
  output$ecgSummaryPlot <- renderPlot({
    tryCatch({
      figure_ecg_summary(df_selected(), save_path = NULL)
    }, error = function(e) {
      ggplot() + 
        geom_text(aes(x = 0, y = 0, label = paste("Error:", e$message)), size = 4) +
        theme_void()
    })
  }, res = 96)
  

  # CFA Approaches Plot
  output$cfaApproachesPlot <- renderPlot({
    tryCatch({
      figure_cfa_removal(df_selected(), save_path = NULL)
    }, error = function(e) {
      ggplot() + 
        geom_text(aes(x = 0, y = 0, label = paste("Error:", e$message)), size = 4) +
        theme_void()
    })
  }, res = 96)
  

  # HER Estimation Summary Plot
  output$herEstimationPlot <- renderPlot({
    tryCatch({
      figure_her_estimation_summary(df_selected(), save_path = NULL)
    }, error = function(e) {
      ggplot() + 
        geom_text(aes(x = 0, y = 0, label = paste("Error:", e$message)), size = 4) +
        theme_void()
    })
  }, res = 96)
  
  # Statistics Plot
  output$statsPlot <- renderPlot({
    tryCatch({
      figure_stats(df_selected(), save_path = NULL)
    }, error = function(e) {
      ggplot() + 
        geom_text(aes(x = 0, y = 0, label = paste("Error:", e$message)), size = 4) +
        theme_void()
    })
  }, res = 96)
  
  # Controls Plot
  output$controlsPlot <- renderPlot({
    tryCatch({
      figure_controls(df_selected(), save_path = NULL)
    }, error = function(e) {
      ggplot() + 
        geom_text(aes(x = 0, y = 0, label = paste("Error:", e$message)), size = 4) +
        theme_void()
    })
  }, res = 96)
  }

library(DT)
library(ggplot2)
library(shiny)

# Source the project initialization
source("init_workspace.R")

# Load the data 
df_included <- read.csv("data/derivatives/included.csv", stringsAsFactors = FALSE)

# Prepare table-friendly versions of columns so filters pick the right input type
prepare_filter_data <- function(df) {
  na_tokens <- c("na", "unknown")
  
  binary_map <- list(
    ICA = c("0" = "No", "1" = "Yes"),
    ica_on_epochs = c("0" = "No", "1" = "Yes"),
    setting = c("0" = "Task", "1" = "Resting"),
    preregistration = c("0" = "No", "1" = "Yes"),
    patients = c("0" = "No", "1" = "Yes"),
    new_data = c("0" = "No", "1" = "Yes"),
    clustering = c("0" = "No", "1" = "Yes"),
    significant_test = c("0" = "No", "1" = "Yes"),
    averaging_channels = c("0" = "No", "1" = "Yes"),
    averaging_time = c("0" = "No", "1" = "Yes"),
    clean_noisy_epochs = c("FALSE" = "No", "TRUE" = "Yes"),
    clean_bad_channels = c("FALSE" = "No", "TRUE" = "Yes"),
    has_resting = c("FALSE" = "No", "TRUE" = "Yes"),
    has_task = c("FALSE" = "No", "TRUE" = "Yes"),
    clean_noisy_epochs = c("FALSE" = "No", "TRUE" = "Yes"),
    clean_bad_channels = c("FALSE" = "No", "TRUE" = "Yes"),
    reject_cfa_ics = c("FALSE" = "No", "TRUE" = "Yes"),
    cfa_use_minimal_rr = c("FALSE" = "No", "TRUE" = "Yes"),
    cfa_use_minimal_artifact_window = c("FALSE" = "No", "TRUE" = "Yes"),
    cfa_csd = c("FALSE" = "No", "TRUE" = "Yes"),
    cfa_regress = c("FALSE" = "No", "TRUE" = "Yes"),
    cfa_pca = c("FALSE" = "No", "TRUE" = "Yes"),
    cfa_subtract_rest = c("FALSE" = "No", "TRUE" = "Yes")
  )
  
  numeric_cols <- c(
    "year", "sample_size", "meeg_num_electrodes", "meeg_sfreq_orig", 
    "meeg_sfreq_final", "meg_num_grad", "meg_num_mag", "ecg_num_electrodes",
    "ecg_sfreq_orig", "ecg_sfreq_final", "length_min", "high_pass", "low_pass",
    "groups", "conditions", "hep_start", "hep_end", "ecg_high_pass",
    "ecg_low_pass", "baseline_start_ms", "baseline_end_ms", "permutations",
    "significant_start_ms", "significant_end_ms", "cfa_minimal_rr"
  )
  
  # Numeric columns: replace na tokens, convert to numeric
  for (nm in numeric_cols) {
    vals <- as.character(df[[nm]])
    vals[tolower(vals) %in% na_tokens] <- NA
    df[[nm]] <- as.numeric(vals)
  }
  
  # Binary columns: map 0/1 to No/Yes, convert to factor
  for (nm in names(binary_map)) {
    vals <- as.character(df[[nm]])
    mapped <- binary_map[[nm]][vals]
    mapped[is.na(mapped)] <- vals[is.na(mapped)]  # keep unmapped values as-is
    df[[nm]] <- factor(mapped)
  }
  
  # Convert character columns with few unique values to factor & with many unique values keep as character
  text_cols <- c("title", "authors", "topic", "age_range", "eeg_locations",
                 "ecg_locations", "rejected_components",
                 "cfa_rej_criteria", "other_cleaning_strategy",
                 "hep_eeg_channels_selected", "trials", "trial_estimation",
                 "significant_eeg_channels", "controls", "journal",
                 "journal_full", "paper")
  for (nm in names(df)) {
    if (is.character(df[[nm]]) && !(nm %in% text_cols)) {
      df[[nm]] <- factor(df[[nm]])
    }
  }
  
  df
}

df_included_table <- prepare_filter_data(df_included)

function(input, output) {
  # Render the datatable with header tooltips
  output$table <- DT::renderDataTable(
    DT::datatable(
      df_included_table,
      filter = list(position = "top", clear = TRUE, plain = FALSE),
      options = list(
        pageLength = 25,
        dom = 'lrtip',
        search = list(regex = TRUE, caseInsensitive = TRUE),
        headerCallback = JS(
          "function(thead, data, start, end, display){",
          "  var tips = ", jsonlite::toJSON(shiny_config$column_tooltips), ";",
          "  $(thead).find('th').each(function(i){",
          "    var col = $(this).text();",
          "    if(tips[col]) $(this).attr('title', tips[col]);",
          "  });",
          "}"
        )
      )
    )
  )

  
  # Wrap df_selected in a reactive
  df_selected <- reactive({
    rows <- input$table_rows_all
    if (is.null(rows)) return(df_included)
    df_included[rows, , drop = FALSE]
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
      figure_hep_estimation_summary(df_selected(), save_path = NULL)
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

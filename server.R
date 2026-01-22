library(DT)
library(dplyr)
library(ggplot2)
library(shiny)

# Source the project initialization
source("init_workspace.R")

# Load the data 
df_included <- read.csv("data/derivatives/included.csv", stringsAsFactors = FALSE)

# Prepare table-friendly versions of columns so filters pick the right input type
prepare_filter_data <- function(df) {
  df_out <- df
  binary_map <- list(
    ICA = c("0" = "No ICA", "1" = "ICA"),
    ica_on_epochs = c("0" = "No", "1" = "Yes"),
    setting = c("0" = "Task", "1" = "Resting"),
    preregistration = c("0" = "No", "1" = "Yes"),
    patients = c("0" = "No", "1" = "Yes"),
    new_data = c("0" = "No", "1" = "Yes"),
    clustering = c("0" = "No", "1" = "Yes")
  )

  # Coerce columns to numeric only when all non-empty entries are numeric (allowing explicit NA tokens)
  factor_prefer <- c(
    "ecg_event_method", "ecg_event_toolbox", "reference_online",
    "cfa_rej_criteria", "journal", "journal_full")

  na_tokens <- c("na", "unknown")
  numeric_cols <- names(df_out)[vapply(seq_along(df_out), function(i) {
    nm <- names(df_out)[i]
    if (nm %in% factor_prefer) return(FALSE)
    vals <- as.character(df_out[[i]])
    vals <- vals[!is.na(vals) & vals != ""]
    #if (length(vals) == 0) return(FALSE)
    is_num <- grepl("^-?\\d+(\\.\\d+)?$", vals)
    is_na_token <- vals %in% na_tokens
    all(is_num | is_na_token)
  }, logical(1))]
  for (nm in numeric_cols) {
    vals <- as.character(df_out[[nm]])
    is_num <- grepl("^-?\\d+(\\.\\d+)?$", vals)
    is_na_token <- vals %in% na_tokens
    vals[!(is_num | is_na_token)] <- NA_character_
    vals[is_na_token] <- NA_character_
    df_out[[nm]] <- suppressWarnings(as.numeric(vals))
  }

  for (nm in factor_prefer) {
    if (nm %in% names(df_out)) {
      if (!is.factor(df_out[[nm]])) {
        df_out[[nm]] <- factor(df_out[[nm]])
      }
    }
  }

  for (nm in names(binary_map)) {
    if (nm %in% names(df_out)) {
      col_chr <- ifelse(is.na(df_out[[nm]]), NA_character_, as.character(df_out[[nm]]))
      col_chr[col_chr %in% c("TRUE", "True", "true")] <- "1"
      col_chr[col_chr %in% c("FALSE", "False", "false")] <- "0"
      mapped <- case_when(
        col_chr %in% names(binary_map[[nm]]) ~ binary_map[[nm]][col_chr],
        TRUE ~ col_chr
      )
      level_candidates <- mapped[!is.na(mapped) & mapped != ""]
      df_out[[nm]] <- factor(mapped, levels = unique(level_candidates))
    }
  }
  low_card_cols <- names(df_out)[sapply(df_out, function(x) {
    vals <- x[!is.na(x)]
    if (is.character(vals)) {
      vals <- vals[vals != ""]
    }
    length(unique(vals)) <= 12
  })]
  low_card_cols <- setdiff(low_card_cols, c(names(binary_map), numeric_cols, factor_prefer))
  for (nm in low_card_cols) {
    if (!is.factor(df_out[[nm]])) {
      df_out[[nm]] <- factor(df_out[[nm]])
    }
  }
  df_out
}
df_included_table <- prepare_filter_data(df_included)

function(input, output) {
  
  output$table <- renderDT({
    datatable(
      df_included_table,
      filter = "top",
      options = list(
        pageLength = 25,
        search = list(regex = TRUE, caseInsensitive = TRUE)
      )
    )})
  
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

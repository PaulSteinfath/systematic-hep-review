library(DT)
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
    ICA = c("0" = "No", "1" = "Yes"),
    ica_on_epochs = c("0" = "No", "1" = "Yes"),
    setting = c("0" = "Task", "1" = "Resting"),
    preregistration = c("0" = "No", "1" = "Yes"),
    patients = c("0" = "No", "1" = "Yes"),
    new_data = c("0" = "No", "1" = "Yes"),
    clustering = c("0" = "No", "1" = "Yes"),
    significant_test = c("0" = "No", "1" = "Yes")

  )

  # Coerce columns to factor
  factor_prefer <- c(
    "ecg_event_method", "ecg_event_toolbox", "reference_online",
    "cfa_rej_criteria", "journal", "journal_full")

  # Tokens to treat as NA
  na_tokens <- c("na", "unknown")

  # List of columns that should be numeric
  numeric_prefer <- c(
    "year", "sample_size", "meeg_num_electrodes", "meeg_sfreq_orig", 
    "meeg_sfreq_final","meg_num_grad", "meg_num_mag",
    "ecg_num_electrodes",  "ecg_sfreq_orig", "ecg_sfreq_final",
    "length_min", "high_pass", "low_pass", "groups",
    "conditions", "hep_start", "hep_end", "ecg_high_pass",
    "ecg_low_pass", "baseline_start_ms", "baseline_end_ms",
    "permutations", "significant_start_ms", "significant_end_ms",
    "cfa_minimal_rr"
  )

  numeric_cols <- intersect(numeric_prefer, names(df_out))
  for (nm in numeric_cols) {
    vals <- as.character(df_out[[nm]])
    vals[tolower(vals) %in% na_tokens] <- NA_character_
    df_out[[nm]] <- as.numeric(vals)
  }

  for (nm in factor_prefer) {
    if (nm %in% names(df_out)) {
      if (!is.factor(df_out[[nm]])) {
        df_out[[nm]] <- factor(df_out[[nm]])
      }
    }
  }

  for (nm in names(binary_map)) {
      col_chr <- ifelse(is.na(df_out[[nm]]), NA_character_, as.character(df_out[[nm]]))
      mapped <- case_when(
        col_chr %in% names(binary_map[[nm]]) ~ binary_map[[nm]][col_chr],
        TRUE ~ col_chr
      )
      level_candidates <- mapped[!is.na(mapped) & mapped != ""]
      df_out[[nm]] <- factor(mapped, levels = unique(level_candidates))
  }
  low_card_cols <- names(df_out)[sapply(df_out, function(x) {
    vals <- x[!is.na(x)]
    if (is.character(vals)) {
      vals <- vals[vals != ""]
    }
    length(unique(vals)) <= 20
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
  
  # Column explanations for tooltips
  col_explanations <- list(
        title = "Title of the paper, exported directly from PubMed without any modifications.",
        authors = "The list of authors, exported directly from PubMed without any modifications.",
        year = "The year of publication, exported directly from PubMed without any modifications.",
        preregistration = "'Yes' if the study was preregistered, 'No' otherwise.",
        topic = "A set of keywords that describes the context for the performed HER analysis. The keywords are grouped hierarchically, allowing one to search for broader categories. The keywords in each row are combined with '/', e.g., \"Affective processes/Arousal\" for the first row. Multiple keyword sets can be assigned to a single study.",
        patients = "'Yes' if patients (of any disorder) were included in the sample, 'No' otherwise.",
        age_mean = "Average across age ranges across participant groups in years.",
        age_min = "Minimum age across participant groups in years.",
        age_max = "Maximum age across participant groups in years.",
        age_group = "Matching of age to different age-groups: < 1 = Infants, >= 1  & < 12  = Children, >= 12 & < 18  = Adolescents, >= 18 = Adults",
        age_range = "Reported age range for each participant group, in years. Possible formats depend on how the range was specified in the reviewed papers and include: min-max, mean+-SD, median[IQR]. If age information was unavailable, it was recorded as 'unknown'. ",
        new_data = "'Yes' if new data were collected for the study, 'No' if existing datasets were used.",
        sample_size = "The total number of participants, summed across all groups/experiments if applicable. If available, how many participants were actually used in analyses after exclusion. If a statistical test was done in only a sub-sample of participants (e.g., only within a healthy controls group), the size of this sub-sample is used. If exactly the same test was performed in several sub-samples separately, the smallest sub-sample size is used.",
        setting = "Indicates whether the analysis focused on HERs during resting-state or changes in HERs between different tasks or conditions.",
        modality = "Recording modality for the described experiment.",
        meeg_num_electrodes = "The total number of sensors used for recording the M/EEG data.",
        meg_num_grad = "The total number of gradiometers used for recording the MEG data.",
        meg_num_mag = "The total number of magnetometers used for recording the MEG data.",
        meeg_sfreq_orig = "If reported, the sampling frequency (in Hz) of the recorded M/EEG data, otherwise 'unknown'. If the sampling frequency differed between participants, then the smallest one was extracted for the purpose of a quality control check.",
        meeg_sfreq_final = "If reported, the sampling frequency of the M/EEG data in the offline analysis (e.g., after downsampling), otherwise 'unknown'.",
        meeg_layout = "Layout of M/EEG sensors. Names of most layouts were normalized to match the built-in montages in the FieldTrip toolbox.",
        eeg_locations = "A list of layout-specific EEG channel names (e.g., Fp1, Fp2 for the 10-20 system or A1, A2 for the 128-channel Biosemi layout), if it is possible to derive them from the text, the name of the cap or the topomaps shown in the paper. The value \"layout\" is used in case it wasn\u2019t possible to derive the locations. In that case, locations from a template montage with a similar number of channels are used for the analysis.",
        length_min = "The total length of recordings in minutes, if specified explicitly. If both task and resting-state recording lengths are reported, the preference is for the resting-state length or for the length of time interval that was actually used for analysis of HERs.",
        ecg_num_electrodes = "The total number of active electrodes placed on the body to record ECG (not including the ECG ground). In case only one ECG electrode is explicitly mentioned in the paper, one of the EEG electrodes was most likely used to derive ECG since at least two electrodes are required. Such configurations were not classified as any of the basic leads.",
        ecg_lead = "If possible to derive from the description, the ECG lead that was used for data analysis (e.g., R-peak detection, correlation with ICA components, control analyses). In case the paper only mentions, for example, Einthoven II configuration or Lead I configuration without specifying the exact locations of the electrodes, the assumption is made that the electrodes were placed in the correct positions to allow the appropriate lead to be captured. ",
        ecg_locations = "Specific locations of ECG electrodes, if possible to derive from the description, not including the ECG ground. When possible, the locations were interpreted as one of the standard positions according to the description in this resource. Otherwise, the description from the mentioning paper is used.",
        ecg_ground = "Location of the ground electrode if indicated.",
        ecg_sfreq_orig = "If reported, the sampling frequency (in Hz) of the recorded ECG data, otherwise 'unknown'.",
        ecg_sfreq_final = "If reported, the sampling frequency (in Hz) of the ECG data in the offline analysis (e.g., after downsampling), otherwise 'unknown'.",
        ecg_high_pass = "If specified, the cutoff frequency of the high-pass filter that was applied to the ECG data, in Hz. Otherwise, 'unknown'.",
        ecg_low_pass = "If specified, the cutoff frequency of the low-pass filter that was applied to the ECG data, in Hz. Otherwise, 'unknown'.",
        #ecg_event_approach = "", # to be removed
        ecg_event_method = "Algorithm that was used for automatic detection of R-/T-peaks in the ECG.",
        ecg_event_toolbox = "Software that was used for automatic detection of R-/T-peaks in the ECG.",
        reference_online = "The channel that was used as reference during the recording of the EEG data.",
        reference_offline = "The reference that was used during the analysis of the data.",
        high_pass = "If specified, the cutoff frequency of the high-pass filter that was applied to the data, in Hz. In case several evoked responses were computed with different filter settings, we specify the high-pass cutoff frequency that was used to compute the HER. In case several filters were applied, the highest value is specified.",
        low_pass = "If specified, the cutoff frequency of the low-pass filter that was applied to the data, in Hz. In case several evoked responses were computed with different filter settings, we specify the low-pass cutoff frequency that was used to compute the HER. In case several filters were applied, the lowest value is specified.",
        ICA = "Whether ICA was applied during data preprocessing to remove artifacts.",
        ica_on_epochs = "Whether epoching relative to the R-peak or T-peak was applied before performing the ICA.",
        rejected_components = "Types of artifactual components that were rejected using ICA.",
        rejected_cardiac_ics = "The number of removed CFA-related ICA components, if specified. Can be a single value (e.g., mean across all participants) or a range.",
        cfa_rej_approach = "Approach used to determine the CFA-related ICA components.",
        cfa_rej_criteria = "Types of data representations that were used for deciding which ICA components to remove as CFA-related.",
        other_cfa_removal_strategy = "Other approaches apart from ICA that were used to remove/suppress/avoid the CFA in the data.",
        other_cleaning_strategy = "Optionally, what were other approaches used to clean the data from artifacts. Free text options are allowed.",
        hep_eeg_channels_selected = "Names of EEG channels that were selected for analysis of HER, if it was possible to derive them from the text or the figures of the paper. The column can also contain “All” if all channels were used or “All except …” if the excluded channels are mentioned in the paper. If it wasn’t possible to determine the selected channels from the description, “unknown” is used.",
        hep_meg_channels_selected = "All if all MEG channels were included in the HER analysis, or ‘Magnetometers’ if only the magnetometers were used.",
        groups = "Number of groups of participants if the analysis was performed for comparison of groups.",
        conditions = "Number of conditions if the analysis was performed for comparison of conditions.",
        trials = "The number of trials/epochs that were averaged to obtain the HERs, derived in the following way: * Per condition. Per participant. * Minimum among conditions (preferable). Mean if only the mean was reported * Final number used in analyses (i.e., after all exclusion criteria were applied), if reported * If a test was performed for comparison of both conditions and groups simultaneously (e.g., regression or ANOVA with an interaction term, comparison of difference between conditions between groups), report the smallest number of trials per subject per group per condition, or mean number, if available * If no information about the number of trials per subject, group, condition is reported explicitly then we estimate it. Details of such estimation are provided in the column \"Trial Estimation\". Examples: (1) length of recording in minutes multiplied by 60 beats per minute or a reported average heart rate; (2) number of trials in a condition of a task multiplied by how many HERs were analysed per trial multiplied by percentage of trials that were not rejected during preprocessing. The estimates are stored as [estX], where X is the estimated number of trials.",
        hep_window_type = "Reported approach that was used to define the HER space-time window of interest. Primary: The selection of the space-time window is not based on the analyzed data (e.g. based on previous research).; Secondary: The selection of the space-time window is based on the analyzed data. Example: using the time window and channels belonging to the significant cluster for additional analyses (e.g., correlation with behavior). ",
        hep_relative_to = "Part of the ECG signal that was used to define the onset of the HER.",
        hep_start = "If specified, the start of the time window (in ms) used for HER analysis.",
        hep_end = "If specified, the end of the time window (in ms) used for HER analysis.",
        baseline_start_ms = "Whether the baseline correction was performed and, if so, the start of the baseline window (in ms). If None, the authors explicitly chose to perform no baseline correction. If unknown, the start of the baseline correction time window could not be derived from the text.",
        baseline_end_ms = "Whether the baseline correction was performed and, if so, the end of the baseline window (in ms). If None, the authors explicitly chose to perform no baseline correction. If unknown, the end of the baseline correction time window could not be derived from the text.",
        hypothesis = "The kind of association that was tested. This column contrasts statistical comparison of groups/conditions to the study of linear or non-linear associations.",
        value = "Features extracted after obtaining HER which are then used in statistical analyses.",
        averaging_channels = "Whether the analysis included averaging across channels.",
        averaging_time = "Whether the analysis included averaging across time points.",
        statistics = "A statistical test that was used for analysis. For brevity, we joined them into larger families of tests.",
        clustering = "'Yes' - permutations were used, 'No' - permutations were not used. ",
        permutations = "The number of permutations used for clustering, if applicable. Unknown if permutations were performed but their number was not stated.",
        significant_test = "'Yes'1 if the result of the performed statistical test was significant, 0 otherwise. None if not applicable (e.g., if no statistical tests were performed). Unknown if a test was performed but its significance was not stated.",
        significant_eeg_channels = "If applicable, names of EEG channels that belong to any significant cluster. If the names could not be derived from the text or the figures of the paper, “unknown” was used.",
        significant_relative_to = "Part of the ECG signal that the significant cluster is referenced to.",
        significant_start_ms = "If specified, the earliest time point of the earliest significant cluster (in ms).",
        significant_end_ms = "If specified, the latest time point of the latest significant cluster (in ms).",
        controls = "Variables/approaches which were either (1) explicitly used in the control analyses or (2) kept comparable between groups/conditions through matched samples. The overview of HRV measures by Shaffer & Ginsberg, (2017) was used for normalization.",
        trial_estimation = "Description of how we approximated the number of trials per participant, per group, per condition if this number was not explicitly provided in a paper.",
        source = "Source of the publication, either PubMed or manual identification from reference lists of papers.",
        baseline_defined = "If baseline correction was performed.",
        has_resting = "True if the study included resting-state recordings, False otherwise.",
        has_task = "True if the study included task recordings, False otherwise.",
        study_category = "Only resting-state, only task, or both performed in the study.",
        clean_noisy_epochs = "Were noisy epochs removed from the data before HER analysis?",
        clean_bad_channels = "Were bad channels removed from the data before HER analysis?",
        reject_cfa_ics = "Were CFA-related ICA components removed from the data before HER analysis?",
        cfa_minimal_rr = "Minimal R-R interval used for HER analysis, in seconds.",
        cfa_use_minimal_rr = "True if minimal R-R interval was used for HER analysis",
        cfa_use_minimal_artifact_window = "Analysis limited to time of minimal artifact",
        cfa_csd = "CSD transformation applied to reduce CFA",
        cfa_regress = "Regression-based CFA removal",
        cfa_pca = "PCA-based CFA removal",
        cfa_subtract_rest = "resting state ECG subtracted from EEG for CFA removal",
        reference_category = "Analysis referenced to R- or T-peak",
        baseline_category = "Baseline correction performed or not",
        determination_category = "In the study, was averaging or clustering or both (or none ) used",
        #hep_approach = "In the pipeline, was averaging or clustering used",
        window_type_category = "In the study, were only primary analyses performed or primary and secondary (both). See also explanation for 'hep_window_type'",
        trials_Mean = "Average number of trials per study. Contains estimated trial counts. See also 'trial_estimation' column for details on estimation procedure.",
        trials_SD = "Standard deviation of number of trials per study",
        trials_original = "Number of trials as reported in the study.",
        #journal = "Journal name abbreviation",
        journal_full = "Full journal name",
        paper = "Publication title in citation style",
        PMID = "PMID of the publication",
        method_category = "Categorization of analysis approach into averaging or clustering. If unclear 'other' is used."
        #method_numeric = "Same as method_category but encoded as numeric (1 = averaging, 0 = clustering / other)."
      )

  # Render the datatable with header tooltips
  output$table <- DT::renderDataTable(
    DT::datatable(
      df_included_table,
      filter = "top",
      options = list(
        pageLength = 25,
        headerCallback = JS(
          "function(thead, data, start, end, display){",
          "  var tips = ", jsonlite::toJSON(col_explanations), ";",
          "  $(thead).find('th').each(function(i){",
          "    var col = $(this).text();",
          "    if(tips[col]) $(this).attr('title', tips[col]);",
          "  });",
          "}"
        )
      )
    ), escape = FALSE
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

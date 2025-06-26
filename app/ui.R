library(bslib)
library(DT)

ui <- page_navbar(
  nav_panel(
    "About",
    "Some notes about our project"
  ),
  nav_panel(
    "Data",
    DTOutput("table")
  ),
  nav_panel(
    "Acquisition & Preprocessing", 
    plotOutput("acquisitionPrepPlot", height = "800px")
  ),
  nav_panel(
    "CFA Removal",
    plotOutput("cfaRemovalPlot", height = "800px")
  ),
  nav_panel(
    "Control Variables",
    plotOutput("controlVariablesPlot", height = "800px")
  ),
  nav_panel(
    "ECG Acquistion",
    plotOutput("ecgSummaryPlot", height = "500px")
  ),
  nav_panel(
    "HEP Time Windows Summary",
    plotOutput("hepTimeWindowsCombinedPlot", height = "900px") # Adjusted height for combined plot
  ),
  nav_panel(
    "EEG Locations by Approach",
    fluidRow(
      column(12,
        h3("EEG Locations: Selected vs Significant Channels by Approach"),
        p("Compare selected and significant EEG channel locations across different values. All available values with sufficient data (≥3 studies) are automatically included.")
      )
    ),
    fluidRow(
      column(4,
        wellPanel(
          h4("Filter Options"),
          selectInput("approach_reference_var", 
                     "Reference Variable:",
                     choices = c("HEP Approach" = "hep_approach",
                               "HEP Window Type" = "hep_window_type",
                               "HEP Relative To" = "hep_relative_to",
                               "Modality" = "modality"),
                     selected = "hep_approach")
        )
      ),
      column(8,
        plotOutput("eegLocationsByApproachPlot", height = "400px")
      )
    )
  ),
  title = "Methods in HEP research"
)

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
    "Time Windows",
    layout_sidebar(
      sidebar = sidebar(
        radioButtons("timeWindowType", "Analysis Type:",
                     choices = c("Averaging" = "1", 
                                "Clustering" = "0"),
                     selected = "1")
      ),
      plotOutput("timeWindowsPlot", height = "800px")
    )
  ),
  nav_panel(
    "ECG Acquistion",
    plotOutput("ecgSummaryPlot", height = "500px")
  ),
  title = "Methods in HEP research"
)

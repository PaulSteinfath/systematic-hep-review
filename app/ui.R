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
    "EEG Acquisition & Preprocessing", 
    plotOutput("acquisitionPrepPlot", height = "800px")
  ),
  nav_panel(
    "ECG Acquistion",
    plotOutput("ecgSummaryPlot", height = "500px")
  ),
  title = "Methods in HEP research"
)

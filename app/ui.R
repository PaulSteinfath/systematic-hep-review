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
    "Overview",
    plotOutput("yearPlot")
  ),
  title = "Methods in HEP research"
)

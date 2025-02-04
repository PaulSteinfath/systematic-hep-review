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
    plotOutput("aquisitionPrepPlot", height = "800px")
  ),
  nav_panel(
    "Overview",
    plotOutput("yearPlot")
  ),
  nav_panel(
    "Filter Cutoffs",
    plotOutput("filterPlot")
  ),
  nav_panel(
    "CFA Removal",
    plotOutput("cfaRejPlot", height = "800px")
  ),
  title = "Methods in HEP research"
)

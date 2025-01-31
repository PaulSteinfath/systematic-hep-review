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
    "Aquisition & Preprocessing", 
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
  title = "Methods in HEP research"
)

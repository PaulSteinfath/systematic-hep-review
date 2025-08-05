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
    "Overview Studies",
    h3("Study Overview"),
    plotOutput("overviewStudiesPlot", height = "600px")
  ),
  nav_panel(
    "Overview Pipelines", 
    h3("Analysis Pipelines Overview"),
    plotOutput("overviewPipelinesPlot", height = "800px")
  ),
  nav_panel(
    "M/EEG Acq & Prep",
    h3("M/EEG Acquisition & Preprocessing"),
    plotOutput("meegAcqPrepPlot", height = "800px")
  ),
  nav_panel(
    "ECG Summary",
    h3("ECG Processing Summary"),
    plotOutput("ecgSummaryPlot", height = "600px")
  ),
  nav_panel(
    "HEP Estimation",
    h3("HEP Estimation Methods"),
    plotOutput("hepEstimationPlot", height = "800px")
  ),
  nav_panel(
    "Statistics",
    h3("Statistical Analysis Overview"),
    plotOutput("statsPlot", height = "600px")
  ),
  nav_panel(
    "Controls",
    h3("Control Variables Analysis"),
    plotOutput("controlsPlot", height = "800px")
  ),
  title = "Methods in HEP research"
)

library(bslib)
library(DT)

ui <- page_navbar(
  nav_panel(
    "About",
    div(
      class = "container-fluid",
      h2("Heartbeat-evoked responses in M/EEG: A systematic review of methods with suggestions for analysis and reporting", style = "margin-bottom: 24px;"),
      
      # div(
      #   class = "alert alert-info",
      #   style = "margin: 20px 0;",
      #   h4("🚧 Work in Progress"),
      #   p("This systematic review and web application are under development. 
      #     Data and analyses may be updated as the review progresses.")
      # ),
      
      h3("Overview"),
      p("This systematic review examines methodological approaches in heartbeat-evoked responses (HER) research 
        using EEG and MEG. The interactive visualizations allow exploration of data acquisition, 
        preprocessing, analysis methods, and reporting practices across studies."),
      
      hr(),
      p(class = "text-muted", 
        em("Citation information will be provided upon publication. Last updated: "), 
        format(Sys.Date(), "%B %Y"))
    )
  ),
  nav_panel(
    "Data",
    div(
      style = "margin-bottom: 10px; font-size: 0.95em; color: #444;",
      p(strong("Search columns using regex patterns:"),
        tags$ul(
          tags$li(tags$code("Perception|Music"), " — match either (OR)"),
          tags$li(tags$code("(?=.*Perception)(?=.*Music)"), " — match both (AND)"),
          tags$li(tags$code("^(?!.*Auditory).*Perception"), " — has Perception, excludes Auditory (NOT)")
        )
      ),
      p(
        strong("Tip:"),
        " Hover your mouse over a column name in the table to see more information about that column."
      )
    ),
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
    "CFA approaches",
    h3("CFA Approaches"),
    plotOutput("cfaApproachesPlot", height = "600px")
  ),
  nav_panel(
    "HER Estimation",
    h3("HER Estimation Methods"),
    plotOutput("herEstimationPlot", height = "800px")
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
  title = "Methods in HER research"
)

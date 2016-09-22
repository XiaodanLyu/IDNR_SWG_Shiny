shinyUI(pageWithSidebar(
  headerPanel("ROC Test Experiment"),
  sidebarPanel(
    h4("Simulation Setting"),
    numericInput("n", "sample size", min = 50, value = 150, step = 100),
    checkboxInput("smooth", "Smooth ROC curve", value = T),
    checkboxInput("thres", "Show Threshold value"),
    p(span("Green", style = "color:green"),
      "line represents the ROC curve of the fitted value from",
      strong('glm'), "procedure."),
    p(span("Blue", style = "color:blue"),
      "line represents the ROC curve of numbers randomly generated from
      a standard uniform distribution.")
  ),
  
  mainPanel(
    fluidRow(
      column(width = 10,
             plotOutput("roc_plot"))
    )
  )
)
)
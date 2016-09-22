shinyUI(pageWithSidebar(
  headerPanel("ROC Test Experiment"),
  sidebarPanel(
    h4("Simulation Setting"),
    numericInput("n", "sample size", value = 150, step = 100),
    p(span("Green", style = "color:green"),
      "line represents the ROC curve of the fitted value from",
      strong('glm'), "procedure."),
    p(span("Blue", style = "color:blue"),
      "line represents the ROC curve of numbers randomly generated from
      a standard uniform distribution.")
  ),
  
  mainPanel(
    fluidRow(
      column(width = 9,
             plotOutput("roc_plot"))
    )
  )
)
)
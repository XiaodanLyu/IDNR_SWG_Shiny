# --------------------------------
# Define UI for application layout
# --------------------------------
shinyUI(
  navbarPage(
    title = "IDNR_SWG App",
    theme = "bootstrap.blue.css",
    inverse = FALSE,
    tabPanel("Map",
             tags$style(type="text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }"),
             # singleton(tags$head(tags$script(src = "https://code.jquery.com/jquery-1.11.3.min.js"))),
             singleton(tags$head(tags$link(rel="stylesheet", type="text/css", href = "https://cdn.datatables.net/buttons/1.1.2/css/buttons.dataTables.min.css"))),
             singleton(tags$head(tags$script(src = "https://cdn.datatables.net/1.10.11/js/jquery.dataTables.min.js"))),
             singleton(tags$head(tags$script(src = "https://cdn.datatables.net/buttons/1.1.2/js/dataTables.buttons.min.js"))),
             singleton(tags$head(tags$script(src = "https://cdn.datatables.net/buttons/1.1.2/js/buttons.flash.min.js"))),
             singleton(tags$head(tags$script(src = "https://cdn.datatables.net/buttons/1.1.2/js/buttons.html5.min.js"))),
             singleton(tags$head(tags$script(src = "https://cdn.datatables.net/buttons/1.1.2/js/buttons.print.min.js"))),
             fluidPage(
               includeCSS("style.css"),
               fluidRow(
                 column(width = 3, wellPanel(
                   selectInput("kind", "Select Specie Category:", width = "auto",
                               choices = unique(index$Kind),
                               selected = "Bird"),
                   conditionalPanel(condition = "input.kind == 'Bird'",
                                    selectInput("cat", "Select Bird Category:",
                                                choices = unique(index$Category),
                                                selected = "Woodland")),
                   selectInput("prob", "Probability Type:",
                               choices = c(Occupancy = "Psi", Colonization = "Gam"),
                               selected = "Psi"),
                   selectInput("specie", "Select Specie:", 
                               choices = "All"),
                   checkboxGroupInput("Boundary", "Boundary to show",
                                      choices = c(`Landform Region` = "Land", "County",
                                                  `Wildlife Management Units` = "WMU", `Public Lands` = "Public"),
                                      select = "Land"),
                   selectInput("public", "Public land name contains (e.g., Adair WMA)",
                               levels(pp$Name), multiple = TRUE, selectize = TRUE),
                   selectInput("county", "County name contains (e.g., Story)",
                               levels(est$COUNTY), multiple = TRUE, selectize = TRUE),
                   selectInput("region", "Landform region name contains (e.g., Iowan Surface)",
                               levels(est$LAND), multiple = TRUE, selectize = TRUE),
                   hr(),
                   actionButton("update", "Update Map", class = "btn btn-primary")
#                    hr(),
#                    radioButtons("maptype", "Map type:", c("tiff", "png", "jpeg", "pdf")),
#                    downloadButton('downloadPlot', 'Download Map', class = "btn btn-success")
                 )
                 ),
                 column(width = 9,
                        br(),
                        h2("Introduction", align = "center"),
                        p("The map displays predicted values for the parameter of interest based on the covariate for the
                                   respective parameter from the", strong("best model.")),
                        p("Shiny is a new package from RStudio that makes it ", 
                          em("incredibly easy"), 
                          " to build interactive web applications with R.
                                 For an introduction and live examples, visit the ",
                          a("Shiny homepage.", 
                            href = "http://www.rstudio.com/shiny")),
                        br(),
                        fluidRow(
                          column(width = 6, offset = 2,
                                 plotOutput("Map", click = clickOpts(id = "plot_click"),
                                            hover = hoverOpts(id = "plot_hover"),
                                            dblclick = "plot_dblclick",
                                            brush = brushOpts(id = "plot_brush", resetOnNew = TRUE),
                                            width = width)
                          )
                        ),
                        helpText("Brush and double-click to zoom in; Double-click to resume.", cex = 1.5),
                        fluidRow(
                          column(width = 6,
                                 conditionalPanel(condition = "input.specie != 'All'",
                                                  strong(textOutput("model_info")))
                          )
                        ),
                        br(),
                        fluidRow(
                          column(width = 5,
                                 tags$b("Click Point Information:"),
                                 verbatimTextOutput("click_info")),
                          column(width = 5,
                                 tags$b("Hover Point Information:"),
                                 verbatimTextOutput("hover_info"))
                        ),
                        br(),
                        fluidRow(
                          column(width = 5,
                                 radioButtons("maptype", "Map type:", c("tiff", "png", "jpeg", "pdf")),
                                 downloadButton('downloadPlot', 'Download Map', class = "btn btn-success")),
                          column(width = 5,
                                 downloadLink('downloaddata', 'Download Predicted Values'))
                        )
                 )
               )
             )
    ),
    tabPanel("Estimate",
             tags$b("Check to see the estimates of all species"),
             checkboxInput("all.est", "Show All", value = FALSE),
             tabsetPanel(
               tabPanel("Model", br(),
                        h5("The table displays the best model for each species as well as the effect size (.Cov) and confidence 
                          interval (.CI) for the respective covariate on occupancy (psi), colonization (gam), and detection (p) 
                          probabilities."),
                        h5(strong("Bold"), "text indicates a significant effect (confidence interval does not include zero)."),
                        br(),
                        DT::dataTableOutput('table1')),
               tabPanel("Real Parameter", br(),
                        h5("The table displays the real parameter estimates and confidence intervals for occupancy (Psi),
                           colonization (Gam), and detection (p) probabilities obtained from the best model for each species."),
                        br(),
                        DT::dataTableOutput('table2')))
    )
  )
)

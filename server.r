## shiny links input and output
shinyServer(
  function(input, output, session) {
    
    choice <- reactive({
      cc <- index %>% filter(Kind == input$kind)
      if(input$kind == "Bird")
        cc <- cc %>% filter(Category == input$cat)
      cc %>% filter(!is.na(input$prob))
    }) 
    
    observe({
      updateSelectInput(session, "specie", choices = c("All", choice()$Specie))
    })
    
    Achoices <- reactive({
      Ac <- levels(est$LAND)
      if (input$specie != "All"){
        Range <- choice() %>% filter(Specie == input$specie) %>% select(Range_Restrict) %>% as.character
        Range <- unlist(strsplit(Range, ", "))
        if(!is.null(Range)) Ac <- unique(Range)
      }
      Ac
    })
    
    observe({
      updateSelectInput(session, "region", choices = Achoices())
    })
    
#    area <- reactiveValues(name = NULL)
    
#    observe({
#      area$name <- levels(pp$Name)[grep(input$public, levels(pp$Name), ignore.case = T)]
#    })
    
#    output$area_choice <- renderPrint({
#      cat(area$name, sep = "; ")
#    })
    
    data <- reactive({
      if (input$specie == "All") s <- choice()$Abbr
      if (input$specie != "All") s <- choice() %>% filter(Specie %in% input$specie) %>% select(Abbr)
      data <- est %>% select(one_of(paste0(s, input$prob)))
      if (input$specie == "All"){
        if (T){
          weight <- choice()[, input$prob]
          w <- matrix(rep(prop.table(weight), each = nrow(data)), nr = nrow(data))
          data <-  rowSums(data * w, na.rm = T)
        }
        else
        {
          data <- rowMeans(data, na.rm = T)
        }
      }
      data <- est %>% select(x, y, LAND, COUNTY, WMU) %>% bind_cols(data.frame(data)) 
      names(data) <- c("x", "y", "LAND", "COUNTY", "WMU", "value")
      data$value[!data$LAND %in% if(is.null(input$region)) levels(data$LAND) else input$region] <- NaN
      return(data)
    })
    
    plotInput <- reactive({
      input$go
      d <- isolate(data())
      type <- isolate(ifelse(input$prob == "Psi", "Occupancy", "Colonization"))
      title <- isolate(paste(sub("\\s+$", "", input$specie), type))
      myplot <- ggplot(d, aes(x = x, y = y, fill = value)) + geom_tile() + 
        scale_fill_gradient2(limits = c(0, 1), low = "skyblue2",
                             high = "red", mid = "yellow", midpoint = 0.5,
                             guide = guide_colorbar(title = "Probability", title.vjust = -0.5)) +
        labs(x = "", y = "", title = title) + theme_classic() +
        coord_cartesian(xlim = range$x, ylim = range$y) +
        theme(plot.title = element_text(size = rel(2)),
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              line = element_blank(),
              legend.position="right",
              #	margin around entire plot (unit with the sizes of the top, right, bottom, and left margins)
              plot.margin = unit(c(1, 1, 0, 0), "lines"))
      border <- isolate(input$Boundary)
      if ("Land" %in% border)
        myplot <- myplot + geom_contour(data = d, aes(x, y, z = as.numeric(LAND)), colour = "black", binwidth = 1)
      if ("County" %in% border)
        myplot <- myplot + geom_contour(data = d, aes(x, y, z = as.numeric(COUNTY)), colour = "black", binwidth = 1)
      if ("WMU" %in% border)
        myplot <- myplot + geom_contour(data = d, aes(x, y, z = as.numeric(WMU)), colour = "black", binwidth = 1)
      if ("Public" %in% border)
        myplot <- myplot + geom_polygon(data = pp %>% filter(Name %in% isolate(if(is.null(input$public)) Name else input$public)),
                                        aes(x, y, group = FID), fill = NA, colour = "black")
      myplot 
    })
    
    output$Map <- renderPlot({
      plotInput()
    })
    
    output$downloadPlot <- downloadHandler(
      filename <- function() paste0(paste(input$kind, input$specie, input$cat, input$prob, input$public, sep = "_"), ".tiff"),
      content <- function(file) ggsave(file, plotInput(), width = 6, height = 4, device = "tiff")
    )
    
    range <- reactiveValues(x = NULL, y = NULL)
    
    observeEvent(input$go, {
      if ("Public" %in% input$Boundary & !is.null(input$public)){
        sp <- pp %>% filter(Name %in% input$public) %>% select(x, y)
        sr <- list(x = diff(range(sp$x)), y = diff(range(sp$y)),
                   x0 = mean(range(sp$x)), y0 = mean(range(sp$y)))
        if(sr$x/sr$y>3/2) {
          range$x <- range(sp$x)
          range$y <- sr$y0+1/3*sr$x*c(-1, 1)
        }
        else {
          range$x <- sr$x0+3/4*sr$y*c(-1, 1)
          range$y <- range(sp$y)
        }
      }
    })
    
    observeEvent(input$plot_dblclick,{
      brush <- input$plot_brush
      if (!is.null(brush)) {
        br <- list(x = brush$xmax-brush$xmin, y = brush$ymax - brush$ymin,
                   x0 = (brush$xmin+brush$xmax)/2, y0 = (brush$ymin+brush$ymax)/2)
        if (br$x/br$y>3/2) {
          range$x <- c(brush$xmin, brush$xmax)
          range$y <- br$y0+1/3*br$x*c(-1, 1)
        }
        else {
          range$x <- br$x0+3/4*br$y*c(-1, 1)
          range$y <- c(brush$ymin, brush$ymax)
        }
      }
      else {
        range$x <- NULL
        range$y <- NULL
      }
    })
    
    output$click_info <- renderPrint({
      cat("Probability: ")
      cat(round(nearPoints(data(), input$plot_click, maxpoints = 1)$value, 3))
      cat("\n")
      cat("County: ")
      cat(as.character(nearPoints(data(), input$plot_click, maxpoints = 1)$COUNTY))
      cat("\n")
      cat("Wildlife Management Unit: ")
      cat(as.character(nearPoints(data(), input$plot_click, maxpoints = 1)$WMU))
    })
    
    output$hover_info <- renderPrint({
#      cat("Nearest three points\n")
      cat("Probability: ")
      cat(round(nearPoints(data(), input$plot_hover, maxpoints = 3)$value, 3))
      cat("\n")
      cat("County: ")
      cat(as.character(nearPoints(data(), input$plot_hover, maxpoints = 1)$COUNTY))
      cat("\n")
      cat("Wildlife Management Unit: ")
      cat(as.character(nearPoints(data(), input$plot_hover, maxpoints = 1)$WMU))
    })
    
    cova <- reactive({
      if (input$specie != "All"){
        s0 <- unlist(strsplit(unlist(model[unlist(indice()), "Model"]), "[()]"))
        s1 <- s0[grep("~", s0)]
        names(s1) <- c("Psi", "Gam", "p")
        if (length(s1) == 4) names(s1) <- c("Psi", "Gam", "Eps", "p")
        sub("~", "", s1)
      }
    })
    
    output$model_info <- renderText({
      if (input$specie != "All")
        paste("Best Model Covariates:", cova()[input$prob])
    })
    
    output$Parameter <- renderTable({
      res <- para[ind(), c("Psi.CI", "Gam.CI", "p.CI")]
      if(!is.na(para[ind(), "Eps.CI"]))
        res <- c(res, para[ind(), "Eps.CI"])
      res
    })
    
    options(DT.options = list(pageLength = 15, language = list(search = 'Species Search:')))
    
    indice <- reactive({
      sp <- input$specie
      if(input$specie == "All") sp <- choice()$Specie
      id <- model %>% filter(Species %in% sp) %>% select(No)
      if (input$all.est) id <- model$No
      id
    })
    
    output$table1 <- DT::renderDataTable({
      datatable(model %>% filter(No %in% unlist(indice())) %>%
                  select(No, Species, Model, Auc, psi.Cov, Psi.CI, gam.Cov, Gam.CI, p.Cov, p.CI, p.s, o.s, g.s),
                #                caption = "This is the table of model coefficient estimates",
                class = "compact display nowrap", rownames = FALSE,
                options = list(autoWidth = FALSE,
                               columnDefs = list(
                                 list(width = '500px', targets = 2),
                                 list(className = 'dt-center', targets = c(0, 3:9)),
                                 list(targets = 10:12, visible = FALSE)),
                               dom = "lfrt<B>p",
                               buttons = c("copy","excel","pdf", "print")
                ) 
      ) %>% formatRound(c("Auc", "psi.Cov", "gam.Cov", "p.Cov"), 2) %>%
        formatStyle(c("psi.Cov", "gam.Cov", "p.Cov"), backgroundColor = "skyblue") %>%
        formatStyle(c("psi.Cov", "Psi.CI"), "o.s", fontWeight = styleEqual(c(0, 1), c("normal", "bold"))) %>%
        formatStyle(c("gam.Cov", "Gam.CI"), "g.s", fontWeight = styleEqual(c(0, 1), c("normal", "bold"))) %>%
        formatStyle(c("p.Cov", "p.CI"), "p.s", fontWeight = styleEqual(c(0, 1), c("normal", "bold")))
    })
    
    output$table2 <- DT::renderDataTable({
      datatable(para %>% filter(No %in% unlist(indice())) %>%
                  select(No, Species, Psi, Psi.CI, Gam, Gam.CI, p, p.CI),
                #                caption = "This is the table of real parameter estimates",
                class = "compact display nowrap", rownames = FALSE,
                options = list(autoWidth = FALSE,
                               columnDefs = list(
                                 list(width = '300px', targets = 1),
                                 list(className = 'dt-center', targets = c(0, 2:7))),
                               dom = "lfrt<B>p",
                               buttons = c("copy","excel","pdf", "print")
                ) 
      ) %>% formatRound(c("Psi", "Gam", "p"), 2) %>% 
        formatStyle(c("Psi", "Gam","p"), backgroundColor = "skyblue")
    })
    
  })

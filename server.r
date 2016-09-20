# ==========================
# Define Application Output
# ==========================
shinyServer(
  function(input, output, session) {
    
    choice <- reactive({
      cc <- index %>% filter(Kind == input$kind)
      if(input$kind == "Bird")
        cc <- cc %>% filter(Category == input$cat)
      cc[!is.na(cc[, input$prob]), ]
    }) 
    
    observe({
      updateSelectInput(session, "specie", choices = c("All", choice()$Specie))
    })
    
    Achoices <- reactive({
      Ac <- levels(est$LAND)
      if (input$specie != "All"){
        Range <- choice() %>% filter(Specie == input$specie) %>% select(Range_Restrict) %>% as.character
        Range <- unlist(strsplit(Range, ", "))
        if(length(Range)>0) Ac <- unique(Range)
      }
      Ac
    })
    
    observe({
      updateSelectInput(session, "region", choices = Achoices())
    })
    
    data <- reactive({
      if (input$specie == "All") s <- choice()$Abbr
      if (input$specie != "All") s <- choice() %>% filter(Specie %in% input$specie) %>% select(Abbr)
      data <- est %>% select(one_of(paste0(s, input$prob)))
      if (input$specie == "All"){
        weight <- choice()[, input$prob]
        w <- matrix(rep(weight, each = nrow(data)), nr = nrow(data))
        w[is.na(data)] <- NA
        w <- w/rowSums(w, na.rm = T)
        data <-  rowSums(data * w, na.rm = T)
      }
      data <- est %>% select(x, y, LAND, COUNTY, WMU) %>% bind_cols(data.frame(data)) 
      names(data) <- c("x", "y", "LAND", "COUNTY", "WMU", "value")
      data$value[!data$LAND %in% if(is.null(input$region)) levels(data$LAND) else input$region] <- NA
      return(data)
    })
    
    output$downloaddata <- downloadHandler(
      filename = function() {
        paste(input$specie, " ", input$kind, " ", input$prob, ".txt", sep = "")
      },
      content = function(con) {
        write.table(data()[, c("x", "y", "value")], con, row.names = FALSE, quote = F)
      }
    )
    
    plotInput <- reactive({
      input$update
      d <- isolate(data())
      type <- isolate(ifelse(input$prob == "Psi", "Occupancy", "Colonization"))
      name <- isolate(if(input$specie == "All") paste(ifelse(input$kind == "Bird", input$cat, ""), input$kind) else input$specie)
      title <- isolate(paste(sub("\\s+$", "", name), type))
      caption <- isolate(if(is.null(input$county)) input$public else input$county)
      myplot <- ggplot(d, aes(x = x, y = y, fill = value)) + geom_tile() + 
        scale_fill_gradient2(limits = c(0, 1), low = "deepskyblue",
                             high = "red", mid = "yellow", midpoint = 0.5,
                             guide = guide_colorbar(title = NULL, barwidth = 0.8, barheight = 4,
                                                    label.theme = element_text(size = 6.5, angle = 0))) +
        labs(x = "", y = "", title = title, caption = caption) + theme_classic() +
        coord_cartesian(xlim = range$x, ylim = range$y) +
        theme(plot.title = element_text(size = rel(1.75), hjust = 0.5, face = "bold"),
              # plot.caption = element_text(size = rel(0.8), face = "italic"),
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
      withProgress(message = "Making map", value = 0, {
        plotInput()
      })
    }, res = 100)
    
    output$downloadPlot <- downloadHandler(
      filename <- function() 
        paste0(paste(input$kind, input$specie, input$cat, input$prob, input$public, input$county, sep = "_"), ".", input$maptype),
      content <- function(file) 
        ggsave(file, plotInput(), width = 6, height = 4, device = input$maptype)
    )
    
    range <- reactiveValues(x = NULL, y = NULL)
    
    observeEvent(input$update, {
      sp <- est %>% select(x, y)
      if (!is.null(input$public)){
        sp <- pp %>% filter(Name %in% input$public) %>% select(x, y)
      }
      if (!is.null(input$county)){
        sp <- est %>% filter(COUNTY %in% input$county) %>% select(x, y)
      }
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
      if ((input$specie) != "All"){
        s0 <- unlist(strsplit(unlist(model[unlist(indice()), "Model"]), "[()]"))
        s1 <- s0[grep("~", s0)]
        names(s1) <- c("Psi", "Gam", "p")
        if (length(s1) == 4) names(s1) <- c("Psi", "Gam", "Eps", "p")
        sub("~", "", s1)
      }
    })
    
    output$model_info <- renderText({
      if (input$specie != "All")
        paste(input$specie, "Best Model Covariates:", cova()[input$prob])
    })
    
    options(DT.options = list(pageLength = 15, language = list(search = 'Species Search:')))
    
    indice <- reactive({
      sp <- input$specie
      if(input$specie == "All") sp <- choice()$Specie
      id <- model %>% filter(Species %in% sp) %>% select(No)
      if (input$all.est) id <- model$No
      id
    })
    
    tableinput1 <- reactive({
      model %>% filter(No %in% unlist(indice())) %>%
        select(No, Species, Model, Auc, Psi.Cov, Psi.CI, Gam.Cov, Gam.CI, p.Cov, p.CI, p.s, o.s, g.s)
    })
    
    output$table1 <- DT::renderDataTable({
      datatable(tableinput1(),
                class = "compact display nowrap", rownames = FALSE,
                options = list(autoWidth = FALSE,
                               columnDefs = list(
                                 list(width = '500px', targets = 2),
                                 list(className = 'dt-center', targets = c(0, 3:9)),
                                 list(targets = 10:12, visible = FALSE))
#                                dom = "lfrt<B>p",
#                                buttons = c("print", "copy", "csv")
                ) 
      ) %>% formatRound(c("Auc", "Psi.Cov", "Gam.Cov", "p.Cov"), 2) %>%
        formatStyle(c("Psi.Cov", "Gam.Cov", "p.Cov"), backgroundColor = "skyblue") %>%
        formatStyle(c("Psi.Cov", "Psi.CI"), "o.s", fontWeight = styleEqual(c(0, 1), c("normal", "bold"))) %>%
        formatStyle(c("Gam.Cov", "Gam.CI"), "g.s", fontWeight = styleEqual(c(0, 1), c("normal", "bold"))) %>%
        formatStyle(c("p.Cov", "p.CI"), "p.s", fontWeight = styleEqual(c(0, 1), c("normal", "bold")))
    })
    
    output$downloadData1 <- downloadHandler(
      filename = function()
        paste(paste(input$kind, input$specie, input$cat, "Estimate", sep = "_"), '.csv', sep = ''),
      content = function(file)
        write.csv(tableinput1()[, -(11:13)], file, row.names = F, na = "")
    )
    
    tableinput2 <- reactive(
      para %>% filter(No %in% unlist(indice())) %>%
        select(No, Species, Psi, Psi.CI, Gam, Gam.CI, p, p.CI)
    )
    
    output$table2 <- DT::renderDataTable({
      datatable(tableinput2(),
                class = "compact display nowrap", rownames = FALSE,
                options = list(autoWidth = FALSE,
                               columnDefs = list(
                                 list(width = '300px', targets = 1),
                                 list(className = 'dt-center', targets = c(0, 2:7)))
#                                dom = "lfrt<B>p",
#                                buttons = c("print", "copy", "csv")
                ) 
      ) %>% formatRound(c("Psi", "Gam", "p"), 2) %>% 
        formatStyle(c("Psi", "Gam","p"), backgroundColor = "skyblue")
    })
    
    output$downloadData2 <- downloadHandler(
      filename = function()
        paste(paste(input$kind, input$specie, input$cat, "Para", sep = "_"), '.csv', sep = ''),
      content = function(file)
        write.csv(tableinput2(), file, row.names = F, na = "")
    )
  })

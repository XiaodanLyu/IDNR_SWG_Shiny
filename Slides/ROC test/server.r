library(lme4)
library(pROC)

shinyServer(function(input, output, session){
  
  data <- reactive({
    nn <- input$n
    x <- rnorm(nn)
    lp <- 0.5 + 0.5*x
    p <- exp(lp)/(1+exp(lp))
    y <- rbinom(nn, size = 1, prob = p)
    # random guess
    yguess <- runif(nn)
    # generalized linear regression
    res <- glm(y~x, family = binomial(link = "logit"))
    yhat <- fitted(res)
    
    return(list(y, yguess, yhat))
  })
  
  
  plotInput <- reactive({
    y <- data()[[1]]
    yguess <- data()[[2]]
    yhat <- data()[[3]]
    plot.roc(y, y, main = "ROC Curve", auc.polygon = T,
             identity.col = "red", identity.lwd = 2)
    plot.roc(y, yguess, smooth = input$smooth, col = "blue",
             print.thres = input$thres, print.thres.col = "blue",
             print.auc = T, print.auc.x = 0.4, print.auc.y = 0.4,
             auc.polygon = T, auc.polygon.col = rgb(0,0,1, alpha = 0.5),
             add = T)
    plot.roc(y, yhat, smooth = input$smooth, col = "darkgreen",
             print.thres = input$thres, print.thres.col = "darkgreen",
             print.thres.adj = c(0.05, -1.25),
             print.auc = T, print.auc.x = 0.95, print.auc.y = 0.8,
             auc.polygon = T, auc.polygon.col = rgb(0,1,0, alpha = 0.3),
             add = T)
  })
  
  output$roc_plot <- renderPlot({
    plotInput()
  }, res = 100)
  
}
)
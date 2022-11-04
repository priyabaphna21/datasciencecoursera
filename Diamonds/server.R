#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# This is a shiny app to determine diamond price based on carat, cut, color, and clarity

library(shiny)
library(ggplot2)
library(curl)

# Define server logic
shinyServer(function(input, output) {
  
  # load data
  data("diamonds")
  
  # create the initial output
  output$distPlot <- renderPlot({
    
    # subset the data based on the inputs
    diamonds_sub <- subset(diamonds, cut == input$cut &
                             color == input$color &
                             clarity == input$clarity)
    
    # plot the diamond data with carat and price
    p <- ggplot(data = diamonds_sub, aes(x = carat, y = price)) + geom_point()
    p <- p + geom_smooth(method = "lm") + xlab("Carat") + ylab("Price")
    p <- p + xlim(0, 6) + ylim (0, 20000)
    p
  }, height = 700)
  
  # create linear model
  output$predict <- renderPrint({
    diamonds_sub <- subset( diamonds, cut == input$cut &
                              color == input$color &
                              clarity == input$clarity)
    fit <- lm(price~carat,data=diamonds_sub)
    unname(predict(fit, data.frame(carat = input$lm)))})
  
  observeEvent(input$predict, {distPlot <<- NULL
  output$distPlot <- renderPlot({p <- ggplot(data = diamonds, aes(x = carat, y = price)) + geom_point()
  p <- p + geom_smooth(method = "lm") + xlab("Carat") + ylab("Price")
  p <- p + xlim(0, 6) + ylim (0, 20000)
  p
  }, height = 700)})})

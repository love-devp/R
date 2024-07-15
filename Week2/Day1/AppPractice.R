#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

ui <- fluidPage(
  numericInput("age", "How old are you?", value = NA),
  textInput("name", "What's your name?"),
  textOutput("greeting"),
  tableOutput("mortgage"),
  sliderInput("x", label = "If x is", min = 1, max = 50, value = 30),
  sliderInput("y", label = "If y is", min = 1, max = 50, value = 30),
  "then x times y is",
  textOutput("product")
)

server <- function(input, output, session) {
  output$greeting <- renderText({
    paste0("Hello ", input$name)
  })
  output$histogram <- renderPlot({
    hist(rnorm(1000))
  }, res = 96)
  output$product <- renderText({ 
    input$x * input$y
  })
}

# Run the application 
shinyApp(ui, server)


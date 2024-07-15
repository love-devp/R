#ui.R
library(shiny)

#ui code
ui <- fluidPage(
  selectInput("graphs", label = "list", choices = graphs),
  verbatimTextOutput("summary"),
  plotOutput("plot")
)
# Load necessary libraries
library(shiny)

# Define the Shiny UI
ui <- fluidPage(
  titlePanel("Enhanced Volcano Plot"),
  sidebarLayout(
    sidebarPanel(
      numericInput("log2FC", "Log2 Fold Change Cutoff:", value = 2),
      numericInput("pValue", "P-value Cutoff:", value = 10e-6),
      textInput("title", "Plot Title:", value = "Volcano Plot")
    ),
    mainPanel(
      plotOutput('volcanoPlot1'),
      plotOutput('volcanoPlot2'),
      plotOutput('volcanoPlot3')
    )
  )
)

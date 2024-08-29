# app.R
library(shiny)
library(shinyjs)
library(dplyr)
library(shinycssloaders)
library(colourpicker)
library(bslib)
library(ggplot2)


ui <- function(id) {
  fluidPage(
    titlePanel('Volcano Plot App'),
    PageUI('page_ui')  # Call the PageUI function defined in page.R
  )
}

server <- function(input, output, session) {

  # Call the PageServer function
  PageServer('page_ui')
}

options(shiny.host = '0.0.0.0', shiny.port = 8789, shiny.maxRequestSize = 30*1024^2)
shinyApp(ui = ui, server = server)
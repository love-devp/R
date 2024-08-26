library(shiny)
library(shinyjs)
library(colourpicker)
source('./R/Enhancedvolcano.R')
source('./R/page.R')

ui <- navbarPage(
  title = 'Enhanced Volcano Plot Explorer', 
  id = 'nav_id',
  tabPanel('Plot', PageUI('page'))
)

server <- function(input, output, session) {
  script_path <- reactive({
    "/home/rstudio"
  })
  
  PageServer('page', script_path)
}

options(shiny.host = '0.0.0.0', shiny.port = 8789, shiny.maxRequestSize = 30*1024^2)
shinyApp(ui = ui, server = server)

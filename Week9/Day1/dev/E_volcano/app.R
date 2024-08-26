library(shiny)
library(shinyjs)
library(colourpicker)
source('./R/Enhancedvolcano.R')

ui = page_navbar(
  title='Enhanced Volcano Plot Explorer', 
  id='nav_id',
  fillable=TRUE,
  nav_panel('Plot', inputPageUI('page'))
  )

server <- function(input, output, session) {
  
  script_path <- reactive({
    "/home/rstudio"
  })
  
  PageServer('page', script_path)

}

options(shiny.host='0.0.0.0', shiny.post=8789, shiny.maxRequestSize=30*1024^2)
shinyApp(ui=ui, server=server)

library(shiny)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(DT)
library(shinycssloaders)
library(shinydashboard)
library(shinydashboardPlus)
library(scales)
library(fgsea)
library(clusterProfiler)
library(stringr)
library(shinythemes)
library(bslib)
library(ggrepel)
library(cyjShiny)
library(htmlwidgets)
library(graph)
library(jsonlite)
library(igraph)
library(colourpicker)

options(scipen=999)

ui = page_navbar(title='Enhanced Volcano Plot Explorer', id='nav_id', fillable=TRUE,
                 nav_panel("Input", inputPageUI('input_page')),
                 nav_panel("Result", dePageUI('DE_page')),
)

server <- function(input, output, session) {
  
  script_path = reactive({
    "/home/rstudio"
  })
  
  input_page = inputPageServer("input_page", script_path)
  de_page = dePageServer("DE_page", input_page$data)
  
}

options(shiny.host = "0.0.0.0", shiny.port = 8789, shiny.maxRequestSize=30*1024^2)
shinyApp(ui = ui, server = server)
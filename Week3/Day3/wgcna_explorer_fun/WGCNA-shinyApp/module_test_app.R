library(shiny)
library(shinyjs)
library(dplyr)
library(DT)
library(stringr)
library(shinythemes)

# Source the module
source("auto_load.R")

ui <- navbarPage(
  theme = shinytheme("spacelab"),
  "Custom Logo",  # Replace customLogo with an actual logo if needed
  tabPanel(
    useShinyjs(),
    title = "Data import and cleaning",
    icon = icon("file-upload"),
    sidebarLayout(
      div(id = "Sidebar",
          sidebarPanel(
            width = 2,
            fileAutoloadUI("ExpMat")
          )
      ),
      mainPanel(
        DTOutput("data")
      )
    )
  )
)

server <- function(input, output, session) {
  pre_data <- fileAutoloadServer("ExpMat", script_path = script_path)
  
  output$data <- renderDT({
    req(pre_data())
    datatable(pre_data())
  })
}

shinyApp(ui = ui, server = server)

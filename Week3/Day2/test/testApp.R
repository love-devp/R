# app.R

# Load necessary libraries
library(shiny)
library(shinyjs)
library(dplyr)
library(DT)
library(stringr)
library(shinythemes)  # Assuming this is needed for `shinytheme`

# Source the module
source("auto_load_module.R")

# Define UI for the main application
ui <- shinyUI(
  navbarPage(
    theme = shinytheme("spacelab"),  # Use the spacelab theme
    title = "Data import and cleaning",  # Title for the app
    tabPanel(
      title = "File Selection",
      icon = icon("file-upload"),  # Icon for the tab
      sidebarLayout(
        sidebarPanel(
          width = 2,
          fileSelectorUI("file_selector")  # Use the file selector module UI
        ),
        mainPanel(
          DTOutput("file_contents")  # Output for the data table
        )
      )
    )
  )
)

# Define server logic for the main application
server <- function(input, output, session) {
  observeEvent(input$toggleSidebar, {
    shinyjs::toggle(id = "Sidebar")
  })
  observeEvent(input$toggleSidebar2, {
    shinyjs::toggle(id = "Sidebar2")
  })
  observeEvent(input$toggleSidebar3, {
    shinyjs::toggle(id = "Sidebar3")
  })
  observeEvent(input$toggleSidebar4, {
    shinyjs::toggle(id = "Sidebar4")
  })
  observeEvent(input$toggleSidebar5, {
    shinyjs::toggle(id = "Sidebar5")
  })
  observeEvent(input$toggleSidebar6, {
    shinyjs::toggle(id = "Sidebar6")
  })
  
  # Use the file selector module server logic
  fileSelectorServer("file_selector", script_path = script_path)
}

# Run the application
shinyApp(ui = ui, server = server)

# input_page.R
library(shiny)

# Define the UI for the RDS file input and output
input_page_ui <- function(id) {
  ns <- NS(id)  # Namespace for the module
  
  tagList(
    selectInput(ns("selected_rds"), "Select RDS File:", choices = NULL),
    actionButton(ns("load_rds"), "Load RDS File"),
    tableOutput(ns("rds_table"))
  )
}

# Define the server logic for handling the RDS file input and output
input_page_server <- function(id, path_to_rds_files) {
  moduleServer(id, function(input, output, session) {
    # Load the RDS file names on server start
    rds_files <- list.files(path_to_rds_files, pattern = "\\.rds$", full.names = TRUE)
    rds_file_names <- basename(rds_files)
    
    # Update the selectInput with the available RDS files
    updateSelectInput(session, "selected_rds", choices = rds_file_names)
    
    # Observe the "Load RDS File" button and load the selected RDS file
    observeEvent(input$load_rds, {
      req(input$selected_rds)  # Ensure a file is selected
      
      # Get the full path of the selected RDS file
      selected_rds_file <- file.path(path_to_rds_files, input$selected_rds)
      
      # Load the selected RDS file
      rds_data <- readRDS(selected_rds_file)
      
      # Display the data in a table
      output$rds_table <- renderTable({
        rds_data  # Display the data frame
      })
    })
  })
}

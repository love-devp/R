# auto_load_module.R

# Load necessary libraries
library(shiny)
library(shinyjs)
library(dplyr)
library(DT)
library(stringr)

# Define a function to generate the base path for file loading
script_path <- function(subdir = NULL) {
  base_path <- getwd()  # Base path to the directory containing files
  if (!is.null(subdir)) {
    return(file.path(base_path, subdir))  # Append subdir to the base path if there is a subdir
  }
  return(base_path)  # Return base path if there is no subdir
}

# Define UI for the auto-load module
fileAutoloadUI <- function(id) {
  ns <- NS(id)  # Create a namespace function using the provided id
  tagList(
    useShinyjs(),  # Enable shinyjs
    
    # Dropdown to select input source (Automatic or Manual)
    selectInput(
      ns("input_source"),  # Use the namespace for the input ID
      "Choose How to load the data",  # Label
      choices = c("Automatic", "Manual"),  # Choices
      selected = "Automatic"  # Initial selected value
    ),
    
    # Conditional panel to show selectizeInput if Automatic is selected
    conditionalPanel(
      condition = paste0("input['", ns("input_source"), "'] == 'Automatic'"),
      selectizeInput(
        ns("automatic_data"),  # Use the namespace for the input ID
        "Loaded data list",  # Label
        choices = NULL  # List of values to select from. It will be updated dynamically
      )
    ),
    
    # Conditional panel to show the file upload control if Manual is selected
    conditionalPanel(
      condition = paste0("input['", ns("input_source"), "'] == 'Manual'"),  # JS expression
      fileInput(
        ns("manual_data"),  # Use the namespace for the input ID
        "Choose file"  # Label
      )
    ),
    textOutput("fileUploaded")
  )
}

# Define server logic for the auto-load module
fileAutoloadServer <- function(id, script_path) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive expression to list all files matching the pattern
    input_files <- reactive({
      all_files <- list.files(path = script_path(), pattern = "\\.txt$|\\.xls$|\\.csv$", recursive = TRUE)  # List all files with .txt or .xls or .csv pattern
      candidates <- character()  # Initialize an empty character vector for candidate files
      for (f in all_files) {
        if (substr(f, 1, 4) == "data" & str_detect(f, "inputs")) {  # Check conditions for the files
          candidates <- append(candidates, f)  # Append file to candidates if conditions are met
        }
      }
      
      # Check if 'candidates' is empty and handle it
      if (length(candidates) == 0) {
        return(NULL)
      }
      
      # Create a data frame of candidate file paths and their base names
      df <- data.frame(Path = candidates, stringsAsFactors = FALSE)
      df <- df %>% mutate(Base = basename(Path))  # Add a new column to the data frame df called Base
      return(setNames(as.list(as.character(df$Path)), nm = df$Base))  # Return a named list of file paths
    })
    
    # Observer to update the selectizeInput choices with detected files
    observe({
      updateSelectizeInput(session, "automatic_data", choices = input_files())
    })
    
    # Observer to toggle between automatic and manual file selection inputs
    observeEvent(input$input_source, {
      if (input$input_source == "Automatic") {
        hideElement("manual_data")  # Hide manual data input
        showElement("automatic_data")  # Show automatic data input
      } else {
        hideElement("automatic_data")  # Hide automatic data input
        showElement("manual_data")  # Show manual data input
      }
    })
    
    # Reactive expression to read the selected file
    selected_file_contents <- reactive({
      if (input$input_source == "Automatic") {
        file_path <- input$automatic_data  # Get the selected automatic file path
      } 
      if (input$input_source == "Manual") {
        file_path <- input$manual_data$datapath  # Get the uploaded file path
      }
      # else {
      #   req(input$manual_data)  # Ensure a file is uploaded if Manual Upload is selected
      #   file_path <- input$manual_data$datapath  # Get the uploaded file path
      # }
      if (!is.null(file_path) && file.exists(file_path)) {
        df <- read.table(file_path, header = TRUE, sep = "\t", stringsAsFactors = FALSE)  # Read the file contents as a table
        if (!is.data.frame(df)) { # If df is not dataframe
          stop("Loaded data is not a data frame") #  Halt the execution and generate an error message
        }
        return(df)
      } else {
        return(NULL)  # Return NULL if the file does not exist
      }
    })
    
    # New reactive expression that processes the data
    data <- reactive({
      file1 <- selected_file_contents()
      if (is.null(file1)) {
        return(NULL)
      }
      return(file1)
    })
    
    return(data)  # Ensure the reactive expression is returned
  })
}

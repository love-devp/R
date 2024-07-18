# auto_load_v2.R

library(shiny)
library(shinyjs)
library(dplyr)
library(DT)
library(stringr)

# Define the script_path function
script_path <- function(subdir = NULL) {
  base_path <- getwd()  # Base path to the directory containing files
  if (!is.null(subdir)) {
    return(file.path(base_path, subdir))  # Append subdir to the base path if there is a subdir
  }
  return(base_path)  # Return base path if there is no subdir
}

# Module UI function
fileAutoloadUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(
      ns("input_source"),  
      "Choose How to load the data", 
      choices = c("Automatic", "Manual"), 
      selected = "Automatic"  # Initial selected value
    ),
    selectizeInput(
      ns('automatic_data'), #ID
      "Count Input:", #Label
      choices = NULL
    ),
    fileInput(
      ns("manual_data"),
      label = "Count Input:"
    ),
    # DTOutput(ns("file_contents"))
  )
}

# Module server function
fileAutoloadServer <- function(id, script_path) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive expression to list all files matching the pattern
    input_files <- reactive({
      all_files <- list.files(path = script_path(), pattern = "\\.txt$|\\.xls$|\\.csv$", recursive = TRUE)  # List all files with .txt or .xls or .csv patterns
      candidates <- character()  # Initialize an empty character vector for candidate files
      for (f in all_files) {
        if (substr(f, 1, 4) == "data" & str_detect(f, "inputs")) {  # Check conditions for the files
          candidates <- append(candidates, f)  # Append file to candidates if conditions are met
        }
      }
      # Create a data frame of candidate file paths and their base names
      df <- data.frame(Path = candidates) %>%
        mutate(Base = basename(Path))  # Add a new column to the data frame df called Base
      return(setNames(as.list(as.character(df$Path)), nm = df$Base))  # Return a named list of file paths
    })
    
    # Observer to toggle between automatic and manual file selection inputs
    observeEvent(input$input_source, {
      if (input$input_source == "Automatic") {
        hideElement(ns("manual_data"))  # Hide manual data input
        showElement(ns("automatic_data"))  # Show automatic data input
      } else {
        hideElement(ns("automatic_data"))  # Hide automatic data input
        showElement(ns("manual_data"))  # Show manual data input
      }
    })
    
    # Observer to update the selectizeInput choices with detected files
    observe({
      updateSelectizeInput(session, "automatic_data", choices = input_files())
    })
    
    # Reactive expressions to read and process the selected file
    file_selected <- reactive({
      if (input$input_source == 'Manual') {
        req(input$manual_data$datapath) # Ensures manual_data$datapath, if datapath is Null, the code execution will stop
        df <- read.delim(input$manual_data$datapath, header = TRUE, sep = '\t', check.names = FALSE) # Reads the uploaded file assuming it is tab-separated. The check.names argument prevents R from altering column names to make them syntactically valid.
        if (ncol(df) > 1) { # Checks if the dataframe has more than one column. If true, it returns df. If false, it attempts to read the file again as comma-separated
          return(df)
        } else {
          return(read.delim(input$manual_data$datapath, header = TRUE, sep = ',', check.names = FALSE))
        }
      } else {
        req(input$automatic_data)
        df <- read.delim(paste0(script_path(), '/', input$automatic_data), sep = '\t', check.names = FALSE)
        if (ncol(df) > 1) {
          return(df)
        } else {
          return(read.delim(paste0(script_path(), '/', input$automatic_data), sep = ',', check.names = FALSE))
        }
      }
    })
    
    # # Convert the data frame to a list
    # file_selected_as_list <- reactive({
    #   req(file_selected())
    #   as.list(file_selected_as_list())
    # })
    # 
    # output$file_contents <- renderDT({
    #   datatable(file_selected_as_list())
    # })
    # 
    # Convert the data frame to a vector
    # file_selected_as_vector <- reactive({
    #   req(file_selected())
    #   as.vector(as.matrix(file_selected()))
    # })
    
    return(file_selected)
  })
}
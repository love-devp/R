# Load necessary libraries.
library(shiny)       # To create Shiny app.
library(shinyjs)     # To enable JavaScript.
library(stringr)     # To enable string manipulation functions.
library(dplyr)       # To enable data manipulation functions.
library(DT)          # To display data tables.

# Define a function to generate the base path for file loading
script_path <- function(subdir = NULL) {
  base_path <- "/Git/R/Week3/Day2/test"  # Base path to the directory containing files.
  if (!is.null(subdir)) {
    return(file.path(base_path, subdir))  # Append subdir to the base path if there is a subdir.
  }
  return(base_path)  # Return base path if there is no subdir.
}

# Define UI for the application.
ui <- fluidPage(
  useShinyjs(),  # Enable shinyjs.
  
  # Dropdown to select input source (Automatic or Manual).
  selectInput(
    "input_source",  # Input ID.
    "Count Input Source:",  # Label.
    choices = c("Automatic", "Manual Upload"),
    selected = "Automatic"  # Initial selected value.
  ),
  
  # Conditional panel to show selectizeInput if Automatic is selected.
  conditionalPanel(
    condition = "input.input_source == 'Automatic'",
    selectizeInput(
      "automatic_data",  # Input ID.
      "Count Input:",  # Label.
      choices = NULL  # List of values to select from. It will be updated dynamically.
    )
  ),
  
  # Conditional panel to show the file upload control if Manual is selected.
  conditionalPanel(
    condition = "input.input_source == 'Manual Upload'",  # JS expression.
    fileInput(
      "manual_data",  # Input ID.
      "Choose file"  # Label.
    )
  ),
  
  # Hidden selectizeInput for feature column selection.
  hidden(
    selectizeInput(
      "feature_column",  # Input ID.
      label = "Feature Column:",  # Label.
      choices = NULL  # List of values to select from. It will be updated dynamically.
    )
  ),
  DTOutput("file_contents")  # Display a data table (outputID).
)

# Define server logic.
server <- function(input, output, session) {
  # Reactive expression to list all files matching the pattern.
  input_files <- reactive({
    all_files <- list.files(path=script_path(), pattern="*.t*", recursive=TRUE)  # List all files with .t* pattern.
    candidates <- character()  # Initialize an empty character vector for candidate files.
    for (f in all_files) {
      if (substr(f, 1, 3) != "run" & substr(f, 1, 15) != "persistent_data" & str_detect(f, "inputs")) {
        candidates <- append(candidates, f)  # Append file to candidates if conditions are met.
      }
    }
    # Create a data frame of candidate file paths and their base names.
    df <- data.frame(Path　=　candidates) %>%
      mutate(Base = basename(Path))  # Add a new column to the data frame df called Base.
    return(setNames(as.list(as.character(df$Path)), nm = df$Base))  # Return a named list of file paths.
  })
  
  # Observer to update the selectizeInput choices with detected files.
  observe({
    updateSelectizeInput(session, "automatic_data", choices = input_files())
  })
  
  # Observer to toggle between automatic and manual file selection inputs.
  observeEvent(input$input_source, {
    if (input$input_source == "Automatic") {
      hideElement("manual_data")  # Hide manual data input.
      showElement("automatic_data")  # Show automatic data input.
    } else {
      hideElement("automatic_data")  # Hide automatic data input.
      showElement("manual_data")  # Show manual data input.
    }
  })
  
  # Reactive expression to read the selected file.
  selected_file_content <- reactive({
    if (input$input_source == "Automatic") {
      file_path <- input$automatic_data  # Get the selected automatic file path.
    } else {
      req(input$manual_data)  # Ensure a file is uploaded if Manual Upload is selected.
      file_path <- input$manual_data$datapath  # Get the uploaded file path.
    }
    if (!is.null(file_path) && file.exists(file_path)) {
      read.table(file_path, header = TRUE, sep = "\t")  # Read the file contents as a table.
    } else {
      NULL  # Return NULL if the file does not exist.
    }
  })
  
  # Render the contents of the selected file in a data table.
  output$file_contents <- renderDT({
    req(selected_file_content())  # Ensure there is content to display.
    datatable(selected_file_content())  # Display the data table.
  })
}

# Run the application.
shinyApp(ui = ui, server = server)

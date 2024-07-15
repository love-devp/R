# app.R

library(shiny)
library(shinyjs)
library(stringr)
library(dplyr)
library(DT)

script_path <- function(subdir = NULL) {
  # Define your function logic here
  base_path <- "/Git/R/Week3/Day1/test"
  if (!is.null(subdir)) {
    return(file.path(base_path, subdir))
  }
  return(base_path)
}

# Define UI for the application
ui <- fluidPage(
  useShinyjs(), # Set up shinyjs
  selectInput(
    "input_source",
    "Count Input Source:",
    choices = c("Automatic", "Manual Upload"),
    selected = "Automatic"
  ),
  selectizeInput(
    "automatic_data", 
    "Count Input:", 
    choices = NULL
  ),
  uiOutput("file_selector"
  ),
  hidden(
    selectizeInput(
      "feature_column",
      label = "Feature Column:", 
      choices = NULL
    )
  ), # Close the hidden function call
  verbatimTextOutput("summary"),
  DTOutput("file_contents")
) # Close the fluidPage function call

# Define server logic
server <- function(input, output, session) {
  observeEvent(input_files(), once = TRUE, {
    if (length(input_files()) == 0) {
      hideElement("input_source")
      hideElement("automatic_data")
      showElement("manual_data")
      updateSelectizeInput(session, "input_source",
                           choices = c("Automatic", "Manual Upload"),
                           selected = "Manual Upload")
    }
  })
  observeEvent(input$input_source, {
    if (input$input_source == "Automatic"){
      hideElement("input_file")
      showElement("automatic_data")
    } else {
      hideElement("automatic_data")
      showElement("input_file")
    }
  })
  input_files = reactive({
    all_files = list.files(path=script_path(), pattern="*.t*", recursive=TRUE)
    candidates = character()
    for (f in all_files) {
      if (substr(f, 1, 3) != "run" & substr(f, 1, 15) != "persistent_data" & str_detect(f, "inputs")) {
        candidates = append(candidates, f)
      }
    }
    df = data.frame(Path=candidates) %>%
      mutate(Base = basename(Path))
    return(setNames(as.list(as.character(df$Path)), nm = df$Base))
  })
  
  output$file_selector <- renderUI({
    selectInput("selected_file", "Select a file", choices = input_files())
  })
  
  output$summary <-renderPrint({
    input_files()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

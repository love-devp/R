# app.R

library(shiny)
library(shinyjs)

script_path("/data")

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
  fileInput(
    "input_file",
    "Count_input: "
  ),
  hidden(
    selectizeInput(
      "feature_column",
      label = "Feature Column:", 
      choices = NULL
    )
  ) # Close the hidden function call
) # Close the fluidPage function call

# Define server logic
server <- function(input, output, session) {
  observeEvent(input$input_file, once = TRUE, {
    if (is.null(input$input_file)) {
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
    all_files = list.files(path = script_path(), 
                           pattern = "*.t*", recursive = TRUE)
    candidates = character()
    for (f in all_files){
      candidates = append(candidates, f)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

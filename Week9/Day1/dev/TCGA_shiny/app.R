library(shiny)

# Define the UI
ui <- fluidPage(
  titlePanel("RDS File Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      # Input to select the RDS file
      selectInput("selected_rds", "Select RDS File:", choices = NULL),
      actionButton("load_rds", "Load RDS File")
    ),
    
    mainPanel(
      # Output: Table to display the contents of the RDS file
      tableOutput("rds_table")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  # Path to the folder containing RDS files
  path_to_rds_files <- 'C:/Git/TCGA/GDCdata/GDC.Data.2023.biolink.3.18/GDC.2023.biolink.3.18.Transcriptome.with.log2.TPM.assay'
  
  
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
      head(rds_data)  # Display the first few rows of the data frame
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)


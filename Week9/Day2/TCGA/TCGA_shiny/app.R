# app.R
library(shiny)

# Define the UI
ui <- fluidPage(
  titlePanel("RDS File Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      # Call the UI from the input page module
      input_page_ui("rds_module")
    ),
    
    mainPanel(
      # Display output from the input page module
      uiOutput("rds_table_output")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  # Path to the folder containing RDS files
  path_to_rds_files <- 'C:/Git/TCGA/GDCdata/GDC.Data.2023.biolink.3.18/GDC.2023.biolink.3.18.Transcriptome.with.log2.TPM.assay'
  
  # Call the server logic from the input page module
  input_page_server("rds_module", path_to_rds_files)
}

# Run the application
shinyApp(ui = ui, server = server)

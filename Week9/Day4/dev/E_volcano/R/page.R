# # page.R
# 
# PageUI <- function(id) {
#   ns <- NS(id)
#     nav_panel(
#       'Volcano Plot',
#       evolPlotUI(ns('e_volcano_plot'))  # Call the volcano plot UI inside a tabPanel
#     )
# }
# 
# PageServer <- function(id) {
#   moduleServer(id, function(input, output, session) {
#     evolPlotServer('e_volcano_plot')  # Adjust 'script_path' and 'org' as necessary
#   })
# }

# page.R

PageUI <- function(id) {
  ns <- NS(id)
  nav_panel(
    'Volcano Plot',
    evolPlotUI(ns('e_volcano_plot'))  # Call the volcano plot UI inside a tabPanel
  )
}

PageServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Define your data here, or pass it as needed
    data <- reactive({
      # Example dataset, replace with actual data
      df <- data.frame(
        Event = letters[1:10],
        FoldChange = runif(10, -2, 2),
        PValue = runif(10, 0, 0.05),
        Expression = rnorm(10)
      )
      return(df)
    })
    
    evolPlotServer(
      'e_volcano_plot',
      data = data,
      event_column = reactive("Event"),
      fold_change_column = reactive("FoldChange"),
      significance_column = reactive("PValue"),
      significance_threshold = 0.05,
      fold_change_threshold = 1
    )
  })
}

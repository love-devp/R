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
    evolPlotServer('e_volcano_plot')  # Adjust 'script_path' and 'org' as necessary
  })
}

# page.R

PageUI <- function(id) {
  ns <- NS(id)
    nav_panel(
      'Volcano Plot',
      evolPlotUI(ns('e_volcano_plot'))  # Call the volcano plot UI inside a tabPanel
    )
}

PageServer <- function(id, script_path) {
  moduleServer(id, function(input, output, session) {
  })
}

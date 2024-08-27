PageUI <- function(id) {
  ns <- NS(id)
  tagList(
    navbarPage(
      title = "Volcano Plot",
      tabPanel('Plot',
               plotOutput(ns('e_volcano_plot')) %>% withSpinner(image = 'spinner.gif')
      )
    )
  )
}

PageServer <- function(id, script_path){
  moduleServer(id, function(input, output, session) {
    
    output$e_volcano_plot <- renderPlot({
      plots$e_volcano_plot()
    })
  })
}

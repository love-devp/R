dePageUI <- function(id) {
  tagList(
    layout_sidebar(
      sidebar= sidebar(
        accordion(
          accordion_panel("Significance Cutoffs", icon=icon('star'),
                          significanceUI(NS(id, 'significance_panel')),
          ),
          accordion_panel("Plot Options", icon=icon('gear'),
                          plotUI(NS(id, 'plot_panel'))
          )
        )
      ),
      layout_column_wrap(
        width=1/2,
        card(
          card_header("Enhanced Volcano Plot"),
          card_body(
            plotOutput(NS(id, "volcano")) %>% withSpinner(image='spinner.gif')
          )
        ),
        card(
          card_header("MA Plot"),
          card_body(
            plotOutput(NS(id,"ma")) %>% withSpinner(image='spinner.gif')
          )
        )
      )
    )
  )
}

dePageServer <- function(id, data) {
  
  moduleServer(id, function(input, output, session) {
    
    significance = significanceServer("significance_panel", isolate(data$raw), data$fold_change_column, data$significance_column, data$expression_column)
    plots = plotServer("plot_panel", significance$df, data$event_column, data$fold_change_column, data$significance_column, data$expression_column, significance$fold_change, significance$significance)
    
    output$volcano = renderPlot({
      plots$volcano()
    })
    
    output$ma = renderPlot({
      plots$ma()
    })
    
    return(significance)
    
  })
}
dePageUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    layout_sidebar(
      sidebar = sidebar(
        accordion(
          accordion_panel("Significance Cutoffs", icon = icon('star'),
                          significanceUI(ns('significance_panel'))
          ),
          accordion_panel("Plot Options", icon = icon('gear'),
                          plotUI(ns('plot_panel'))
          )
        )
      ),
      layout_column_wrap(
        width = 1/2,
        card(
          card_header("Enhanced Volcano Plot"),
          card_body(
            evolPlotUI(ns("volcano")) %>% withSpinner(image = 'spinner.gif'),
            fluidRow(
              selectInput(ns('file_type_vol'), 'Select file type', choices = c('PNG' = 'png', 'JPG' = 'jpg', 'PDF' = 'pdf')),
              downloadButton(ns('volcano_download'), label = 'Download Volcano Plot')
            )
          )
        ),
        card(
          card_header("MA Plot"),
          card_body(
            plotOutput(ns("ma")) %>% withSpinner(image = 'spinner.gif'),
            fluidRow(
              selectInput(ns('file_type_ma'), 'Select file type', choices = c('PNG' = 'png', 'JPG' = 'jpg', 'PDF' = 'pdf')),
              downloadButton(ns('ma_download'), label = 'Download MA Plot')
            )
          )
        )
      )
    )
  )
}


dePageServer <- function(id, data) {
  
  moduleServer(id, function(input, output, session) {
    
    significance <- significanceServer("significance_panel", isolate(data$raw), data$fold_change_column, data$significance_column, data$expression_column)
    plots <- plotServer("plot_panel", significance$df, data$event_column, data$fold_change_column, data$significance_column, data$expression_column, significance$fold_change, significance$significance)
    
    evol_plot_data <- evolPlotServer('volcano', data, data$event_column, data$fold_change_column, data$significance_column, significance$significance, significance$fold_change)
    
    output$volcano <- renderPlot({
      evol_plot_data$e_volcano_plot()
    })

    output$volcano_download <- downloadHandler(
      filename = function() {
        paste0('volcano.', input$file_type_vol)
      },
      content = function(file) {
        ggsave(file, plot = plots$volcano(), device = input$file_type_vol)
      }
    )
    
    output$ma <- renderPlot({
      plots$ma()
    })
    
    output$ma_download <- downloadHandler(
      filename = function() {
        paste0('ma.', input$file_type_ma)
      },
      content = function(file) {
        ggsave(file, plot = plots$ma(), device = input$file_type_ma)
      }
    )
    
    return(significance)
    
  })
}


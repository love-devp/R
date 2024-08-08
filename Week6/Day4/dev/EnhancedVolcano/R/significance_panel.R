significanceUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    fluidRow(
      column(7, numericInput(ns("fold_change_threshold"), "Fold Change:", value = 1, step=.1)),
      column(5, numericInput(ns("significance_threshold"), "P-value:", value = .05, step=.01)),
      checkboxInput(ns('limit_fc'), "Limit Fold Change", value=FALSE),
      numericInput(ns("fc_ceiling"), "Fold Change Ceiling:", value = 16, step=.1),
      checkboxInput(ns('limit_sig'), "Limit Significance", value=FALSE),
      numericInput(ns("sig_floor"), "Significance Floor (10^):", value = -40, step=.1)
    )
  )
}

significanceServer <- function(id, data, fold_change_column, significance_column, expression_column) {
  
  moduleServer(id, function(input, output, session) {
    
    df = reactive({
      req(fold_change_column())
      req(significance_column())
      data() %>%
        mutate(!!sym(fold_change_column()) := case_when(
          input$limit_fc == TRUE & !!sym(fold_change_column()) < -input$fc_ceiling ~ -input$fc_ceiling,
          input$limit_fc == TRUE & !!sym(fold_change_column()) > input$fc_ceiling ~ input$fc_ceiling,
          TRUE ~ !!sym(fold_change_column())
        )) %>%
        mutate(!!sym(significance_column()) := case_when(
          input$limit_sig == TRUE & !!sym(significance_column()) < 10^(input$sig_floor) ~ 10^(input$sig_floor),
          TRUE ~ !!sym(significance_column())
        )) %>%
        mutate(Direction = case_when(!!sym(significance_column()) < input$significance_threshold & !!sym(fold_change_column()) > input$fold_change_threshold ~ 'Upregulated',
                                     !!sym(significance_column()) < input$significance_threshold & !!sym(fold_change_column()) < -input$fold_change_threshold ~ 'Downregulated',
                                     TRUE ~ 'No Change')) %>%
        mutate(Direction = factor(Direction, levels=c("Upregulated", "No Change", "Downregulated"))) %>%
        mutate(Significant = case_when(Direction == 'No Change' ~ 'Non-Significant',
                                       TRUE ~ 'Significant'))
    })
    
    observeEvent(input$limit_fc, ignoreNULL=TRUE, {
      if (input$limit_fc) {
        showElement("fc_ceiling")
      } else {
        hideElement("fc_ceiling")
      }
    })
    
    observeEvent(input$limit_sig, ignoreNULL=TRUE, {
      if (input$limit_sig) {
        showElement("sig_floor")
      } else {
        hideElement("sig_floor")
      }
    })
    
    return(
      list(
        fold_change = reactive(input$fold_change_threshold),
        significance = reactive(input$significance_threshold),
        df = df
      )
    )
    
  })
}
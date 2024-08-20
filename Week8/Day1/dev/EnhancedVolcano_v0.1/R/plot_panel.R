# Load EnhancedVolcano function
source('./R/Enhancedvolcano.R')

# Reverse log transformation function
reverselog = function() {
  trans_new('reverselog', function(x) -log10(x), function(x) 10^(-x), log_breaks(base = 10), domain = c(1e-1000, Inf))
}

volcanoPlotUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    accordion(
      open=FALSE,
      accordion_panel('Label Options',
                      selectizeInput(ns('label_strategy'), 'Event Labels:', choices = c('None', 'Significant/Highlighted', 'Highlighted Only'), selected='None'),
                      selectizeInput(ns('highlighted'), 'Highlighted Events:', choices=NULL, multiple=TRUE, options = list(create = TRUE, delimiter = ',')),
                      numericInput(ns('label_size'), 'Label Size:', value=4), 
                      selectizeInput(ns('legend_position'), 'Legend Position', choices = c('top', 'bottom', 'right', 'left', 'none'), selected = 'none'),
                      checkboxInput(ns('upregulated_check'), 'Upregulated Count', value = FALSE), 
                      checkboxInput(ns('downregulated_check'), 'Downregulated Count', value = FALSE),
                      checkboxInput(ns('noChange_check'), 'No Change Count', value = FALSE)
      ),
      accordion_panel('Point Options',
                      colourInput(ns('downregulated_color'), 'Downregulated', value='#4682B4'),
                      colourInput(ns('noChange_color'), 'No Change:', value='#e7e7e7'),
                      colourInput(ns('upregulated_color'), 'Upregulated:', value='#b22222'),
                      numericInput(ns('downregulated_size'), 'Downregulated', value=2),
                      numericInput(ns('noChange_size'), 'No Change', value=1),
                      numericInput(ns('upregulated_size'), 'Upregulated', value=2),
                      numericInput(ns('downregulated_alpha'), 'Downregulated', value=1),
                      numericInput(ns('noChange_alpha'), 'No Change', value=1),
                      numericInput(ns('upregulated_alpha'), 'Upregulated', value=1)
      ),
      accordion_panel('Axis Options',
                      checkboxInput(ns('include_zero_line'), 'Zero Line', value=TRUE),
                      colourInput(ns('zero_line_color'), 'Color', value='#000000'),
                      checkboxInput(ns('include_fc_line'), 'Fold Change Cutoff Line', value=TRUE),
                      colourInput(ns('fc_line_color'), 'Color', value='#e7e7e7'),
                      checkboxInput(ns('include_sig_line'), 'Significance Cutoff Line', value=TRUE),
                      colourInput(ns('sig_line_color'), 'Color', value='#e7e7e7')
      )
    ),
    navset_card_pill(
      full_screen = TRUE,
      height = 400,
      title=NULL,
      nav_panel("Volcano Plot",
                plotOutput(ns('volcano_plot')) %>% withSpinner(image='spinner.gif'),
                downloadButton(ns('volcano_plot_download'), label = 'Download Volcano Plot')
      )
    )
  )
}

volcanoPlotServer <- function(id, data, event_column, fold_change_column, significance_column, fold_change_threshold, significance_threshold) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive expression for data manipulation
    df <- reactive({
      data() %>%
        mutate(Highlighted = !!sym(event_column()) %in% input$highlighted) %>%
        mutate(Direction = case_when(
          Highlighted == TRUE & Direction == 'Downregulated' ~ 'Highlighted Downregulated',
          Highlighted == TRUE & Direction == 'No Change' ~ 'Highlighted',
          Highlighted == TRUE & Direction == 'Upregulated' ~ 'Highlighted Upregulated',
          TRUE ~ Direction
        )) %>%
        mutate(Label = case_when(
          Highlighted == TRUE ~ !!sym(event_column()),
          input$label_strategy == 'Significant/Highlighted' & Significant == 'Significant' ~ !!sym(event_column())
        ))
    })
    
    # Volcano plot reactive rendering
    volcano_plot <- reactive({
      EnhancedVolcano(
        toptable = df(),
        lab = df()$Label,
        x = fold_change_column(),
        y = significance_column(),
        title = NULL,
        subtitle = NULL,
        caption = NULL,
        legendPosition = input$legend_position,
        pCutoff = significance_threshold(),
        pCutoffCol = significance_column(),
        FCcutoff = fold_change_threshold(),
        labSize = input$label_size,
        cutoffLineType = 'blank',
        cutoffLineCol = input$fc_line_color,
        hline = NULL,
        hlineCol = input$sig_line_color,
        vline = NULL,
        vlineCol = input$fc_line_color,
        drawConnectors = TRUE,
        colCustom = colors(),
        colAlpha = alphas(),
        pointSize = sizes()
      )
    })
    
    # Render volcano plot
    output$volcano_plot <- renderPlot({
      req(volcano_plot())
      volcano_plot()
    })
    
    # Download handler for volcano plot
    output$volcano_plot_download <- downloadHandler(
      filename = function() { 'volcano_plot.png' },
      content = function(file) {
        png(file=file)
        print(volcano_plot())
        dev.off()
      }
    )
  })
}

evolPlotUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    fileInput(ns("input_file"), label='Input Data:'),
    shinyjs::hidden(selectizeInput(ns("event_column"), label='Event Column:', choices=NULL)),
    shinyjs::hidden(selectizeInput(ns("fold_change_column"), label='Fold Change Column:', choices=NULL)),
    shinyjs::hidden(selectizeInput(ns("significance_column"), label='P-Value Column:', choices=NULL)),
    shinyjs::hidden(selectizeInput(ns("expression_column"), label='Expression Column:', choices=NULL),
    layout_sidebar(
      sidebar = sidebar(
        accordion(open=FALSE,
                  accordion_panel("Label Options",
                                  selectizeInput(ns("label_strategy"), "Event Labels:", choices = c("None", "Significant/Highlighted", "Highlighted Only"), selected='None'),
                                  selectizeInput(ns("highlighted"), "Highlighted Events:", choices=NULL, multiple=TRUE),
                                  numericInput(ns('label_size'), "Label Size:", value=4)
                  )
                  ),
        accordion(open=FALSE,
                  accordion_panel("Point Options",
                                  accordion(open=FALSE,
                                            accordion_panel("Color",
                                                            h5("Standard:"),
                                                            textInput(ns('downregulated_color'), "Downregulated", value='#4682B4'),
                                                            textInput(ns('noChange_color'), "No Change:", value='#e7e7e7'),
                                                            textInput(ns('upregulated_color'), "Upregulated:", value='#b22222'),
                                                            h5("Highlighted:"),
                                                            textInput(ns('highlight_downregulated_color'), "Downregulated", value='#1e6234'),
                                                            textInput(ns('highlight_noChange_color'), "No Change:", value='#000000'),
                                                            textInput(ns('highlight_upregulated_color'), "Upregulated:", value='#1e6234')
                                            ),
                                            accordion_panel("Size",
                                                            h5("Standard:"),
                                                            numericInput(ns('downregulated_size'), "Downregulated", value=2),
                                                            numericInput(ns('noChange_size'), "No Change", value=1),
                                                            numericInput(ns('upregulated_size'), "Upregulated", value=2),
                                                            h5("Highlighted:"),
                                                            numericInput(ns('highlight_downregulated_size'), "Downregulated", value=2),
                                                            numericInput(ns('highlight_noChange_size'), "No Change", value=2),
                                                            numericInput(ns('highlight_upregulated_size'), "Upregulated", value=2)
                                            ),
                                            accordion_panel("Transparency",
                                                            h5("Standard:"),
                                                            numericInput(ns('downregulated_alpha'), "Downregulated", value=1),
                                                            numericInput(ns('noChange_alpha'), "No Change", value=1),
                                                            numericInput(ns('upregulated_alpha'), "Upregulated", value=1),
                                                            h5("Highlighted:"),
                                                            numericInput(ns('highlight_downregulated_alpha'), "Downregulated", value=1),
                                                            numericInput(ns('highlight_noChange_alpha'), "No Change", value=1),
                                                            numericInput(ns('highlight_upregulated_alpha'), "Upregulated", value=1)
                                            )
                                  )
                  )
                  ),
        accordion(open=FALSE,
                  accordion_panel("Axis Options",
                                  checkboxInput(ns('include_zero_line'), "Zero Line", value=TRUE),
                                  textInput(ns('zero_line_color'), "Color", value='#000000'),
                                  checkboxInput(ns('include_fc_line'), "Fold Change Cutoff Line", value=TRUE),
                                  textInput(ns('fc_line_color'), "Color", value='#e7e7e7'),
                                  checkboxInput(ns('include_sig_line'), "Significance Cutoff Line", value=TRUE),
                                  textInput(ns('sig_line_color'), "Color", value='#e7e7e7')
                  )
        )
      )
      )
    ),
    card_body(
      plotOutput(ns('e_volcano_plot')) %>% withSpinner(image='spinner.gif', id=ns('spinner'))
    )
  )
}


# e_volcano_plot.R

evolPlotServer <- function(id, data, event_column, fold_change_column, significance_column, significance_threshold, fold_change_threshold) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$input_file$datapath, ignoreInit = TRUE, {
      shinyjs::showElement("event_column")
      shinyjs::showElement("fold_change_column")
      shinyjs::showElement("significance_column")
      shinyjs::showElement("expression_column")
    })
    
    observeEvent(input$automatic_data, ignoreInit = TRUE, {
      shinyjs::showElement("event_column")
      shinyjs::showElement("fold_change_column")
      shinyjs::showElement("significance_column")
      shinyjs::showElement("expression_column")
    })
    
    raw_data = reactive({
      if (req(input$input_file$datapath)) {
        df = read.delim(input$input_file$datapath, header=TRUE, sep = '\t')
        if (length(df) > 1) {
          return(df)
        } else {
          return(read.delim(input$input_file$datapath, header=TRUE, sep = ','))
        }
      }
    })
    
    observeEvent(data(), {
      updateSelectizeInput(session, 'highlighted', 'Highlighted Events:', choices=df()[,event_column()], server=TRUE)
    })
    
    colors = reactive({
      c('Upregulated'=input$upregulated_color, 'No Change'=input$noChange_color, 'Downregulated'=input$downregulated_color,
        'Highlighted Upregulated'=input$highlight_upregulated_color, 'Highlighted'=input$highlight_noChange_color, 'Highlighted Downregulated'=input$highlight_downregulated_color)
    })
    
    alphas = reactive({ 
      c('Upregulated'=input$upregulated_alpha, 'No Change'=input$noChange_alpha, 'Downregulated'=input$downregulated_alpha,
        'Highlighted Upregulated'=input$highlight_upregulated_alpha, 'Highlighted'=input$highlight_noChange_alpha, 'Highlighted Downregulated'=input$highlight_downregulated_alpha)
    })
    
    sizes = reactive({
      c('Upregulated'=input$upregulated_size, 'No Change'=input$noChange_size, 'Downregulated'=input$downregulated_size,
        'Highlighted Upregulated'=input$highlight_upregulated_size, 'Highlighted'=input$highlight_noChange_size, 'Highlighted Downregulated'=input$highlight_downregulated_size)
    })
    
    df = reactive({
      data() %>%
        mutate(Highlighted = !!sym(event_column()) %in% input$highlighted) %>%
        mutate(Direction = case_when(Highlighted == TRUE & Direction == 'Downregulated' ~ 'Highlighted Downregulated',
                                     Highlighted == TRUE & Direction == 'No Change' ~ 'Highlighted',
                                     Highlighted == TRUE & Direction == 'Upregulated' ~ 'Highlighted Upregulated',
                                     TRUE ~ Direction
        )) %>%
        mutate(Label = case_when(Highlighted == TRUE ~ !!sym(event_column()),
                                 input$label_strategy == 'Significant/Highlighted' & Significant == 'Significant' ~ !!sym(event_column())))
    })
    
    output$e_volcano_plot <- renderPlot({
      EnhancedVolcano(
        toptable = df(),
        lab = df()$Label,
        x = fold_change_column(),
        y = significance_column(),
        title = NULL,
        subtitle = NULL,
        caption = NULL,
        legendPosition = input$legend_position,
        pCutoff = 0.05,
        pCutoffCol = significance_column(),
        FCcutoff = 1,
        labSize = input$label_size,
        cutoffLineType = 'blank',
        cutoffLineCol = input$fc_line_color,
        hline = NULL,
        hlineCol = input$sig_line_color,
        vline = NULL,
        vlineCol = input$fc_line_color,
        drawConnectors = if (input$label_strategy != 'None') TRUE else FALSE,
        widthConnectors = 0.10,
        boxedLabels = TRUE,
        colCustom = c('Upregulated'=input$upregulated_color, 'No Change'=input$noChange_color, 'Downregulated'=input$downregulated_color),
        colAlpha = c('Upregulated'=input$upregulated_alpha, 'No Change'=input$noChange_alpha, 'Downregulated'=input$downregulated_alpha),
        pointSize = c('Upregulated'=input$upregulated_size, 'No Change'=input$noChange_size, 'Downregulated'=input$downregulated_size)
      )
    })
  })
}



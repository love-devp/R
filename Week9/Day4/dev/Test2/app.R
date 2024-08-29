library(shiny)
library(shinyjs)
library(dplyr)
library(EnhancedVolcano)
library(shinyWidgets)
library(colourpicker)
library(bslib)
library(ggplot2)

# UI Module
evolPlotUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    fileInput(ns("input_file"), label = 'Input Data:'),
    layout_sidebar(
      sidebar = sidebar(
        accordion(open = FALSE,
                  accordion_panel("Label Options",
                                  selectizeInput(ns("label_strategy"), "Event Labels:", 
                                                 choices = c("None", "Significant/Highlighted", "Highlighted Only"), 
                                                 selected = 'None'),
                                  selectizeInput(ns("highlighted"), "Highlighted Events:", 
                                                 choices = NULL, multiple = TRUE),
                                  numericInput(ns('label_size'), "Label Size:", value = 4)
                  )),
        accordion(open = FALSE,
                  accordion_panel("Point Options",
                                  accordion(open = FALSE,
                                            accordion_panel("Color",
                                                            h5("Standard:"),
                                                            textInput(ns('downregulated_color'), "Downregulated", value = '#4682B4'),
                                                            textInput(ns('noChange_color'), "No Change:", value = '#e7e7e7'),
                                                            textInput(ns('upregulated_color'), "Upregulated:", value = '#b22222'),
                                                            h5("Highlighted:"),
                                                            textInput(ns('highlight_downregulated_color'), "Downregulated", value = '#1e6234'),
                                                            textInput(ns('highlight_noChange_color'), "No Change:", value = '#000000'),
                                                            textInput(ns('highlight_upregulated_color'), "Upregulated", value = '#1e6234')
                                            ),
                                            accordion_panel("Size",
                                                            h5("Standard:"),
                                                            numericInput(ns('downregulated_size'), "Downregulated", value = 2),
                                                            numericInput(ns('noChange_size'), "No Change", value = 1),
                                                            numericInput(ns('upregulated_size'), "Upregulated", value = 2),
                                                            h5("Highlighted:"),
                                                            numericInput(ns('highlight_downregulated_size'), "Downregulated", value = 2),
                                                            numericInput(ns('highlight_noChange_size'), "No Change", value = 2),
                                                            numericInput(ns('highlight_upregulated_size'), "Upregulated", value = 2)
                                            ),
                                            accordion_panel("Transparency",
                                                            h5("Standard:"),
                                                            numericInput(ns('downregulated_alpha'), "Downregulated", value = 1),
                                                            numericInput(ns('noChange_alpha'), "No Change", value = 1),
                                                            numericInput(ns('upregulated_alpha'), "Upregulated", value = 1),
                                                            h5("Highlighted:"),
                                                            numericInput(ns('highlight_downregulated_alpha'), "Downregulated", value = 1),
                                                            numericInput(ns('highlight_noChange_alpha'), "No Change", value = 1),
                                                            numericInput(ns('highlight_upregulated_alpha'), "Upregulated", value = 1)
                                            )
                                  )
                  )
        ),
        accordion(open = FALSE,
                  accordion_panel("Axis Options",
                                  checkboxInput(ns('include_zero_line'), "Zero Line", value = TRUE),
                                  textInput(ns('zero_line_color'), "Color", value = '#000000'),
                                  checkboxInput(ns('include_fc_line'), "Fold Change Cutoff Line", value = TRUE),
                                  textInput(ns('fc_line_color'), "Color", value = '#e7e7e7'),
                                  checkboxInput(ns('include_sig_line'), "Significance Cutoff Line", value = TRUE),
                                  textInput(ns('sig_line_color'), "Color", value = '#e7e7e7')
                  )
        )
      )
    ),
    card_body(
      plotOutput(ns('e_volcano_plot')) %>% withSpinner(image = 'spinner.gif', id = ns('spinner'))
    )
  )
}

# Server Module
evolPlotServer <- function(id, data, event_column, fold_change_column, significance_column, significance_threshold, fold_change_threshold) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(data(), {
      updateSelectizeInput(session, 'highlighted', 'Highlighted Events:', choices = data()[, event_column()], server = TRUE)
    })
    
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
          input$label_strategy == 'Significant/Highlighted' & !!sym(significance_column()) <= significance_threshold ~ !!sym(event_column())
        ))
    })
    
    output$e_volcano_plot <- renderPlot({
      
      upregulated_count = nrow(df() %>% filter(Direction == 'Upregulated'))
      noChange_count = nrow(df() %>% filter(Direction == 'No Change'))
      downregulated_count = nrow(df() %>% filter(Direction == 'Downregulated'))
      
      req(df())
      
      EnhancedVolcano(
        toptable = df(),
        lab = df()$Label,
        x = fold_change_column(),
        y = significance_column(),
        title = NULL,
        subtitle = NULL,
        caption = NULL,
        legendPosition = input$legend_position,
        pCutoff = significance_threshold,
        FCcutoff = fold_change_threshold,
        labSize = input$label_size,
        cutoffLineType = 'blank',
        cutoffLineCol = input$fc_line_color,
        hline = NULL,
        hlineCol = input$sig_line_color,
        vline = NULL,
        vlineCol = input$fc_line_color,
        drawConnectors = ifelse(input$label_strategy != 'None', TRUE, FALSE),
        widthConnectors = 0.10,
        boxedLabels = TRUE,
        colCustom = colors(),
        colAlpha = alphas(),
        pointSize = sizes()
      )+ {if (input$include_fc_line) geom_vline(xintercept = fold_change_threshold(), linetype=2, color=input$fc_line_color)} +
        {if (input$include_zero_line) geom_vline(xintercept = 0, linetype=2, color=input$zero_line_color)} +
        {if (input$include_fc_line) geom_vline(xintercept = -fold_change_threshold(), linetype=2, color=input$fc_line_color)} +
        {if (input$include_sig_line) geom_hline(yintercept = significance_threshold(), linetype=2, color=input$sig_line_color)} +
        {if (input$upregulated_check) annotate('text', x=Inf, y=Inf, hjust=1, vjust=1, color=input$upregulated_color, label=paste0('Upregulated: ', upregulated_count))} +
        {if (input$downregulated_check) annotate('text', x=-Inf, y=Inf, hjust=-.01, vjust=1, color=input$downregulated_color, label=paste0('Downregulated: ', downregulated_count))} +
        {if (input$noChange_check) annotate('text', x=0, y=Inf, vjust=1, color=input$noChange_color, label=paste0('No Change: ', noChange_count))}
    })
  })
}

# UI
ui <- fluidPage(
  evolPlotUI("evolPlot")
)

# Server
server <- function(input, output, session) {
  
  raw_data <- reactive({
    req(input$evolPlot_input_file$datapath)  # Note the use of module input ID
    df <- read.delim(input$evolPlot_input_file$datapath, header = TRUE, sep = '\t')
    if (ncol(df) > 1) {
      return(df)
    } else {
      return(read.delim(input$evolPlot_input_file$datapath, header = TRUE, sep = ','))
    }
  })
  
  evolPlotServer('e_volcano_plot', 
                 data = raw_data, 
                 event_column = reactive("Event"),
                 fold_change_column = reactive("FoldChange"), 
                 significance_column = reactive("PValue"), 
                 significance_threshold = 0.05, 
                 fold_change_threshold = 1.5)
}

# Run the application 
shinyApp(ui = ui, server = server)

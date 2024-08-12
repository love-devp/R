source('./R/volcano_module.R')

reverselog = function() {
  trans_new("reverselog", function(x) -log10(x), function(x) 10^(-x), log_breaks(base = 10), domain = c(1e-1000, Inf))
}

plotUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    accordion(open=FALSE,
              accordion_panel("Label Options",
                              selectizeInput(ns("label_strategy"), "Event Labels:", choices = c("None", "Significant/Highlighted", "Highlighted Only"), selected='None'),
                              selectizeInput(ns("highlighted"), "Highlighted Events:", choices=NULL, multiple=TRUE, options = list(create = TRUE, delimiter = ",")), # Add Copy&Paste option
                              numericInput(ns('label_size'), "Label Size:", value=4),
              ),
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
                                        ),
                              )
              ),
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
}

plotServer <- function(id, data, event_column, fold_change_column, significance_column, expression_column, fold_change_threshold, significance_threshold) {
  
  moduleServer(id, function(input, output, session) {
    
    observeEvent(data(), {
      updateSelectizeInput(session, 'highlighted', "Highlighted Events:", choices=df()[,event_column()], server=TRUE)
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
        'Highlighted Upregulated'=input$highlight_upregulated_size, 'Highlighted Downregulated'=input$highlight_downregulated_size)
    })
    
    df = reactive({
      data() %>%
        mutate(Highlighted = !!sym(event_column()) %in% input$highlighted) %>%
        mutate(Direction = case_when(Highlighted == TRUE & Direction == 'Downregulated' ~ "Highlighted Downregulated",
                                     Highlighted == TRUE & Direction == "No Change" ~ "Highlighted",
                                     Highlighted == TRUE & Direction == 'Upregulated' ~ "Highlighted Upregulated",
                                     TRUE ~ Direction
        )) %>%
        mutate(Label = case_when(Highlighted == TRUE ~ !!sym(event_column()),
                                 input$label_strategy == 'Significant/Highlighted' & Significant == 'Significant' ~ !!sym(event_column())))
    })
    
    volcano_plot <- reactive({
      create_volcano_plot(
        res = df(),
        lab = df()$Label,
        x = fold_change_column(),
        y = significance_column(),
        title = NULL,
        subtitle = NULL,
        caption = NULL,
        pCutoff = significance_threshold(),
        FCcutoff = fold_change_threshold(),
        labSize = input$label_size,
        cutoffLineCol = input$fc_line_color,
        hline = if (input$include_sig_line) significance_threshold() else NULL,
        vline = c(
          if (input$include_fc_line) fold_change_threshold() else NULL,
          if (input$include_fc_line) -fold_change_threshold() else NULL
        ),
        vlineCol = input$fc_line_color
        # col = colors(),
        # colAlpha = alphas()
      )
    })
    
    ma_plot = reactive({
      
      if (expression_column() != '' & fold_change_column() != '') {
        return(
          ggplot(df(), aes(x=!!sym(expression_column()), y=!!sym(fold_change_column()), color=Direction, size=Direction, alpha=Direction, label=Label)) +
            theme_classic(base_size = 16) +
            theme(legend.position = 'none') +
            scale_x_continuous(trans='log10', name="Expression") +
            scale_y_continuous(name="Fold Change (log2)") +
            scale_color_manual(values=colors()) +
            scale_alpha_manual(values=alphas()) +
            scale_size_manual(values=sizes()) +
            geom_point() +
            {if (input$include_fc_line) geom_hline(yintercept = fold_change_threshold(), linetype=2, color=input$fc_line_color)} +
            {if (input$include_zero_line) geom_hline(yintercept = 0, linetype=2, color=input$zero_line_color)} +
            {if (input$include_fc_line) geom_hline(yintercept = -fold_change_threshold(), linetype=2, color=input$fc_line_color)} +
            {if (input$label_strategy != 'None') geom_label_repel(label.size=NA, size=input$label_size, fill=NA, na.rm=TRUE, max.overlaps = 50, max.time = 5)}
        )
      } else {
        return(NULL)
      }
    })
    
    observeEvent(input$include_zero_line, ignoreNULL=TRUE, {
      if (input$include_zero_line) {
        showElement("zero_line_color")
      } else {
        hideElement("zero_line_color")
      }
    })
    
    observeEvent(input$include_fc_line, ignoreNULL=TRUE, {
      if (input$include_fc_line) {
        showElement("fc_line_color")
      } else {
        hideElement("fc_line_color")
      }
    })
    
    observeEvent(input$include_sig_line, ignoreNULL=TRUE, {
      if (input$include_sig_line) {
        showElement("sig_line_color")
      } else {
        hideElement("sig_line_color")
      }
    })
    
    observeEvent(input$label_strategy, ignoreNULL=TRUE, {
      if (input$label_strategy == 'None') {
        hideElement("label_size")
      } else {
        showElement("label_size")
      }
    })
    
    return(
      list(
        volcano = volcano_plot,
        ma = ma_plot
      )
    )
    
  })
}

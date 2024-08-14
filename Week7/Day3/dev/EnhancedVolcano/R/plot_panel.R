source('./R/Enhancedvolcano.R')


reverselog = function() {
  trans_new('reverselog', function(x) -log10(x), function(x) 10^(-x), log_breaks(base = 10), domain = c(1e-1000, Inf))
}

plotUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    accordion(open=FALSE,
              accordion_panel('Label Options',
                              selectizeInput(ns('label_strategy'), 'Event Labels:', choices = c('None', 'Significant/Highlighted', 'Highlighted Only'), selected='None'),
                              selectizeInput(ns('highlighted'), 'Highlighted Events:', choices=NULL, multiple=TRUE, options = list(create = TRUE, delimiter = ',')), # Add Copy&Paste option
                              numericInput(ns('label_size'), 'Label Size:', value=4), 
                              selectizeInput(ns('legend_position'), 'Legend Position', choices = c('top', 'bottom', 'right', 'left', 'invisible')),
                              checkboxInput(ns('upregulated_check'), 'Upregulated Count', value = FALSE), 
                              checkboxInput(ns('downregulated_check'), 'Downregulated Count', value = FALSE),
                              checkboxInput(ns('noChange_check'), 'No Change Count', value = FALSE)
                              
              ),
              accordion_panel('Point Options',
                              accordion(open=FALSE,
                                        accordion_panel('Color',
                                                        h5('Standard:'),
                                                        colourInput(ns('downregulated_color'), 'Downregulated', value='#4682B4'),
                                                        colourInput(ns('noChange_color'), 'No Change:', value='#e7e7e7'),
                                                        colourInput(ns('upregulated_color'), 'Upregulated:', value='#b22222'),
                                                        h5('Highlighted:'),
                                                        colourInput(ns('highlight_downregulated_color'), 'Downregulated', value='#1e6234'),
                                                        colourInput(ns('highlight_noChange_color'), 'No Change:', value='#000000'),
                                                        colourInput(ns('highlight_upregulated_color'), 'Upregulated:', value='#1e6234')
                                        ),
                                        accordion_panel('Size',
                                                        h5('Standard:'),
                                                        numericInput(ns('downregulated_size'), 'Downregulated', value=2),
                                                        numericInput(ns('noChange_size'), 'No Change', value=1),
                                                        numericInput(ns('upregulated_size'), 'Upregulated', value=2),
                                                        h5('Highlighted:'),
                                                        numericInput(ns('highlight_downregulated_size'), 'Downregulated', value=2),
                                                        numericInput(ns('highlight_noChange_size'), 'No Change', value=2),
                                                        numericInput(ns('highlight_upregulated_size'), 'Upregulated', value=2)
                                        ),
                                        accordion_panel('Transparency',
                                                        h5('Standard:'),
                                                        numericInput(ns('downregulated_alpha'), 'Downregulated', value=1),
                                                        numericInput(ns('noChange_alpha'), 'No Change', value=1),
                                                        numericInput(ns('upregulated_alpha'), 'Upregulated', value=1),
                                                        h5('Highlighted:'),
                                                        numericInput(ns('highlight_downregulated_alpha'), 'Downregulated', value=1),
                                                        numericInput(ns('highlight_noChange_alpha'), 'No Change', value=1),
                                                        numericInput(ns('highlight_upregulated_alpha'), 'Upregulated', value=1)
                                        ),
                              )
              ),
              accordion_panel('Axis Options',
                              checkboxInput(ns('include_zero_line'), 'Zero Line', value=FALSE),
                              colourInput(ns('zero_line_color'), 'Color', value='#000000'),
                              checkboxInput(ns('include_fc_line'), 'Fold Change Cutoff Line', value=FALSE),
                              colourInput(ns('fc_line_color'), 'Color', value='#e7e7e7'),
                              checkboxInput(ns('include_sig_line'), 'Significance Cutoff Line', value=FALSE),
                              colourInput(ns('sig_line_color'), 'Color', value='#e7e7e7')
              )
    )
  )
}

plotServer <- function(id, data, event_column, fold_change_column, significance_column, expression_column, fold_change_threshold, significance_threshold) {
  
  moduleServer(id, function(input, output, session) {
    
    observeEvent(data(), {
      updateSelectizeInput(session, 'highlighted', 'Highlighted Events:', choices=df()[,event_column()], server=TRUE)
    })
    
    colors = reactive({
      color_mapping <- c(
        'Upregulated' = input$upregulated_color,
        'No Change' = input$noChange_color,
        'Downregulated' = input$downregulated_color,
        'Highlighted Upregulated' = input$highlight_upregulated_color,
        'Highlighted' = input$highlight_noChange_color,
        'Highlighted Downregulated' = input$highlight_downregulated_color
      )
      df_temp <- df()
      df_temp$Direction <- factor(df_temp$Direction, levels = names(color_mapping))
      
      # Apply the mapping and ensure that it is consistent
      color_values <- color_mapping[as.character(df_temp$Direction)]
      
      # Replace NA or missing color values with a default color
      color_values[is.na(color_values)] <- 1 # Default to color = 1 for any NA values	  
      return(color_values)
    })
    
    alphas <- reactive({
      alpha_mapping <- c(
        'Upregulated' = input$upregulated_alpha,
        'No Change' = input$noChange_alpha,
        'Downregulated' = input$downregulated_alpha,
        'Highlighted Upregulated' = input$highlight_upregulated_alpha,
        'Highlighted' = input$highlight_noChange_alpha,
        'Highlighted Downregulated' = input$highlight_downregulated_alpha
      )
      
      df_temp <- df()
      df_temp$Direction <- factor(df_temp$Direction, levels = names(alpha_mapping))
      
      alpha_values <- alpha_mapping[as.character(df_temp$Direction)]
      alpha_values[is.na(alpha_values)] <- 1  # Default to alpha = 1 for any NA values
      return(alpha_values)
    })
    
    sizes <- reactive({
      size_mapping <- c(
        'Upregulated' = input$upregulated_size,
        'No Change' = input$noChange_size,
        'Downregulated' = input$downregulated_size,
        'Highlighted Upregulated' = input$highlight_upregulated_size,
        'Highlighted' = input$highlight_noChange_size,
        'Highlighted Downregulated' = input$highlight_downregulated_size
      )
      
      df_temp <- df()
      df_temp$Direction <- factor(df_temp$Direction, levels = names(size_mapping))
      
      size_values <- size_mapping[as.character(df_temp$Direction)]
      size_values[is.na(size_values)] <- 1  # Default to size = 1 for any NA values
      
      return(size_values)
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
    
    volcano_plot <- reactive({
      
      upregulated_count = nrow(df() %>% filter(Direction == 'Upregulated'))
      noChange_count = nrow(df() %>% filter(Direction == 'No Change'))
      downregulated_count = nrow(df() %>% filter(Direction == 'Downregulated'))
      
      EnhancedVolcano(
        toptable = df(),
        lab = df()$Label,
        x = fold_change_column(),
        y = significance_column(),
        title = NULL,
        subtitle = NULL,
        caption = NULL,
        legendPosition = if (input$legend_position == 'invisible') 'none' else input$legend_position,
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
        drawConnectors = if (input$label_strategy != 'None') TRUE else FALSE,
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
    
    ma_plot = reactive({
      
      if (expression_column() != '' & fold_change_column() != '') {
        return(
          ggplot(df(), aes(x=!!sym(expression_column()), y=!!sym(fold_change_column()), color=Direction, size=Direction, alpha=Direction, label=Label)) +
            theme_classic(base_size = 16) +
            theme(legend.position = 'none') +
            scale_x_continuous(trans='log10', name='Expression') +
            scale_y_continuous(name='Fold Change (log2)') +
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
        showElement('zero_line_color')
      } else {
        hideElement('zero_line_color')
      }
    })
    
    observeEvent(input$include_fc_line, ignoreNULL=TRUE, {
      if (input$include_fc_line) {
        showElement('fc_line_color')
      } else {
        hideElement('fc_line_color')
      }
    })
    
    observeEvent(input$include_sig_line, ignoreNULL=TRUE, {
      if (input$include_sig_line) {
        showElement('sig_line_color')
      } else {
        hideElement('sig_line_color')
      }
    })
    
    observeEvent(input$label_strategy, ignoreNULL=TRUE, {
      if (input$label_strategy == 'None') {
        hideElement('label_size')
      } else {
        showElement('label_size')
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

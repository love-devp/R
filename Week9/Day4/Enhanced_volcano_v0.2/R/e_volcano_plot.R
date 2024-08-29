inputDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    selectInput(ns("input_source"), "Input Source:", choices=c("Automatic", "Manual Upload"), selected="Automatic"),
    selectizeInput(ns('automatic_data'), "Input Data:", choices=NULL),
    fileInput(ns("input_file"), label='Input Data:'),
    shinyjs::hidden(selectizeInput(ns("event_column"), label='Event Column:', choices=NULL)),
    shinyjs::hidden(selectizeInput(ns("fold_change_column"), label='Fold Change Column:', choices=NULL)),
    shinyjs::hidden(selectizeInput(ns("significance_column"), label='P-Value Column:', choices=NULL)),
    shinyjs::hidden(selectizeInput(ns("expression_column"), label='Expression Column:', choices=NULL))
  )
}

inputDataServer <- function(id, script_path, org) {
  
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input_files(), once=TRUE, {
      if (length(input_files()) == 0) {
        shinyjs::hide('input_source')
        shinyjs::hide('automatic_data')
        shinyjs::show('manual_data')
        updateSelectizeInput(session, 'input_source', "Input Source:", choices=c("Automatic", "Manual Upload"), selected="Manual Upload", server=TRUE)
      }
    })
    
    observeEvent(input$input_source, {
      if (input$input_source == 'Automatic') {
        hideElement('input_file')
        showElement('automatic_data')
      } else {
        hideElement('automatic_data')
        showElement('input_file')
      }
    })
    
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
    
    input_files = reactive({
      all_files = c(list.files(path=script_path(), pattern='*deseq2_results.tsv', recursive=TRUE))
      candidates = character()
      for (f in all_files) {
        if (substr(f, 1, 3) != 'run' & str_detect(f, 'outputs')) {
          candidates = append(candidates, f)
        }
      }
      df = data.frame(Path=candidates) %>%
        mutate(Base = basename(Path))
      return(setNames(as.list(as.character(df$Path)), nm = df$Base))
    })
    
    observeEvent(input_files(), {
      updateSelectizeInput(session, 'automatic_data', "Input Data:", choices=input_files(), server=TRUE)
    })
    
    raw_data = reactive({
      if (input$input_source == 'Manual Upload') {
        req(input$input_file$datapath)
        df = read.delim(input$input_file$datapath, header=TRUE, sep = '\t')
        if (length(df) > 1) {
          return(df)
        } else {
          return(read.delim(input$input_file$datapath, header=TRUE, sep = ','))
        }
      } else {
        req(input$automatic_data)
        df = read.delim(paste0(script_path(), '/', input$automatic_data), sep='\t')
        if (length(df) > 1) {
          return(df)
        } else {
          return(read.delim(paste0(script_path(), '/', input$automatic_data), sep=','))
        }
      }
    })
    
    column_names = reactive({
      if (input$input_source == 'Manual Upload') {
        req(input$input_file$datapath)
      } else {
        req(input$automatic_data)
      }
      return(colnames(raw_data()))
    })
    
    column_classes = reactive({ 
      if (input$input_source == 'Manual Upload') {
        req(input$input_file$datapath)
      } else {
        req(input$automatic_data)
      }
      return(sapply(raw_data(), class))
    })
    
    numeric_columns = reactive({ 
      if (input$input_source == 'Manual Upload') {
        req(input$input_file$datapath)
      } else {
        req(input$automatic_data)
      }
      return((data.frame(Column=column_names(), Class=column_classes()) %>% filter(Class == 'numeric' | Class == 'integer'))$Column)
    })
    
    observeEvent(column_names(), {
      
      if ('gene' %in% column_names()) {
        default_event = 'gene'
      }  else {
        default_event = column_names()[1]
      }
      updateSelectizeInput(session, 'event_column', "Event Column:", choices=c("", column_names()), selected=default_event, server=TRUE)
    })
    
    observeEvent(numeric_columns(), {
      
      if ('log2FoldChange_shrink' %in% numeric_columns()) {
        default_fc = 'log2FoldChange_shrink'
      } else if ('log2FoldChange' %in% numeric_columns()) {
        default_fc = 'log2FoldChange'
      } else {
        default_fc = ''
      }
      
      if ('padj' %in% numeric_columns()) {
        default_sig = 'padj'
      } else if ('pvalue' %in% numeric_columns()) {
        default_sig = 'pvalue'
      } else {
        default_sig = ''
      }
      
      if ('baseMean' %in% numeric_columns()) {
        default_exp = 'baseMean'
      }  else {
        default_exp = ''
      }
      updateSelectizeInput(session, 'fold_change_column', "Fold Change Column:", choices=c("", numeric_columns()), selected=default_fc, server=TRUE)
      updateSelectizeInput(session, 'significance_column', "P-Value Column:", choices=c("", numeric_columns()), selected=default_sig, server=TRUE)
      updateSelectizeInput(session, 'expression_column', "Expression Column:", choices=c("", numeric_columns()), selected=default_exp, server=TRUE)
    })
    
    return(
      list(
        raw = raw_data,
        event_column = reactive(input$event_column),
        fold_change_column = reactive(input$fold_change_column),
        significance_column = reactive(input$significance_column),
        expression_column = reactive(input$expression_column)
      )
    )
    
  })
}
inputPageUI <- function(id) {
  ns <- NS(id)
  tagList(
    layout_sidebar(
      sidebar= sidebar(
        accordion(open='Data',
                  accordion_panel("Species", icon=icon('dog'),
                                  inputSpeciesUI(ns('input_species'))
                  ),
                  accordion_panel("Data", icon=icon('file'),
                                  inputDataUI(ns('input_data'))
                  )
        )
      ),
      DTOutput(ns("raw_data_output")) %>% withSpinner(image='spinner.gif')
    )
  )
}

inputPageServer <- function(id, script_path) {
  
  moduleServer(id, function(input, output, session) {
    
    species = inputSpeciesServer('input_species')
    data = inputDataServer('input_data', script_path, species)
    
    output$raw_data_output = renderDT({
      datatable(data$raw())
    })
    
    return(
      list(
        species = species,
        data = data
      )
    )
  })
}

inputSpeciesUI <- function(id) {
  ns <- NS(id)
  selectizeInput(ns('species'), "Select Species:", 
                 choices=c("Human"="org.Hs.eg.db",
                           "Mouse"='org.Mm.eg.db',
                           "Rat"='org.Rn.eg.db',
                           "Dog"='org.Cf.eg.db',
                           "Fly"='org.Dm.eg.db',
                           "Zebrafish"='org.Dr.eg.db',
                           "C. elegans"='org.Ce.eg.db',
                           "S. cerevisiae"='org.Sc.eg.db',
                           "E. coli"='org.EcK12.eg.db'
                 ),
                 selected='Human'
  )
}

inputSpeciesServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    org = reactive({
      if (input$species != 'org.Hs.eg.db' & input$species != 'org.Mm.eg.db') {
        withProgress(message = 'Download annotations', detail = input$species, {
          BiocManager::install(input$species, update=FALSE)
        })
      }
      return(input$species)
    })
    
    return(org)
    
  })
}

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
                              # Drop down menu for Highlighted data,enabled Copy&Paste option, separated by comma
                              selectizeInput(ns('highlighted'), 'Highlighted Events:', choices=NULL, multiple=TRUE, options = list(create = TRUE, delimiter = ',')),
                              numericInput(ns('label_size'), 'Label Size:', value=4), 
                              # Drop down menu for legend position, default value is None
                              selectizeInput(ns('legend_position'), 'Legend Position', choices = c('top', 'bottom', 'right', 'left', 'none'), selected = 'none'),
                              # 3 Check boxes for Data counting 
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
                              checkboxInput(ns('include_zero_line'), 'Zero Line', value=TRUE),
                              colourInput(ns('zero_line_color'), 'Color', value='#000000'),
                              checkboxInput(ns('include_fc_line'), 'Fold Change Cutoff Line', value=TRUE),
                              colourInput(ns('fc_line_color'), 'Color', value='#e7e7e7'),
                              checkboxInput(ns('include_sig_line'), 'Significance Cutoff Line', value=TRUE),
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
      
      # Render Enhanced Volcano Plot
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
      
      upregulated_count = nrow(df() %>% filter(Direction == 'Upregulated'))
      noChange_count = nrow(df() %>% filter(Direction == 'No Change'))
      downregulated_count = nrow(df() %>% filter(Direction == 'Downregulated'))
      
      if (expression_column() != '' & fold_change_column() != '') {
        return(
          ggplot(df(), aes(x=!!sym(expression_column()), y=!!sym(fold_change_column()), color=Direction, size=Direction, alpha=Direction, label=Label)) +
            theme_classic(base_size = 16) +
            theme(legend.title=element_blank()) +
            theme(legend.text=element_text(size = 15)) +
            theme(legend.position = input$legend_position) +
            scale_x_continuous(trans='log10', name='Expression') +
            scale_y_continuous(name='Fold Change (log2)') +
            scale_color_manual(values=colors()) +
            scale_alpha_manual(values=alphas()) +
            scale_size_manual(values=sizes()) +
            geom_point() +
            {if (input$include_fc_line) geom_hline(yintercept = fold_change_threshold(), linetype=2, color=input$fc_line_color)} +
            {if (input$include_zero_line) geom_hline(yintercept = 0, linetype=2, color=input$zero_line_color)} +
            {if (input$include_fc_line) geom_hline(yintercept = -fold_change_threshold(), linetype=2, color=input$fc_line_color)} +
            {if (input$label_strategy != 'None') geom_label_repel(label.size=NA, size=input$label_size, fill=NA, na.rm=TRUE, max.overlaps = 50, max.time = 5)} +
            {if (input$upregulated_check) annotate('text', x=Inf, y=Inf, hjust=1, vjust=1, color=input$upregulated_color, label=paste0('Upregulated: ', upregulated_count))} +
            {if (input$downregulated_check) annotate('text', x=0, y=Inf, hjust=-.01, vjust=1, color=input$downregulated_color, label=paste0('Downregulated: ', downregulated_count))} +
            {if (input$noChange_check) annotate('text', x=1000, y=Inf, vjust=1, color=input$noChange_color, label=paste0('No Change: ', noChange_count))}
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


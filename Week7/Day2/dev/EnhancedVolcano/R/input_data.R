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
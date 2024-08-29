# # e_volcano_plot.R

# # UI for the Volcano Plot and Input Data
# evolPlotUI <- function(id) {
#   ns <- NS(id)
#   tagList(
#     layout_sidebar(
#       sidebar= sidebar(
#         accordion(open='Data',
#                   accordion_panel("Data Input",
#                                   fileInput(ns("input_file"), label='')
#                   )
#         )
#       )
#     ),
#     useShinyjs(),
#     accordion(open=FALSE,
#               accordion_panel("Label Options",
#                               selectizeInput(ns("label_strategy"), "Event Labels:", choices = c("None", "Significant/Highlighted", "Highlighted Only"), selected='None'),
#                               selectizeInput(ns("highlighted"), "Highlighted Events:", choices=NULL, multiple=TRUE),
#                               numericInput(ns('label_size'), "Label Size:", value=4)
#               ),
#               accordion_panel("Point Options",
#                               accordion(open=FALSE,
#                                         accordion_panel("Color",
#                                                         h5("Standard:"),
#                                                         textInput(ns('downregulated_color'), "Downregulated", value='#4682B4'),
#                                                         textInput(ns('noChange_color'), "No Change:", value='#e7e7e7'),
#                                                         textInput(ns('upregulated_color'), "Upregulated:", value='#b22222'),
#                                                         h5("Highlighted:"),
#                                                         textInput(ns('highlight_downregulated_color'), "Downregulated", value='#1e6234'),
#                                                         textInput(ns('highlight_noChange_color'), "No Change:", value='#000000'),
#                                                         textInput(ns('highlight_upregulated_color'), "Upregulated:", value='#1e6234')
#                                         ),
#                                         accordion_panel("Size",
#                                                         h5("Standard:"),
#                                                         numericInput(ns('downregulated_size'), "Downregulated", value=2),
#                                                         numericInput(ns('noChange_size'), "No Change", value=1),
#                                                         numericInput(ns('upregulated_size'), "Upregulated", value=2),
#                                                         h5("Highlighted:"),
#                                                         numericInput(ns('highlight_downregulated_size'), "Downregulated", value=2),
#                                                         numericInput(ns('highlight_noChange_size'), "No Change", value=2),
#                                                         numericInput(ns('highlight_upregulated_size'), "Upregulated", value=2)
#                                         ),
#                                         accordion_panel("Transparency",
#                                                         h5("Standard:"),
#                                                         numericInput(ns('downregulated_alpha'), "Downregulated", value=1),
#                                                         numericInput(ns('noChange_alpha'), "No Change", value=1),
#                                                         numericInput(ns('upregulated_alpha'), "Upregulated", value=1),
#                                                         h5("Highlighted:"),
#                                                         numericInput(ns('highlight_downregulated_alpha'), "Downregulated", value=1),
#                                                         numericInput(ns('highlight_noChange_alpha'), "No Change", value=1),
#                                                         numericInput(ns('highlight_upregulated_alpha'), "Upregulated", value=1)
#                                         )
#                               )
#               ),
#               accordion_panel("Axis Options",
#                               checkboxInput(ns('include_zero_line'), "Zero Line", value=TRUE),
#                               textInput(ns('zero_line_color'), "Color", value='#000000'),
#                               checkboxInput(ns('include_fc_line'), "Fold Change Cutoff Line", value=TRUE),
#                               textInput(ns('fc_line_color'), "Color", value='#e7e7e7'),
#                               checkboxInput(ns('include_sig_line'), "Significance Cutoff Line", value=TRUE),
#                               textInput(ns('sig_line_color'), "Color", value='#e7e7e7')
#               )
#     ),
#     card_body(
#       plotOutput(ns('e_volcano_plot')) %>% withSpinner(image='spinner.gif', id=ns('spinner'))
#     )
#   )
# }

# # Server for the Volcano Plot and Input Data
# # Server for the Volcano Plot and Input Data
# evolPlotServer <- function(id, script_path, org) {
#   moduleServer(id, function(input, output, session) {
    
#     observeEvent(input_files(), {
#       updateSelectizeInput(session, 'automatic_data', "Input Data:", choices=input_files(), server=TRUE)
#     })
    
#     raw_data = reactive({
#       if (input$input_source == 'Manual Upload') {
#         req(input$input_file$datapath)
#         df = read.delim(input$input_file$datapath, header=TRUE, sep = '\t')
#         if (length(df) > 1) {
#           return(df)
#         } else {
#           return(read.delim(input$input_file$datapath, header=TRUE, sep = ','))
#         }
#       } else {
#         req(input$automatic_data)
#         df = read.delim(paste0(script_path(), '/', input$automatic_data), sep='\t')
#         if (length(df) > 1) {
#           return(df)
#         } else {
#           return(read.delim(paste0(script_path(), '/', input$automatic_data), sep=','))
#         }
#       }
#     })
    
#     column_names = reactive({
#       return(colnames(raw_data()))
#     })
    
#     numeric_columns = reactive({
#       return((data.frame(Column=column_names(), Class=sapply(raw_data(), class)) %>% 
#                 filter(Class == 'numeric' | Class == 'integer'))$Column)
#     })
    
#     observeEvent(column_names(), {
#       default_event = ifelse('gene' %in% column_names(), 'gene', column_names()[1])
#       updateSelectizeInput(session, 'event_column', "Event Column:", choices=c("", column_names()), selected=default_event, server=TRUE)
#     })
    
#     observeEvent(numeric_columns(), {
#       default_fc = ifelse('log2FoldChange_shrink' %in% numeric_columns(), 'log2FoldChange_shrink', 'log2FoldChange')
#       default_sig = ifelse('padj' %in% numeric_columns(), 'padj', 'pvalue')
#       default_exp = ifelse('baseMean' %in% numeric_columns(), 'baseMean', '')
      
#       updateSelectizeInput(session, 'fold_change_column', "Fold Change Column:", choices=c("", numeric_columns()), selected=default_fc, server=TRUE)
#       updateSelectizeInput(session, 'significance_column', "P-Value Column:", choices=c("", numeric_columns()), selected=default_sig, server=TRUE)
#       updateSelectizeInput(session, 'expression_column', "Expression Column:", choices=c("", numeric_columns()), selected=default_exp, server=TRUE)
#     })
    
#     # Data manipulation for Volcano Plot
#     df = reactive({
#       raw_data() %>%
#         mutate(Highlighted = !!sym(input$event_column) %in% input$highlighted) %>%
#         mutate(Direction = case_when(
#           Highlighted == TRUE & Direction == 'Downregulated' ~ "Highlighted Downregulated",
#           Highlighted == TRUE & Direction == "No Change" ~ "Highlighted",
#           Highlighted == TRUE & Direction == 'Upregulated' ~ "Highlighted Upregulated",
#           TRUE ~ Direction
#         )) %>%
#         mutate(Label = case_when(
#           Highlighted == TRUE ~ !!sym(input$event_column),
#           input$label_strategy == 'Significant/Highlighted' & Significant == 'Significant' ~ !!sym(input$event_column)
#         ))
#     })
    
#     output$e_volcano_plot <- renderPlot({
#       EnhancedVolcano(
#         toptable = df(),
#         lab = df()$Label,
#         x = input$fold_change_column,
#         y = input$significance_column,
#         title = NULL,
#         subtitle = NULL,
#         caption = NULL,
#         legendPosition = input$legend_position,
#         pCutoff = 0.05,
#         pCutoffCol = input$significance_column,
#         FCcutoff = 1,
#         labSize = input$label_size,
#         cutoffLineType = 'blank',
#         cutoffLineCol = input$fc_line_color,
#         hline = NULL,
#         hlineCol = input$sig_line_color,
#         vline = NULL,
#         vlineCol = input$fc_line_color,
#         drawConnectors = if (input$label_strategy != 'None') TRUE else FALSE,
#         widthConnectors = 0.10,
#         boxedLabels = TRUE,
#         colCustom = c('Upregulated'=input$upregulated_color, 'No Change'=input$noChange_color, 'Downregulated'=input$downregulated_color),
#         colAlpha = c('Upregulated'=input$upregulated_alpha, 'No Change'=input$noChange_alpha, 'Downregulated'=input$downregulated_alpha),
#         pointSize = c('Upregulated'=input$upregulated_size, 'No Change'=input$noChange_size, 'Downregulated'=input$downregulated_size)
#       )
#     })
#   })
# }

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
                                                            textInput(ns('highlight_upregulated_color'), "Upregulated:", value = '#1e6234')
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
      )
      card_body(
        plotOutput(ns('e_volcano_plot')) %>% withSpinner(image = 'spinner.gif', id = ns('spinner'))
        )
        )
        )
        
}
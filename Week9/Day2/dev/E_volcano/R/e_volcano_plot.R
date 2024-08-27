evolPlotUI <- function(id) {
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
      )
    ),
    card_body(
      plotOutput(ns('e_volcano_plot')) %>% withSpinner(image='spinner.gif', id=ns('spinner'))
      )
  )}

evolPlotServer <- function(id, data, script_path, event_column, fold_change_column, significance_column, significance_threshold, fold_change_threshold) {
  moduleServer(id, function(input, output, session) {
    
    species = inputSpeciesServer('input_species')
    data = inputDataServer('input_data', script_path, species)

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
        pCutoff = significance_threshold(),
        pCutoffCol = significance_column(),
        FCcutoff = fold_change_threshold(),
        labSize = input$label_size,
        colCustom = colors(),
        colAlpha = alphas(),
        pointSize = sizes(),
        drawConnectors = if (input$label_strategy != 'None') TRUE else FALSE,
        widthConnectors = 0.10,
        boxedLabels = TRUE
      )
    })
  })
}


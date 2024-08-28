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
      ),
      card_body(
        plotOutput(ns('e_volcano_plot')) %>% withSpinner(image='spinner.gif', id=ns('spinner'))
      )
    )
  )
}

evolPlotServer <- function(id, script_path, significance_threshold, fold_change_threshold) {
  moduleServer(id, function(input, output, session) {
    
    # Input Page and Data Servers
    species <- inputSpeciesServer('input_species')
    data <- inputDataServer('input_data', script_path, species)
    
    # Data Table Output
    output$raw_data_output <- renderDT({
      datatable(data$raw())
    })
    
    # Volcano Plot Data Preparation
    df <- reactive({
      data$raw() %>%
        mutate(Highlighted = !!sym(data$event_column()) %in% input$highlighted) %>%
        mutate(Direction = case_when(Highlighted == TRUE & Direction == 'Downregulated' ~ 'Highlighted Downregulated',
                                     Highlighted == TRUE & Direction == 'No Change' ~ 'Highlighted',
                                     Highlighted == TRUE & Direction == 'Upregulated' ~ 'Highlighted Upregulated',
                                     TRUE ~ Direction
        )) %>%
        mutate(Label = case_when(Highlighted == TRUE ~ !!sym(data$event_column()),
                                 input$label_strategy == 'Significant/Highlighted' & Significant == 'Significant' ~ !!sym(data$event_column())))
    })
    
    # Volcano Plot Output
    output$e_volcano_plot <- renderPlot({
      req(df())
      
      EnhancedVolcano(
        toptable = df(),
        lab = df()$Label,
        x = data$fold_change_column(),
        y = data$significance_column(),
        title = NULL,
        subtitle = NULL,
        caption = NULL,
        legendPosition = input$legend_position,
        pCutoff = significance_threshold(),
        pCutoffCol = data$significance_column(),
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
    
    return(
      list(
        species = species,
        data = data
      )
    )
  })
}

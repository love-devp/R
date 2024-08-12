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
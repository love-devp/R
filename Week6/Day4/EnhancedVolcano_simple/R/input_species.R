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
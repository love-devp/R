metadataColumnTypeSelectionUI <- function(id) {
  ns <- NS(id)
  tagList(
    rHandsontableOutput(ns("metadata_column_type"), height = "250px") %>% withSpinner(image='spinner.gif')
  )
}

metadataColumnTypeServer <- function(id, metadata) {
  
  moduleServer(id, function(input, output, session) {
    
    column_type_hot = reactive({
      columns = colnames(metadata())
      types = sapply(metadata(), typeof)
      df = data.frame(Column=columns, Raw_Type=types) %>%
           mutate(Numeric = case_when(Raw_Type == 'character' ~ FALSE,
                                   Raw_Type == 'logical' ~ FALSE,
                                   Raw_Type == 'complex' ~ FALSE,
                                   Raw_Type == 'numeric' ~ TRUE,
                                   Raw_Type == 'integer' ~ TRUE
           )) %>%
           select(-Raw_Type)
      
      return(rhandsontable(df, rowHeaders = NULL) %>%
              hot_col("Column", readOnly = TRUE)) #%>%
              #hot_col(col = "Type", type = "dropdown", source = c("Categorical", "Numerical")))
    })
    
    output$metadata_column_type = renderRHandsontable({
      column_type_hot()
    })
    
    column_type = reactive({
    	if (is.null(input$metadata_column_type)) {
    		return(NULL)
    	} else {
      	df = hot_to_r(input$metadata_column_type) %>%
      	     mutate(Numeric = case_when(Numeric == FALSE ~ 'character',
      	                             TRUE ~ 'numeric'))
      	return(df)
    	}
    })
    
    return(column_type)
  })
}
sampleSelectionUI <- function(id) {
	ns <- NS(id)
	tagList(
		numericInput(ns('min_counts_per_sample'), "Minimal Total Counts per Sample", value=1000, min=1)
	)
}

sampleSelectionServer <- function(id, input_data, metadata) {
	
	moduleServer(id, function(input, output, session) {
		
		sample_totals = reactive({
			req(input_data())
			data.frame(Total_Counts=colSums(input_data())) %>% tibble::rownames_to_column('sample_name')
		})
		
		sample_selection_hot = reactive({
			df = sample_totals() %>% 
					 left_join(metadata(), by='sample_name') %>%
					 mutate(include = Total_Counts >= input$min_counts_per_sample & sample_name %in% metadata()$sample_name) %>%
					 mutate(ID = row_number()) %>%
					 relocate(ID, include)
			return(rhandsontable(df, rowHeaders = NULL) %>% hot_col("ID", readOnly = TRUE) %>% hot_cols(columnSorting = TRUE))
		})
		
		return(sample_selection_hot)
	})
}
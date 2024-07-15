filterUI <- function(id) {
	ns <- NS(id)
	tagList(
		numericInput(ns('min_count'), "Minimal Counts", value=10, min=0),
		numericInput(ns('min_samples'), "Minimal Samples", value=1, min=0),
	)
}

filterServer <- function(id, raw_counts) {
	
	moduleServer(id, function(input, output, session) {
		
		bad_samples = reactive({
			req(raw_counts())
			df =(data.frame(Counts=colSums(raw_counts())) %>% tibble::rownames_to_column('Sample') %>% filter(Counts < input$min_counts_per_sample))$Sample
			print(df)
			return(df)
		})
		
		pass_filter = reactive({
			req(raw_counts())
			keep = rowSums(raw_counts() >= input$min_count) >= input$min_samples
			return(raw_counts()[keep,])
		})
		
		return(pass_filter)
		
	})
}
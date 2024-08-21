plotPageUI <- function(id) {
	ns <- NS(id)
	tagList(
		layout_sidebar(
			sidebar= sidebar(
				accordion(
					accordion_panel("Count Input", icon=icon("file"),
						countInputUI(ns('count_input_panel'))
					),
					accordion_panel("Metadata Input", icon=icon("book"),
						metadataInputUI(ns('metadata_input_panel')),		
					),
					accordion_panel("Sample Selection", icon=icon("check"),
						sampleSelectionUI(ns('sample_selection_panel')),				
					),
					accordion_panel("Batch Correction", icon=icon("border-all"),
						batchCorrectionUI(ns('batch_correction_panel')),					
					),
					accordion_panel("Filtering", icon=icon("filter"),
						filterUI(ns('filter_panel')),								
					),
					accordion_panel("PCA Settings", icon=icon("gear"),
						pcaUI(ns('pca_panel')),							
					)
				),
			),
			navset_card_pill(
				full_screen = TRUE,
				height = 400,
				title=NULL,
				nav_panel("PCA",
          textOutput(ns('sampleErrorText_pca')),
					pcaPlotUI(ns('pca_plot'))
				),
				nav_panel("Scree",
          textOutput(ns('sampleErrorText_scree')),
					screePlotUI(ns('scree_plot'))
				)
			),
			navset_card_pill(
			  full_screen= TRUE,
				height=250,
				title="Metadata",
				nav_panel("Sample Control",
				  fluidRow(
				    actionButton(ns('include_all'), 'Include All'),
				    actionButton(ns('exclude_all'), 'Exclude All')
				  ),
				  rHandsontableOutput(ns("final_sample_selection_hot"), height = "250px") %>% withSpinner(image='spinner.gif')
				),
				nav_panel("Column Options",
				  metadataColumnTypeSelectionUI(ns('metadata_column_type'))
			  )
			)
		)
	)
}

plotPageServer <- function(id, script_path) {
	
	moduleServer(id, function(input, output, session) {
		
		input_data = countInputServer('count_input_panel', script_path)
		metadata = metadataInputServer('metadata_input_panel', script_path)
		metadata_column_types = metadataColumnTypeServer('metadata_column_type', metadata)
		sample_selection_table = sampleSelectionServer('sample_selection_panel', input_data, metadata)
		
		tempVal = reactiveVal()

		observeEvent(sample_selection_table(), ignoreNULL = TRUE, {
		  df = sample_selection_table()
		  tempVal(df)
		})	
		
		observeEvent(input$include_all, {
		  df = hot_to_r(input$final_sample_selection_hot) %>% mutate(include=TRUE)
		  newTable = rhandsontable(df)
		  tempVal(newTable)
		})
		
		observeEvent(input$exclude_all, {
		  df = hot_to_r(input$final_sample_selection_hot) %>% mutate(include=FALSE)
		  newTable = rhandsontable(df)
		  tempVal(newTable)
		})
		
		output$final_sample_selection_hot = renderRHandsontable({
			req(sample_selection_table())
		  tempVal()
		})
		
		selected_samples = reactive({
			req(input$final_sample_selection_hot)
			selected = (hot_to_r(input$final_sample_selection_hot) %>% filter(include == TRUE))$sample_name
			if (length(selected) > 1) {
			  sampleErrorText("")
			  return(selected)
			} else {
			  sampleErrorText("More than one sample is required to run PCA")
			  return(NULL)
		  }
		})
		
		sampleErrorText = reactiveVal("")
		
		output$sampleErrorText_pca = renderText({
		  sampleErrorText()
		})
		
		output$sampleErrorText_scree = renderText({
		  sampleErrorText()
		})
		
		selected_data = reactive({
			req(input_data())
			df = as.data.frame(input_data()) %>% 
				tibble::rownames_to_column('sample_name') %>% 
				select(sample_name, all_of(selected_samples())) %>% 
				tibble::column_to_rownames('sample_name')
			df = as.matrix(df)
			mode(df) = 'integer'
			return(df)
		})
		
		metadata_hot = reactive({
			req(input$final_sample_selection_hot)
			hot_to_r(input$final_sample_selection_hot) %>% filter(include == TRUE) %>% select(-include)
		})
		
		metadata_hot_header = reactive({
			req(metadata())
			c('', 'ID', colnames(metadata()))
		})
		
		batch_corrected_counts = batchCorrectionServer('batch_correction_panel', selected_data, metadata_hot, metadata_hot_header)
		filtered_counts = filterServer('filter_panel', batch_corrected_counts)
		pca = pcaServer('pca_panel', filtered_counts)
		pcaPlotServer("pca_plot", metadata_hot, metadata_hot_header, metadata_column_types, pca$pca, pca$PoV)
		screePlotServer("scree_plot", metadata_hot, pca$PoV)
	})
}
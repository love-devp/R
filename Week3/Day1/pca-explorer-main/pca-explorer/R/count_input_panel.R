countInputUI <- function(id) {
	ns <- NS(id)
	tagList(
		useShinyjs(),
		selectInput(ns("input_source"), "Count Input Source:", choices=c("Automatic", "Manual Upload"), selected="Automatic"),
		selectizeInput(ns('automatic_data'), "Count Input:", choices=NULL),
		fileInput(ns("input_file"), label='Count Input:'),
		hidden(selectizeInput(ns("feature_column"), label='Feature Column:', choices=NULL))
	)
}

countInputServer <- function(id, script_path) {
	
	moduleServer(id, function(input, output, session) {
		
		observeEvent(input_files(), once=TRUE, {
			if (length(input_files()) == 0) {
				hideElement('input_source')
				hideElement('automatic_data')
				showElement('manual_data')
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
		
		observeEvent(input$input_file$datapath, {
				showElement("feature_column")
		})
		
		observeEvent(input$automatic_data, {
			showElement("feature_column")
		})
		
		eventReactive(input$automatic_data, {
			updateSelectizeInput(session, 'feature_column', "Feature Column:", choices=column_names(), server=TRUE)
		})
		
		input_files = reactive({
			all_files = list.files(path=script_path(), pattern='*.t*', recursive=TRUE)
			candidates = character()
			for (f in all_files) {
				if (substr(f, 1, 3) != 'run' & substr(f, 1, 15) != 'persistent_data' & str_detect(f, 'inputs')) {
					candidates = append(candidates, f)
				}
			}
			df = data.frame(Path=candidates) %>%
				mutate(Base = basename(Path))
			return(setNames(as.list(as.character(df$Path)), nm = df$Base))
		})
		
		observeEvent(input_files(), {
			if ('genes_expression_expected_count.tsv' %in% input_files()) {
				default_file = 'genes_expression_expected_count.tsv'
			} else {
				default_file = ''
				for (i in input_files()) {
					if (str_detect(tolower(i), 'count')) {
						default_file = i
					}
				}
			}
			
			updateSelectizeInput(session, 'automatic_data', "Count Input:", choices=input_files(), selected=default_file, server=TRUE)
		})
		
		raw_counts = reactive({
			if (input$input_source == 'Manual Upload') {
				req(input$input_file$datapath)
				df = read.delim(input$input_file$datapath, header=TRUE, sep = '\t', check.names = FALSE)
				if (length(df) > 1) {
					return(df)
				} else {
					return(read.delim(input$input_file$datapath, header=TRUE, sep = ',', check.names = FALSE))
				}
			} else {
				req(input$automatic_data)
				df = read.delim(paste0(script_path(), '/', input$automatic_data), sep='\t', check.names=FALSE)
				if (length(df) > 1) {
					return(df)
				} else {
					return(read.delim(paste0(script_path(), '/', input$automatic_data), sep=',', check.names=FALSE))
				}
			}
		})
		
		column_names = reactive({
			if (input$input_source == 'Manual Upload') {
				req(input$input_file$datapath)
			} else {
				req(input$automatic_data)
			}
			return(colnames(raw_counts()))
		})
		
		column_classes = reactive({
			if (input$input_source == 'Manual Upload') {
				req(input$input_file$datapath)
			} else {
				req(input$automatic_data)
			}
			return(sapply(raw_counts(), class))
		})
		
		numeric_columns = reactive({ 
			if (input$input_source == 'Manual Upload') {
				req(input$input_file$datapath)
			} else {
				req(input$automatic_data)
			}
			return((data.frame(Column=column_names(), Class=column_classes()) %>% filter(Class == 'numeric' | Class == 'integer') %>% filter(Column != 'length' & Column != 'transcript'))$Column)
		})
		
		observeEvent(column_names(), {
			updateSelectizeInput(session, 'feature_column', "Feature Column:", choices=column_names(), server=TRUE)
		})

		counts = reactive({
			req(input$feature_column)
			isolate(as.matrix(raw_counts() %>% select(!!sym(input$feature_column), numeric_columns()) %>% tibble::column_to_rownames(input$feature_column)))
		})
		
		return(counts)
		
	})
}
metadataInputUI <- function(id) {
	ns <- NS(id)
	tagList(
		useShinyjs(),
		selectInput(ns("input_source"), "Input Source:", choices=c("Automatic", "Manual Upload"), selected="Automatic"),
		selectizeInput(ns('automatic_data'), "Metadata Input:", choices=NULL),
		fileInput(ns("input_file"), label='Metadata Input:'),
	)
}

metadataInputServer <- function(id, script_path) {
	
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
		
		input_files = reactive({
			all_files = list.files(path=script_path(), pattern='.t*', recursive=TRUE)
			candidates = character()
			for (f in all_files) {
				if (substr(f, 1, 3) != 'run' & str_detect(f, 'inputs')) {
					candidates = append(candidates, f)
				}
			}
			df = data.frame(Path=candidates) %>%
				mutate(Base = basename(Path))
			return(setNames(as.list(as.character(df$Path)), nm = df$Base))
		})
		
		observeEvent(input_files(), {
			
			default_file = ''
			for (i in input_files()) {
				if (str_detect(tolower(i), 'group') | str_detect(tolower(i), 'meta')) {
					default_file = i
				}
			}
			updateSelectizeInput(session, 'automatic_data', "Metadata Input:", choices=input_files(), selected=default_file, server=TRUE)
		})
		
		metadata = reactive({
			
			if (input$input_source == 'Manual Upload') {
				req(input$input_file$datapath)
				df = read.delim(input$input_file$datapath, header=TRUE, sep = '\t')
				if (length(df) > 1) {
					return(df)
				} else {
					return(read.delim(input$input_file$datapath, header=TRUE, sep = ','))
				}
			} else {
				req(input$automatic_data)
				df = read.delim(paste0(script_path(), '/', input$automatic_data), sep='\t')
				if (length(df) > 1) {
					return(df)
				} else {
					return(read.delim(paste0(script_path(), '/', input$automatic_data), sep=','))
				}
			}
		})
		
		return(metadata)
	})
}
source('./R/EnhancedVolcano.R')

# reclass <- function (df, vec_types) {
#   for (i in 1:ncol(df)) {
#     type <- vec_types[i]
#     class(df[ , i]) <- type
#   }
#   return(df)
# }

pcaPlotUI <- function(id) {
	ns <- NS(id)
	tagList(
		layout_column_wrap(
			width = 1/2,
			card_body(min_height = 250,
				textOutput(ns('plot_error')),
				plotOutput(ns('pca_plot')) %>% withSpinner(image='spinner.gif', id=ns('spinner')),
				plotlyOutput(ns('pca_plot_interactive')) %>% withSpinner(image='spinner.gif', id=ns('interactive_spinner'))
			),
			card_body(
				checkboxInput(ns('interactive'), "Interactive Plot", value=FALSE),
				accordion(open=FALSE,
					h6('Plot Settings:'),
					accordion_panel('Label Options',
					                selectizeInput(ns('label_strategy'), 'Event Labels:', choices = c('None', 'Significant/Highlighted', 'Highlighted Only'), selected='None'),
					                selectizeInput(ns('highlighted'), 'Highlighted Events:', choices=NULL, multiple=TRUE, options = list(create = TRUE, delimiter = ',')),
					                numericInput(ns('label_size'), 'Label Size:', value=4), 
					                selectizeInput(ns('legend_position'), 'Legend Position', choices = c('top', 'bottom', 'right', 'left', 'none'), selected = 'none'),
					                checkboxInput(ns('upregulated_check'), 'Upregulated Count', value = FALSE), 
					                checkboxInput(ns('downregulated_check'), 'Downregulated Count', value = FALSE),
					                checkboxInput(ns('noChange_check'), 'No Change Count', value = FALSE)
					),
					accordion_panel('Point Options',
					                colourInput(ns('downregulated_color'), 'Downregulated', value='#4682B4'),
					                colourInput(ns('noChange_color'), 'No Change:', value='#e7e7e7'),
					                colourInput(ns('upregulated_color'), 'Upregulated:', value='#b22222'),
					                numericInput(ns('downregulated_size'), 'Downregulated', value=2),
					                numericInput(ns('noChange_size'), 'No Change', value=1),
					                numericInput(ns('upregulated_size'), 'Upregulated', value=2),
					                numericInput(ns('downregulated_alpha'), 'Downregulated', value=1),
					                numericInput(ns('noChange_alpha'), 'No Change', value=1),
					                numericInput(ns('upregulated_alpha'), 'Upregulated', value=1)
					),
					accordion_panel('Axis Options',
					                checkboxInput(ns('include_zero_line'), 'Zero Line', value=TRUE),
					                colourInput(ns('zero_line_color'), 'Color', value='#000000'),
					                checkboxInput(ns('include_fc_line'), 'Fold Change Cutoff Line', value=TRUE),
					                colourInput(ns('fc_line_color'), 'Color', value='#e7e7e7'),
					                checkboxInput(ns('include_sig_line'), 'Significance Cutoff Line', value=TRUE),
					                colourInput(ns('sig_line_color'), 'Color', value='#e7e7e7')
					)
					# accordion_panel("Grouping",
					# 	fluidRow(
					# 		column(6, selectizeInput(ns("color_by"), label='Color:', choices=NULL)),
					# 		column(6, selectizeInput(ns("shape_by"), label='Shape:', choices=NULL))
					# 	),
					# 	fluidRow(
					# 		column(6, selectizeInput(ns("outline_by"), label='Outline*:', choices=NULL)),
					# 		column(6, selectizeInput(ns("alpha_by"), label='Transparency:', choices=NULL))
					# 	),
					# 	p("*When setting outline - there can exist at most 5 shapes", style = "font-size:xx-small")
					# ),
					# accordion_panel("Point Options",
					# 	fluidRow(
					# 		column(3, tags$div(id = "point_inline", numericInput(ns("size"), label='Size:', value=6, min=1, step=1))),
					# 		column(3, tags$div(id = "point_inline", numericInput(ns("outline"), label='Outline:', value=1, min=1, step=1)))
					# 	)
					# ),
					# accordion_panel("Labels",
					# 	fluidRow(
					# 		column(6, selectizeInput(ns("label_by"), label='Label*:', choices=NULL)),
					# 		column(6,selectizeInput(ns("highlight"), label='Highlighted IDs:', choices=NULL, multiple=TRUE))
					# 	),
					# 	p('*Set to "Highlighted IDs" to label specific row with its ID', style = "font-size:xx-small"),
					# 	fluidRow(
					# 		column(6, tags$div(id = "inline", numericInput(ns("label_size"), label='Label Size:', value=7, min=1, step=1))),
					# 		column(6, tags$div(id = "inline", textInput(ns("label_color"), label='Label Color:', value='#000000')))
					# 	)
					# ),
					# accordion_panel("Axes",
					# 	fluidRow(
					# 		column(3, tags$div(id = "axes_inline", selectizeInput(ns("x"), label='X:', choices=NULL))),
					# 		column(3, tags$div(id = "axes_inline", selectizeInput(ns("y"), label='Y:', choices=NULL))),
					# 		column(5, tags$div(id = "axes_inline", numericInput(ns("axes_size"), label='Text Size:', value=12, min=1, step=1)))
					# 	)
					# ),
					# accordion_panel("Legend",
					# 	checkboxInput(ns('hide_legend'), label='Hide Legend', value=FALSE)
					# )
				),
				h6("Download:"),
				fluidRow(
					column(2, downloadButton(ns('data_download'), label='Data',  style='width: 80px; font-size: .7em; padding: 2px 0px 3px 0px; margin: 0px 5px 0px 5px')),
					column(2, downloadButton(ns('pca_plot_download'), label='Plot', style='width: 80px; font-size: .7em; padding: 2px 0px 3px 0px; margin: 0px 5px 0px 5px')),
					column(3, tags$div(id = "inline", numericInput(ns('download_height'), "Height: ", value=350))),
					column(3, tags$div(id = "inline", numericInput(ns('download_width'), "Width: ", value=500)))
				)
			)
		),
		tags$head(
			tags$style(
				HTML('
				.form-control.selectize-control{width: 150px}
				')
			)
		),
		tags$head(
			tags$style(type="text/css", 
				"#point_inline label{ display: table-cell; text-align: center; vertical-align: middle; padding-right: 3px} 
      	#point_inline .form-group{display: table-row;}
				#point_inline .form-control{padding: 3px 10px 3px 10px; width: 60px}
				#axes_inline label{ display: table-cell; text-align: center; vertical-align: middle; padding-right: 3px} 
				#axes_inline .form-group{display: table-row;}
				#axes_inline .form-control{padding: -1px 10px 3px 10px; width: 100px}
				")
		)
	)
}

pcaPlotServer <- function(id, metadata, metadata_header, metadata_column_types, pca, PoV) {
	
	moduleServer(id, function(input, output, session) {
		
		# observeEvent(PoV(), {
		# 	updateSelectizeInput(session, 'x', "X:", choices=PoV()$PC, selected='PC1', server=TRUE)
		# })
		# 
		# observeEvent(PoV(), {
		# 	updateSelectizeInput(session, 'y', "Y:", choices=PoV()$PC, selected='PC2', server=TRUE)
		# })
		# 
		# observeEvent(metadata_header(), {
		# 	updateSelectizeInput(session, 'color_by', "Color:", choices=metadata_header(), selected='group', server=TRUE)
		# })
		# 
		# observeEvent(metadata_header(), {
		# 	updateSelectizeInput(session, 'shape_by', "Shape:", choices=metadata_header(), selected="", server=TRUE)
		# })
		# 
		# observeEvent(metadata_header(), {
		# 	updateSelectizeInput(session, 'outline_by', "Outline*:", choices=metadata_header(), selected="", server=TRUE)
		# })
		# 
		# observeEvent(metadata_header(), {
		# 	updateSelectizeInput(session, 'alpha_by', "Transparency:", choices=metadata_header(), selected="", server=TRUE)
		# })
		# 
		# observeEvent(metadata_header(), {
		# 	updateSelectizeInput(session, 'label_by', "Label*:", choices=c(metadata_header(), "Highlighted IDs"), server=TRUE)
		# })
		# 
		# observeEvent(input$label_by, ignoreNULL = TRUE, {
		# 	if (input$label_by=='') {
		# 		hideElement("label_size")
		# 		hideElement("label_color")
		# 	} else {
		# 		showElement("label_size")
		# 		showElement("label_color")
		# 	}
		# 	
		# 	if (input$label_by=='Highlighted IDs') {
		# 		showElement("highlight")
		# 	} else {
		# 		hideElement("highlight")
		# 	}
		# 	
		# })
		# 
		# observeEvent(input$interactive, ignoreNULL = TRUE, {
		# 	if (input$interactive) {
		# 		hideElement("pca_plot")
		# 		hideElement("spinner")
		# 		showElement("pca_plot_interactive")
		# 		showElement("interactive_spinner")
		# 	} else {
		# 		showElement("pca_plot")
		# 		showElement("spinner")
		# 		hideElement("pca_plot_interactive")
		# 		hideElement("interactive_spinner")
		# 	}
		# })
		# 	
		# observeEvent(metadata(), {
		# 	updateSelectizeInput(session, 'highlight', "Highlighted IDs:", choices=metadata()$ID, server=TRUE)
		# })
		# 
		# data = reactive({
		#   df = metadata() %>% select(-ID, -Total_Counts) %>% reclass(metadata_column_types()$Numeric)
		#   reclassed_metadata = metadata() %>% select(ID, sample_name, Total_Counts) %>% left_join(df, by='sample_name')
		#   
		# 	reclassed_metadata %>%
		# 	left_join(as.data.frame(pca()$x) %>% tibble::rownames_to_column('sample_name'), by='sample_name') %>%
		# 	mutate(`Highlighted IDs` = case_when(ID %in% input$highlight ~ ID))
		# })
		
	  df <- reactive({
	    metadata() %>%
	      mutate(Highlighted = !!sym(event_column()) %in% input$highlighted) %>%
	      mutate(Direction = case_when(
	        Highlighted == TRUE & Direction == 'Downregulated' ~ 'Highlighted Downregulated',
	        Highlighted == TRUE & Direction == 'No Change' ~ 'Highlighted',
	        Highlighted == TRUE & Direction == 'Upregulated' ~ 'Highlighted Upregulated',
	        TRUE ~ Direction
	      )) %>%
	      mutate(Label = case_when(
	        Highlighted == TRUE ~ !!sym(event_column()),
	        input$label_strategy == 'Significant/Highlighted' & Significant == 'Significant' ~ !!sym(event_column())
	      ))
	  })
		
		
		pca_plot = reactive({
			req(pca())
			req(PoV())
			req(input$x)
			req(input$y)
			req(data())
			PoV_X = round((PoV() %>% filter(PC==input$x))$PoV, 2)
			PoV_Y = round((PoV() %>% filter(PC==input$y))$PoV, 2)
			
			# if (input$color_by == '' & input$outline_by == '') {
			# 	color_by = NULL
			# 	fill_by = NULL
			# 	point_shape = 16
			# 	shapes = c(16, 17, 15, 3, 7, 8, 1, 2, 4, 11, 12, 5, 6, 9, 10, 13, 14, 18, 0)
			# } else if (input$color_by != '' & input$outline_by == '') {
			# 	color_by = sym(input$color_by)
			# 	fill_by = sym(input$color_by)
			# 	point_shape = 16
			# 	shapes = c(16, 17, 15, 3, 7, 8, 1, 2, 4, 11, 12, 5, 6, 9, 10, 13, 14, 18, 0)
			# } else if (input$color_by == '' & input$outline_by != '') {
			# 	fill_by = NULL
			# 	color_by = sym(input$outline_by)
			# 	point_shape = 21
			# 	shapes = c(21, 22, 23, 24, 25)
			# } else {
			# 	color_by = sym(input$outline_by)
			# 	fill_by = sym(input$color_by)
			# 	point_shape = 21
			# 	shapes = c(21, 22, 23, 24, 25)
			# }
			upregulated_count = nrow(df() %>% filter(Direction == 'Upregulated'))
			noChange_count = nrow(df() %>% filter(Direction == 'No Change'))
			downregulated_count = nrow(df() %>% filter(Direction == 'Downregulated'))
			
			return(
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
			      cutoffLineType = 'blank',
			      cutoffLineCol = input$fc_line_color,
			      hline = NULL,
			      hlineCol = input$sig_line_color,
			      vline = NULL,
			      vlineCol = input$fc_line_color,
			      drawConnectors = if (input$label_strategy != 'None') TRUE else FALSE,
			      widthConnectors = 0.10,
			      boxedLabels = TRUE,
			      colCustom = colors(),
			      colAlpha = alphas(),
			      pointSize = sizes()
			    )+ {if (input$include_fc_line) geom_vline(xintercept = fold_change_threshold(), linetype=2, color=input$fc_line_color)} +
			      {if (input$include_zero_line) geom_vline(xintercept = 0, linetype=2, color=input$zero_line_color)} +
			      {if (input$include_fc_line) geom_vline(xintercept = -fold_change_threshold(), linetype=2, color=input$fc_line_color)} +
			      {if (input$include_sig_line) geom_hline(yintercept = significance_threshold(), linetype=2, color=input$sig_line_color)} +
			      {if (input$upregulated_check) annotate('text', x=Inf, y=Inf, hjust=1, vjust=1, color=input$upregulated_color, label=paste0('Upregulated: ', upregulated_count))} +
			      {if (input$downregulated_check) annotate('text', x=-Inf, y=Inf, hjust=-.01, vjust=1, color=input$downregulated_color, label=paste0('Downregulated: ', downregulated_count))} +
			      {if (input$noChange_check) annotate('text', x=0, y=Inf, vjust=1, color=input$noChange_color, label=paste0('No Change: ', noChange_count))}
			  )
			
		})
		
		plot_errors <- reactive({
			tryCatch({
				print(pca_plot())
			}, message = function(e) {
				return(e$message)
			}, warning = function(e) {
				return(e$message)
			}, error = function(e) {
				return(e$message)
			})
		})

		output$plot_error = renderText({
			
			if (length(plot_errors()) == 1 && substring(plot_errors(), 1, 36) == 'Insufficient values in manual scale.') {
				if (input$outline_by != '') {
					return("ERROR: R only has 5 distinct shapes that support an outline. Please change either the outline or shape grouping column.")
				} else {
					return("ERROR: R only has 19 distinct shapes. Please change the shape grouping column.")
				}
			} else {
				return(NULL)
			}
		})
		
		output$pca_plot = renderPlot(width=reactive(input$download_width), height=reactive(input$download_height), {
			req(pca_plot())
			if (length(plot_errors()) == 1 && substring(plot_errors(), 1, 36) == 'Insufficient values in manual scale.') {
				return(NULL)
			} else {
				pca_plot()
			}
		})
	
		output$pca_plot_interactive = renderPlotly({
			req(pca_plot())
			if (length(plot_errors()) == 1 && substring(plot_errors(), 1, 36) == 'Insufficient values in manual scale.') {
				return(NULL)
			} else {
				ggplotly(pca_plot(), height = input$download_height, width=input$download_width)
			}
		})
		
		output$pca_plot_download <- downloadHandler(
			filename = function() {'pca_plot.png'},
			content = function(file) {
				png(file=file, width=input$download_width, height=input$download_height)
				plot(pca_plot())
				dev.off()
			}
		)
		
		output$data_download <- downloadHandler(
			filename = function(){"pca_data.tsv"}, 
			content = function(fname){
				write.table(data(), file=fname, sep='\t', quote = FALSE, row.names=FALSE)
			}
		)
	
	})
}
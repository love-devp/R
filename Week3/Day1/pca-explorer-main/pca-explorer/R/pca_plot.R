reclass <- function (df, vec_types) {
  for (i in 1:ncol(df)) {
    type <- vec_types[i]
    class(df[ , i]) <- type
  }
  return(df)
}

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
					accordion_panel("Grouping",
						fluidRow(
							column(6, selectizeInput(ns("color_by"), label='Color:', choices=NULL)),
							column(6, selectizeInput(ns("shape_by"), label='Shape:', choices=NULL))
						),
						fluidRow(
							column(6, selectizeInput(ns("outline_by"), label='Outline*:', choices=NULL)),
							column(6, selectizeInput(ns("alpha_by"), label='Transparency:', choices=NULL))
						),
						p("*When setting outline - there can exist at most 5 shapes", style = "font-size:xx-small")
					),
					accordion_panel("Points",
						fluidRow(
							column(3, tags$div(id = "point_inline", numericInput(ns("size"), label='Size:', value=6, min=1, step=1))),
							column(3, tags$div(id = "point_inline", numericInput(ns("outline"), label='Outline:', value=1, min=1, step=1)))
						)
					),
					accordion_panel("Labels",
						fluidRow(
							column(6, selectizeInput(ns("label_by"), label='Label*:', choices=NULL)),
							column(6,selectizeInput(ns("highlight"), label='Highlighted IDs:', choices=NULL, multiple=TRUE))
						),
						p('*Set to "Highlighted IDs" to label specific row with its ID', style = "font-size:xx-small"),
						fluidRow(
							column(6, tags$div(id = "inline", numericInput(ns("label_size"), label='Label Size:', value=7, min=1, step=1))),
							column(6, tags$div(id = "inline", textInput(ns("label_color"), label='Label Color:', value='#000000')))
						)
					),
					accordion_panel("Axes",
						fluidRow(
							column(3, tags$div(id = "axes_inline", selectizeInput(ns("x"), label='X:', choices=NULL))),
							column(3, tags$div(id = "axes_inline", selectizeInput(ns("y"), label='Y:', choices=NULL))),
							column(5, tags$div(id = "axes_inline", numericInput(ns("axes_size"), label='Text Size:', value=12, min=1, step=1)))
						)
					),
					accordion_panel("Legend",
						checkboxInput(ns('hide_legend'), label='Hide Legend', value=FALSE)
					)
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
		
		observeEvent(PoV(), {
			updateSelectizeInput(session, 'x', "X:", choices=PoV()$PC, selected='PC1', server=TRUE)
		})
		
		observeEvent(PoV(), {
			updateSelectizeInput(session, 'y', "Y:", choices=PoV()$PC, selected='PC2', server=TRUE)
		})
		
		observeEvent(metadata_header(), {
			updateSelectizeInput(session, 'color_by', "Color:", choices=metadata_header(), selected='group', server=TRUE)
		})
		
		observeEvent(metadata_header(), {
			updateSelectizeInput(session, 'shape_by', "Shape:", choices=metadata_header(), selected="", server=TRUE)
		})
		
		observeEvent(metadata_header(), {
			updateSelectizeInput(session, 'outline_by', "Outline*:", choices=metadata_header(), selected="", server=TRUE)
		})
		
		observeEvent(metadata_header(), {
			updateSelectizeInput(session, 'alpha_by', "Transparency:", choices=metadata_header(), selected="", server=TRUE)
		})
		
		observeEvent(metadata_header(), {
			updateSelectizeInput(session, 'label_by', "Label*:", choices=c(metadata_header(), "Highlighted IDs"), server=TRUE)
		})
		
		observeEvent(input$label_by, ignoreNULL = TRUE, {
			if (input$label_by=='') {
				hideElement("label_size")
				hideElement("label_color")
			} else {
				showElement("label_size")
				showElement("label_color")
			}
			
			if (input$label_by=='Highlighted IDs') {
				showElement("highlight")
			} else {
				hideElement("highlight")
			}
			
		})
	
		observeEvent(input$interactive, ignoreNULL = TRUE, {
			if (input$interactive) {
				hideElement("pca_plot")
				hideElement("spinner")
				showElement("pca_plot_interactive")
				showElement("interactive_spinner")
			} else {
				showElement("pca_plot")
				showElement("spinner")
				hideElement("pca_plot_interactive")
				hideElement("interactive_spinner")
			}
		})
			
		observeEvent(metadata(), {
			updateSelectizeInput(session, 'highlight', "Highlighted IDs:", choices=metadata()$ID, server=TRUE)
		})
		
		data = reactive({
		  df = metadata() %>% select(-ID, -Total_Counts) %>% reclass(metadata_column_types()$Numeric)
		  reclassed_metadata = metadata() %>% select(ID, sample_name, Total_Counts) %>% left_join(df, by='sample_name')
		  
			reclassed_metadata %>%
			left_join(as.data.frame(pca()$x) %>% tibble::rownames_to_column('sample_name'), by='sample_name') %>%
			mutate(`Highlighted IDs` = case_when(ID %in% input$highlight ~ ID))
		})
		
		pca_plot = reactive({
			req(pca())
			req(PoV())
			req(input$x)
			req(input$y)
			req(data())
			PoV_X = round((PoV() %>% filter(PC==input$x))$PoV, 2)
			PoV_Y = round((PoV() %>% filter(PC==input$y))$PoV, 2)
			
			if (input$color_by == '' & input$outline_by == '') {
				color_by = NULL
				fill_by = NULL
				point_shape = 16
				shapes = c(16, 17, 15, 3, 7, 8, 1, 2, 4, 11, 12, 5, 6, 9, 10, 13, 14, 18, 0)
			} else if (input$color_by != '' & input$outline_by == '') {
				color_by = sym(input$color_by)
				fill_by = sym(input$color_by)
				point_shape = 16
				shapes = c(16, 17, 15, 3, 7, 8, 1, 2, 4, 11, 12, 5, 6, 9, 10, 13, 14, 18, 0)
			} else if (input$color_by == '' & input$outline_by != '') {
				fill_by = NULL
				color_by = sym(input$outline_by)
				point_shape = 21
				shapes = c(21, 22, 23, 24, 25)
			} else {
				color_by = sym(input$outline_by)
				fill_by = sym(input$color_by)
				point_shape = 21
				shapes = c(21, 22, 23, 24, 25)
			}
			
			return(
				ggplot(data(), aes(x=!!sym(input$x), y=!!sym(input$y), color=!!color_by, shape=!!sym(input$shape_by), fill=!!fill_by, alpha=!!sym(input$alpha_by), label=!!sym(input$label_by))) +
					theme_classic(base_size = input$axes_size) +
					{if (input$hide_legend) theme(legend.position="none")} +
					xlab(paste0(input$x, ': ', PoV_X, '%')) +
					ylab(paste0(input$y, ': ', PoV_Y, '%')) +
					scale_shape_manual(values=shapes) +
					{if(input$shape_by == '') 
							geom_point(size=input$size, stroke=input$outline, shape=point_shape)
					 else 
					 		geom_point(size=input$size, stroke=input$outline)} +
					guides(fill = guide_legend(override.aes = list(shape=21))) +
					{if (input$label_by != '') geom_label(label.size=NA, fill=NA, color=input$label_color, size=input$label_size)}
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
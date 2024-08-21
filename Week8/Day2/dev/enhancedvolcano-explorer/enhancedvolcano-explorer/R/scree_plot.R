screePlotUI <- function(id) {
	ns <- NS(id)
	tagList(
		layout_column_wrap(
			width = 1/2,
			card_body(min_height = 250,
				plotOutput(ns('scree_plot')) %>% withSpinner(image='spinner.gif')
			),
			card_body(
				accordion(open=FALSE,
					h6('Settings:'),
					accordion_panel("Bars",
						fluidRow(
							column(6, tags$div(id = "inline", textInput(ns("bar_fill"), label='Color:', value='#007bc2'))),
							column(6, tags$div(id = "inline", textInput(ns("bar_color"), label='Outline:', value='#007bc2')))
						)
					),
					accordion_panel("Axes",
						tags$div(id = "inline", numericInput(ns("axes_size"), label='Text Size:', value=12, min=1, step=1))
					),
					accordion_panel("Labels",
						tags$div(id = "inline", checkboxInput(ns("label_bars"), label='Label Bars', value=TRUE)),
						numericInput(ns("label_size"), label='Label Size:', value=4, min=1, step=1)
					),
					h6("Download:"),
					fluidRow(
						column(2, downloadButton(ns('data_download'), label='Data')),
						column(2, downloadButton(ns('scree_plot_download'), label='Plot')),
						column(3, tags$div(id = "inline", numericInput(ns('download_height'), "Height: ", value=350))),
						column(3, tags$div(id = "inline", numericInput(ns('download_width'), "Width: ", value=500))),
					)
				)
			)
		)
	)
}

screePlotServer <- function(id, metadata, PoV) {
	
	moduleServer(id, function(input, output, session) {
		
		observeEvent(input$label_bars, {
			if (input$label_bars) {
				showElement("label_size")
			} else {
				hideElement("label_size")
			}
		})
		
		scree_plot = reactive({
			return(
				ggplot(PoV(), aes(x=PC_num, y=PoV, label=Label)) +
					theme_classic(base_size = input$axes_size) +
					scale_x_continuous(name="PC", breaks=seq(1, min(nrow(PoV()), 20), 1), limits=c(0, min(nrow(PoV()), 20))) +
					scale_y_continuous(name="Percent of Variation", limits=c(0,max(PoV()$PoV * 1.05)), expand=c(0,0)) +
					geom_bar(stat='identity', fill=input$bar_fill, color=input$bar_color) +
					{if (input$label_bars) geom_label(fill=NA, label.size = NA, vjust=-.05, size=input$label_size)}
			)
		})
		
		output$scree_plot = renderPlot(width=reactive(input$download_width), height=reactive(input$download_height), {
			req(scree_plot())
			scree_plot()
		})
		
		output$plot_download <- downloadHandler(
			filename = function() {'scree_plot.png'},
			content = function(file) {
				png(file=file, width=input$download_width, height=input$download_height)
				plot(scree_plot())
				dev.off()
			}
		)
		
		output$data_download <- downloadHandler(
			filename = function(){"pca_data.tsv"}, 
			content = function(fname){
				write.table(PoV(), file=fname, sep='\t', quote = FALSE, row.names=FALSE)
			}
		)
		
	})
}
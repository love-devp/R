library(shiny)
library(shinyjs)
library(dplyr)
library(DT)
library(DESeq2)
library(sva)
library(stringr)
library(ggplot2)
library(shinycssloaders)
library(shinyjs)
library(bslib)
library(rhandsontable)
library(htmlwidgets)
library(plotly)

ui = page_navbar(
	title='PCA Explorer', id='nav_id', inverse=TRUE,
	nav_panel("Plots", plotPageUI('pca_page')),
	tags$head(
		tags$style(
			HTML('
				.navbar.navbar-default.navbar-static-top{padding: 2px}
				.accordion-title{font-size: x-small}
				.accordion-button{padding: 4px 4px 4px 4px}
				.accordion-body{padding: 4px 8px 4px 20px; background: #E7E7E7; font-size:x-small}
				.nav-link.nav-link.active{background: #1D1F21}
				.nav-link.nav-link{color: #1D1F21}
				.control-label{margin: 0px 0px 0px 0px; font-size: x-small}
				.item{font-size: .7rem}
				.checkbox{font-size: xx-small}
				.shiny-input-text.form-control.shinyjs-resettable.shiny-bound-input{font-size: xx-small}
				.shiny-input-number.form-control.shinyjs-resettable.shiny-bound-input{font-size: xx-small}
				.selectize-input.items.full.has-options.has-items{padding: 1px 12px 1px 12px}
				.card-body.bslib-gap-spacing.html-fill-item.html-fill-container{padding: 2px}
				.card-header.bslib-navs-card-title{padding: 2px 8px}
				.main.bslib-gap-spacing.html-fill-container{padding: 3px}
				.form-control.selectize-control{width: 220px}
				#pca_page-scree_plot-bar_fill{width: 90px}
				#pca_page-scree_plot-bar_color{width: 90px}
				#pca_page-scree_plot-data_download.btn.btn-default.shiny-download-link.shiny-bound-output{width: 80px; font-size: .7em; padding: 2px 0px 3px 0px; margin: 0px 5px 0px 5px}
				#pca_page-scree_plot-scree_plot_download.btn.btn-default.shiny-download-link.shiny-bound-output{width: 80px; font-size: .7em; padding: 2px 0px 3px 0px; margin: 0px 5px 0px 5px}
				#pca_page-sample_selection_hot{font-size: x-small}
				#pca_page-include_all.btn.btn-default.action-button.shiny-bound-input{width: 70px; font-size: .7em; padding: 2px 0px 3px 0px; margin: 0px 5px 2px 12px}
				#pca_page-exclude_all.btn.btn-default.action-button.shiny-bound-input{width: 70px; font-size: .7em; padding: 2px 0px 3px 0px; margin: 0px 5px 2px 5px}
				')
		),
		tags$head(
			tags$style(type="text/css", 
				"#inline label{ display: table-cell; text-align: center; vertical-align: middle; padding-right: 3px} 
      	#inline .form-group{display: table-row;}
				#inline .form-control{padding: 3px 10px 3px 10px}
				")
		)
	)
)

server <- function(input, output, session) {
	
	script_path = reactive({
		if (Sys.info()[[6]] == 'zdwyer') {
			"test/"
		} else {
			return("/home/rstudio/")
		}
	})
	
	plotPageServer('pca_page', script_path)
}

options(shiny.host = "0.0.0.0", shiny.port = 8789, shiny.maxRequestSize=30*1024^2)
shinyApp(ui = ui, server = server)
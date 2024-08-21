#EnhancedVolcano_v0.2
##List of errors
1. Endless roop of spinner.gif
2. Warning: Error in UseMethod: no applicable method for 'mutate' applied to an object of class "packageIQR"

##Solution
1. Modified DE_page.R
	- ` output$volcano = evolPlotServer('volvano_plot', data, event_column, fold_change_column, significance_column, significance_threshold, fold_change_threshold)
    `
	↓↓↓↓↓
	` evol_plot_data <- evolPlotServer('volcano', data, data$event_column, data$fold_change_column, data$significance_column, significance$significance, significance$fold_change) `
	`output$volcano <- renderPlot({evol_plot_data$e_volcano_plot()})`

2. on-going
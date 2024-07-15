pcaUI <- function(id) {
	ns <- NS(id)
	tagList(
		useShinyjs(),
		selectInput(ns('transformation'), 'Transformation', choices=c("Default", "vst", "rlog", "None"), selected="Default"),
		checkboxInput(ns('retx'), "Rotate Variables", value=TRUE),
		checkboxInput(ns('center'), "Center", value=TRUE),
		checkboxInput(ns('scale'), "Scale", value=TRUE)
	)
}

pcaServer <- function(id, filtered_counts) {
	
	moduleServer(id, function(input, output, session) {
		
		observeEvent(input$transformation, {
			if (input$transformation == 'None') {
				showElement("scale")
			} else {
				hideElement("scale")
			}
		})
		
		pca = reactive({
			req(filtered_counts())
			if (input$transformation == 'None') {
			
				keep = subset(filtered_counts(), apply(filtered_counts(), 1, var, na.rm = TRUE) >  0)
				return(prcomp(t(keep), retx=input$retx, center=input$center, scale.=input$scale))
			
			} else if (input$transformation == 'vst' | (input$transformation == 'Default' & ncol(filtered_counts()) > 50)) {
				
				transformed = vst(filtered_counts())
				return(prcomp(t(transformed), retx = input$retx, center = input$center, scale. = FALSE))
			
			} else {
				
				transformed = rlog(filtered_counts())
				return(prcomp(t(transformed), retx = input$retx, center = input$center, scale. = FALSE))
			}
		})
		
		PoV = reactive({
			data.frame(PoV=pca()$sdev^2/sum(pca()$sdev^2)*100) %>% 
				mutate(PC = paste0("PC", row_number())) %>%
				mutate(PC_num = row_number()) %>%
				mutate(Label = case_when(PoV >=.01 ~ as.character(round(PoV, 2)),
																 TRUE ~ "<.01"))
		})
		
		return(
			list(
				pca=pca,
				PoV=PoV
			)
		)
	})
}
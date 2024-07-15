quiet = function(x) { 
	sink(tempfile()) 
	on.exit(sink()) 
	invisible(force(x)) 
} 

getNormalizedMatrix <- function(M = NULL, method = "TMM") {
	if (is.null(M) ) return (NULL)
	M[is.na(M)] <- 0
	norm <- M
	if (!(method == "none" || method == "MRN")){
		norm.factors <- edgeR::calcNormFactors(M, method = method)
		norm <- edgeR::equalizeLibSizes(edgeR::DGEList(M,
																									 norm.factors = norm.factors))$pseudo.counts
	}else if(method == "MRN"){
		columns <- colnames(M)
		conds <- columns
		coldata <- prepGroup(conds, columns)
		M[, columns] <- apply(M[, columns], 2,
													function(x) as.integer(x))
		dds <- DESeqDataSetFromMatrix(countData = as.matrix(M),
																	colData = coldata, design = ~group)
		dds <- estimateSizeFactors(dds)
		norm <- counts(dds, normalized=TRUE)
	}
	return(norm)
}

batchCorrectionUI <- function(id) {
	ns <- NS(id)
	tagList(
		useShinyjs(),
		checkboxInput(ns("apply_batch_correction"), 'Apply Batch Correction', value=FALSE),
		hidden(selectInput(ns("batch_correction_method"), "Method", choices=c("MRN","TMM","RLE","upperquartile"), selected='TMM')),
		hidden(selectInput(ns("batch_correction_column"), "Batch Correction Column", choices=NULL)),
		hidden(selectInput(ns("biological_column"), "Biological Variable Grouping", choices=NULL))
	)
}

batchCorrectionServer <- function(id, raw_counts, metadata, metadata_header) {
	
	moduleServer(id, function(input, output, session) {
		
		observeEvent(input$apply_batch_correction, {
			if (input$apply_batch_correction == TRUE) {
				showElement("batch_correction_method")
				showElement("batch_correction_column")
				showElement("biological_column")
			} else {
				hideElement("batch_correction_method")
				hideElement("batch_correction_column")
				hideElement("biological_column")
			}
		})
		
		observeEvent(metadata_header(), {
			if ('batch' %in% metadata_header()) {
				default_batch = 'batch'
			} else {
				default_batch = metadata_header()[0]
			}
			updateSelectInput(session, 'batch_correction_column', "Batch Correction Column:", choices=metadata_header(), selected=default_batch)
		})
		
		observeEvent(metadata_header(), {
			if ('group' %in% metadata_header()) {
				default_biological = 'group'
			} else {
				default_biological = metadata_header()[0]
			}
			updateSelectInput(session, 'biological_column', "Biological Variable Grouping:",  choices=metadata_header(), selected=default_biological)
		})
		
		batch_corrected_counts = reactive({
			
			if (input$apply_batch_correction == TRUE) {
				
				req(input$batch_correction_column)
				req(input$biological_column)
				batch_correction_column_list = as.factor(metadata()[,input$batch_correction_column])
				biological_column_list = as.factor(metadata()[,input$biological_column])
				
				df = quiet(ComBat_seq(getNormalizedMatrix(raw_counts(), method=input$batch_correction_method), batch=batch_correction_column_list, group=biological_column_list))
				mode(df) = 'integer'
				return(df)
			} else {
				return(raw_counts())
			}
		})
		
		return(batch_corrected_counts)
		
	})
}
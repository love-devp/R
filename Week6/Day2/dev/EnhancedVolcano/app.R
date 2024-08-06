if (!require('BiocManager', quietly = TRUE))
  install.packages('BiocManager')
if (!require('airway', quietly = TRUE))
  BiocManager::install('airway')
if (!require('AnnotationDbi', quietly = TRUE))
  BiocManager::install('AnnotationDbi')
if (!require('org.Hs.eg.db', quietly = TRUE))
  BiocManager::install('org.Hs.eg.db')
source('./R/EnhancedVolcano.R')

# Load necessary libraries
library(shiny)
library(DESeq2)
library(BiocManager)
library(airway)
library(AnnotationDbi)
library(org.Hs.eg.db)
library(EnhancedVolcano)
library(magrittr)

# Global variables
# Reading the data.
file <- "./data/data.tsv"
# Read the data with row.names.
rsem <- read.table(file,sep="\t", header=TRUE, row.names=1)
# Creating the data structure for DESeq Analysis.
columns <- c("exper_rep1", "exper_rep2", "exper_rep3", "control_rep1", "control_rep2", "control_rep3")
data <- data.frame(rsem[, columns])

# Define the Shiny UI
ui <- fluidPage(
  titlePanel('Enhanced Volcano Plot'),
  sidebarLayout(
    sidebarPanel(
      numericInput('log2FC', 'Log2 Fold Change Cutoff:', value = 2),
      numericInput('pValue', 'P-value Cutoff:', value = 10e-6),
      textInput('title', 'Plot Title:', value = 'Volcano Plot')
    ),
    mainPanel(
      plotOutput('volcanoPlot1'),
      plotOutput('volcanoPlot2'),
      plotOutput('volcanoPlot3')
    )
  )
)

# Define the Shiny server
server <- function(input, output) {
  output$volcanoPlot1 <- renderPlot({
    # Load and prepare data
    data('airway')
    airway$dex %<>% relevel('untrt')
    #airway$dex <- relevel(airway$dex, 'untrt')
    ens <- rownames(airway)
    symbols <- mapIds(org.Hs.eg.db, keys = ens, column = c('SYMBOL'), keytype = 'ENSEMBL')
    symbols <- symbols[!is.na(symbols)]
    symbols <- symbols[match(rownames(airway), names(symbols))]
    rownames(airway) <- symbols
    keep <- !is.na(rownames(airway))
    airway <- airway[keep,]
    
    # Conduct differential expression analysis
    dds <- DESeqDataSet(airway, design = ~ cell + dex)
    dds <- DESeq(dds, betaPrior = FALSE)
    res <- results(dds, contrast = c('dex', 'trt', 'untrt'))
    res <- lfcShrink(dds, contrast = c('dex', 'trt', 'untrt'), res = res, type = 'normal')
    
    # Render the volcano plot
    EnhancedVolcano(res,
                    lab = rownames(res),
                    x = 'log2FoldChange',
                    y = 'pvalue')
  })
  
  output$volcanoPlot2 <- renderPlot({
    # Load and prepare data
    data('airway')
    airway$dex <- relevel(airway$dex, 'untrt')
    ens <- rownames(airway)
    symbols <- mapIds(org.Hs.eg.db, keys = ens, column = c('SYMBOL'), keytype = 'ENSEMBL')
    symbols <- symbols[!is.na(symbols)]
    symbols <- symbols[match(rownames(airway), names(symbols))]
    rownames(airway) <- symbols
    keep <- !is.na(rownames(airway))
    airway <- airway[keep,]
    
    # Conduct differential expression analysis
    dds <- DESeqDataSet(airway, design = ~ cell + dex)
    dds <- DESeq(dds, betaPrior = FALSE)
    res <- results(dds, contrast = c('dex', 'trt', 'untrt'))
    res <- lfcShrink(dds, contrast = c('dex', 'trt', 'untrt'), res = res, type = 'normal')
    
    # Render the volcano plot
    EnhancedVolcano(res,
                    lab = rownames(res),
                    x = 'log2FoldChange',
                    y = 'pvalue',
                    title = input$title,
                    pCutoff = input$pValue,
                    FCcutoff = input$log2FC,
                    pointSize = 3.0,
                    labSize = 6.0)
  })
  output$volcanoPlot3 <- renderPlot({
    # Load and prepare data
    data('airway')
    airway$dex <- relevel(airway$dex, 'untrt')
    ens <- rownames(airway)
    symbols <- mapIds(org.Hs.eg.db, keys = ens, column = c('SYMBOL'), keytype = 'ENSEMBL')
    symbols <- symbols[!is.na(symbols)]
    symbols <- symbols[match(rownames(airway), names(symbols))]
    rownames(airway) <- symbols
    keep <- !is.na(rownames(airway))
    airway <- airway[keep,]
    
    # Conduct differential expression analysis
    dds <- DESeqDataSet(airway, design = ~ cell + dex)
    dds <- DESeq(dds, betaPrior = FALSE)
    res <- results(dds, contrast = c('dex', 'trt', 'untrt'))
    res <- lfcShrink(dds, contrast = c('dex', 'trt', 'untrt'), res = res, type = 'normal')
    
    # Render the volcano plot
    EnhancedVolcano(res,
                    lab = rownames(res),
                    x = 'log2FoldChange',
                    y = 'pvalue',
                    title = input$title,
                    pCutoff = input$pValue,
                    FCcutoff = input$log2FC,
                    pointSize = 3.0,
                    labSize = 6.0,
                    col=c('black', 'black', 'black', 'red3'),
                    colAlpha = 1)
  })
}


# Run the Shiny app
shinyApp(ui = ui, server = server)
# Check necessary packages
if (!require('BiocManager', quietly = TRUE))
  install.packages('BiocManager')
if (!require('airway', quietly = TRUE))
  BiocManager::install('airway')
if (!require('AnnotationDbi', quietly = TRUE))
  BiocManager::install('AnnotationDbi')
if (!require('org.Hs.eg.db', quietly = TRUE))
  BiocManager::install('org.Hs.eg.db')

# Load EnhancedVolcano package
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

# Define the Shiny server
server <- function(input, output) {
  # Load and prepare data
  data('airway')
  airway$dex %<>% relevel('untrt')
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
  
  output$volcanoPlot1 <- renderPlot({
    # Render the volcano plot
    EnhancedVolcano(res,
                    lab = rownames(res),
                    x = 'log2FoldChange',
                    y = 'pvalue')
  })
  
  output$volcanoPlot2 <- renderPlot({
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
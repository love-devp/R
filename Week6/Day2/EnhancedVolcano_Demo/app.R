# Check necessary packages and install if missing
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
if (!requireNamespace("airway", quietly = TRUE))
  BiocManager::install("airway")
if (!requireNamespace("AnnotationDbi", quietly = TRUE))
  BiocManager::install("AnnotationDbi")
if (!requireNamespace("org.Hs.eg.db", quietly = TRUE))
  BiocManager::install("org.Hs.eg.db")

source("./R/EnhancedVolcano.R")

# Load necessary libraries
library(shiny)
library(DESeq2)
library(airway)
library(AnnotationDbi)
library(org.Hs.eg.db)
library(EnhancedVolcano)
library(magrittr)

# Define the Shiny UI
ui <- fluidPage(
  titlePanel('Enhanced Volcano Plot'),
  sidebarLayout(
    sidebarPanel(
      numericInput('log2FC1', 'Log2 Fold Change Cutoff for plot 1:', value = 2),
      numericInput('pValue1', 'P-value Cutoff for plot 1:', value = 10e-6),
      numericInput('log2FC2', 'Log2 Fold Change Cutoff for plot 2:', value = 0.5),
      numericInput('pValue2', 'P-value Cutoff for plot 2:', value = 10e-32),
      numericInput('log2FC3', 'Log2 Fold Change Cutoff for plot 3:', value = 1.5),
      numericInput('pValue3', 'P-value Cutoff for plot 3:', value = 10e-16)
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
  # Load and prepare data
  data("airway")
  airway$dex %<>% relevel("untrt")
  ens <- rownames(airway)
  symbols <- mapIds(org.Hs.eg.db, keys = ens, column = "SYMBOL", keytype = "ENSEMBL")
  symbols <- symbols[!is.na(symbols)]
  symbols <- symbols[match(rownames(airway), names(symbols))]
  rownames(airway) <- symbols
  keep <- !is.na(rownames(airway))
  airway <- airway[keep,]
  
  # Conduct differential expression analysis
  dds <- DESeqDataSet(airway, design = ~ cell + dex)
  dds <- DESeq(dds, betaPrior = FALSE)
  res <- results(dds, contrast = c("dex", "trt", "untrt"))
  res <- lfcShrink(dds, contrast = c("dex", "trt", "untrt"), res = res, type = "normal")
  
  output$volcanoPlot1 <- renderPlot({
    # Render the volcano plot
    EnhancedVolcano(res,
                    lab = rownames(res),
                    x = "log2FoldChange",
                    y = "pvalue",
                    title = "Basic volcano plot",
                    pCutoff = input$pValue1,
                    FCcutoff = input$log2FC1)
  })
  
  output$volcanoPlot2 <- renderPlot({
    # Render the volcano plot
    EnhancedVolcano(res,
                    lab = rownames(res),
                    x = "log2FoldChange",
                    y = "pvalue",
                    title = "N061011 versus N61311",
                    pCutoff = input$pValue2,
                    FCcutoff = input$log2FC2,
                    pointSize = 3.0,
                    labSize = 6.0)
  })
  
  output$volcanoPlot3 <- renderPlot({
    # Render the volcano plot
    EnhancedVolcano(res,
                    lab = rownames(res),
                    x = "log2FoldChange",
                    y = "pvalue",
                    title = "N061011 versus N61311",
                    pCutoff = input$pValue3,
                    FCcutoff = input$log2FC3,
                    pointSize = 3.0,
                    labSize = 6.0,
                    col = c("black", "black", "black", "red3"),
                    colAlpha = 1)
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

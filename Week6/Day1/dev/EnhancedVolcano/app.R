# Load necessary libraries
library(shiny)
source("./R/EnhancedVolcano.R")
library(DESeq2)
library(BiocManager)
BiocManager::install("airway")
library(airway)

# Define the Shiny UI
ui <- fluidPage(
  titlePanel("Enhanced Volcano Plot"),
  sidebarLayout(
    sidebarPanel(
      numericInput("log2FC", "Log2 Fold Change Cutoff:", value = 2),
      numericInput("pValue", "P-value Cutoff:", value = 10e-6),
      textInput("title", "Plot Title:", value = "Volcano Plot")
    ),
    mainPanel(
      plotOutput("volcanoPlot")
    )
  )
)

# Define the Shiny server
server <- function(input, output) {
  output$volcanoPlot <- renderPlot({
    # Load and prepare data
    data("airway")
    airway$dex <- relevel(airway$dex, "untrt")
    ens <- rownames(airway)
    symbols <- mapIds(org.Hs.eg.db, keys = ens, column = c("SYMBOL"), keytype = "ENSEMBL")
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
    
    # Render the volcano plot
    EnhancedVolcano(res,
                    lab = rownames(res),
                    x = "log2FoldChange",
                    y = "pvalue",
                    title = input$title,
                    pCutoff = input$pValue,
                    FCcutoff = input$log2FC,
                    pointSize = 3.0,
                    labSize = 6.0)
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

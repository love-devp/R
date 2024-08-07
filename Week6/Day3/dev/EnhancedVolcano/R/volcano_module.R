# volcano_module.R
source('./R/Enhancedvolcano.R')

# library(EnhancedVolcano)

create_volcano_plot <- function(df, event_column, fold_change_column, significance_column, input, fold_change_threshold, significance_threshold, colors, sizes) {
  EnhancedVolcano(
    toptable = df,
    lab = df[[event_column]],
    x = fold_change_column,
    y = significance_column,
    selectLab = input$highlighted,
    xlim = c(min(df[[fold_change_column]], na.rm = TRUE) - 1.5, max(df[[fold_change_column]], na.rm = TRUE) + 1.5),
    ylim = c(0, max(-log10(df[[significance_column]]), na.rm = TRUE) + 5),
    xlab = "Fold Change (log2)",
    ylab = "-Log10(p-value)",
    pCutoff = significance_threshold,
    FCcutoff = fold_change_threshold,
    pointSize = sizes,
    labSize = input$label_size,
    col = colors,
    colAlpha = 0.9,
    drawConnectors = TRUE,
    widthConnectors = 0.5
  )
}
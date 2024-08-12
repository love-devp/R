# volcano_module.R
source('./R/Enhancedvolcano.R')

create_volcano_plot <- function(toptable, lab, x, y, title, subtitle, caption, pCutoff, FCcutoff, pointSize, labSize, col, colAlpha, shape, cutoffLineType, cutoffLineCol, cutoffLineWidth, drawConnectors, widthConnectors, colConnectors, hline, vline, vlineType, vlineCol, vlineWidth, legendPosition, max.overlaps) {
  EnhancedVolcano(toptable,
                  lab = lab,
                  x = x,
                  y = y,
                  title = title,
                  subtitle = subtitle,
                  caption = caption,
                  pCutoff = pCutoff,
                  FCcutoff = FCcutoff,
                  #pointSize = pointSize,
                  labSize = labSize,
                  #col = col,
                  #colAlpha = unlist(colAlpha),
                  shape = shape, 
                  cutoffLineType =cutoffLineType,
                  cutoffLineCol = cutoffLineCol,
                  cutoffLineWidth = cutoffLineWidth,
                  drawConnectors = drawConnectors,
                  widthConnectors = widthConnectors,
                  colConnectors = colConnectors, 
                  hline = hline, 
                  vline = vline,
                  vlineWidth = vlineWidth,
                  vlineType = vlineType,
                  vlineCol = vlineCol,
                  legendPosition = legendPosition,
                  max.overlaps = max.overlaps
                  )
}

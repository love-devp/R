# volcano_module.R
source('./R/Enhancedvolcano.R')

create_volcano_plot <- function(res, lab, x, y, title, subtitle, caption, pCutoff, FCcutoff, labSize, cutoffLineCol, hline, vline, vlineCol) {
  EnhancedVolcano(res,
                  lab = lab,
                  x = x,
                  y = y,
                  title = title,
                  subtitle = subtitle,
                  caption = caption,
                  pCutoff = pCutoff,
                  FCcutoff = FCcutoff,
                  labSize = labSize,
                  cutoffLineCol = cutoffLineCol,
                  hline = hline, 
                  vline = vline,
                  vlineCol = vlineCol
                  #col = col,
                  #colAlpha = colAlpha
                  )
}

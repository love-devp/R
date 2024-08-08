# volcano_module.R
source('./R/Enhancedvolcano.R')

create_volcano_plot <- function(res, lab, x, y, title, subtitle, caption, pCutoff, FCcutoff, pointSize, labSize, col, colAlpha) {
  EnhancedVolcano(res,
                  lab = lab,
                  x = x,
                  y = y,
                  title = title,
                  subtitle = subtitle,
                  caption = caption,
                  pCutoff = pCutoff,
                  FCcutoff = FCcutoff,
                  pointSize = pointSize,
                  labSize = labSize,
                  #col = col,
                  #colAlpha = colAlpha
                  )
}

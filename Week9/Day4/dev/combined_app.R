inputSpeciesUI <- function(id) {
  ns <- NS(id)
  selectizeInput(ns('species'), "Select Species:", 
                 choices=c("Human"="org.Hs.eg.db",
                           "Mouse"='org.Mm.eg.db',
                           "Rat"='org.Rn.eg.db',
                           "Dog"='org.Cf.eg.db',
                           "Fly"='org.Dm.eg.db',
                           "Zebrafish"='org.Dr.eg.db',
                           "C. elegans"='org.Ce.eg.db',
                           "S. cerevisiae"='org.Sc.eg.db',
                           "E. coli"='org.EcK12.eg.db'
                 ),
                 selected='Human'
  )
}

inputSpeciesServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    org = reactive({
      if (input$species != 'org.Hs.eg.db' & input$species != 'org.Mm.eg.db') {
        withProgress(message = 'Download annotations', detail = input$species, {
          BiocManager::install(input$species, update=FALSE)
        })
      }
      return(input$species)
    })
    
    return(org)
    
  })
}

inputDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    selectInput(ns("input_source"), "Input Source:", choices=c("Automatic", "Manual Upload"), selected="Automatic"),
    selectizeInput(ns('automatic_data'), "Input Data:", choices=NULL),
    fileInput(ns("input_file"), label='Input Data:'),
    shinyjs::hidden(selectizeInput(ns("event_column"), label='Event Column:', choices=NULL)),
    shinyjs::hidden(selectizeInput(ns("fold_change_column"), label='Fold Change Column:', choices=NULL)),
    shinyjs::hidden(selectizeInput(ns("significance_column"), label='P-Value Column:', choices=NULL)),
    shinyjs::hidden(selectizeInput(ns("expression_column"), label='Expression Column:', choices=NULL))
  )
}

inputDataServer <- function(id, script_path, org) {
  
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input_files(), once=TRUE, {
      if (length(input_files()) == 0) {
        shinyjs::hide('input_source')
        shinyjs::hide('automatic_data')
        shinyjs::show('manual_data')
        updateSelectizeInput(session, 'input_source', "Input Source:", choices=c("Automatic", "Manual Upload"), selected="Manual Upload", server=TRUE)
      }
    })
    
    observeEvent(input$input_source, {
      if (input$input_source == 'Automatic') {
        hideElement('input_file')
        showElement('automatic_data')
      } else {
        hideElement('automatic_data')
        showElement('input_file')
      }
    })
    
    observeEvent(input$input_file$datapath, ignoreInit = TRUE, {
      shinyjs::showElement("event_column")
      shinyjs::showElement("fold_change_column")
      shinyjs::showElement("significance_column")
      shinyjs::showElement("expression_column")
    })
    
    observeEvent(input$automatic_data, ignoreInit = TRUE, {
      shinyjs::showElement("event_column")
      shinyjs::showElement("fold_change_column")
      shinyjs::showElement("significance_column")
      shinyjs::showElement("expression_column")
    })
    
    input_files = reactive({
      all_files = c(list.files(path=script_path(), pattern='*deseq2_results.tsv', recursive=TRUE))
      candidates = character()
      for (f in all_files) {
        if (substr(f, 1, 3) != 'run' & str_detect(f, 'outputs')) {
          candidates = append(candidates, f)
        }
      }
      df = data.frame(Path=candidates) %>%
        mutate(Base = basename(Path))
      return(setNames(as.list(as.character(df$Path)), nm = df$Base))
    })
    
    observeEvent(input_files(), {
      updateSelectizeInput(session, 'automatic_data', "Input Data:", choices=input_files(), server=TRUE)
    })
    
    raw_data = reactive({
      if (input$input_source == 'Manual Upload') {
        req(input$input_file$datapath)
        df = read.delim(input$input_file$datapath, header=TRUE, sep = '\t')
        if (length(df) > 1) {
          return(df)
        } else {
          return(read.delim(input$input_file$datapath, header=TRUE, sep = ','))
        }
      } else {
        req(input$automatic_data)
        df = read.delim(paste0(script_path(), '/', input$automatic_data), sep='\t')
        if (length(df) > 1) {
          return(df)
        } else {
          return(read.delim(paste0(script_path(), '/', input$automatic_data), sep=','))
        }
      }
    })
    
    column_names = reactive({
      if (input$input_source == 'Manual Upload') {
        req(input$input_file$datapath)
      } else {
        req(input$automatic_data)
      }
      return(colnames(raw_data()))
    })
    
    column_classes = reactive({ 
      if (input$input_source == 'Manual Upload') {
        req(input$input_file$datapath)
      } else {
        req(input$automatic_data)
      }
      return(sapply(raw_data(), class))
    })
    
    numeric_columns = reactive({ 
      if (input$input_source == 'Manual Upload') {
        req(input$input_file$datapath)
      } else {
        req(input$automatic_data)
      }
      return((data.frame(Column=column_names(), Class=column_classes()) %>% filter(Class == 'numeric' | Class == 'integer'))$Column)
    })
    
    observeEvent(column_names(), {
      
      if ('gene' %in% column_names()) {
        default_event = 'gene'
      }  else {
        default_event = column_names()[1]
      }
      updateSelectizeInput(session, 'event_column', "Event Column:", choices=c("", column_names()), selected=default_event, server=TRUE)
    })
    
    observeEvent(numeric_columns(), {
      
      if ('log2FoldChange_shrink' %in% numeric_columns()) {
        default_fc = 'log2FoldChange_shrink'
      } else if ('log2FoldChange' %in% numeric_columns()) {
        default_fc = 'log2FoldChange'
      } else {
        default_fc = ''
      }
      
      if ('padj' %in% numeric_columns()) {
        default_sig = 'padj'
      } else if ('pvalue' %in% numeric_columns()) {
        default_sig = 'pvalue'
      } else {
        default_sig = ''
      }
      
      if ('baseMean' %in% numeric_columns()) {
        default_exp = 'baseMean'
      }  else {
        default_exp = ''
      }
      updateSelectizeInput(session, 'fold_change_column', "Fold Change Column:", choices=c("", numeric_columns()), selected=default_fc, server=TRUE)
      updateSelectizeInput(session, 'significance_column', "P-Value Column:", choices=c("", numeric_columns()), selected=default_sig, server=TRUE)
      updateSelectizeInput(session, 'expression_column', "Expression Column:", choices=c("", numeric_columns()), selected=default_exp, server=TRUE)
    })
    
    return(
      list(
        raw = raw_data,
        event_column = reactive(input$event_column),
        fold_change_column = reactive(input$fold_change_column),
        significance_column = reactive(input$significance_column),
        expression_column = reactive(input$expression_column)
      )
    )
    
  })
}

inputPageUI <- function(id) {
  ns <- NS(id)
  tagList(
    layout_sidebar(
      sidebar= sidebar(
        accordion(open='Data',
                  accordion_panel("Species", icon=icon('dog'),
                                  inputSpeciesUI(ns('input_species'))
                  ),
                  accordion_panel("Data", icon=icon('file'),
                                  inputDataUI(ns('input_data'))
                  )
        )
      ),
      DTOutput(ns("raw_data_output")) %>% withSpinner(image='spinner.gif')
    )
  )
}

inputPageServer <- function(id, script_path) {
  
  moduleServer(id, function(input, output, session) {
    
    species = inputSpeciesServer('input_species')
    data = inputDataServer('input_data', script_path, species)
    
    output$raw_data_output = renderDT({
      datatable(data$raw())
    })
    
    return(
      list(
        species = species,
        data = data
      )
    )
  })
}

dePageUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    layout_sidebar(
      sidebar = sidebar(
        accordion(
          accordion_panel("Significance Cutoffs", icon = icon('star'),
                          significanceUI(ns('significance_panel'))
          ),
          accordion_panel("Plot Options", icon = icon('gear'),
                          plotUI(ns('plot_panel'))
          )
        )
      ),
      layout_column_wrap(
        width = 1/2,
        card(
          card_header("Enhanced Volcano Plot"),
          card_body(
            plotOutput(ns("volcano")) %>% withSpinner(image = 'spinner.gif'),
            fluidRow(
              selectInput(ns('file_type_vol'), 'Select file type', choices = c('PNG' = 'png', 'JPG' = 'jpg', 'PDF' = 'pdf')),
              downloadButton(ns('volcano_download'), label = 'Download Volcano Plot')
            )
          )
        ),
        card(
          card_header("MA Plot"),
          card_body(
            plotOutput(ns("ma")) %>% withSpinner(image = 'spinner.gif'),
            fluidRow(
              selectInput(ns('file_type_ma'), 'Select file type', choices = c('PNG' = 'png', 'JPG' = 'jpg', 'PDF' = 'pdf')),
              downloadButton(ns('ma_download'), label = 'Download MA Plot')
            )
          )
        )
      )
    )
  )
}


dePageServer <- function(id, data) {
  
  moduleServer(id, function(input, output, session) {
    
    significance <- significanceServer("significance_panel", isolate(data$raw), data$fold_change_column, data$significance_column, data$expression_column)
    plots <- plotServer("plot_panel", significance$df, data$event_column, data$fold_change_column, data$significance_column, data$expression_column, significance$fold_change, significance$significance)
    
    output$volcano <- renderPlot({
      plots$volcano()
    })
    
    output$volcano_download <- downloadHandler(
      filename = function() {
        paste0('volcano.', input$file_type_vol)
      },
      content = function(file) {
        ggsave(file, plot = plots$volcano(), device = input$file_type_vol)
      }
    )
    
    output$ma <- renderPlot({
      plots$ma()
    })
    
    output$ma_download <- downloadHandler(
      filename = function() {
        paste0('ma.', input$file_type_ma)
      },
      content = function(file) {
        ggsave(file, plot = plots$ma(), device = input$file_type_ma)
      }
    )
    
    return(significance)
    
  })
}



significanceUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    fluidRow(
      column(7, numericInput(ns("fold_change_threshold"), "Fold Change:", value = 1, step=.1)),
      column(5, numericInput(ns("significance_threshold"), "P-value:", value = .05, step=.01)),
      checkboxInput(ns('limit_fc'), "Limit Fold Change", value=FALSE),
      numericInput(ns("fc_ceiling"), "Fold Change Ceiling:", value = 16, step=.1),
      checkboxInput(ns('limit_sig'), "Limit Significance", value=FALSE),
      numericInput(ns("sig_floor"), "Significance Floor (10^):", value = -40, step=.1)
    )
  )
}

significanceServer <- function(id, data, fold_change_column, significance_column, expression_column) {
  
  moduleServer(id, function(input, output, session) {
    
    df = reactive({
      req(fold_change_column())
      req(significance_column())
      data() %>%
        mutate(!!sym(fold_change_column()) := case_when(
          input$limit_fc == TRUE & !!sym(fold_change_column()) < -input$fc_ceiling ~ -input$fc_ceiling,
          input$limit_fc == TRUE & !!sym(fold_change_column()) > input$fc_ceiling ~ input$fc_ceiling,
          TRUE ~ !!sym(fold_change_column())
        )) %>%
        mutate(!!sym(significance_column()) := case_when(
          input$limit_sig == TRUE & !!sym(significance_column()) < 10^(input$sig_floor) ~ 10^(input$sig_floor),
          TRUE ~ !!sym(significance_column())
        )) %>%
        mutate(Direction = case_when(!!sym(significance_column()) < input$significance_threshold & !!sym(fold_change_column()) > input$fold_change_threshold ~ 'Upregulated',
                                     !!sym(significance_column()) < input$significance_threshold & !!sym(fold_change_column()) < -input$fold_change_threshold ~ 'Downregulated',
                                     TRUE ~ 'No Change')) %>%
        mutate(Direction = factor(Direction, levels=c("Upregulated", "No Change", "Downregulated"))) %>%
        mutate(Significant = case_when(Direction == 'No Change' ~ 'Non-Significant',
                                       TRUE ~ 'Significant'))
    })
    
    observeEvent(input$limit_fc, ignoreNULL=TRUE, {
      if (input$limit_fc) {
        showElement("fc_ceiling")
      } else {
        hideElement("fc_ceiling")
      }
    })
    
    observeEvent(input$limit_sig, ignoreNULL=TRUE, {
      if (input$limit_sig) {
        showElement("sig_floor")
      } else {
        hideElement("sig_floor")
      }
    })
    
    return(
      list(
        fold_change = reactive(input$fold_change_threshold),
        significance = reactive(input$significance_threshold),
        df = df
      )
    )
    
  })
}

#' Publication-ready volcano plots with enhanced colouring and labeling.
#'
#' Volcano plots represent a useful way to visualise the results of
#' differential expression analyses. Here, we present a highly-configurable
#' function that produces publication-ready volcano plots [@EnhancedVolcano].
#' \code{EnhancedVolcano} will attempt to fit as many variable names in
#' the plot window as possible, thus avoiding 'clogging' up the plot with
#' labels that could not otherwise have been read.
#'
#' @param toptable A data-frame of test statistics (if not, a data frame,
#'   an attempt will be made to convert it to one). Requires at least
#'   the following: column for variable names (can be rownames); a column
#'   for log2 fold changes; a column for nominal or adjusted p-value.
#' @param lab A column name in \code{toptable} containing variable names.
#'    Can be \code{rownames(toptable)}.
#' @param x A column name in \code{toptable} containing log2 fold changes.
#' @param y A column name in \code{toptable} containing nominal or adjusted
#'    p-values.
#' @param selectLab A vector containing a subset of lab.
#' @param xlim Limits of the x-axis.
#' @param ylim Limits of the y-axis.
#' @param xlab Label for x-axis.
#' @param ylab Label for y-axis.
#' @param axisLabSize Size of x- and y-axis labels.
#' @param title Plot title.
#' @param subtitle Plot subtitle.
#' @param caption Plot caption.
#' @param titleLabSize Size of plot title.
#' @param subtitleLabSize Size of plot subtitle.
#' @param captionLabSize Size of plot caption.
#' @param pCutoff Cut-off for statistical significance. A horizontal line
#'   will be drawn at -log10(pCutoff).
#' @param pCutoffCol Column name of statistical significance values to be used as
#'   the cut-off. A typical usage situation would be to pass nominal [un-adjusted]
#'   p-values as 'y', but adjusted p-values as pCutoffCol. In this way, a
#'   plot is generated via -log10(unadjusted p-value), but cut-offs based on
#'   adjusted p-values.
#' @param FCcutoff Cut-off for absolute log2 fold-change. Vertical lines will
#'   be drawn at the negative and positive values of log2FCcutoff.
#' @param cutoffLineType Line type for \code{FCcutoff} and \code{pCutoff}
#'   ('blank', 'solid', 'dashed', 'dotted', 'dotdash', 'longdash', 'twodash').
#' @param cutoffLineCol Line colour for \code{FCcutoff} and \code{pCutoff}.
#' @param cutoffLineWidth Line width for \code{FCcutoff} and \code{pCutoff}.
#' @param pointSize Size of plotted points for each variable. Can be
#'   a single value or a vector of sizes.
#' @param labSize Size of labels for each variable.
#' @param labCol Colour of labels for each variable.
#' @param labFace Font face of labels for each variable.
#' @param boxedLabels Logical, indicating whether or not to draw labels in
#'   boxes.
#' @param parseLabels Logical, indicating whether or not to parse expressions
#'   in labels
#' @param shape Shape of the plotted points. Either a single value for
#'   all points, or 4 values corresponding to the default 4 legend labels
#'   specified by \code{legendLabels}.
#' @param shapeCustom Named vector / key-value pairs that will over-ride the
#'   default shape scheme. The order must match that of \code{toptable}.
#'   Names / keys relate to groups / categories; values relate to shape encodings.
#' @param col Colour shading for plotted points, corresponding to
#'   the default 4 legend labels specified by \code{legendLabels}.
#' @param colCustom Named vector / key-value pairs that will over-ride the
#'   default colour scheme. The order must match that of \code{toptable}.
#'   Names / keys relate to groups / categories; values relate to colour.
#' @param colAlpha Alpha for purposes of controlling colour transparency of
#'   variable points.
#' @param colGradient If activated, over-rides the default discrete colour scheme
#'   and replaces it with a continous scheme that shades based on nominal or 
#'   adjusted p-value specified by \code{y}. For example, c('red2', 'blue2').
#' @param colGradientBreaks Break-points for the two colours specified by
#'   colGradient.
#' @param colGradientLabels Labels for the break-points specified by
#'   colGradientBreaks.
#' @param colGradientLimits Limits of the colour scheme specified by
#'   colGradient, i.e., max and min possible p-values.
#' @param legendLabels Plot legend text labels.
#' @param legendPosition Position of legend ('top', 'bottom', 'left',
#'   'right').
#' @param legendLabSize Size of plot legend text.
#' @param legendIconSize Size of plot legend icons / symbols.
#' @param legendDropLevels Logical, drop unused factor levels from legend.
#' @param encircle A vector of variable names to encircle. Requires installation
#'   of package \code{\link[ggalt:geom_encircle]{ggalt}}.
#' @param encircleCol Colour of the encircled line.
#' @param encircleFill Colour fill of the encircled region.
#' @param encircleAlpha Alpha for purposes of controlling colour transparency of
#'   encircled region.
#' @param encircleSize Line width of the encircled line.
#' @param shade A vector of variable names to shade.
#' @param shadeFill Colour of shaded regions.
#' @param shadeAlpha Alpha for purposes of controlling colour transparency of
#'   shaded region.
#' @param shadeSize Size of the shade contour lines.
#' @param shadeBins Number of bins for the density of the shade.
#' @param drawConnectors Logical, indicating whether or not to connect plot
#'   labels to their corresponding points by line connectors.
#' @param widthConnectors Line width of connectors.
#' @param typeConnectors Have the arrow head open ('open') or filled ('closed')?
#' @param endsConnectors Which end of connectors to draw arrow head? ('last',
#'   'first', 'both').
#' @param lengthConnectors Length (size) of the connector arrowheads.
#' @param colConnectors Line colour of connectors and line segments.
#' @param max.overlaps Equivalent of max.overlaps in ggrepel. Set to
#'   'Inf' to always display all labels when drawConnectors = TRUE.
#' @param maxoverlapsConnectors See max.overlaps.
#' @param min.segment.length When drawConnectors = TRUE, specifies the minimum
#'   length of the connector line segments.
#' @param directionConnectors direction in which to draw connectors.
#'   'both', 'x', or 'y'.
#' @param arrowheads Logical, indicating whether or not to draw arrow heads or
#'   or just have straight lines.
#' @param hline Draw one or more horizontal lines passing through this/these
#'   values on y-axis. For single values, only a single numerical value is
#'   necessary. For multiple lines, pass these as a vector, e.g., c(60,90).
#' @param hlineType Line type for \code{hline} ('blank', 'solid', 'dashed', 'dotted',
#'   'dotdash', 'longdash', 'twodash').
#' @param hlineCol Colour of \code{hline}.
#' @param hlineWidth Width of \code{hline}.
#' @param vline Draw one or more vertical lines passing through this/these
#'   values on x-axis. For single values, only a single numerical value is
#'   necessary. For multiple lines, pass these as a vector, e.g., c(60,90).
#' @param vlineType Line type for \code{vline} ('blank', 'solid', 'dashed', 'dotted',
#'   'dotdash', 'longdash', 'twodash').
#' @param vlineCol Colour of \code{vline}.
#' @param vlineWidth Width of \code{vline}.
#' @param gridlines.major Logical, indicating whether or not to draw major
#'   gridlines.
#' @param gridlines.minor Logical, indicating whether or not to draw minor
#'   gridlines.
#' @param border Add a border for just the x and y axes ('partial') or the
#'   entire plot grid ('full')?
#' @param borderWidth Width of the border on the x and y axes.
#' @param borderColour Colour of the border on the x and y axes. 
#' @param raster Logical, indicating whether to rasterize the geom_point layer. 
#'   Requires installation of \code{\link[ggrastr:geom_point_rast]{ggrastr}}.
#'
#' @details
#' Volcano plots represent a useful way to visualise the results of differential expression analyses. Here, we present a highly-configurable function that produces publication-ready volcano plots [@EnhancedVolcano]. \code{EnhancedVolcano} will attempt to fit as many variable names in the plot window as possible, thus avoiding 'clogging' up the plot with labels that could not otherwise have been read.
#'
#' @return A \code{\link{ggplot2}} object.
#'
#' @author Kevin Blighe <kevin@clinicalbioinformatics.co.uk>
#'
#' @examples
#' library('pasilla')
#' pasCts <- system.file('extdata', 'pasilla_gene_counts.tsv',
#'   package='pasilla', mustWork=TRUE)
#' pasAnno <- system.file('extdata', 'pasilla_sample_annotation.csv',
#'   package='pasilla', mustWork=TRUE)
#' cts <- as.matrix(read.csv(pasCts,sep='\t',row.names='gene_id'))
#' coldata <- read.csv(pasAnno, row.names=1)
#' coldata <- coldata[,c('condition','type')]
#' rownames(coldata) <- sub('fb', '', rownames(coldata))
#' cts <- cts[, rownames(coldata)]
#' library('DESeq2')
#' dds <- DESeqDataSetFromMatrix(countData = cts,
#'   colData = coldata,
#'   design = ~ condition)
#' 
#' featureData <- data.frame(gene=rownames(cts))
#' mcols(dds) <- DataFrame(mcols(dds), featureData)
#' dds <- DESeq(dds)
#' res <- results(dds)
#' 
#' EnhancedVolcano(res,
#'   lab = rownames(res),
#'   x = 'log2FoldChange',
#'   y = 'pvalue',
#'   pCutoff = 10e-4,
#'   FCcutoff = 1.333,
#'   xlim = c(-5.5, 5.5),
#'   ylim = c(0, -log10(10e-12)),
#'   pointSize = 1.5,
#'   labSize = 2.5,
#'   title = 'DESeq2 results',
#'   subtitle = 'Differential expression',
#'   caption = 'FC cutoff, 1.333; p-value cutoff, 10e-4',
#'   legendPosition = "right",
#'   legendLabSize = 14,
#'   col = c('grey30', 'forestgreen', 'royalblue', 'red2'),
#'   colAlpha = 0.9,
#'   drawConnectors = TRUE,
#'   hline = c(10e-8),
#'   widthConnectors = 0.5)
#'
#' @import ggplot2
#' @import ggrepel
#' @importFrom methods is
#' 
#' @export
EnhancedVolcano <- function(
  toptable,
  lab,
  x,
  y,
  selectLab = NULL,
  xlim = c(min(toptable[[x]], na.rm=TRUE) - 1.5,
    max(toptable[[x]], na.rm=TRUE) + 1.5),
  ylim = c(0, max(-log10(toptable[[y]]), na.rm=TRUE) + 5),
  xlab = bquote(~Log[2]~ "fold change"),
  ylab = bquote(~-Log[10]~italic(P)),
  axisLabSize = 18,
  title = 'Volcano plot',
  subtitle = bquote(italic(EnhancedVolcano)),
  caption = paste0('total = ', nrow(toptable), ' variables'),
  titleLabSize = 18,
  subtitleLabSize = 14,
  captionLabSize = 14,
  pCutoff = 10e-6,
  pCutoffCol = y,
  FCcutoff = 1.0,
  cutoffLineType = 'longdash',
  cutoffLineCol = 'black',
  cutoffLineWidth = 0.4,
  pointSize = 2.0,
  labSize = 5.0,
  labCol = 'black',
  labFace = 'plain',
  boxedLabels = FALSE,
  parseLabels = FALSE,
  shape = 19,
  shapeCustom = NULL,
  col = c('grey30', 'forestgreen', 'royalblue', 'red2'),
  colCustom = NULL,
  colAlpha = 1/2,
  colGradient = NULL,
  colGradientBreaks = c(pCutoff, 1.0),
  colGradientLabels = c('0', '1.0'),
  colGradientLimits = c(0, 1.0),
  legendLabels = c('NS', expression(Log[2]~FC),
    'p-value', expression(p-value~and~log[2]~FC)),
  legendPosition = 'top',
  legendLabSize = 14,
  legendIconSize = 5.0,
  legendDropLevels = TRUE,
  encircle = NULL,
  encircleCol = 'black',
  encircleFill = 'pink',
  encircleAlpha = 3/4,
  encircleSize = 2.5,
  shade = NULL,
  shadeFill = 'grey',
  shadeAlpha = 1/2,
  shadeSize = 0.01,
  shadeBins = 2,
  drawConnectors = FALSE,
  widthConnectors = 0.5,
  typeConnectors = 'closed',
  endsConnectors = 'first',
  lengthConnectors = unit(0.01, 'npc'),
  colConnectors = 'grey10',
  max.overlaps = 15,
  maxoverlapsConnectors = NULL,
  min.segment.length = 0,
  directionConnectors = 'both',
  arrowheads = TRUE,
  hline = NULL,
  hlineType = 'longdash',
  hlineCol = 'black',
  hlineWidth = 0.4,
  vline = NULL,
  vlineType = 'longdash',
  vlineCol = 'black',
  vlineWidth = 0.4,
  gridlines.major = TRUE,
  gridlines.minor = TRUE,
  border = 'partial',
  borderWidth = 0.8,
  borderColour = 'black', 
  raster = FALSE)
{
  if(!is.numeric(toptable[[x]])) {
    stop(paste(x, ' is not numeric!', sep=''))
  }

  if(!is.numeric(toptable[[pCutoffCol]])) {
    stop(paste(y, ' is not numeric!', sep=''))
  }
  
  if (raster) {

    has_ggrastr <- ! is(try(find.package("ggrastr"), silent=TRUE), "try-error")

    if (has_ggrastr) {
      geom_point <- ggrastr::geom_point_rast
    } else {
      warning("raster disabled, required package \"ggrastr\" not installed")
    }
  }

  if (!is.null(maxoverlapsConnectors)) {
    max.overlaps <- maxoverlapsConnectors
  }

  i <- xvals <- yvals <- Sig <- NULL

  toptable <- as.data.frame(toptable)
  toptable$Sig <- 'NS'
  toptable$Sig[(abs(toptable[[x]]) > FCcutoff)] <- 'FC'

  toptable$Sig[(toptable[[pCutoffCol]] < pCutoff)] <- 'P'
  toptable$Sig[(toptable[[pCutoffCol]] < pCutoff) &
    (abs(toptable[[x]]) > FCcutoff)] <- 'FC_P'
  toptable$Sig <- factor(toptable$Sig,
    levels=c('NS','FC','P','FC_P'))
  # reset pCutoff to corresponding value on y
  # allowing to draw hline at the correct
  # threshold
  if (pCutoffCol != y) {
    pCutoff = max(
      toptable[which(
        toptable[pCutoffCol] <= pCutoff), y]
      )
  }
  # some software programs return 0 for very low p-values
  # These throw an error in EnhancedVolcano
  # Detect these, issue warning, and convert these to
  # machine-lowest value possible
  #####
  # New functionality in > v1.2:
  # Now convert to 10^-1 lower than lowest non-zero p-value
  if (min(toptable[[y]], na.rm=TRUE) == 0) {
    # <= v1.2
    #warning(paste("One or more P values is 0.",
    #  "Converting to minimum possible value..."),
    #  call. = FALSE)
    #toptable[which(toptable[[y]] == 0), y] <- .Machine$double.xmin
    warning(paste('One or more p-values is 0.',
      'Converting to 10^-1 * current',
      'lowest non-zero p-value...'),
      call. = FALSE)
    toptable[which(toptable[[y]] == 0), y] <- min(
      toptable[which(toptable[[y]] != 0), y],
      na.rm = TRUE) * 10^-1
  }

  toptable$lab <- lab
  toptable$xvals <- toptable[[x]]
  toptable$yvals <- toptable[[y]]

  # If user has supplied values in selectLab, convert labels to
  # NA and then re-set with those in selectLab
  if (!is.null(selectLab)) {
    names.new <- rep(NA, length(toptable$lab))
    indices <- which(toptable$lab %in% selectLab)
    names.new[indices] <- toptable$lab[indices]
    toptable$lab <- names.new
  }

  # create a base theme that will later be modified
  th <- theme_bw(base_size = 24) +

    theme(
      legend.background = element_rect(),

      # title, subtitle, and caption
      plot.title = element_text(
        angle = 0,
        size = titleLabSize,
        face = 'bold',
        vjust = 1),
      plot.subtitle = element_text(
        angle = 0,
        size = subtitleLabSize,
        face = 'plain',
        vjust = 1),
      plot.caption = element_text(
        angle = 0,
        size = captionLabSize,
        face = 'plain',
        vjust = 1),

      # axis text
      axis.text.x = element_text(
        angle = 0,
        size = axisLabSize,
        vjust = 1),
      axis.text.y = element_text(
        angle = 0,
        size = axisLabSize,
        vjust = 0.5),
      axis.title = element_text(
        size = axisLabSize),

      # legend
      legend.position = legendPosition,
      legend.key = element_blank(),
      legend.key.size = unit(0.5, 'cm'),
      legend.text = element_text(
        size = legendLabSize),
      title = element_text(
        size = legendLabSize),
      legend.title = element_blank())

  # Create the plot object differently based on whether colCustom 
  # and shapeCustom are NULL or not. This helps to avoid messing up
  # the legend.
  #
  # 1, both colCustom and shapeCustom are activated
  if (!is.null(colCustom) & !is.null(shapeCustom)) {

    plot <- ggplot(toptable, aes(x=xvals, y=-log10(yvals))) + th +

      # over-ride legend icon sizes for colour and shape.
      # guide_legends are separate for colour and shape;
      # so, legends will be drawn separate
      guides(
        colour = guide_legend(
          order = 1,
          override.aes = list(
            size = legendIconSize)),
        shape = guide_legend(
          order = 2,
          override.aes = list(
            size = legendIconSize))) +

      # include new shape and colour encodings as aes
      geom_point(
        aes(
          color = factor(names(colCustom)),
          shape = factor(names(shapeCustom))),
        alpha = colAlpha,
        size = pointSize,
        na.rm = TRUE) +

      # specify the colour and shape with the supplied encoding
      scale_color_manual(values = colCustom) +
      scale_shape_manual(values = shapeCustom)

  # 2, only colCustom is activated and 'shape' has just a single value
  } else if (!is.null(colCustom) & is.null(shapeCustom) & length(shape) == 1) {

    plot <- ggplot(toptable, aes(x=xvals, y=-log10(yvals))) + th +

      # over-ride legend icon sizes for colour and shape.
      # guide_legends are separate for colour and shape;
      # so, legends will be drawn separate IF shape is also
      # included as aes to geom_point (it is not, here)
      guides(
        colour = guide_legend(
          order = 1,
          override.aes = list(
            size = legendIconSize)),
        shape = guide_legend(
          order = 2,
          override.aes = list(
            size = legendIconSize))) +

      # include new colour encodings as aes.
      # 'shape' is included, but outside aes
      geom_point(
        aes(
          color = factor(names(colCustom))),
        alpha = colAlpha,
        shape = shape,
        size = pointSize,
        na.rm = TRUE) +

      # specify the colour with the supplied encoding
      scale_color_manual(values = colCustom) +

      # 'shape' is not included as aes. Specifying guide = TRUE
      # here will result in legends merging
      scale_shape_manual(guide = TRUE)

  # 3, only colCustom is activated and 'shape' has 4 values
  } else if (!is.null(colCustom) & is.null(shapeCustom) & length(shape) == 4) {

    plot <- ggplot(toptable, aes(x=xvals, y=-log10(yvals))) + th +

      # over-ride legend icon sizes for colour and shape.
      # guide_legends are separate for colour and shape;
      # so, legends will be drawn separate
      guides(
        colour = guide_legend(
          order = 1,
          override.aes = list(
            size = legendIconSize)),
        shape = guide_legend(
          order = 2,
          override.aes = list(
            size = legendIconSize))) +

      # include new colour encodings as aes.
      # 'shape' is included in aes and mapped to 4
      # categories of NS, FC, P, FC_P
      geom_point(
        aes(
          color = factor(names(colCustom)),
          shape = Sig),
        alpha = colAlpha,
        size = pointSize,
        na.rm = TRUE) +

      # specify the colour with the supplied encoding
      scale_color_manual(values = colCustom) +

      # as it is included as aes, a separate legend
      # for 'shape' will be drawn. Here, over-ride that
      # legend
      scale_shape_manual(
        values = c(
          NS = shape[1],
          FC = shape[2],
          P = shape[3],
          FC_P = shape[4]),
        labels = c(
          NS = legendLabels[1],
          FC = legendLabels[2],
          P = legendLabels[3],
          FC_P = legendLabels[4]),
        guide = TRUE,
        drop = legendDropLevels)

  # 4, only shapeCustom is activated
  } else if (is.null(colCustom) & !is.null(shapeCustom)) {

    if (is.null(colGradient)) {

      plot <- ggplot(toptable, aes(x = xvals, y = -log10(yvals))) + th +

        # over-ride legend icon sizes for colour and shape.
        # guide_legends are separate for colour and shape;
        # so, legends will be drawn separate
        guides(
          colour = guide_legend(
            order = 1,
            override.aes = list(
              size = legendIconSize)),
          shape = guide_legend(
            order = 2,
            override.aes = list(
              size = legendIconSize))) +

        # include new shape encodings as aes.
        # Standard colour for NS, FC, P, FC_P,
        # are added to aes, too.
        geom_point(
          aes(
            color = Sig,
            shape = factor(names(shapeCustom))),
          alpha = colAlpha,
          size = pointSize,
          na.rm = TRUE) +

        # as it is included as aes, a separate legend
        # for 'colour' will be drawn. Here, over-ride that
        # legend
        scale_color_manual(
          values = c(
            NS = col[1],
            FC = col[2],
            P = col[3],
            FC_P = col[4]),
          labels = c(
            NS = legendLabels[1],
            FC = legendLabels[2],
            P = legendLabels[3],
            FC_P = legendLabels[4]),
          drop = legendDropLevels) +

        # specify the shape with the supplied encoding
        scale_shape_manual(values = shapeCustom)

    } else {

      plot <- ggplot(toptable, aes(x = xvals, y = -log10(yvals))) + th +

        # over-ride legend icon sizes for colour and shape.
        # guide_legends are separate for colour and shape;
        # so, legends will be drawn separate
        guides(
          shape = guide_legend(
            order = 2,
            override.aes = list(
              size = legendIconSize))) +

        # include new shape encodings as aes.
        # Standard colour for NS, FC, P, FC_P,
        # are added to aes, too.
        geom_point(
          aes(
            color = Sig,
            shape = factor(names(shapeCustom))),
          alpha = colAlpha,
          size = pointSize,
          na.rm = TRUE) +

        scale_colour_gradient(
          low = colGradient[1],
          high = colGradient[2],
          limits = colGradientLimits,
          breaks = colGradientBreaks,
          labels = colGradientLabels)

        # specify the shape with the supplied encoding
        scale_shape_manual(values = shapeCustom)

    }

  # 5, both colCustom and shapeCustom are null;
  # only a single shape value specified
  } else if (is.null(colCustom) & is.null(shapeCustom) & length(shape) == 1) {

    if (is.null(colGradient)) {

      plot <- ggplot(toptable, aes(x = xvals, y = -log10(yvals))) + th +

        # over-ride legend icon sizes for colour and shape.
        # including 'shape' in the colour guide_legend here
        # results in the legends merging
        guides(colour = guide_legend(
          order = 1,
          override.aes = list(
            shape = shape,
            size = legendIconSize))) +

        geom_point(
          aes(color = Sig),
          alpha = colAlpha,
          shape = shape,
          size = pointSize,
          na.rm = TRUE) +

        scale_color_manual(
          values = c(
            NS = col[1],
            FC = col[2],
            P = col[3],
            FC_P = col[4]),
          labels = c(
            NS = legendLabels[1],
            FC = legendLabels[2],
            P = legendLabels[3],
            FC_P = legendLabels[4]),
          drop = legendDropLevels)

    } else {

      plot <- ggplot(toptable, aes(x = xvals, y = -log10(yvals))) + th +

        geom_point(
          aes(color = yvals),
          alpha = colAlpha,
          shape = shape,
          size = pointSize,
          na.rm = TRUE) +

        scale_colour_gradient(
          low = colGradient[1],
          high = colGradient[2],
          limits = colGradientLimits,
          breaks = colGradientBreaks,
          labels = colGradientLabels)
    }

  # 6, both colCustom and shapeCustom are null;
  # four shape values are specified
  } else if (is.null(colCustom) & is.null(shapeCustom) & length(shape) == 4) {

    if (is.null(colGradient)) {
      plot <- ggplot(toptable, aes(x = xvals, y = -log10(yvals))) + th +

        # over-ride legend icon sizes for colour and shape.
        # including 'shape' in the colour guide_legend here
        # results in the legends merging
        guides(colour = guide_legend(
          order = 1,
          override.aes = list(
            shape = c(
              NS = shape[1],
              FC = shape[2],
              P = shape[3],
              FC_P = shape[4]),
            size = legendIconSize))) +

        geom_point(
          aes(
            color = Sig,
            shape = Sig),
          alpha = colAlpha,
          size = pointSize,
          na.rm = TRUE) +

        scale_color_manual(
          values = c(
            NS = col[1],
            FC = col[2],
            P = col[3],
            FC_P = col[4]),
          labels = c(
            NS = legendLabels[1],
            FC = legendLabels[2],
            P = legendLabels[3],
            FC_P = legendLabels[4]),
          drop = legendDropLevels) +

        scale_shape_manual(
          values = c(
            NS = shape[1],
            FC = shape[2],
            P = shape[3],
            FC_P = shape[4]),
          guide = FALSE,
          drop = legendDropLevels)

    } else {

      plot <- ggplot(toptable, aes(x = xvals, y = -log10(yvals))) + th +

        geom_point(
          aes(
            color = yvals,
            shape = Sig),
          alpha = colAlpha,
          size = pointSize,
          na.rm = TRUE) +

        scale_colour_gradient(
          low = colGradient[1],
          high = colGradient[2],
          limits = colGradientLimits,
          breaks = colGradientBreaks,
          labels = colGradientLabels) +

        scale_shape_manual(
          values = c(
            NS = shape[1],
            FC = shape[2],
            P = shape[3],
            FC_P = shape[4]),
          guide = FALSE,
          drop = legendDropLevels)

    }
  }

  # add more elements to the plot
  plot <- plot +

    xlab(xlab) +
    ylab(ylab) +

    xlim(xlim[1], xlim[2]) +
    ylim(ylim[1], ylim[2]) +

    geom_vline(xintercept = c(-FCcutoff, FCcutoff),
      linetype = cutoffLineType,
      colour = cutoffLineCol,
      size = cutoffLineWidth) +

    geom_hline(yintercept = -log10(pCutoff),
      linetype = cutoffLineType,
      colour = cutoffLineCol,
      size = cutoffLineWidth)

  # add elements to the plot for title, subtitle, caption
  plot <- plot + labs(title = title, 
    subtitle = subtitle, caption = caption)

  # add elements to the plot for vlines and hlines
  if (!is.null(vline)) {
    plot <- plot + geom_vline(xintercept = vline,
      linetype = vlineType,
      colour = vlineCol,
      size = vlineWidth)
  }
  if (!is.null(hline)) {
    plot <- plot + geom_hline(yintercept = -log10(hline),
      linetype = hlineType,
      colour = hlineCol,
      size = hlineWidth)
  }

  # Border around plot
  if (border == 'full') {
    plot <- plot + theme(panel.border = element_rect(
      colour = borderColour, fill = NA, size = borderWidth))
  } else if (border == 'partial') {
    plot <- plot + theme(axis.line = element_line(
      size = borderWidth, colour = borderColour),
      panel.border = element_blank(),
      panel.background = element_blank())
  } else {
    stop('Unrecognised value passed to \'border\'. Must be \'full\' or \'partial\'')
  }

  # Gridlines
  if (gridlines.major) {
    plot <- plot + theme(panel.grid.major = element_line())
  } else {
    plot <- plot + theme(panel.grid.major = element_blank())
  }
  if (gridlines.minor) {
    plot <- plot + theme(panel.grid.minor = element_line())
  } else {
    plot <- plot + theme(panel.grid.minor = element_blank())
  }

  # user has specified to draw with geom_text or geom_label?
  if (!boxedLabels) {

    # For labeling with geom_[text|label]_repel and
    # geom_[text|label] with check_overlap = TRUE, 4 possible
    # scenarios can arise
    if (drawConnectors && is.null(selectLab)) {

      if (arrowheads) {
        arr <- arrow(length = lengthConnectors,
          type = typeConnectors, ends = endsConnectors)
      } else {
        arr <- NULL
      }

      plot <- plot + geom_text_repel(
        data = subset(toptable,
          toptable[[y]] < pCutoff &
            abs(toptable[[x]]) > FCcutoff),
        aes(label = subset(toptable,
          toptable[[y]] < pCutoff &
            abs(toptable[[x]]) > FCcutoff)[["lab"]]),
        xlim = c(NA, NA),
        ylim = c(NA, NA),
        size = labSize,
        segment.color = colConnectors,
        segment.size = widthConnectors,
        arrow = arr,
        colour = labCol,
        fontface = labFace,
        parse = parseLabels,
        na.rm = TRUE,
        direction = directionConnectors,
        max.overlaps = max.overlaps,
        min.segment.length = min.segment.length)

    } else if (drawConnectors && !is.null(selectLab)) {

      if (arrowheads) {
        arr <- arrow(length = lengthConnectors,
          type = typeConnectors, ends = endsConnectors)
      } else {
        arr <- NULL
      }

      plot <- plot + geom_text_repel(
        data = subset(toptable,
          !is.na(toptable[['lab']])),
        aes(label = subset(toptable,
          !is.na(toptable[['lab']]))[['lab']]),
        xlim = c(NA, NA),
        ylim = c(NA, NA),
        size = labSize,
        segment.color = colConnectors,
        segment.size = widthConnectors,
        arrow = arr,
        colour = labCol,
        fontface = labFace,
        parse = parseLabels,
        na.rm = TRUE,
        direction = directionConnectors,
        max.overlaps = max.overlaps,
        min.segment.length = min.segment.length)

    } else if (!drawConnectors && !is.null(selectLab)) {

      plot <- plot + geom_text(
        data = subset(toptable,
          !is.na(toptable[['lab']])),
        aes(
          label = subset(toptable,
            !is.na(toptable[['lab']]))[['lab']]),
        size = labSize,
        check_overlap = TRUE,
        colour = labCol,
        fontface = labFace,
        parse = parseLabels,
        na.rm = TRUE)

    } else if (!drawConnectors && is.null(selectLab)) {

      plot <- plot + geom_text(
        data = subset(toptable,
          toptable[[y]] < pCutoff &
            abs(toptable[[x]]) > FCcutoff),
        aes(label = subset(toptable,
          toptable[[y]] < pCutoff &
            abs(toptable[[x]]) > FCcutoff)[['lab']]),
        size = labSize,
        check_overlap = TRUE,
        colour = labCol,
        fontface = labFace,
        parse = parseLabels,
        na.rm = TRUE)
    }

  } else {

    # For labeling with geom_[text|label]_repel and
    # geom_[text|label] with check_overlap = TRUE, 4 possible
    # scenarios can arise
    if (drawConnectors && is.null(selectLab)) {

      if (arrowheads) {
        arr <- arrow(length = lengthConnectors,
          type = typeConnectors, ends = endsConnectors)
      } else {
        arr <- NULL
      }

      plot <- plot + geom_label_repel(
        data = subset(toptable,
          toptable[[y]] < pCutoff &
            abs(toptable[[x]]) > FCcutoff),
        aes(label = subset(toptable,
          toptable[[y]]<pCutoff &
            abs(toptable[[x]]) > FCcutoff)[['lab']]),
        xlim = c(NA, NA),
        ylim = c(NA, NA),
        size = labSize,
        segment.color = colConnectors,
        segment.size = widthConnectors,
        arrow = arr,
        colour = labCol,
        fontface = labFace,
        parse = parseLabels,
        na.rm = TRUE,
        direction = directionConnectors,
        max.overlaps = max.overlaps,
        min.segment.length = min.segment.length)

    } else if (drawConnectors && !is.null(selectLab)) {

      if (arrowheads) {
        arr <- arrow(length = lengthConnectors,
          type = typeConnectors, ends = endsConnectors)
      } else {
        arr <- NULL
      }

      plot <- plot + geom_label_repel(
        data = subset(toptable,
          !is.na(toptable[['lab']])),
        aes(label = subset(toptable,
          !is.na(toptable[['lab']]))[['lab']]),
        xlim = c(NA, NA),
        ylim = c(NA, NA),
        size = labSize,
        segment.color = colConnectors,
        segment.size = widthConnectors,
        arrow = arr,
        colour = labCol,
        fontface = labFace,
        parse = parseLabels,
        na.rm = TRUE,
        direction = directionConnectors,
        max.overlaps = max.overlaps,
        min.segment.length = min.segment.length)

    } else if (!drawConnectors && !is.null(selectLab)) {

      plot <- plot + geom_label(
        data = subset(toptable,
          !is.na(toptable[["lab"]])),
        aes(
          label = subset(toptable,
            !is.na(toptable[['lab']]))[['lab']]),
        size = labSize,
        colour = labCol,
        fontface = labFace,
        parse = parseLabels,
        na.rm = TRUE)

    } else if (!drawConnectors && is.null(selectLab)) {

      plot <- plot + geom_label(
        data = subset(toptable,
          toptable[[y]] < pCutoff &
            abs(toptable[[x]]) > FCcutoff),
        aes(label = subset(toptable,
          toptable[[y]] < pCutoff &
            abs(toptable[[x]]) > FCcutoff)[['lab']]),
        size = labSize,
        colour = labCol,
        fontface = labFace,
        parse = parseLabels,
        na.rm = TRUE)

    }
  }

  # encircle
  if (!is.null(encircle)) {

    if (is(try(find.package("ggalt"), silent=TRUE), "try-error")) {
      stop("Please install package \"ggalt\" to access the \"encircle\" features")
    }

    plot <- plot + 
      ggalt::geom_encircle(
        data = subset(toptable,
          rownames(toptable) %in% encircle),
        colour = encircleCol,
        fill = encircleFill,
        alpha = encircleAlpha,
        size = encircleSize,
        show.legend = FALSE,
        na.rm = TRUE)
  }

  # shade
  if (!is.null(shade)) {
    plot <- plot + 
      stat_density2d(
        data = subset(toptable,
          rownames(toptable) %in% shade),
        fill = shadeFill,
        alpha = shadeAlpha,
        geom = 'polygon',
        contour = TRUE,
        size = shadeSize,
        bins = shadeBins,
        show.legend = FALSE,
        na.rm = TRUE)
  }

  plot <- plot + coord_cartesian(clip = 'off')

  return(plot)
}


# Load Enhanced volcano.R from local directory
source('./R/Enhancedvolcano.R')


reverselog = function() {
  trans_new('reverselog', function(x) -log10(x), function(x) 10^(-x), log_breaks(base = 10), domain = c(1e-1000, Inf))
}

plotUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    accordion(open=FALSE,
              accordion_panel('Label Options',
                              selectizeInput(ns('label_strategy'), 'Event Labels:', choices = c('None', 'Significant/Highlighted', 'Highlighted Only'), selected='None'),
                              # Drop down menu for Highlighted data,enabled Copy&Paste option, separated by comma
                              selectizeInput(ns('highlighted'), 'Highlighted Events:', choices=NULL, multiple=TRUE, options = list(create = TRUE, delimiter = ',')),
                              numericInput(ns('label_size'), 'Label Size:', value=4), 
                              # Drop down menu for legend position, default value is None
                              selectizeInput(ns('legend_position'), 'Legend Position', choices = c('top', 'bottom', 'right', 'left', 'none'), selected = 'none'),
                              # 3 Check boxes for Data counting 
                              checkboxInput(ns('upregulated_check'), 'Upregulated Count', value = FALSE), 
                              checkboxInput(ns('downregulated_check'), 'Downregulated Count', value = FALSE),
                              checkboxInput(ns('noChange_check'), 'No Change Count', value = FALSE)
                              
              ),
              accordion_panel('Point Options',
                              accordion(open=FALSE,
                                        accordion_panel('Color',
                                                        h5('Standard:'),
                                                        colourInput(ns('downregulated_color'), 'Downregulated', value='#4682B4'),
                                                        colourInput(ns('noChange_color'), 'No Change:', value='#e7e7e7'),
                                                        colourInput(ns('upregulated_color'), 'Upregulated:', value='#b22222'),
                                                        h5('Highlighted:'),
                                                        colourInput(ns('highlight_downregulated_color'), 'Downregulated', value='#1e6234'),
                                                        colourInput(ns('highlight_noChange_color'), 'No Change:', value='#000000'),
                                                        colourInput(ns('highlight_upregulated_color'), 'Upregulated:', value='#1e6234')
                                        ),
                                        accordion_panel('Size',
                                                        h5('Standard:'),
                                                        numericInput(ns('downregulated_size'), 'Downregulated', value=2),
                                                        numericInput(ns('noChange_size'), 'No Change', value=1),
                                                        numericInput(ns('upregulated_size'), 'Upregulated', value=2),
                                                        h5('Highlighted:'),
                                                        numericInput(ns('highlight_downregulated_size'), 'Downregulated', value=2),
                                                        numericInput(ns('highlight_noChange_size'), 'No Change', value=2),
                                                        numericInput(ns('highlight_upregulated_size'), 'Upregulated', value=2)
                                        ),
                                        accordion_panel('Transparency',
                                                        h5('Standard:'),
                                                        numericInput(ns('downregulated_alpha'), 'Downregulated', value=1),
                                                        numericInput(ns('noChange_alpha'), 'No Change', value=1),
                                                        numericInput(ns('upregulated_alpha'), 'Upregulated', value=1),
                                                        h5('Highlighted:'),
                                                        numericInput(ns('highlight_downregulated_alpha'), 'Downregulated', value=1),
                                                        numericInput(ns('highlight_noChange_alpha'), 'No Change', value=1),
                                                        numericInput(ns('highlight_upregulated_alpha'), 'Upregulated', value=1)
                                        ),
                              )
              ),
              accordion_panel('Axis Options',
                              checkboxInput(ns('include_zero_line'), 'Zero Line', value=TRUE),
                              colourInput(ns('zero_line_color'), 'Color', value='#000000'),
                              checkboxInput(ns('include_fc_line'), 'Fold Change Cutoff Line', value=TRUE),
                              colourInput(ns('fc_line_color'), 'Color', value='#e7e7e7'),
                              checkboxInput(ns('include_sig_line'), 'Significance Cutoff Line', value=TRUE),
                              colourInput(ns('sig_line_color'), 'Color', value='#e7e7e7')
              )
    )
  )
}

plotServer <- function(id, data, event_column, fold_change_column, significance_column, expression_column, fold_change_threshold, significance_threshold) {
  
  moduleServer(id, function(input, output, session) {
    
    observeEvent(data(), {
      updateSelectizeInput(session, 'highlighted', 'Highlighted Events:', choices=df()[,event_column()], server=TRUE)
    })
    
    colors = reactive({
      color_mapping <- c(
        'Upregulated' = input$upregulated_color,
        'No Change' = input$noChange_color,
        'Downregulated' = input$downregulated_color,
        'Highlighted Upregulated' = input$highlight_upregulated_color,
        'Highlighted' = input$highlight_noChange_color,
        'Highlighted Downregulated' = input$highlight_downregulated_color
      )
      df_temp <- df()
      df_temp$Direction <- factor(df_temp$Direction, levels = names(color_mapping))
      
      # Apply the mapping and ensure that it is consistent
      color_values <- color_mapping[as.character(df_temp$Direction)]
      
      # Replace NA or missing color values with a default color
      color_values[is.na(color_values)] <- 1 # Default to color = 1 for any NA values	  
      return(color_values)
    })
    
    alphas <- reactive({
      alpha_mapping <- c(
        'Upregulated' = input$upregulated_alpha,
        'No Change' = input$noChange_alpha,
        'Downregulated' = input$downregulated_alpha,
        'Highlighted Upregulated' = input$highlight_upregulated_alpha,
        'Highlighted' = input$highlight_noChange_alpha,
        'Highlighted Downregulated' = input$highlight_downregulated_alpha
      )
      
      df_temp <- df()
      df_temp$Direction <- factor(df_temp$Direction, levels = names(alpha_mapping))
      
      alpha_values <- alpha_mapping[as.character(df_temp$Direction)]
      alpha_values[is.na(alpha_values)] <- 1  # Default to alpha = 1 for any NA values
      return(alpha_values)
    })
    
    sizes <- reactive({
      size_mapping <- c(
        'Upregulated' = input$upregulated_size,
        'No Change' = input$noChange_size,
        'Downregulated' = input$downregulated_size,
        'Highlighted Upregulated' = input$highlight_upregulated_size,
        'Highlighted' = input$highlight_noChange_size,
        'Highlighted Downregulated' = input$highlight_downregulated_size
      )
      
      df_temp <- df()
      df_temp$Direction <- factor(df_temp$Direction, levels = names(size_mapping))
      
      size_values <- size_mapping[as.character(df_temp$Direction)]
      size_values[is.na(size_values)] <- 1  # Default to size = 1 for any NA values
      
      return(size_values)
    })
    
    df = reactive({
      data() %>%
        mutate(Highlighted = !!sym(event_column()) %in% input$highlighted) %>%
        mutate(Direction = case_when(Highlighted == TRUE & Direction == 'Downregulated' ~ 'Highlighted Downregulated',
                                     Highlighted == TRUE & Direction == 'No Change' ~ 'Highlighted',
                                     Highlighted == TRUE & Direction == 'Upregulated' ~ 'Highlighted Upregulated',
                                     TRUE ~ Direction
        )) %>%
        mutate(Label = case_when(Highlighted == TRUE ~ !!sym(event_column()),
                                 input$label_strategy == 'Significant/Highlighted' & Significant == 'Significant' ~ !!sym(event_column())))
    })
    
    volcano_plot <- reactive({
      
      upregulated_count = nrow(df() %>% filter(Direction == 'Upregulated'))
      noChange_count = nrow(df() %>% filter(Direction == 'No Change'))
      downregulated_count = nrow(df() %>% filter(Direction == 'Downregulated'))
      
      # Render Enhanced Volcano Plot
      EnhancedVolcano(
        toptable = df(),
        lab = df()$Label,
        x = fold_change_column(),
        y = significance_column(),
        title = NULL,
        subtitle = NULL,
        caption = NULL,
        legendPosition = input$legend_position,
        pCutoff = significance_threshold(),
        pCutoffCol = significance_column(),
        FCcutoff = fold_change_threshold(),
        labSize = input$label_size,
        cutoffLineType = 'blank',
        cutoffLineCol = input$fc_line_color,
        hline = NULL,
        hlineCol = input$sig_line_color,
        vline = NULL,
        vlineCol = input$fc_line_color,
        drawConnectors = if (input$label_strategy != 'None') TRUE else FALSE,
        widthConnectors = 0.10,
        boxedLabels = TRUE,
        colCustom = colors(),
        colAlpha = alphas(),
        pointSize = sizes()
      )+ {if (input$include_fc_line) geom_vline(xintercept = fold_change_threshold(), linetype=2, color=input$fc_line_color)} +
        {if (input$include_zero_line) geom_vline(xintercept = 0, linetype=2, color=input$zero_line_color)} +
        {if (input$include_fc_line) geom_vline(xintercept = -fold_change_threshold(), linetype=2, color=input$fc_line_color)} +
        {if (input$include_sig_line) geom_hline(yintercept = significance_threshold(), linetype=2, color=input$sig_line_color)} +
        {if (input$upregulated_check) annotate('text', x=Inf, y=Inf, hjust=1, vjust=1, color=input$upregulated_color, label=paste0('Upregulated: ', upregulated_count))} +
        {if (input$downregulated_check) annotate('text', x=-Inf, y=Inf, hjust=-.01, vjust=1, color=input$downregulated_color, label=paste0('Downregulated: ', downregulated_count))} +
        {if (input$noChange_check) annotate('text', x=0, y=Inf, vjust=1, color=input$noChange_color, label=paste0('No Change: ', noChange_count))}
    })
    
    ma_plot = reactive({
      
      upregulated_count = nrow(df() %>% filter(Direction == 'Upregulated'))
      noChange_count = nrow(df() %>% filter(Direction == 'No Change'))
      downregulated_count = nrow(df() %>% filter(Direction == 'Downregulated'))
      
      if (expression_column() != '' & fold_change_column() != '') {
        return(
          ggplot(df(), aes(x=!!sym(expression_column()), y=!!sym(fold_change_column()), color=Direction, size=Direction, alpha=Direction, label=Label)) +
            theme_classic(base_size = 16) +
            theme(legend.title=element_blank()) +
            theme(legend.text=element_text(size = 15)) +
            theme(legend.position = input$legend_position) +
            scale_x_continuous(trans='log10', name='Expression') +
            scale_y_continuous(name='Fold Change (log2)') +
            scale_color_manual(values=colors()) +
            scale_alpha_manual(values=alphas()) +
            scale_size_manual(values=sizes()) +
            geom_point() +
            {if (input$include_fc_line) geom_hline(yintercept = fold_change_threshold(), linetype=2, color=input$fc_line_color)} +
            {if (input$include_zero_line) geom_hline(yintercept = 0, linetype=2, color=input$zero_line_color)} +
            {if (input$include_fc_line) geom_hline(yintercept = -fold_change_threshold(), linetype=2, color=input$fc_line_color)} +
            {if (input$label_strategy != 'None') geom_label_repel(label.size=NA, size=input$label_size, fill=NA, na.rm=TRUE, max.overlaps = 50, max.time = 5)} +
            {if (input$upregulated_check) annotate('text', x=Inf, y=Inf, hjust=1, vjust=1, color=input$upregulated_color, label=paste0('Upregulated: ', upregulated_count))} +
            {if (input$downregulated_check) annotate('text', x=0, y=Inf, hjust=-.01, vjust=1, color=input$downregulated_color, label=paste0('Downregulated: ', downregulated_count))} +
            {if (input$noChange_check) annotate('text', x=1000, y=Inf, vjust=1, color=input$noChange_color, label=paste0('No Change: ', noChange_count))}
        )
      } else {
        return(NULL)
      }
    })
    
    observeEvent(input$include_zero_line, ignoreNULL=TRUE, {
      if (input$include_zero_line) {
        showElement('zero_line_color')
      } else {
        hideElement('zero_line_color')
      }
    })
    
    observeEvent(input$include_fc_line, ignoreNULL=TRUE, {
      if (input$include_fc_line) {
        showElement('fc_line_color')
      } else {
        hideElement('fc_line_color')
      }
    })
    
    observeEvent(input$include_sig_line, ignoreNULL=TRUE, {
      if (input$include_sig_line) {
        showElement('sig_line_color')
      } else {
        hideElement('sig_line_color')
      }
    })
    
    observeEvent(input$label_strategy, ignoreNULL=TRUE, {
      if (input$label_strategy == 'None') {
        hideElement('label_size')
      } else {
        showElement('label_size')
      }
    })
    
    return(
      list(
        volcano = volcano_plot,
        ma = ma_plot
      )
    )
    
  })
}


library(shiny)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(DT)
library(shinycssloaders)
library(shinydashboard)
library(shinydashboardPlus)
library(scales)
library(fgsea)
library(clusterProfiler)
library(stringr)
library(shinythemes)
library(bslib)
library(ggrepel)
library(cyjShiny)
library(htmlwidgets)
library(graph)
library(jsonlite)
library(igraph)
library(colourpicker)

options(scipen=999)

ui = page_navbar(title='Enhanced Volcano Plot Explorer', id='nav_id', fillable=TRUE,
                 nav_panel("Input", inputPageUI('input_page')),
                 nav_panel("Result", dePageUI('DE_page')),
)

server <- function(input, output, session) {
  
  script_path = reactive({
    "/home/rstudio"
  })
  
  input_page = inputPageServer("input_page", script_path)
  de_page = dePageServer("DE_page", input_page$data)

}

options(shiny.host = "0.0.0.0", shiny.port = 8789, shiny.maxRequestSize=30*1024^2)
shinyApp(ui = ui, server = server)
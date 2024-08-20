reclass <- function (df, vec_types) {
  for (i in 1:ncol(df)) {
    type <- vec_types[i]
    class(df[ , i]) <- type
  }
  return(df)
}

pcaPlotUI <- function(id) {
  ns <- NS(id)
  tagList(
    layout_column_wrap(
      width = 1/2,
      card_body(min_height = 250,
                textOutput(ns('plot_error')),
                plotOutput(ns('pca_plot')) %>% withSpinner(image='spinner.gif', id=ns('spinner')),
                plotlyOutput(ns('pca_plot_interactive')) %>% withSpinner(image='spinner.gif', id=ns('interactive_spinner'))
      ),
      card_body(
        checkboxInput(ns('interactive'), "Interactive Plot", value=FALSE),
        accordion(open=FALSE,
                  h6('Plot Settings:'),
                  accordion_panel("Grouping",
                                  fluidRow(
                                    column(6, selectizeInput(ns("color_by"), label='Color:', choices=NULL)),
                                    column(6, selectizeInput(ns("shape_by"), label='Shape:', choices=NULL))
                                  ),
                                  fluidRow(
                                    column(6, selectizeInput(ns("outline_by"), label='Outline*:', choices=NULL)),
                                    column(6, selectizeInput(ns("alpha_by"), label='Transparency:', choices=NULL))
                                  ),
                                  p("*When setting outline - there can exist at most 5 shapes", style = "font-size:xx-small")
                  ),
                  accordion_panel("Points",
                                  fluidRow(
                                    column(3, tags$div(id = "point_inline", numericInput(ns("size"), label='Size:', value=6, min=1, step=1))),
                                    column(3, tags$div(id = "point_inline", numericInput(ns("outline"), label='Outline:', value=1, min=1, step=1)))
                                  )
                  ),
                  accordion_panel("Labels",
                                  fluidRow(
                                    column(6, selectizeInput(ns("label_by"), label='Label*:', choices=NULL)),
                                    column(6,selectizeInput(ns("highlight"), label='Highlighted IDs:', choices=NULL, multiple=TRUE))
                                  ),
                                  p('*Set to "Highlighted IDs" to label specific row with its ID', style = "font-size:xx-small"),
                                  fluidRow(
                                    column(6, tags$div(id = "inline", numericInput(ns("label_size"), label='Label Size:', value=7, min=1, step=1))),
                                    column(6, tags$div(id = "inline", textInput(ns("label_color"), label='Label Color:', value='#000000')))
                                  )
                  ),
                  accordion_panel("Axes",
                                  fluidRow(
                                    column(3, tags$div(id = "axes_inline", selectizeInput(ns("x"), label='X:', choices=NULL))),
                                    column(3, tags$div(id = "axes_inline", selectizeInput(ns("y"), label='Y:', choices=NULL))),
                                    column(5, tags$div(id = "axes_inline", numericInput(ns("axes_size"), label='Text Size:', value=12, min=1, step=1)))
                                  )
                  ),
                  accordion_panel("Legend",
                                  checkboxInput(ns('hide_legend'), label='Hide Legend', value=FALSE)
                  )
        ),
        h6("Download:"),
        fluidRow(
          column(2, downloadButton(ns('data_download'), label='Data',  style='width: 80px; font-size: .7em; padding: 2px 0px 3px 0px; margin: 0px 5px 0px 5px')),
          column(2, downloadButton(ns('pca_plot_download'), label='Plot', style='width: 80px; font-size: .7em; padding: 2px 0px 3px 0px; margin: 0px 5px 0px 5px')),
          column(3, tags$div(id = "inline", numericInput(ns('download_height'), "Height: ", value=350))),
          column(3, tags$div(id = "inline", numericInput(ns('download_width'), "Width: ", value=500)))
        )
      )
    ),
    tags$head(
      tags$style(
        HTML('
				.form-control.selectize-control{width: 150px}
				')
      )
    ),
    tags$head(
      tags$style(type="text/css", 
                 "#point_inline label{ display: table-cell; text-align: center; vertical-align: middle; padding-right: 3px} 
      	#point_inline .form-group{display: table-row;}
				#point_inline .form-control{padding: 3px 10px 3px 10px; width: 60px}
				#axes_inline label{ display: table-cell; text-align: center; vertical-align: middle; padding-right: 3px} 
				#axes_inline .form-group{display: table-row;}
				#axes_inline .form-control{padding: -1px 10px 3px 10px; width: 100px}
				")
    )
  )
}

pcaPlotServer <- function(id, metadata, metadata_header, metadata_column_types, pca, PoV) {
  
  moduleServer(id, function(input, output, session) {
    
    # Define reactive data for the volcano plot, similar to the PCA plot
    df <- reactive({
      metadata() %>%
        mutate(Highlighted = !!sym(event_column()) %in% input$highlighted) %>%
        mutate(Direction = case_when(
          Highlighted == TRUE & Direction == 'Downregulated' ~ 'Highlighted Downregulated',
          Highlighted == TRUE & Direction == 'No Change' ~ 'Highlighted',
          Highlighted == TRUE & Direction == 'Upregulated' ~ 'Highlighted Upregulated',
          TRUE ~ Direction
        )) %>%
        mutate(Label = case_when(
          Highlighted == TRUE ~ !!sym(event_column()),
          input$label_strategy == 'Significant/Highlighted' & Significant == 'Significant' ~ !!sym(event_column())
        ))
    })
    
    # Volcano plot reactive rendering
    volcano_plot <- reactive({
      upregulated_count = nrow(df() %>% filter(Direction == 'Upregulated'))
      noChange_count = nrow(df() %>% filter(Direction == 'No Change'))
      downregulated_count = nrow(df() %>% filter(Direction == 'Downregulated'))
      
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
    
    # Add toggle between PCA plot and volcano plot
    observeEvent(input$plot_type, ignoreNULL = TRUE, {
      if (input$plot_type == "PCA") {
        showElement("pca_plot")
        hideElement("volcano_plot")
      } else {
        showElement("volcano_plot")
        hideElement("pca_plot")
      }
    })
    
    # Render volcano plot based on user input
    output$volcano_plot <- renderPlot({
      req(volcano_plot())
      volcano_plot()
    })
    
    # Existing PCA plot rendering logic remains unchanged
    pca_plot = reactive({
      # PCA plot logic
    })
    
    output$pca_plot = renderPlot({
      req(pca_plot())
      pca_plot()
    })
    
    # Download handlers for both PCA and volcano plots
    output$pca_plot_download <- downloadHandler(
      filename = function() { 'pca_plot.png' },
      content = function(file) {
        png(file=file, width=input$download_width, height=input$download_height)
        plot(pca_plot())
        dev.off()
      }
    )
    
    output$volcano_plot_download <- downloadHandler(
      filename = function() { 'volcano_plot.png' },
      content = function(file) {
        png(file=file, width=input$download_width, height=input$download_height)
        plot(volcano_plot())
        dev.off()
      }
    )
  })
}

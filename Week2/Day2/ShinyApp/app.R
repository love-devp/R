#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(DESeq2)
library(ggplot2)
library(RColorBrewer)
library(gplots)

# Global variables
# Reading the data.
file <- "/Users/loved/OneDrive/Desktop/Intern/Day1/data.tsv"
# Read the data with row.names.
rsem <- read.table(file,sep="\t", header=TRUE, row.names=1)
# Creating the data structure for DESeq Analysis.
columns <- c("exper_rep1", "exper_rep2", "exper_rep3", "control_rep1", "control_rep2", "control_rep3")
data <- data.frame(rsem[, columns])
# Plot the average expression value of two conditions.
avgall <- cbind(rowSums(data[c("exper_rep1","exper_rep2","exper_rep3")])/3, 
                rowSums(data[c("control_rep1","control_rep2","control_rep3")])/3)
# Change the column names using colnames function.
colnames(avgall) <- c("Treat", "Control")
# Indicate the condition that each column in the table represent.
conds <- factor( c("Control","Control", "Control",
                   "Treat", "Treat","Treat") )
# Set p-value and log2FoldChange conditions.
de_res <- runDESeq(data, columns, conds,  padj=0.01, log2FoldChange=1)
overlaid_data <- overlaySig(gdat, de_res$res_selected)
# Add a pseudocount value 0.1 to generate Euclideans and Heatmaps
ld <- log2(data[rownames(de_res$res_selected),]+0.1)
# Scaling the value using their mean centers
cldt <- scale(t(ld), center=TRUE, scale=TRUE);
cld <- t(cldt)
# Set a vector for the select input.
graphs <- c("Scatter plot", "Histogram", "all2all", "Scatter plot with different",
            "MA plot", "Volcano Plot", "Euclidean", "Heatmap1", 
            "Correlation", "Heatmap2")

# Specify the application port
options(shiny.host = "0.0.0.0")
options(shiny.port = 8180)

ui <- fluidPage(
  selectInput("graphs", label = "list", choices = graphs),
  verbatimTextOutput("summary"),
  plotOutput("plot")
)

server <- function(input, output, session) {
  # Visualize the summary for each plots.
  output$summary <-renderPrint({
    if (input$graphs == "Scatter plot" || input$graphs == "Scatter plot with different"){
      summary(gdat)
    }
    else if (input$graphs == "Histogram"){
      summary(sumd)
    }
    else if (input$graphs == "all2all"){
      summary(data)
    }
    else if (input$graphs == "MA plot"){
      summary(de_res$res_detected)
    }
    else if (input$graphs == "Volcano Plot"){
      summary(de_res)
    }
    else { 
      summary(cld)
    }
  })
  output$plot <- renderPlot({
    if (input$graphs == "Scatter plot"){
      # Generate the scatter plot.
      gdat <- data.frame(avgall)
      ggplot() + geom_point(data=gdat, aes_string(x="Treat", y="Control"),
                            colour="black", alpha=6/10, size=3) +
        scale_x_log10() +scale_y_log10()
    }
    else if (input$graphs == "Histogram"){
      # Generate the Histogram.
      sumd <- rowSums(data)
      hist(log10(sumd), breaks=100)
    }
    else if (input$graphs == "all2all"){
      # Generate the all2all plot.
      all2all(data)
    }
    else if (input$graphs == "Scatter plot with different"){
      # Generate the Scatter plot.
      ggplot() +
        geom_point(data=overlaid_data, aes_string(x="Treat", y="Control",
                                                  colour="Legend"), alpha=6/10, size=3) +
        scale_colour_manual(values=c("All"="darkgrey","Significant"="red"))+
        scale_x_log10() +scale_y_log10()
    }
    else if (input$graphs == "MA plot"){
      # Generate the MA plot.
      plotMA(de_res$res_detected,ylim=c(-2,2),main="DESeq2");
    }
    else if (input$graphs == "Volcano Plot"){
      # Generate the Volcano plot.
      volcanoPlot(de_res,  padj=0.01, log2FoldChange=1)
    }
    else if (input$graphs == "Euclidean"){
      # Generate the Euclidean distance.
      distance<-dist(cldt, method = "euclidean")
      plot(hclust(distance, method = "complete"),
           main="Euclidean", xlab="")
    }
    else if (input$graphs == "Heatmap1"){
      # Generate the Heatmap.
      heatmap.2(cld, Rowv=TRUE,dendrogram="column",
                Colv=TRUE, col=redblue(256),labRow=NA,
                density.info="none",trace="none", cexCol=0.8);
    }
    else if (input$graphs == "Correlation"){
      # Generate the Correlation.
      dissimilarity <- 1 - cor(cld)
      distance <- as.dist(dissimilarity)
      plot(hclust(distance, method = "complete"),
           main="1-cor", xlab="")
    }
    else if (input$graphs == "Heatmap2"){
      # Generate the Heatmap.
      heatmap.2(cld, Rowv=TRUE,dendrogram="column",
                Colv=TRUE, col=redblue(256),labRow=NA,
                density.info="none",trace="none", cexCol=0.8,
                hclust=function(x) hclust(x,method="complete"),
                distfun=function(x) as.dist((1-cor(t(x)))/2))
    }
  })
}

shinyApp(ui, server)
# Load EnhancedVolcano
source('./R/EnhancedVolcano.R')

#

# Create a sample data frame
set.seed(123)
df <- data.frame(
  gene = paste0("Gene", 1:1000),
  log2FoldChange = rnorm(1000, 0, 2),
  pvalue = runif(1000, 0, 0.05)
)

# Add a column for -log10 p-value
df$negLog10Pvalue <- -log10(df$pvalue)

# Define cutoffs for significance
log2FC_cutoff <- 1
pvalue_cutoff <- 0.05

# Label the top 10 most significant genes
top_genes <- df[order(df$pvalue)[1:10], ]

# Create a volcano plot
ggplot(df, aes(x = log2FoldChange, y = negLog10Pvalue)) +
  geom_point(aes(color = (abs(log2FoldChange) > log2FC_cutoff & pvalue < pvalue_cutoff))) +
  geom_text(data = top_genes, aes(label = gene), vjust = 1, hjust = 1) +
  scale_color_manual(values = c("black", "red")) +
  labs(title = "Volcano Plot",
       x = "Log2 Fold Change",
       y = "-Log10 P-value") +
  theme_minimal()


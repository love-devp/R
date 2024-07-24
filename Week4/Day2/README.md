# The list of target Shiny apps
1. Gene module scoring (Z-score-based method) → couldn't find it
2. Pathway analysis (SCPA, sasGSEA) →　**ROGUE***, specifically Gene Set Enrichment Analysis under Advanced Analysis*
3. L-R and cell communication (cellchat, connectome, nichenetr) → **InterCellar**
4. Disease enrichment analysis  (using a publicly available or custom gmt file) -ClusterProfiler → **Nasqar2** , **clusterprofiler-ui-shiny** *←simple one*
5. Mapping to LINCs/chembl
6. Feature selection – variable importance scores

# What I found on the web
1. ShinyCell: This Shiny app is designed for visualizing single-cell data, including gene expression. It supports various visualizations such as UMAP, violin plots, and bubble plots, which can be used to display module scores. More details and the source code are available on its [GitHub](https://github.com/SGDDNB/ShinyCell) and You can explore examples of ShinyCell-generated apps [here](http://shinycell1.ddnetbio.com/).
2. **ROGUE**: This Shiny app focuses on RNA sequencing analysis and biomarker discovery. It includes tools for differential expression analysis, gene set enrichment analysis, and various visualization techniques such as PCA, t-SNE, and heatmaps. More details and the source code are available on its [GitHub](https://github.com/afarrel/ROGUE)​ and you can execute shiny app from [here](https://marisshiny.research.chop.edu/ROGUE/)​.
3. GraphBio: This Shiny app designed for omics data visualization, GraphBio includes numerous popular visualization methods like heatmaps, volcano plots, and PCA, which can be used to analyze and visualize gene expression and module scores. You can access GraphBio's [Github](https://github.com/databio2022/GraphBio)​, and execute shiny app from [here](http://www.graphbio1.com/en/).
4. **InterCellar**: This Shiny app is designed for interactive analysis of cell-cell communication from single-cell RNA-seq data. It allows users to explore precomputed ligand-receptor interactions, filter and annotate them, and visualize clusters, genes, and functions. You can explore more about InterCellar on [Bioconductor page](https://bioconductor.org/packages/release/bioc/html/InterCellar.html) and [Github](https://github.com/martaint/InterCellar)
5. **Nasqar2/ClusterProfShinyORA**: This shiny app allows users to perform Gene Set Enrichment Analysis (GSEA) using the ClusterProfiler package along with Shiny R libraries. It supports the analysis and visualization of functional profiles of genomic coordinates, genes, and gene clusters. Users can upload their differential gene expression (DGE) data from DESeq2 or import data from the upstream Deseq2Shiny app. You can access the shiny app from [here](http://nasqar2.abudhabi.nyu.edu/ClusterProfShinyORA/) or explore their [Github](https://github.com/nyuad-corebio/Nasqar2)
6. **clusterprofiler-ui-shiny**: This Shiny app is specifically developed for functional enrichment analysis using the ClusterProfiler R package. It facilitates the analysis and visualization of enrichment results, making it easier for users to interpret their data without extensive programming knowledge. You can explore their [Github](https://github.com/mitraak/clusterprofiler-ui-shiny/tree/master)

# Glossary

## Gene Module Scoring
	- Gene Modules: These are groups of genes that function together or are co-expressed. They might be related to specific biological processes, pathways, or disease states.
		・Purpose: The goal is to determine how active or expressed these gene modules are in different samples (e.g., different tissues, conditions, or time points).
	- How Does Z-Score-Based Scoring Work?
		・Data Collection: First, you need gene expression data, which shows how much each gene is expressed in each sample. This data is often obtained from experiments like RNA sequencing (RNA-seq) or microarrays.
		・Normalization: To ensure that the data from different samples can be compared, it’s normalized. This step adjusts for technical variations and ensures that the differences seen are biologically meaningful.
	- Why Use Z-Scores?
		・Standardization: Z-scores standardize the data, making it easier to compare gene expression levels across different genes and samples.
		・Interpretability: Z-scores indicate how much a gene’s expression deviates from the average, helping identify upregulated or downregulated genes in a sample.
		・Example
		Imagine you have a gene module related to inflammation consisting of three genes: A, B, and C. You have expression data for these genes in five samples. By calculating the Z-scores for each gene in each sample and then averaging these scores, you can see which samples show higher or lower activity of the inflammation-related gene module.
	- Summary
	Gene module scoring using the Z-score-based method is a powerful tool in bioinformatics for evaluating the activity of groups of genes. By converting expression data into standardized scores, researchers can easily identify and compare the activity of biological pathways across different conditions or treatments.

## Single-sample Gene Set Enrichment Analysis (sasGSEA)
	- Description: sasGSEA is a variation of the Gene Set Enrichment Analysis (GSEA) that evaluates the enrichment of predefined gene sets in a single sample. This method is useful for obtaining pathway scores for individual samples, rather than comparing groups of samples.
	- Features
	・Single-Sample Resolution: Unlike traditional GSEA, which compares two or more groups of samples, sasGSEA scores the enrichment of gene sets for each sample individually.
	・Flexibility: This method is particularly useful in clinical settings where sample sizes might be small, and each sample needs to be evaluated independently.

## L-R (Ligand-Receptor) Interactions
	- Ligands: Molecules such as hormones, neurotransmitters, or growth factors that bind to specific receptors on the surface of cells.
	- Receptors: Proteins on the cell surface or within cells that bind to ligands and initiate a cellular response.
	- L-R Interactions: The binding of a ligand to its receptor triggers a series of intracellular events, leading to a specific cellular response.
	- Scope:
		・Molecular Level: Focuses on the specific molecular interaction between a ligand and its receptor.
		・Specificity: Each ligand typically has a specific receptor or a set of receptors it can bind to, and this interaction is crucial for precise cellular responses.
		
## Cell Communication
	- Definition: The process by which cells convey information to each other to coordinate various functions. This can involve several types of signals, including chemical signals (like ligands), electrical signals, and mechanical signals.
	- Scope:
		・Systemic Level: Encompasses all forms of communication between cells, including but not limited to L-R interactions.
		・Diverse Mechanisms: Includes various signaling pathways, such as paracrine signaling (local signaling), autocrine signaling (self-signaling), endocrine signaling (hormones via bloodstream), and juxtacrine signaling (direct contact between neighboring cells).

## Disease Enrichment Analysis
	- Description: The purpose of Disease Enrichment Analysis is, to determine whether a set of genes (e.g., differentially expressed genes from an experiment) is significantly associated with specific diseases.
	- Method: Uses statistical tests to assess the over-representation of disease-associated genes within the input gene set compared to a background gene set.
	- ClusterProfiler: ClusterProfiler is an R package designed for functional enrichment analysis, including disease enrichment analysis. It supports various types of analysis and visualization for biological data.
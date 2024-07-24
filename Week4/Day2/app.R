install.packages("Holomics")

# Install devtools if it is not already installed
install.packages("devtools")
library(devtools)

# Install Holomics package 
install_github("MolinLab/Holomics")

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("mixOmics")
BiocManager::install("BiocParallel")

library(Holomics)
run_app()

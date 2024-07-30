FROM ubuntu:22.04
LABEL author="woonghee.lee001@umb.edu" description="Docker image containing all requirements for sig2lead"

# Use the official R base image
FROM rocker/r-ver:4.0.3

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libopenbabel-dev \
    openbabel \
    librsvg2-dev \
    libeigen3-dev

# Install R packages
RUN R -e "install.packages(c('shiny', 'shinyjs', 'ggplot2', 'plotly', 'httr', 'jsonlite', 'DT', 'visNetwork', 'plyr', 'dendextend', 'colorspace', 'ggforce', 'rlist', 'scatterpie', 'ggrepel', 'bazar', 'XML', 'RCurl', 'bitops', 'scrapeR', 'igraph', 'circlize', 'enrichR', 'readr', 'dplyr', 'gplots'), repos = 'https://packagemanager.rstudio.com/cran/__linux__/focal/2021-02-17')"

# Install Bioconductor packages
RUN R -e "if (!requireNamespace('BiocManager', quietly = TRUE)) install.packages('BiocManager'); \
BiocManager::install(version = '3.19'); \
BiocManager::install(c('ChemmineOB', 'ChemmineR'), force = TRUE, update = FALSE, ask = FALSE)"

# Copy the Shiny app files to the image
COPY . /srv/shiny-server/

# Expose the port where the app runs
EXPOSE 8789

# Run the Shiny app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host = '0.0.0.0', port = 8789)"]

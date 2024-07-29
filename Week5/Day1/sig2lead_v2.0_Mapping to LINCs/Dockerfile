FROM ubuntu:22.04
LABEL author="woonghee.lee001@umb.edu" description="Docker image containing all requirements for sig2lead"

# Use the official R base image
FROM rocker/r-ver:4.1.0

# Install system dependencies
RUN apt-get update && apt-get install -y \
	libopenbabel-dev \
	libeigen3-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev

# Install R packages
RUN R -e "install.packages(c('shiny', 'shinyjs', 'ggplot2', 'plotly', 'httr', 'jsonlite', 'DT', 'visNetwork', 'plyr', 'dendextend', 'colorspace', 'ggforce', 'rlist', 'scatterpie', 'ggrepel', 'bazar', 'XML', 'RCurl', 'bitops', 'scrapeR', 'igraph', 'circlize', 'enrichR', 'readr', 'dplyr', 'gplots'))"

# Install Bioconductor packages
RUN R -e "if (!requireNamespace('BiocManager', quietly=TRUE)) install.packages('BiocManager'); BiocManager::install(c('ChemmineOB', 'ChemmineR'), update=FALSE, ask=FALSE)"

# Copy the Shiny app files to the image
COPY . /srv/shiny-server/

# Expose the port where the app runs
EXPOSE 3838

# Run the Shiny app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host = '0.0.0.0', port = 3838)"]
